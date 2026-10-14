//! Finds, reads and parses every module a program imports.
//!
//! Import forms:
//!
//! | Import                          | Meaning                                   |
//! |---------------------------------|-------------------------------------------|
//! | `import std.math;`              | embedded stdlib module (`aion.` is alias) |
//! | `import std.sockets.TcpStream;` | …and use `TcpStream` unqualified          |
//! | `import utils;`                 | `utils.aion` next to the importer or in `lib/` |
//!
//! Modules come back in dependency order: the prelude first, the entry last.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::diag::{Diagnostic, Result, SourceMap, Span};
use crate::embedded;
use crate::syntax::ast::{self, Ident};
use crate::syntax::parse_module;

pub struct LoadedModule {
    /// Unique, symbol-safe name used as the prefix of generated symbols.
    pub name: String,
    pub ast: ast::Module,
    pub imports: Vec<ResolvedImport>,
}

pub struct ResolvedImport {
    /// Index into [`Loaded::modules`].
    pub module: usize,
    /// How code refers to the module: `math` in `math.sqrt(2.0)`.
    pub alias: String,
    /// `Some` for `import m.Symbol;` — brings `Symbol` into scope unqualified.
    pub symbol: Option<Ident>,
    pub span: Span,
}

pub struct Loaded {
    pub modules: Vec<LoadedModule>,
    /// `(file name, source)` of the C half of every imported stdlib module.
    pub c_sources: Vec<(String, &'static str)>,
}

pub fn load(map: &mut SourceMap, entry: &Path) -> Result<Loaded> {
    let text = std::fs::read_to_string(entry)
        .map_err(|e| Diagnostic::general(format!("cannot read {}: {e}", entry.display())))?;
    let entry_dir = entry.parent().map(Path::to_path_buf).unwrap_or_default();

    let mut loader = Loader {
        map,
        modules: Vec::new(),
        by_key: HashMap::new(),
        stack: Vec::new(),
        c_sources: Vec::new(),
        entry_dir: entry_dir.clone(),
    };

    let (prelude, _) = embedded::stdlib_module("prelude").expect("stdlib/prelude.aion exists");
    loader.add("std:prelude".into(), "<std>/prelude.aion", prelude, "prelude".into(), None)?;

    let key = std::fs::canonicalize(entry).unwrap_or_else(|_| entry.to_path_buf());
    let stem = entry.file_stem().map_or("main".into(), |s| s.to_string_lossy().into_owned());
    loader.add(key.display().to_string(), &entry.display().to_string(), &text, stem, Some(entry_dir))?;

    Ok(Loaded { modules: loader.modules, c_sources: loader.c_sources })
}

struct Loader<'a> {
    map: &'a mut SourceMap,
    modules: Vec<LoadedModule>,
    by_key: HashMap<String, usize>,
    /// Keys of modules currently being loaded, for cycle detection.
    stack: Vec<String>,
    c_sources: Vec<(String, &'static str)>,
    entry_dir: PathBuf,
}

impl Loader<'_> {
    /// Parse a module and everything it imports, then register it.
    /// `dir` is where its own relative imports are searched (`None` for stdlib).
    fn add(&mut self, key: String, display: &str, text: &str, name: String, dir: Option<PathBuf>) -> Result<usize> {
        let file = self.map.add(display, text);
        let ast = parse_module(file, text)?;

        self.stack.push(key.clone());
        let mut imports = Vec::new();
        for imp in &ast.imports {
            imports.push(self.resolve(imp, dir.as_deref())?);
        }
        self.stack.pop();

        let name = self.unique_name(&name);
        self.modules.push(LoadedModule { name, ast, imports });
        let idx = self.modules.len() - 1;
        self.by_key.insert(key, idx);
        Ok(idx)
    }

    fn unique_name(&self, wanted: &str) -> String {
        let clean: String = wanted.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
        if self.modules.iter().all(|m| m.name != clean) { clean } else { format!("{clean}_{}", self.modules.len()) }
    }

    fn resolve(&mut self, imp: &ast::Import, dir: Option<&Path>) -> Result<ResolvedImport> {
        let segs: Vec<&str> = imp.path.iter().map(|i| i.name.as_str()).collect();
        let (module, alias, symbol) = if matches!(segs[0], "std" | "aion") {
            if segs.len() < 2 || segs.len() > 3 {
                return Err(Diagnostic::new("expected `std.<module>` or `std.<module>.<Name>`", imp.span));
            }
            (self.load_std(segs[1], imp)?, segs[1].to_string(), imp.path.get(2).cloned())
        } else {
            let Some(dir) = dir else {
                return Err(Diagnostic::new("standard-library modules cannot import user modules", imp.span));
            };
            // Try the whole path as a module first, then "module + symbol".
            let (n, symbol) = match self.find_file(&segs, dir) {
                Some(_) => (segs.len(), None),
                None if segs.len() > 1 && self.find_file(&segs[..segs.len() - 1], dir).is_some() => {
                    (segs.len() - 1, imp.path.last().cloned())
                }
                None => return Err(self.not_found(&segs, dir, imp.span)),
            };
            let path = self.find_file(&segs[..n], dir).expect("checked above");
            (self.load_file(&path)?, segs[n - 1].to_string(), symbol)
        };
        Ok(ResolvedImport { module, alias, symbol, span: imp.span })
    }

    fn load_std(&mut self, name: &str, imp: &ast::Import) -> Result<usize> {
        let key = format!("std:{name}");
        if let Some(&i) = self.by_key.get(&key) {
            return Ok(i);
        }
        let Some((aion, c)) = embedded::stdlib_module(name) else {
            return Err(Diagnostic::new(format!("unknown standard-library module `{name}`"), imp.path[1].span)
                .hint(format!("available: {}", embedded::stdlib_names().join(", "))));
        };
        if let Some(c) = c {
            self.c_sources.push((format!("{name}.c"), c));
        }
        self.add(key, &format!("<std>/{name}.aion"), aion, name.to_string(), None)
    }

    fn load_file(&mut self, path: &Path) -> Result<usize> {
        let key = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf()).display().to_string();
        if self.stack.contains(&key) {
            return Err(Diagnostic::general(format!("circular import of {}", path.display())));
        }
        if let Some(&i) = self.by_key.get(&key) {
            return Ok(i);
        }
        let text = std::fs::read_to_string(path)
            .map_err(|e| Diagnostic::general(format!("cannot read {}: {e}", path.display())))?;
        let stem = path.file_stem().map_or("module".into(), |s| s.to_string_lossy().into_owned());
        self.add(key, &path.display().to_string(), &text, stem, path.parent().map(Path::to_path_buf))
    }

    fn search_dirs(&self, dir: &Path) -> Vec<PathBuf> {
        let mut dirs = Vec::new();
        for base in [dir, self.entry_dir.as_path()] {
            for d in [base.to_path_buf(), base.join("lib")] {
                if !dirs.contains(&d) {
                    dirs.push(d);
                }
            }
        }
        dirs
    }

    fn find_file(&self, segs: &[&str], dir: &Path) -> Option<PathBuf> {
        let rel = format!("{}.aion", segs.join("/"));
        self.search_dirs(dir).into_iter().map(|d| d.join(&rel)).find(|p| p.is_file())
    }

    fn not_found(&self, segs: &[&str], dir: &Path, span: Span) -> Diagnostic {
        let rel = format!("{}.aion", segs.join("/"));
        let searched: Vec<String> =
            self.search_dirs(dir).iter().map(|d| d.join(&rel).display().to_string()).collect();
        Diagnostic::new(format!("cannot find module `{}`", segs.join(".")), span)
            .hint(format!("looked in:\n  {}", searched.join("\n  ")))
    }
}
