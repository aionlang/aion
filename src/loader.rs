//! Finds, reads and parses every module a program imports.
//!
//! Import forms:
//!
//! | Import                              | Meaning                                         |
//! |-------------------------------------|-------------------------------------------------|
//! | `import utils`                      | `utils.aion` next to the importer or in `lib/`  |
//! | `import math`                       | the standard-library module, if no file shadows it |
//! | `import sockets.TcpStream`          | …and use `TcpStream` unqualified                |
//! | `from sockets import TcpStream, TcpClient` | only those names, unqualified            |
//! | `import utils as u`                 | refer to the module as `u`                      |
//! | `import std.math`                   | explicitly the standard library (`aion.` too)   |
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
    /// How code refers to the module (`math` in `math.sqrt(2.0)`); `None` for
    /// `from m import …`, which binds only the names.
    pub alias: Option<String>,
    /// `Some` brings that item of the module into scope unqualified.
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
            imports.extend(self.resolve(imp, dir.as_deref())?);
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

    fn resolve(&mut self, imp: &ast::Import, dir: Option<&Path>) -> Result<Vec<ResolvedImport>> {
        let segs: Vec<&str> = imp.path.iter().map(|i| i.name.as_str()).collect();
        let (module, consumed) = self.find_module(&segs, imp, dir)?;
        let alias = imp.alias.as_ref().map_or_else(|| segs[consumed - 1].to_string(), |a| a.name.clone());
        let make = |alias, symbol| ResolvedImport { module, alias, symbol, span: imp.span };

        match &imp.names {
            // `from m import a, b`
            Some(names) => {
                if consumed != segs.len() {
                    return Err(self.not_found(&segs, dir, imp.span));
                }
                Ok(names.iter().map(|n| make(None, Some(n.clone()))).collect())
            }
            // `import m` / `import m.Name`
            None => match segs.len() - consumed {
                0 => Ok(vec![make(Some(alias), None)]),
                1 => Ok(vec![make(Some(alias), Some(imp.path[consumed].clone()))]),
                _ => Err(self.not_found(&segs, dir, imp.span)),
            },
        }
    }

    /// Load the module an import names.  Returns its index and how many leading
    /// path segments formed the module path (any remainder names an item in it).
    fn find_module(&mut self, segs: &[&str], imp: &ast::Import, dir: Option<&Path>) -> Result<(usize, usize)> {
        // `std.math` / `aion.math`: explicitly the standard library.
        if matches!(segs[0], "std" | "aion") {
            if segs.len() < 2 {
                return Err(Diagnostic::new("expected `std.<module>`", imp.span));
            }
            return Ok((self.load_std(segs[1], imp.path[1].span)?, 2));
        }
        // A user file wins over a standard-library module of the same name.
        if let Some(dir) = dir {
            for n in (1..=segs.len()).rev() {
                if let Some(path) = self.find_file(&segs[..n], dir) {
                    return Ok((self.load_file(&path)?, n));
                }
            }
        }
        if embedded::stdlib_module(segs[0]).is_some() {
            return Ok((self.load_std(segs[0], imp.path[0].span)?, 1));
        }
        match dir {
            Some(dir) => Err(self.not_found(segs, Some(dir), imp.span)),
            None => Err(Diagnostic::new(format!("cannot find module `{}`", segs.join(".")), imp.span)),
        }
    }

    fn load_std(&mut self, name: &str, span: Span) -> Result<usize> {
        let key = format!("std:{name}");
        if let Some(&i) = self.by_key.get(&key) {
            return Ok(i);
        }
        let Some((aion, c)) = embedded::stdlib_module(name) else {
            return Err(Diagnostic::new(format!("unknown standard-library module `{name}`"), span)
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

    fn not_found(&self, segs: &[&str], dir: Option<&Path>, span: Span) -> Diagnostic {
        let rel = format!("{}.aion", segs.join("/"));
        let mut places: Vec<String> = dir
            .map(|d| self.search_dirs(d).iter().map(|d| d.join(&rel).display().to_string()).collect())
            .unwrap_or_default();
        places.push(format!("the standard library ({})", embedded::stdlib_names().join(", ")));
        Diagnostic::new(format!("cannot find module `{}`", segs.join(".")), span)
            .hint(format!("looked in:\n  {}", places.join("\n  ")))
    }
}
