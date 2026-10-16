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
use crate::project::Package;
use crate::syntax::ast::{self, Ident};
use crate::syntax::parse_module;

pub struct LoadedModule {
    /// The module's source file in the [`SourceMap`].
    pub file: u32,
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
    pub c_sources: Vec<(String, String)>,
    /// System libraries to link: the packages' `link` settings and the `// link:` lines of stdlib modules.
    pub libs: Vec<String>,
}

/// `packages[0]` is the program itself; the rest are its (transitive) dependencies.
pub fn load(map: &mut SourceMap, entry: &Path, packages: Vec<Package>) -> Result<Loaded> {
    let text = std::fs::read_to_string(entry)
        .map_err(|e| Diagnostic::general(format!("cannot read {}: {e}", entry.display())))?;
    let entry_dir = entry.parent().map(Path::to_path_buf).unwrap_or_default();

    let mut loader = Loader {
        map,
        modules: Vec::new(),
        by_key: HashMap::new(),
        stack: Vec::new(),
        c_sources: Vec::new(),
        libs: Vec::new(),
        packages,
    };
    for lib in loader.packages.iter().flat_map(|p| p.link.clone()).collect::<Vec<_>>() {
        loader.link(lib);
    }
    // C files that packages bring along are compiled into the program.
    let bundled: Vec<PathBuf> = loader.packages.iter().flat_map(|p| p.c.clone()).collect();
    for (i, path) in bundled.iter().enumerate() {
        let text = std::fs::read_to_string(path)
            .map_err(|e| Diagnostic::general(format!("cannot read {}: {e}", path.display())))?;
        let base = path.file_name().map_or("code.c".into(), |n| n.to_string_lossy().into_owned());
        loader.c_sources.push((format!("pkg{i}_{base}"), text));
    }

    let (prelude, prelude_c) = embedded::stdlib_module("prelude").expect("stdlib/prelude.aion exists");
    if let Some(c) = prelude_c {
        loader.c_sources.push(("prelude.c".into(), c.to_string()));
    }
    loader.add("std:prelude".into(), "<std>/prelude.aion", prelude, "prelude".into(), None, 0)?;

    let key = std::fs::canonicalize(entry).unwrap_or_else(|_| entry.to_path_buf());
    let stem = entry.file_stem().map_or("main".into(), |s| s.to_string_lossy().into_owned());
    loader.add(key.display().to_string(), &entry.display().to_string(), &text, stem, Some(entry_dir), 0)?;

    Ok(Loaded { modules: loader.modules, c_sources: loader.c_sources, libs: loader.libs })
}

struct Loader<'a> {
    map: &'a mut SourceMap,
    modules: Vec<LoadedModule>,
    by_key: HashMap<String, usize>,
    /// Keys of modules currently being loaded, for cycle detection.
    stack: Vec<String>,
    c_sources: Vec<(String, String)>,
    libs: Vec<String>,
    packages: Vec<Package>,
}

impl Loader<'_> {
    /// Parse a module and everything it imports, then register it.
    /// `dir` is where its own relative imports are searched (`None` for stdlib);
    /// `pkg` is the package it belongs to, which decides what dependencies it sees.
    fn add(
        &mut self,
        key: String,
        display: &str,
        text: &str,
        name: String,
        dir: Option<PathBuf>,
        pkg: usize,
    ) -> Result<usize> {
        let file = self.map.add(display, text);
        let ast = parse_module(file, text)?;

        self.stack.push(key.clone());
        let mut imports = Vec::new();
        for imp in &ast.imports {
            imports.extend(self.resolve(imp, dir.as_deref(), pkg)?);
        }
        self.stack.pop();

        let name = self.unique_name(&name);
        self.modules.push(LoadedModule { file, name, ast, imports });
        let idx = self.modules.len() - 1;
        self.by_key.insert(key, idx);
        Ok(idx)
    }

    fn unique_name(&self, wanted: &str) -> String {
        let clean: String = wanted.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
        if self.modules.iter().all(|m| m.name != clean) { clean } else { format!("{clean}_{}", self.modules.len()) }
    }

    fn resolve(&mut self, imp: &ast::Import, dir: Option<&Path>, pkg: usize) -> Result<Vec<ResolvedImport>> {
        let segs: Vec<&str> = imp.path.iter().map(|i| i.name.as_str()).collect();
        let (module, consumed) = match imp.base {
            ast::ImportBase::Bare => self.find_module(&segs, imp, dir, pkg)?,
            base => self.find_anchored(base, &segs, imp, dir, pkg)?,
        };
        let alias = imp.alias.as_ref().map_or_else(|| segs[consumed - 1].to_string(), |a| a.name.clone());
        let make = |alias, symbol| ResolvedImport { module, alias, symbol, span: imp.span };

        match &imp.names {
            // `from m import a, b`
            Some(names) => {
                if consumed != segs.len() {
                    return Err(self.not_found(&segs, dir, pkg, imp.span));
                }
                Ok(names.iter().map(|n| make(None, Some(n.clone()))).collect())
            }
            // `import m` / `import m.Name`
            None => match segs.len() - consumed {
                0 => Ok(vec![make(Some(alias), None)]),
                1 => Ok(vec![make(Some(alias), Some(imp.path[consumed].clone()))]),
                _ => Err(self.not_found(&segs, dir, pkg, imp.span)),
            },
        }
    }

    /// Load the module an import names.  Returns its index and how many leading
    /// path segments formed the module path (any remainder names an item in it).
    /// `import @a.b` (from the package root) and `import .a` / `..a` (from this file's folder, going up).
    fn find_anchored(
        &mut self,
        base: ast::ImportBase,
        segs: &[&str],
        imp: &ast::Import,
        dir: Option<&Path>,
        pkg: usize,
    ) -> Result<(usize, usize)> {
        let root = self.packages[pkg].root.clone();
        let (start, shown) = match base {
            ast::ImportBase::Relative(up) => {
                let mut start = dir.map(Path::to_path_buf).unwrap_or_else(|| root.clone());
                for _ in 0..up {
                    start = start.parent().map(Path::to_path_buf).unwrap_or(start);
                }
                let inside = std::fs::canonicalize(&start)
                    .ok()
                    .zip(std::fs::canonicalize(&root).ok())
                    .is_none_or(|(s, r)| s.starts_with(r));
                if !inside {
                    return Err(Diagnostic::new("a relative import cannot leave the package", imp.span)
                        .hint(format!("the package starts at {}", root.display())));
                }
                (start, "relative to this file")
            }
            _ => (root, "the package root"),
        };
        match find_entry(&start, segs) {
            Some((path, n)) => Ok((self.load_file(&path, pkg)?, n)),
            None => Err(Diagnostic::new(format!("cannot find module `{}`", segs.join(".")), imp.span).hint(format!(
                "looked for {rel}.aion and {rel}/lib.aion in {} ({shown})",
                start.display(),
                rel = segs.join("/")
            ))),
        }
    }

    fn find_module(&mut self, segs: &[&str], imp: &ast::Import, dir: Option<&Path>, pkg: usize) -> Result<(usize, usize)> {
        // `std.math` / `aion.math`: explicitly the standard library.
        if matches!(segs[0], "std" | "aion") {
            if segs.len() < 2 {
                return Err(Diagnostic::new("expected `std.<module>`", imp.span));
            }
            return Ok((self.load_std(segs[1], imp.path[1].span)?, 2));
        }
        if let Some(dir) = dir {
            // A file next to the importer wins…
            if let Some((path, n)) = self.find_file(segs, dir, pkg) {
                return Ok((self.load_file(&path, pkg)?, n));
            }
            // …then a declared dependency…
            if let Some(&dep) = self.packages[pkg].deps.get(segs[0]) {
                return self.find_in_package(dep, segs, imp);
            }
        }
        // …then the standard library.
        if embedded::stdlib_module(segs[0]).is_some() {
            return Ok((self.load_std(segs[0], imp.path[0].span)?, 1));
        }
        Err(self.not_found(segs, dir, pkg, imp.span))
    }

    /// `import dep`, `import dep.sub.mod`, `import dep.Name` where `dep` is a dependency.
    fn find_in_package(&mut self, dep: usize, segs: &[&str], imp: &ast::Import) -> Result<(usize, usize)> {
        let rest = &segs[1..];
        let root = self.packages[dep].root.clone();
        if !rest.is_empty()
            && let Some((path, n)) = self.find_file(rest, &root, dep)
        {
            return Ok((self.load_file(&path, dep)?, 1 + n));
        }
        match self.packages[dep].lib.clone() {
            Some(lib) if lib.is_file() && rest.len() <= 1 => Ok((self.load_file(&lib, dep)?, 1)),
            _ => Err(Diagnostic::new(format!("dependency `{}` has no module `{}`", segs[0], rest.join(".")), imp.span)),
        }
    }

    fn link(&mut self, lib: String) {
        if !self.libs.contains(&lib) {
            self.libs.push(lib);
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
            self.c_sources.push((format!("{name}.c"), c.to_string()));
        }
        // `// link: sqlite3 z` at the top of a module names the system libraries its C code needs.
        for lib in aion.lines().take_while(|l| l.starts_with("//")).filter_map(|l| l.strip_prefix("// link:")) {
            for name in lib.split_whitespace() {
                self.link(name.to_string());
            }
        }
        self.add(key, &format!("<std>/{name}.aion"), aion, name.to_string(), None, 0)
    }

    fn load_file(&mut self, path: &Path, pkg: usize) -> Result<usize> {
        let key = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf()).display().to_string();
        if self.stack.contains(&key) {
            return Err(Diagnostic::general(format!("circular import of {}", path.display())));
        }
        if let Some(&i) = self.by_key.get(&key) {
            return Ok(i);
        }
        let text = std::fs::read_to_string(path)
            .map_err(|e| Diagnostic::general(format!("cannot read {}: {e}", path.display())))?;
        let mut stem = path.file_stem().map_or("module".into(), |s| s.to_string_lossy().into_owned());
        // a folder package's `lib.aion` is named after the folder
        if stem == "lib"
            && let Some(folder) = path.parent().and_then(|p| std::fs::canonicalize(p).ok()).and_then(|p| p.file_name().map(|n| n.to_string_lossy().into_owned()))
        {
            stem = folder;
        }
        self.add(key, &path.display().to_string(), &text, stem, path.parent().map(Path::to_path_buf), pkg)
    }

    /// Where `import x` looks for `x.aion`: beside the importer and in the
    /// package root, each also under `lib/`.
    fn search_dirs(&self, dir: &Path, pkg: usize) -> Vec<PathBuf> {
        let mut dirs = Vec::new();
        for base in [dir, self.packages[pkg].root.as_path()] {
            for d in [base.to_path_buf(), base.join("lib")] {
                if !dirs.contains(&d) {
                    dirs.push(d);
                }
            }
        }
        dirs
    }

    fn find_file(&self, segs: &[&str], dir: &Path, pkg: usize) -> Option<(PathBuf, usize)> {
        self.search_dirs(dir, pkg).into_iter().find_map(|d| find_entry(&d, segs))
    }

    fn not_found(&self, segs: &[&str], dir: Option<&Path>, pkg: usize, span: Span) -> Diagnostic {
        let rel = format!("{}.aion", segs.join("/"));
        let mut places: Vec<String> = dir
            .map(|d| self.search_dirs(d, pkg).iter().map(|d| d.join(&rel).display().to_string()).collect())
            .unwrap_or_default();
        let deps: Vec<&str> = self.packages[pkg].deps.keys().map(String::as_str).collect();
        if !deps.is_empty() {
            places.push(format!("dependencies ({})", deps.join(", ")));
        }
        places.push(format!("the standard library ({})", embedded::stdlib_names().join(", ")));
        Diagnostic::new(format!("cannot find module `{}`", segs.join(".")), span)
            .hint(format!("looked in:\n  {}", places.join("\n  ")))
    }
}

/// The module `segs` names below `base`: the longest prefix that is a file `a/b.aion`, or a folder
/// `a/b/` whose entry is `lib.aion` (else `b.aion` inside it).  Returns the file and how many
/// segments it consumed.
fn find_entry(base: &Path, segs: &[&str]) -> Option<(PathBuf, usize)> {
    (1..=segs.len()).rev().find_map(|n| {
        let dir = segs[..n].iter().fold(base.to_path_buf(), |p, s| p.join(s));
        let file = dir.with_extension("aion");
        if file.is_file() {
            return Some((file, n));
        }
        ["lib.aion".to_string(), format!("{}.aion", segs[n - 1])]
            .iter()
            .map(|f| dir.join(f))
            .find(|p| p.is_file())
            .map(|p| (p, n))
    })
}
