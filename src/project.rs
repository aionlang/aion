//! `aion.project`: a program's name, entry point and dependencies.
//!
//! ```text
//! name = "myapp"
//! version = "0.1.0"
//! entry = "main.aion"             # default
//! lib = "src/lib.aion"            # what `import myapp` loads when used as a dependency
//! link = ["sqlite3"]              # system libraries (`-lsqlite3`) the C code of this package needs
//! c = ["native/fast.c"]           # C files compiled into every program that uses this package
//!
//! [dependencies]
//! utils = { path = "../utils" }
//! http  = { git = "https://github.com/me/http", tag = "v1.2" }
//! ```
//!
//! Git dependencies are cloned into the cache and pinned to an exact commit in
//! `aion.lock` next to the project file.  With `packages = "aion-dependencies"` they are put in that folder of the
//! project instead (one folder per package and commit, without the `.git` directory).

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::cache;
use crate::diag::{Diagnostic, Result};

pub const FILE_NAME: &str = "aion.project";
pub const LOCK_NAME: &str = "aion.lock";
/// Written into a folder of downloaded packages, so that `aion fmt` and `aion test` know to leave it alone.
pub const PACKAGES_MARKER: &str = ".aion-packages";

#[derive(Debug)]
pub struct Project {
    /// Directory containing `aion.project`.
    pub root: PathBuf,
    pub name: String,
    pub entry: PathBuf,
    pub lib: Option<PathBuf>,
    /// System libraries to link (`-l<name>`).
    pub link: Vec<String>,
    /// C source files (inside the project) compiled into the program.
    pub c: Vec<PathBuf>,
    /// `packages`: the folder git dependencies are downloaded into (default: the shared cache).
    pub packages: Option<PathBuf>,
    pub deps: Vec<Dependency>,
}

#[derive(Debug)]
pub struct Dependency {
    pub name: String,
    pub source: Source,
}

#[derive(Debug)]
pub enum Source {
    Path(PathBuf),
    Git { url: String, spec: GitRef },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitRef {
    Default,
    Tag(String),
    Branch(String),
    Rev(String),
}

impl GitRef {
    pub fn describe(&self) -> String {
        match self {
            GitRef::Default => "HEAD".into(),
            GitRef::Tag(t) => format!("tag:{t}"),
            GitRef::Branch(b) => format!("branch:{b}"),
            GitRef::Rev(r) => format!("rev:{r}"),
        }
    }
}

/// A resolved package: where its files live and which packages it depends on.
#[derive(Debug)]
pub struct Package {
    pub root: PathBuf,
    /// How the dependency was asked for, for `aion tree` ("git https://… @ tag:v1 (9fceb02)").
    pub source: String,
    /// The package's `aion.project`, if it has one.
    pub manifest: Option<PathBuf>,
    /// The module `import <dependency-name>` loads.
    pub lib: Option<PathBuf>,
    /// System libraries this package's C code needs.
    pub link: Vec<String>,
    /// C source files of the package.
    pub c: Vec<PathBuf>,
    /// Dependency name → index into the package list.
    pub deps: HashMap<String, usize>,
}

/// Is `name` fit to follow `-l`?  (No spaces or leading dashes: it ends up on a command line.)
pub fn valid_library(name: &str) -> bool {
    !name.is_empty()
        && !name.starts_with('-')
        && name.chars().all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '+' | '.'))
}

/// Find the nearest `aion.project` in `start` or its ancestors, never looking at or above the home folder
/// (a stray `~/aion.project` must not capture every script below it).
pub fn find(start: &Path) -> Option<PathBuf> {
    let home = std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")).map(PathBuf::from);
    find_from(start, home.as_deref())
}

pub fn find_from(start: &Path, home: Option<&Path>) -> Option<PathBuf> {
    let start = std::fs::canonicalize(start).ok()?;
    let home = home.and_then(|h| std::fs::canonicalize(h).ok());
    let below_home = home.as_ref().is_some_and(|h| start.starts_with(h) && start != *h);
    start
        .ancestors()
        .take_while(|d| !(below_home && Some(*d) == home.as_deref()))
        .filter(|d| d.parent().is_some() || *d == start)
        .map(|d| d.join(FILE_NAME))
        .find(|p| p.is_file())
}

/// `--project`: an `aion.project` file, or the folder that holds it.
pub fn explicit(path: &Path) -> Result<Project> {
    let file = if path.is_dir() { path.join(FILE_NAME) } else { path.to_path_buf() };
    if !file.is_file() {
        return Err(Diagnostic::general(format!("no {FILE_NAME} at {}", path.display())));
    }
    load(&file)
}

pub fn load(file: &Path) -> Result<Project> {
    let text = std::fs::read_to_string(file)
        .map_err(|e| Diagnostic::general(format!("cannot read {}: {e}", file.display())))?;
    let root = file.parent().map(Path::to_path_buf).unwrap_or_default();
    let toml = parse(&text).map_err(|(line, msg)| {
        Diagnostic::general(format!("{}:{line}: {msg}", file.display()))
    })?;
    let err = |msg: String| Diagnostic::general(format!("{}: {msg}", file.display()));

    let string = |key: &str| -> Result<Option<String>> {
        match toml.top.iter().find(|(k, _, _)| k == key) {
            None => Ok(None),
            Some((_, Value::Str(s), _)) => Ok(Some(s.clone())),
            Some((_, _, line)) => Err(Diagnostic::general(format!("{}:{line}: `{key}` must be a string", file.display()))),
        }
    };
    for (k, _, line) in &toml.top {
        if !["name", "version", "entry", "lib", "link", "c", "packages"].contains(&k.as_str()) {
            return Err(Diagnostic::general(format!("{}:{line}: unknown key `{k}`", file.display())));
        }
    }

    let name = string("name")?.ok_or_else(|| err("missing `name`".into()))?;
    let entry = root.join(string("entry")?.unwrap_or_else(|| "main.aion".into()));
    let lib = string("lib")?.map(|l| root.join(l));
    let packages = match string("packages")? {
        Some(p) if p.trim().is_empty() => return Err(err("`packages` must name a folder, like \"aion-dependencies\"".into())),
        other => other.map(|p| root.join(p)),
    };
    let link = match toml.top.iter().find(|(k, _, _)| k == "link") {
        None => Vec::new(),
        Some((_, Value::List(names), line)) => {
            if let Some(bad) = names.iter().find(|n| !valid_library(n)) {
                return Err(Diagnostic::general(format!("{}:{line}: `{bad}` is not a library name", file.display())));
            }
            names.clone()
        }
        Some((_, _, line)) => {
            return Err(Diagnostic::general(format!("{}:{line}: `link` must be a list: [\"sqlite3\"]", file.display())));
        }
    };

    let c = match toml.top.iter().find(|(k, _, _)| k == "c") {
        None => Vec::new(),
        Some((_, Value::List(files), line)) => {
            let mut paths = Vec::new();
            for f in files {
                let path = Path::new(f);
                let inside = path.is_relative() && !path.components().any(|c| matches!(c, std::path::Component::ParentDir));
                if !inside || path.extension().is_none_or(|e| e != "c") {
                    return Err(Diagnostic::general(format!(
                        "{}:{line}: `{f}` must be a .c file inside the project (a relative path without `..`)",
                        file.display()
                    )));
                }
                paths.push(root.join(path));
            }
            paths
        }
        Some((_, _, line)) => {
            return Err(Diagnostic::general(format!("{}:{line}: `c` must be a list: [\"native/fast.c\"]", file.display())));
        }
    };

    let mut deps = Vec::new();
    for (section, entries) in &toml.sections {
        if section != "dependencies" {
            return Err(err(format!("unknown section `[{section}]`")));
        }
        for (dep, value, line) in entries {
            let here = |m: String| Diagnostic::general(format!("{}:{line}: {m}", file.display()));
            let Value::Table(fields) = value else {
                return Err(here(format!("`{dep}` must look like {{ path = \"…\" }} or {{ git = \"…\" }}")));
            };
            let get = |k: &str| fields.iter().find(|(fk, _)| fk == k).map(|(_, v)| v.clone());
            for (k, _) in fields {
                if !["path", "git", "tag", "branch", "rev"].contains(&k.as_str()) {
                    return Err(here(format!("unknown key `{k}` in dependency `{dep}`")));
                }
            }
            let source = match (get("path"), get("git")) {
                (Some(p), None) => Source::Path(root.join(p)),
                (None, Some(url)) => {
                    let spec = match (get("tag"), get("branch"), get("rev")) {
                        (None, None, None) => GitRef::Default,
                        (Some(t), None, None) => GitRef::Tag(t),
                        (None, Some(b), None) => GitRef::Branch(b),
                        (None, None, Some(r)) => GitRef::Rev(r),
                        _ => return Err(here(format!("`{dep}`: use only one of tag, branch, rev"))),
                    };
                    Source::Git { url, spec }
                }
                _ => return Err(here(format!("`{dep}` needs exactly one of `path` or `git`"))),
            };
            deps.push(Dependency { name: dep.clone(), source });
        }
    }
    Ok(Project { root, name, entry, lib, link, c, packages, deps })
}

// ── dependencies written in a script ────────────────────────────────

const GIT_HOSTS: [&str; 4] = ["github.com", "gitlab.com", "codeberg.org", "bitbucket.org"];

/// Read a dependency source as written after `aion add` or in a `// dep:` line: a path
/// (`../utils`), or a git repository with an optional `@ref` (`github.com/me/http@v1.2`,
/// `https://host/x.git@branch:dev`).  Relative paths are taken from `base`.
pub fn parse_source(spec: &str, base: &Path) -> std::result::Result<Source, String> {
    let spec = spec.trim();
    if spec.is_empty() {
        return Err("expected a path or a git repository".into());
    }
    if spec.starts_with("./") || spec.starts_with("../") || spec.starts_with('/') || spec == "." || spec == ".." {
        return Ok(Source::Path(base.join(spec)));
    }
    if let Some(rest) = spec.strip_prefix("~/") {
        let home = std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")).ok_or("cannot find your home folder")?;
        return Ok(Source::Path(PathBuf::from(home).join(rest)));
    }
    // `@ref` is the part after the last `@` that follows the last `/` (so `git@host:x/y` is safe)
    let slash = spec.rfind('/').map_or(0, |i| i + 1);
    let (url, spec_ref) = match spec[slash..].rfind('@') {
        Some(at) => (&spec[..slash + at], Some(&spec[slash + at + 1..])),
        None => (spec, None),
    };
    let url = if url.contains("://") || url.starts_with("git@") {
        url.to_string()
    } else if GIT_HOSTS.iter().any(|h| url.starts_with(&format!("{h}/"))) {
        format!("https://{url}")
    } else {
        return Err(format!("`{spec}` is neither a path (./x, ../x) nor a git repository (github.com/user/repo)"));
    };
    let git_ref = match spec_ref {
        None => GitRef::Default,
        Some("") => return Err("empty `@` reference".into()),
        Some(r) => match r.split_once(':') {
            Some(("tag", v)) => GitRef::Tag(v.into()),
            Some(("branch", v)) => GitRef::Branch(v.into()),
            Some(("rev", v)) => GitRef::Rev(v.into()),
            _ => GitRef::Tag(r.into()),
        },
    };
    Ok(Source::Git { url, spec: git_ref })
}

/// `// dep: name = source` lines in the leading comments of a script.
pub fn inline_deps(text: &str, script_dir: &Path, file: &Path) -> Result<Vec<Dependency>> {
    let mut deps = Vec::new();
    for (i, line) in text.lines().enumerate() {
        let t = line.trim();
        if t.is_empty() || t.starts_with("#!") {
            continue;
        }
        let Some(comment) = t.strip_prefix("//") else { break };
        let Some(body) = comment.trim().strip_prefix("dep:") else { continue };
        let here = |m: String| Diagnostic::general(format!("{}:{}: {m}", file.display(), i + 1));
        let Some((name, source)) = body.split_once('=') else {
            return Err(here("expected `// dep: name = source`".into()));
        };
        let name = name.trim();
        if name.is_empty() || !name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
            return Err(here(format!("`{name}` is not a usable dependency name")));
        }
        let source = parse_source(source, script_dir).map_err(here)?;
        deps.push(Dependency { name: name.to_string(), source });
    }
    Ok(deps)
}

// ── resolving dependencies ──────────────────────────────────────────

/// Fetch every dependency (cloning git ones) and build the package list.
/// Package 0 is the program itself; `entry_dir` is used when there is no project.
///
/// `extra` are a loose script's inline dependencies and `lock` where their pins live
/// (default: `aion.lock` in the root).
pub fn resolve(
    project: Option<&Project>,
    entry_dir: &Path,
    extra: &[Dependency],
    lock: Option<&Path>,
) -> Result<Vec<Package>> {
    let root = project.map_or_else(|| entry_dir.to_path_buf(), |p| p.root.clone());
    let lock_path = lock.map_or_else(|| root.join(LOCK_NAME), Path::to_path_buf);
    let mut r = Resolver {
        packages: vec![Package {
            root: root.clone(),
            source: String::new(),
            manifest: project.map(|p| p.root.join(FILE_NAME)),
            lib: project.and_then(|p| p.lib.clone()),
            link: project.map(|p| p.link.clone()).unwrap_or_default(),
            c: project.map(|p| p.c.clone()).unwrap_or_default(),
            deps: HashMap::new(),
        }],
        by_root: HashMap::new(),
        lock: Lock::read(&lock_path),
        lock_changed: false,
        store: project.and_then(|p| p.packages.clone()),
    };
    r.by_root.insert(std::fs::canonicalize(&root).unwrap_or(root.clone()), 0);
    if let Some(p) = project {
        r.add_deps(0, &p.deps)?;
    }
    r.add_deps(0, extra)?;
    if r.lock_changed {
        r.lock.write(&lock_path)?;
    }
    Ok(r.packages)
}

struct Resolver {
    packages: Vec<Package>,
    by_root: HashMap<PathBuf, usize>,
    lock: Lock,
    lock_changed: bool,
    /// The project's `packages` folder, where git dependencies go instead of the cache.
    store: Option<PathBuf>,
}

impl Resolver {
    fn add_deps(&mut self, owner: usize, deps: &[Dependency]) -> Result<()> {
        for dep in deps {
            let dir = match &dep.source {
                Source::Path(p) => {
                    std::fs::canonicalize(p).map_err(|e| {
                        Diagnostic::general(format!("dependency `{}`: cannot use {}: {e}", dep.name, p.display()))
                    })?
                }
                Source::Git { url, spec } => self.fetch_git(&dep.name, url, spec)?,
            };
            let idx = match self.by_root.get(&dir) {
                Some(&i) => i,
                None => {
                    let sub = dir.join(FILE_NAME).is_file().then(|| load(&dir.join(FILE_NAME))).transpose()?;
                    let lib = sub.as_ref().and_then(|p| p.lib.clone()).or_else(|| default_lib(&dir, &dep.name));
                    let idx = self.packages.len();
                    let link = sub.as_ref().map(|p| p.link.clone()).unwrap_or_default();
                    let c = sub.as_ref().map(|p| p.c.clone()).unwrap_or_default();
                    let manifest = sub.as_ref().map(|_| dir.join(FILE_NAME));
                    let source = match &dep.source {
                        Source::Path(_) => format!("path {}", dir.display()),
                        Source::Git { url, spec } => {
                            let commit = dir.file_name().map_or(String::new(), |n| n.to_string_lossy().chars().take(7).collect());
                            format!("git {url} @ {} ({commit})", spec.describe())
                        }
                    };
                    self.packages.push(Package { root: dir.clone(), source, manifest, lib, link, c, deps: HashMap::new() });
                    self.by_root.insert(dir, idx);
                    if let Some(sub) = sub {
                        self.add_deps(idx, &sub.deps)?;
                    }
                    idx
                }
            };
            self.packages[owner].deps.insert(dep.name.clone(), idx);
        }
        Ok(())
    }

    /// Make sure `url@spec` is at an exact commit, in the project's `packages` folder if it has one and in the cache
    /// otherwise; return its directory.
    fn fetch_git(&mut self, name: &str, url: &str, spec: &GitRef) -> Result<PathBuf> {
        let key = format!("{url}#{}", spec.describe());
        let repo_dir = cache::dir().join("git").join(format!("{}-{:016x}", sanitize(name), cache::hash(&[url.as_bytes()])));
        let store = self.store.clone();
        // Where the files of one commit live: `aion-dependencies/http-9fceb02a41c3` or `<cache>/git/http-<hash>/<commit>`.
        let place = |commit: &str| match &store {
            Some(store) => store.join(format!("{}-{}", sanitize(name), commit.get(..12).unwrap_or(commit))),
            None => repo_dir.join(commit),
        };
        let git = |dir: &Path, args: &[&str]| -> Result<String> {
            let out = Command::new("git").arg("-C").arg(dir).args(args).output().map_err(|e| {
                Diagnostic::general(format!("dependency `{name}`: cannot run `git`: {e}"))
                    .hint("git dependencies need git on PATH")
            })?;
            if !out.status.success() {
                return Err(Diagnostic::general(format!(
                    "dependency `{name}`: `git {}` failed:\n{}",
                    args.join(" "),
                    String::from_utf8_lossy(&out.stderr).trim_end()
                )));
            }
            Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
        };

        if let Some(commit) = self.lock.get(&key) {
            let dest = place(commit);
            if dest.is_dir() {
                return Ok(dest);
            }
        }

        // The clone is made next to where it ends up, so that moving it into place is a rename.
        let staging = store.clone().unwrap_or_else(|| repo_dir.clone());
        std::fs::create_dir_all(&staging)
            .map_err(|e| Diagnostic::general(format!("cannot create {}: {e}", staging.display())))?;
        if store.is_some() {
            let marker = staging.join(PACKAGES_MARKER);
            if !marker.exists() {
                let _ = std::fs::write(&marker, "Packages downloaded by `aion install` (see `packages` in aion.project).\n");
            }
        }
        let tmp = staging.join(format!("tmp-{}", std::process::id()));
        remove_tree(&tmp);
        eprintln!("fetching {name} from {url}");
        let cloned = (|| -> Result<String> {
            git(&staging, &["clone", "--quiet", url, tmp.to_str().unwrap()])?;
            let wanted = match (self.lock.get(&key), spec) {
                (Some(c), _) => Some(c.to_string()),
                (None, GitRef::Default) => None,
                (None, GitRef::Tag(r) | GitRef::Branch(r) | GitRef::Rev(r)) => Some(r.clone()),
            };
            if let Some(rev) = wanted {
                git(&tmp, &["checkout", "--quiet", &rev])?;
            }
            git(&tmp, &["rev-parse", "HEAD"])
        })();
        let commit = match cloned {
            Ok(commit) => commit,
            Err(e) => {
                remove_tree(&tmp);
                return Err(e);
            }
        };
        let dest = place(&commit);
        if dest.is_dir() {
            remove_tree(&tmp);
        } else {
            if store.is_some() {
                // a copy in the project is just the files; the history stays in git
                remove_tree(&tmp.join(".git"));
            }
            std::fs::rename(&tmp, &dest)
                .map_err(|e| Diagnostic::general(format!("cannot move the clone into place: {e}")))?;
        }
        if self.lock.get(&key) != Some(commit.as_str()) {
            self.lock.set(key, commit);
            self.lock_changed = true;
        }
        Ok(dest)
    }
}

/// Delete a directory tree, including the read-only files git leaves in `.git` (Windows will not delete those).
fn remove_tree(path: &Path) {
    #[cfg(windows)]
    {
        #[allow(clippy::permissions_set_readonly_false)]
        fn writable(dir: &Path) {
            let Ok(entries) = std::fs::read_dir(dir) else { return };
            for entry in entries.flatten() {
                let (path, Ok(meta)) = (entry.path(), entry.metadata()) else { continue };
                if meta.is_dir() {
                    writable(&path);
                } else if meta.permissions().readonly() {
                    let mut permissions = meta.permissions();
                    permissions.set_readonly(false);
                    let _ = std::fs::set_permissions(&path, permissions);
                }
            }
        }
        writable(path);
    }
    let _ = std::fs::remove_dir_all(path);
}

/// `lib.aion`, then `<name>.aion`, in the package root.
fn default_lib(root: &Path, name: &str) -> Option<PathBuf> {
    ["lib.aion".to_string(), format!("{name}.aion")].iter().map(|f| root.join(f)).find(|p| p.is_file())
}

fn sanitize(s: &str) -> String {
    s.chars().map(|c| if c.is_ascii_alphanumeric() || c == '-' || c == '_' { c } else { '_' }).collect()
}

/// Forget the pinned commits of these dependencies in the lock file, so the next resolve fetches
/// them again.  Returns how many pins were dropped.
pub fn unlock(deps: &[Dependency], lock_path: &Path) -> Result<usize> {
    let mut lock = Lock::read(lock_path);
    let mut dropped = 0;
    for d in deps {
        if let Source::Git { url, spec } = &d.source
            && lock.remove(&format!("{url}#{}", spec.describe()))
        {
            dropped += 1;
        }
    }
    if dropped > 0 {
        lock.write(lock_path)?;
    }
    Ok(dropped)
}

// ── lock file ───────────────────────────────────────────────────────

#[derive(Default)]
struct Lock {
    entries: Vec<(String, String)>,
}

impl Lock {
    fn read(path: &Path) -> Lock {
        let Ok(text) = std::fs::read_to_string(path) else { return Lock::default() };
        let Ok(toml) = parse(&text) else { return Lock::default() };
        Lock {
            entries: toml
                .top
                .into_iter()
                .filter_map(|(k, v, _)| if let Value::Str(s) = v { Some((k, s)) } else { None })
                .collect(),
        }
    }

    fn get(&self, key: &str) -> Option<&str> {
        self.entries.iter().find(|(k, _)| k == key).map(|(_, v)| v.as_str())
    }

    fn remove(&mut self, key: &str) -> bool {
        let before = self.entries.len();
        self.entries.retain(|(k, _)| k != key);
        self.entries.len() != before
    }

    fn set(&mut self, key: String, commit: String) {
        match self.entries.iter_mut().find(|(k, _)| *k == key) {
            Some(e) => e.1 = commit,
            None => self.entries.push((key, commit)),
        }
    }

    fn write(&self, path: &Path) -> Result<()> {
        let mut text = String::from("# Generated by aion; pins git dependencies to exact commits.\n");
        for (k, v) in &self.entries {
            text.push_str(&format!("\"{k}\" = \"{v}\"\n"));
        }
        std::fs::write(path, text).map_err(|e| Diagnostic::general(format!("cannot write {}: {e}", path.display())))
    }
}

// ── a tiny TOML subset ──────────────────────────────────────────────

#[derive(Clone, Debug)]
enum Value {
    Str(String),
    List(Vec<String>),
    Table(Vec<(String, String)>),
}

/// `(key, value, line number)`
type Entry = (String, Value, usize);

#[derive(Default)]
struct Toml {
    /// Entries before any `[section]`.
    top: Vec<Entry>,
    sections: Vec<(String, Vec<Entry>)>,
}

/// Parse `key = "string"`, `key = { k = "v", … }`, `[section]` and `# comments`.
fn parse(text: &str) -> std::result::Result<Toml, (usize, String)> {
    let mut toml = Toml::default();
    for (i, raw) in text.lines().enumerate() {
        let line_no = i + 1;
        let line = strip_comment(raw).trim();
        if line.is_empty() {
            continue;
        }
        if let Some(name) = line.strip_prefix('[') {
            let name = name.strip_suffix(']').ok_or((line_no, "expected `]`".to_string()))?;
            toml.sections.push((name.trim().to_string(), Vec::new()));
            continue;
        }
        let (key, rest) = line.split_once('=').ok_or((line_no, "expected `key = value`".to_string()))?;
        let key = unquote(key.trim()).map_err(|m| (line_no, m))?;
        let value = parse_value(rest.trim()).map_err(|m| (line_no, m))?;
        match toml.sections.last_mut() {
            Some((_, entries)) => entries.push((key, value, line_no)),
            None => toml.top.push((key, value, line_no)),
        }
    }
    Ok(toml)
}

/// Drop a trailing `# comment` that isn't inside quotes.
fn strip_comment(line: &str) -> &str {
    let mut in_str = false;
    for (i, c) in line.char_indices() {
        match c {
            '"' => in_str = !in_str,
            '#' if !in_str => return &line[..i],
            _ => {}
        }
    }
    line
}

fn unquote(s: &str) -> std::result::Result<String, String> {
    match s.strip_prefix('"') {
        Some(rest) => rest.strip_suffix('"').map(str::to_string).ok_or_else(|| "unterminated string".to_string()),
        None if s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-') && !s.is_empty() => Ok(s.to_string()),
        None => Err(format!("invalid key `{s}`")),
    }
}

fn parse_value(s: &str) -> std::result::Result<Value, String> {
    if let Some(inner) = s.strip_prefix('[') {
        let inner = inner.strip_suffix(']').ok_or("expected `]`")?;
        let items = split_commas(inner)
            .into_iter()
            .filter(|p| !p.trim().is_empty())
            .map(|p| unquote_string(p.trim()))
            .collect::<std::result::Result<Vec<_>, _>>()?;
        return Ok(Value::List(items));
    }
    if let Some(inner) = s.strip_prefix('{') {
        let inner = inner.strip_suffix('}').ok_or("expected `}`")?;
        let mut fields = Vec::new();
        for piece in split_commas(inner).into_iter().filter(|p| !p.trim().is_empty()) {
            let (k, v) = piece.split_once('=').ok_or("expected `key = \"value\"`")?;
            let Value::Str(v) = parse_value(v.trim())? else { return Err("nested tables are not supported".into()) };
            fields.push((unquote(k.trim())?, v));
        }
        return Ok(Value::Table(fields));
    }
    unquote_string(s).map(Value::Str)
}

/// Split on the commas that aren't inside quotes.
fn split_commas(s: &str) -> Vec<&str> {
    let (mut in_quote, mut start) = (false, 0);
    let mut pieces = Vec::new();
    for (i, c) in s.char_indices() {
        match c {
            '"' => in_quote = !in_quote,
            ',' if !in_quote => {
                pieces.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    pieces.push(&s[start..]);
    pieces
}

fn unquote_string(s: &str) -> std::result::Result<String, String> {
    s.strip_prefix('"')
        .and_then(|r| r.strip_suffix('"'))
        .map(str::to_string)
        .ok_or_else(|| format!("expected a quoted string, found `{s}`"))
}

#[cfg(test)]
mod tests {
    #[test]
    fn dependency_sources_are_read_from_short_forms() {
        let base = Path::new("/work");
        let git = |spec: &str| match parse_source(spec, base).unwrap() {
            Source::Git { url, spec } => (url, spec),
            Source::Path(p) => panic!("expected git, got {p:?}"),
        };
        assert_eq!(git("github.com/me/http"), ("https://github.com/me/http".into(), GitRef::Default));
        assert_eq!(git("github.com/me/http@v1.2"), ("https://github.com/me/http".into(), GitRef::Tag("v1.2".into())));
        assert_eq!(git("https://h.io/x.git@branch:dev"), ("https://h.io/x.git".into(), GitRef::Branch("dev".into())));
        assert_eq!(git("git@github.com:me/x.git"), ("git@github.com:me/x.git".into(), GitRef::Default));
        assert_eq!(git("git@github.com:me/x.git@rev:abc"), ("git@github.com:me/x.git".into(), GitRef::Rev("abc".into())));
        assert!(matches!(parse_source("../utils", base).unwrap(), Source::Path(p) if p == Path::new("/work/../utils")));
        assert!(parse_source("http", base).is_err());
        assert!(parse_source("github.com/me/x@", base).is_err());
    }

    #[test]
    fn dep_lines_are_read_from_the_header_only() {
        let text = "#!/usr/bin/env aion\n\n// A tool.\n// dep: http = github.com/me/http@v1\n// dep: utils = ../utils\nprintln(1)\n// dep: late = ../late\n";
        let deps = inline_deps(text, Path::new("/w"), Path::new("t.aion")).unwrap();
        let names: Vec<&str> = deps.iter().map(|d| d.name.as_str()).collect();
        assert_eq!(names, ["http", "utils"]);
        let err = inline_deps("// dep: oops\n", Path::new("/w"), Path::new("t.aion")).unwrap_err();
        assert!(err.message.contains("t.aion:1: expected `// dep: name = source`"), "{}", err.message);
    }

    use super::*;

    #[test]
    fn parses_a_project_file() {
        let t = parse(
            "# my app\nname = \"app\"  # trailing comment\nversion = \"0.1\"\n\n[dependencies]\n\
             utils = { path = \"../utils\" }\nhttp = { git = \"https://x/y#z\", tag = \"v1\" }\n",
        )
        .unwrap();
        assert_eq!(t.top.len(), 2);
        assert_eq!(t.sections.len(), 1);
        let (name, entries) = &t.sections[0];
        assert_eq!(name, "dependencies");
        assert_eq!(entries.len(), 2);
        let Value::Table(fields) = &entries[1].1 else { panic!() };
        assert_eq!(fields[0], ("git".into(), "https://x/y#z".into()));
        assert_eq!(fields[1], ("tag".into(), "v1".into()));
    }

    #[test]
    fn parses_lists() {
        let t = parse("link = [\"sqlite3\", \"m\"]\nnone = []\n").unwrap();
        let Value::List(libs) = &t.top[0].1 else { panic!("expected a list") };
        assert_eq!(libs, &["sqlite3", "m"]);
        let Value::List(empty) = &t.top[1].1 else { panic!("expected a list") };
        assert!(empty.is_empty());
        assert!(parse("link = [\"a\"").is_err());
        assert!(valid_library("sqlite3") && valid_library("stdc++") && !valid_library("-lx") && !valid_library("a b"));
    }

    #[test]
    fn reports_the_line_of_a_syntax_error() {
        let (line, msg) = parse("name = \"a\"\nbroken line\n").map(|_| ()).unwrap_err();
        assert_eq!(line, 2);
        assert!(msg.contains("key = value"));
    }

    #[test]
    fn hashing_is_stable_and_separates_parts() {
        assert_eq!(cache::hash(&[b"ab", b"c"]), cache::hash(&[b"ab", b"c"]));
        assert_ne!(cache::hash(&[b"ab", b"c"]), cache::hash(&[b"a", b"bc"]));
    }
}
