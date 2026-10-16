//! `aion add`, `aion remove`, `aion update` and `aion tree`: managing dependencies from the command line.
//!
//! A dependency lives either in a project's `aion.project` or, for a loose script, in a `// dep:` line at the
//! top of the script.  The editing functions here are pure (text in, text out) so they can be tested.

use std::io::{BufRead, IsTerminal, Write};
use std::path::{Component, Path, PathBuf};

use crate::diag::Diagnostic;
use crate::project::{self, Dependency, GitRef, Source};

// ── questions ───────────────────────────────────────────────────────

/// Asks yes/no questions; the real one only asks when a person can answer.
pub trait Prompt {
    fn confirm(&mut self, question: &str, default_yes: bool) -> bool;
}

pub struct Terminal {
    /// `--yes`: take every default without asking.
    pub yes: bool,
}

impl Prompt for Terminal {
    fn confirm(&mut self, question: &str, default_yes: bool) -> bool {
        if self.yes || !std::io::stdin().is_terminal() {
            return default_yes;
        }
        eprint!("{question} [{}] ", if default_yes { "Y/n" } else { "y/N" });
        let _ = std::io::stderr().flush();
        let mut line = String::new();
        if std::io::stdin().lock().read_line(&mut line).is_err() {
            return default_yes;
        }
        match line.trim().to_lowercase().as_str() {
            "" => default_yes,
            "y" | "yes" => true,
            _ => false,
        }
    }
}

// ── command line ────────────────────────────────────────────────────

#[derive(Default, Debug)]
pub struct Opts {
    pub positional: Vec<String>,
    pub name: Option<String>,
    pub tag: Option<String>,
    pub branch: Option<String>,
    pub rev: Option<String>,
    pub script: Option<PathBuf>,
    pub project: Option<PathBuf>,
    pub yes: bool,
}

pub fn parse_opts(args: &[String]) -> Result<Opts, String> {
    let mut o = Opts::default();
    let mut it = args.iter();
    while let Some(a) = it.next() {
        let mut value = |what: &str| it.next().cloned().ok_or_else(|| format!("`{what}` needs a value"));
        match a.as_str() {
            "--as" => o.name = Some(value("--as")?),
            "--tag" => o.tag = Some(value("--tag")?),
            "--branch" => o.branch = Some(value("--branch")?),
            "--rev" => o.rev = Some(value("--rev")?),
            "--script" => o.script = Some(PathBuf::from(value("--script")?)),
            "--project" => o.project = Some(PathBuf::from(value("--project")?)),
            "--yes" | "-y" => o.yes = true,
            f if f.starts_with('-') => return Err(format!("unknown option `{f}`")),
            _ => o.positional.push(a.clone()),
        }
    }
    Ok(o)
}

// ── pure helpers ────────────────────────────────────────────────────

/// A dependency's name when `--as` is not given: the repository or folder name, without `.git` or `aion-`.
pub fn default_name(source: &Source) -> String {
    let raw = match source {
        Source::Git { url, .. } => url.rsplit(['/', ':']).find(|s| !s.is_empty()).unwrap_or("dep").to_string(),
        Source::Path(p) => normalize(p).file_name().map_or("dep".into(), |n| n.to_string_lossy().into_owned()),
    };
    let raw = raw.strip_suffix(".git").unwrap_or(&raw);
    let raw = raw.strip_prefix("aion-").unwrap_or(raw);
    let clean: String = raw.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
    if clean.is_empty() || clean.starts_with(|c: char| c.is_ascii_digit()) { format!("dep_{clean}") } else { clean }
}

/// `a/./b/../c` → `a/c`, without touching the file system.
fn normalize(p: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for c in p.components() {
        match c {
            Component::CurDir => {}
            Component::ParentDir if matches!(out.components().next_back(), Some(Component::Normal(_))) => {
                out.pop();
            }
            other => out.push(other),
        }
    }
    out
}

/// `p` written relative to `base` (`../utils`, `libs/x`), or absolute if they share nothing.
fn relative(p: &Path, base: &Path) -> String {
    let (p, base) = (normalize(p), normalize(base));
    let (pc, bc): (Vec<_>, Vec<_>) = (p.components().collect(), base.components().collect());
    let common = pc.iter().zip(&bc).take_while(|(a, b)| a == b).count();
    if common == 0 {
        return p.display().to_string();
    }
    let mut parts: Vec<String> = vec!["..".into(); bc.len() - common];
    parts.extend(pc[common..].iter().map(|c| c.as_os_str().to_string_lossy().into_owned()));
    if parts.is_empty() { ".".into() } else { parts.join("/") }
}

/// A source as `// dep:` lines and `aion add` write it.
pub fn source_text(source: &Source, base: &Path) -> String {
    match source {
        Source::Path(p) => {
            let r = relative(p, base);
            if r.starts_with('.') || r.starts_with('/') { r } else { format!("./{r}") }
        }
        Source::Git { url, spec } => match spec {
            GitRef::Default => url.clone(),
            GitRef::Tag(t) => format!("{url}@tag:{t}"),
            GitRef::Branch(b) => format!("{url}@branch:{b}"),
            GitRef::Rev(r) => format!("{url}@rev:{r}"),
        },
    }
}

/// The right-hand side of a line in `[dependencies]`.
pub fn manifest_value(source: &Source, root: &Path) -> String {
    match source {
        Source::Path(p) => format!("{{ path = \"{}\" }}", relative(p, root)),
        Source::Git { url, spec } => match spec {
            GitRef::Default => format!("{{ git = \"{url}\" }}"),
            GitRef::Tag(t) => format!("{{ git = \"{url}\", tag = \"{t}\" }}"),
            GitRef::Branch(b) => format!("{{ git = \"{url}\", branch = \"{b}\" }}"),
            GitRef::Rev(r) => format!("{{ git = \"{url}\", rev = \"{r}\" }}"),
        },
    }
}

fn is_entry_of(line: &str, name: &str) -> bool {
    let t = line.trim_start();
    t.strip_prefix(name).is_some_and(|rest| rest.trim_start().starts_with('='))
}

/// The lines of the `[dependencies]` section: (index of the header, index one past the last entry).
fn dependencies_section(lines: &[String]) -> Option<(usize, usize)> {
    let start = lines.iter().position(|l| l.trim() == "[dependencies]")?;
    let mut end = start + 1;
    for (i, l) in lines.iter().enumerate().skip(start + 1) {
        let t = l.trim();
        if t.starts_with('[') {
            break;
        }
        if !t.is_empty() && !t.starts_with('#') {
            end = i + 1;
        }
    }
    Some((start, end))
}

fn join_lines(lines: &[String]) -> String {
    let mut s = lines.join("\n");
    s.push('\n');
    s
}

/// `aion.project` with `name = value` in `[dependencies]` (replacing an entry of that name).
pub fn add_to_manifest(text: &str, name: &str, value: &str) -> String {
    let mut lines: Vec<String> = text.lines().map(String::from).collect();
    let entry = format!("{name} = {value}");
    match dependencies_section(&lines) {
        Some((start, end)) => {
            match (start + 1..end).find(|&i| is_entry_of(&lines[i], name)) {
                Some(i) => lines[i] = entry,
                None => lines.insert(end, entry),
            }
        }
        None => {
            if lines.last().is_some_and(|l| !l.trim().is_empty()) {
                lines.push(String::new());
            }
            lines.push("[dependencies]".into());
            lines.push(entry);
        }
    }
    join_lines(&lines)
}

/// `aion.project` without the dependency `name`; `None` if it has no such entry.
pub fn remove_from_manifest(text: &str, name: &str) -> Option<String> {
    let mut lines: Vec<String> = text.lines().map(String::from).collect();
    let (start, end) = dependencies_section(&lines)?;
    let i = (start + 1..end).find(|&i| is_entry_of(&lines[i], name))?;
    lines.remove(i);
    Some(join_lines(&lines))
}

/// Where the header of a script ends: the blank lines, `#!` line and `//` comments it starts with
/// (not counting blank lines at the end of that run).
fn header_end(lines: &[String]) -> usize {
    let mut end = 0;
    for (i, l) in lines.iter().enumerate() {
        let t = l.trim();
        if t.is_empty() {
            continue;
        }
        if t.starts_with("#!") || t.starts_with("//") {
            end = i + 1;
        } else {
            break;
        }
    }
    end
}

fn dep_line_name(line: &str) -> Option<&str> {
    let body = line.trim().strip_prefix("//")?.trim().strip_prefix("dep:")?;
    Some(body.split_once('=')?.0.trim())
}

/// A script with `// dep: name = source` in its header (replacing a line of that name).
pub fn add_to_script(text: &str, name: &str, source: &str) -> String {
    let mut lines: Vec<String> = text.lines().map(String::from).collect();
    let entry = format!("// dep: {name} = {source}");
    let header = header_end(&lines);
    if let Some(i) = (0..header).find(|&i| dep_line_name(&lines[i]) == Some(name)) {
        lines[i] = entry;
    } else if let Some(i) = (0..header).rev().find(|&i| dep_line_name(&lines[i]).is_some()) {
        lines.insert(i + 1, entry);
    } else if header > 0 {
        lines.insert(header, entry);
    } else {
        lines.insert(0, entry);
        if lines.len() > 1 && !lines[1].trim().is_empty() {
            lines.insert(1, String::new());
        }
    }
    join_lines(&lines)
}

pub fn remove_from_script(text: &str, name: &str) -> Option<String> {
    let mut lines: Vec<String> = text.lines().map(String::from).collect();
    let header = header_end(&lines);
    let i = (0..header).find(|&i| dep_line_name(&lines[i]) == Some(name))?;
    lines.remove(i);
    Some(join_lines(&lines))
}

// ── the commands ────────────────────────────────────────────────────

/// Where a dependency is written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Target {
    /// An `aion.project` file.
    Manifest(PathBuf),
    /// A script with `// dep:` lines.
    Script(PathBuf),
}

fn io_err(what: &str, path: &Path, e: std::io::Error) -> String {
    format!("cannot {what} {}: {e}", path.display())
}

fn write_atomically(path: &Path, text: &str) -> Result<(), String> {
    let mut tmp = path.as_os_str().to_owned();
    tmp.push(".tmp");
    let tmp = PathBuf::from(tmp);
    std::fs::write(&tmp, text).map_err(|e| io_err("write", &tmp, e))?;
    std::fs::rename(&tmp, path).map_err(|e| io_err("replace", path, e))
}

fn levels_up(from: &Path, file: &Path) -> usize {
    let dir = file.parent().unwrap_or(file);
    from.ancestors().position(|a| a == dir).unwrap_or(0)
}

/// Decide where `add` (and friends) work: the script, `--project`, or the nearest project, asking when unsure.
pub fn choose_target(
    opts: &Opts,
    cwd: &Path,
    home: Option<&Path>,
    prompt: &mut dyn Prompt,
    may_create: bool,
) -> Result<Target, String> {
    if let Some(s) = &opts.script {
        return Ok(Target::Script(s.clone()));
    }
    if let Some(p) = &opts.project {
        let file = if p.is_dir() { p.join(project::FILE_NAME) } else { p.clone() };
        return if file.is_file() { Ok(Target::Manifest(file)) } else { Err(format!("no {} at {}", project::FILE_NAME, p.display())) };
    }
    let cwd = std::fs::canonicalize(cwd).map_err(|e| io_err("read", cwd, e))?;
    match project::find_from(&cwd, home) {
        Some(file) if file.parent() == Some(cwd.as_path()) => Ok(Target::Manifest(file)),
        Some(file) => {
            let up = levels_up(&cwd, &file);
            let q = format!("Found {} ({up} level{} up). Use it?", file.display(), if up == 1 { "" } else { "s" });
            if prompt.confirm(&q, true) {
                Ok(Target::Manifest(file))
            } else {
                Err("nothing changed (use --script FILE to keep the dependency in a script)".into())
            }
        }
        None if may_create => {
            let q = "No aion.project in this folder or above. Create one here? (or use --script FILE)";
            if prompt.confirm(q, true) {
                Ok(Target::Manifest(cwd.join(project::FILE_NAME)))
            } else {
                Err("nothing changed (use --script FILE to keep the dependency in a script)".into())
            }
        }
        None => Err("no aion.project in this folder or above (use --script FILE for a script)".into()),
    }
}

fn source_from(opts: &Opts, base: &Path) -> Result<Source, String> {
    let spec = opts.positional.first().ok_or("usage: aion add <source> [--as NAME] [--tag T | --branch B | --rev R]")?;
    let source = project::parse_source(spec, base)?;
    let flags = [&opts.tag, &opts.branch, &opts.rev].iter().filter(|f| f.is_some()).count();
    if flags > 1 {
        return Err("use only one of --tag, --branch, --rev".into());
    }
    match source {
        Source::Git { url, spec } => {
            let spec = match (&opts.tag, &opts.branch, &opts.rev) {
                (Some(t), ..) => GitRef::Tag(t.clone()),
                (_, Some(b), _) => GitRef::Branch(b.clone()),
                (.., Some(r)) => GitRef::Rev(r.clone()),
                _ => spec,
            };
            Ok(Source::Git { url, spec })
        }
        path if flags == 0 => Ok(path),
        _ => Err("--tag, --branch and --rev only apply to git repositories".into()),
    }
}

/// The dependencies a target declares now.
fn current(target: &Target) -> Result<Vec<Dependency>, String> {
    match target {
        Target::Manifest(f) if f.is_file() => Ok(project::load(f).map_err(|d| d.message)?.deps),
        Target::Manifest(_) => Ok(Vec::new()),
        Target::Script(s) => {
            let text = std::fs::read_to_string(s).map_err(|e| io_err("read", s, e))?;
            let dir = s.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
            project::inline_deps(&text, dir, s).map_err(|d| d.message)
        }
    }
}

fn lock_path(target: &Target) -> Option<PathBuf> {
    match target {
        Target::Manifest(_) => None,
        Target::Script(s) => {
            let mut p = s.as_os_str().to_owned();
            p.push(".lock");
            Some(PathBuf::from(p))
        }
    }
}

/// Fetch everything the target depends on (checks that it works, and records pins).
fn resolve_target(target: &Target) -> Result<Vec<project::Package>, String> {
    match target {
        Target::Manifest(f) => {
            let p = project::load(f).map_err(|d| d.message)?;
            project::resolve(Some(&p), &p.root, &[], None).map_err(|d| d.message)
        }
        Target::Script(s) => {
            let deps = current(target)?;
            let dir = s.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
            project::resolve(None, dir, &deps, lock_path(target).as_deref()).map_err(|d: Diagnostic| d.message)
        }
    }
}

fn base_of(target: &Target) -> PathBuf {
    let file = match target {
        Target::Manifest(f) | Target::Script(f) => f,
    };
    let dir = file.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
    std::fs::canonicalize(dir).unwrap_or_else(|_| dir.to_path_buf())
}

fn file_of(target: &Target) -> &Path {
    match target {
        Target::Manifest(f) | Target::Script(f) => f,
    }
}

pub fn add(opts: &Opts, cwd: &Path, home: Option<&Path>, prompt: &mut dyn Prompt) -> Result<String, String> {
    let target = choose_target(opts, cwd, home, prompt, true)?;
    let base = base_of(&target);
    let source = source_from(opts, &std::fs::canonicalize(cwd).unwrap_or_else(|_| cwd.to_path_buf()))?;
    let name = opts.name.clone().unwrap_or_else(|| default_name(&source));
    if name.is_empty() || !name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return Err(format!("`{name}` is not a usable dependency name (use --as NAME)"));
    }
    let file = file_of(&target).to_path_buf();
    let existed = file.is_file();
    let before = if existed {
        std::fs::read_to_string(&file).map_err(|e| io_err("read", &file, e))?
    } else {
        let folder = base.file_name().map_or("app".to_string(), |n| n.to_string_lossy().into_owned());
        let clean: String = folder.chars().map(|c| if c.is_ascii_alphanumeric() || c == '-' || c == '_' { c } else { '_' }).collect();
        format!("name = \"{clean}\"\n")
    };

    if let Some(have) = current(&target)?.into_iter().find(|d| d.name == name) {
        let wanted = match &source {
            Source::Path(p) => Source::Path(normalize(p)),
            other => Source::Git { url: git_url(other), spec: git_ref(other) },
        };
        let same = match (&have.source, &wanted) {
            (Source::Path(a), Source::Path(b)) => normalize(a) == *b,
            (a, b) => format!("{a:?}") == format!("{b:?}"),
        };
        if same {
            return Ok(format!("{name} is already a dependency of {}", file.display()));
        }
        if !prompt.confirm(&format!("{name} is already a dependency, from a different source. Replace it?"), false) {
            return Err("nothing changed".into());
        }
    }

    let after = match &target {
        Target::Manifest(_) => add_to_manifest(&before, &name, &manifest_value(&source, &base)),
        Target::Script(_) => add_to_script(&before, &name, &source_text(&source, &base)),
    };
    write_atomically(&file, &after)?;
    if let Err(e) = resolve_target(&target) {
        // put things back as they were
        if existed {
            let _ = write_atomically(&file, &before);
        } else {
            let _ = std::fs::remove_file(&file);
        }
        return Err(e);
    }
    let created = if existed { "" } else { " (created)" };
    Ok(format!("added {name} ({}) to {}{created}", source_text(&source, &base), file.display()))
}

fn git_url(s: &Source) -> String {
    match s {
        Source::Git { url, .. } => url.clone(),
        Source::Path(p) => p.display().to_string(),
    }
}

fn git_ref(s: &Source) -> GitRef {
    match s {
        Source::Git { spec, .. } => spec.clone(),
        Source::Path(_) => GitRef::Default,
    }
}

pub fn remove(opts: &Opts, cwd: &Path, home: Option<&Path>, prompt: &mut dyn Prompt) -> Result<String, String> {
    let name = opts.positional.first().ok_or("usage: aion remove <name>")?;
    let target = choose_target(opts, cwd, home, prompt, false)?;
    let file = file_of(&target).to_path_buf();
    let text = std::fs::read_to_string(&file).map_err(|e| io_err("read", &file, e))?;
    let after = match &target {
        Target::Manifest(_) => remove_from_manifest(&text, name),
        Target::Script(_) => remove_from_script(&text, name),
    }
    .ok_or_else(|| format!("{name} is not a dependency of {}", file.display()))?;
    write_atomically(&file, &after)?;
    Ok(format!("removed {name} from {}", file.display()))
}

pub fn update(opts: &Opts, cwd: &Path, home: Option<&Path>, prompt: &mut dyn Prompt) -> Result<String, String> {
    let target = choose_target(opts, cwd, home, prompt, false)?;
    let lock = match &target {
        Target::Manifest(f) => f.with_file_name(project::LOCK_NAME),
        Target::Script(_) => lock_path(&target).expect("scripts have a lock path"),
    };
    let deps = current(&target)?;
    let name = opts.positional.first();
    if let Some(n) = name
        && !deps.iter().any(|d| &d.name == n)
    {
        return Err(format!("{n} is not a dependency of {}", file_of(&target).display()));
    }
    let wanted: Vec<Dependency> = deps.into_iter().filter(|d| name.is_none_or(|n| &d.name == n)).collect();
    let dropped = project::unlock(&wanted, &lock).map_err(|d| d.message)?;
    resolve_target(&target)?;
    Ok(format!("updated {} ({dropped} pin{} refreshed)", name.map_or("all dependencies", String::as_str), if dropped == 1 { "" } else { "s" }))
}

pub fn tree(opts: &Opts, cwd: &Path, home: Option<&Path>, prompt: &mut dyn Prompt) -> Result<String, String> {
    let mut opts_for_target = Opts { project: opts.project.clone(), ..Opts::default() };
    opts_for_target.script = opts.positional.first().map(PathBuf::from).or_else(|| opts.script.clone());
    let target = choose_target(&opts_for_target, cwd, home, prompt, false)?;
    let packages = resolve_target(&target)?;
    let title = match &target {
        Target::Manifest(f) => {
            let p = project::load(f).map_err(|d| d.message)?;
            format!("{}  ({})", p.name, p.root.display())
        }
        Target::Script(s) => format!("{}  (script)", s.display()),
    };
    let mut out = format!("{title}\n");
    print_children(&packages, 0, "", &mut out, &mut Vec::new());
    Ok(out)
}

fn print_children(packages: &[project::Package], at: usize, prefix: &str, out: &mut String, seen: &mut Vec<usize>) {
    let mut names: Vec<(&String, &usize)> = packages[at].deps.iter().collect();
    names.sort();
    for (i, &(name, &idx)) in names.iter().enumerate() {
        let last = i + 1 == names.len();
        out.push_str(&format!("{prefix}{}── {name}  {}\n", if last { "└" } else { "├" }, packages[idx].source));
        if seen.contains(&idx) {
            continue;
        }
        seen.push(idx);
        let next = format!("{prefix}{}   ", if last { " " } else { "│" });
        print_children(packages, idx, &next, out, seen);
        seen.pop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifests_gain_and_lose_dependencies_keeping_everything_else() {
        let text = "name = \"app\"  # mine\n\n[dependencies]\nutils = { path = \"../utils\" }\n\n# more later\n";
        let added = add_to_manifest(text, "http", "{ git = \"https://h/x\", tag = \"v1\" }");
        assert_eq!(
            added,
            "name = \"app\"  # mine\n\n[dependencies]\nutils = { path = \"../utils\" }\nhttp = { git = \"https://h/x\", tag = \"v1\" }\n\n# more later\n"
        );
        // replacing an entry of the same name
        let again = add_to_manifest(&added, "utils", "{ path = \"../u2\" }");
        assert!(again.contains("utils = { path = \"../u2\" }") && !again.contains("../utils"), "{again}");
        assert_eq!(remove_from_manifest(&again, "http").unwrap(), again.replace("http = { git = \"https://h/x\", tag = \"v1\" }\n", ""));
        assert!(remove_from_manifest(&again, "nope").is_none());
        // no section yet
        assert_eq!(add_to_manifest("name = \"a\"\n", "u", "{ path = \"x\" }"), "name = \"a\"\n\n[dependencies]\nu = { path = \"x\" }\n");
        // a prefix of another name is not a match
        assert!(remove_from_manifest("[dependencies]\nhttp2 = { path = \"x\" }\n", "http").is_none());
    }

    #[test]
    fn scripts_gain_and_lose_dep_lines_in_their_header() {
        let plain = "println(1)\n";
        let one = add_to_script(plain, "a", "../a");
        assert_eq!(one, "// dep: a = ../a\n\nprintln(1)\n");
        let two = add_to_script(&one, "b", "github.com/x/b@v1");
        assert_eq!(two, "// dep: a = ../a\n// dep: b = github.com/x/b@v1\n\nprintln(1)\n");
        let shebang = "#!/usr/bin/env aion\n// A tool.\n\nprintln(1)\n";
        assert_eq!(add_to_script(shebang, "a", "../a"), "#!/usr/bin/env aion\n// A tool.\n// dep: a = ../a\n\nprintln(1)\n");
        assert_eq!(add_to_script(&two, "a", "../other").lines().next(), Some("// dep: a = ../other"));
        assert_eq!(remove_from_script(&two, "a").unwrap(), "// dep: b = github.com/x/b@v1\n\nprintln(1)\n");
        assert!(remove_from_script(&two, "zzz").is_none());
    }

    #[test]
    fn names_and_paths_are_derived_from_sources() {
        let git = |u: &str| Source::Git { url: u.into(), spec: GitRef::Default };
        assert_eq!(default_name(&git("https://github.com/me/aion-http.git")), "http");
        assert_eq!(default_name(&git("git@github.com:me/json-tools")), "json_tools");
        assert_eq!(default_name(&Source::Path(PathBuf::from("/w/../w/utils"))), "utils");
        assert_eq!(relative(Path::new("/w/utils"), Path::new("/w/app")), "../utils");
        assert_eq!(relative(Path::new("/w/app/libs/x"), Path::new("/w/app")), "libs/x");
        assert_eq!(source_text(&Source::Path(PathBuf::from("/w/app/libs/x")), Path::new("/w/app")), "./libs/x");
        assert_eq!(
            manifest_value(&Source::Git { url: "https://h/x".into(), spec: GitRef::Branch("dev".into()) }, Path::new("/w")),
            "{ git = \"https://h/x\", branch = \"dev\" }"
        );
    }

    struct Fake(Vec<(String, bool)>, bool);
    impl Prompt for Fake {
        fn confirm(&mut self, q: &str, _: bool) -> bool {
            self.0.push((q.to_string(), self.1));
            self.1
        }
    }

    #[test]
    fn the_target_is_chosen_by_asking_only_when_unsure() {
        let root = std::env::temp_dir().join(format!("aion-manage-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(root.join("app/sub")).unwrap();
        std::fs::write(root.join("app/aion.project"), "name = \"app\"\n").unwrap();
        let root = std::fs::canonicalize(&root).unwrap();
        let home = root.join("home");
        let none = Opts::default();

        let mut no_questions = Fake(Vec::new(), true);
        let here = choose_target(&none, &root.join("app"), Some(&home), &mut no_questions, true).unwrap();
        assert_eq!(here, Target::Manifest(root.join("app/aion.project")));
        assert!(no_questions.0.is_empty());

        let mut asked = Fake(Vec::new(), true);
        let above = choose_target(&none, &root.join("app/sub"), Some(&home), &mut asked, true).unwrap();
        assert_eq!(above, Target::Manifest(root.join("app/aion.project")));
        assert!(asked.0[0].0.contains("1 level up"), "{:?}", asked.0);
        assert!(choose_target(&none, &root.join("app/sub"), Some(&home), &mut Fake(Vec::new(), false), true).is_err());

        std::fs::create_dir_all(root.join("loose")).unwrap();
        let created = choose_target(&none, &root.join("loose"), Some(&home), &mut Fake(Vec::new(), true), true).unwrap();
        assert_eq!(created, Target::Manifest(root.join("loose/aion.project")));
        assert!(choose_target(&none, &root.join("loose"), Some(&home), &mut Fake(Vec::new(), true), false).is_err());

        let script = Opts { script: Some(PathBuf::from("x.aion")), ..Opts::default() };
        assert_eq!(choose_target(&script, &root, None, &mut no_questions, true).unwrap(), Target::Script(PathBuf::from("x.aion")));
    }
}
