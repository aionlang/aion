//! `aion test` and `aion fmt`: running a project's tests and tidying its files.

use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::time::Instant;

use crate::{format, project};

/// Every `.aion` file under `path` (or `path` itself), sorted.  With `skip_helpers`, names starting
/// with `_` are left out: they are shared code for the tests, not tests.
fn aion_files(path: &Path, skip_helpers: bool, out: &mut Vec<PathBuf>) {
    if path.is_file() {
        out.push(path.to_path_buf());
        return;
    }
    let Ok(entries) = std::fs::read_dir(path) else { return };
    let mut entries: Vec<PathBuf> = entries.filter_map(|e| e.ok().map(|e| e.path())).collect();
    entries.sort();
    for p in entries {
        let name = p.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or_default();
        if p.is_dir() {
            // a folder of downloaded packages is somebody else's code
            if !name.starts_with('.') && name != "target" && !p.join(crate::project::PACKAGES_MARKER).exists() {
                aion_files(&p, skip_helpers, out);
            }
        } else if p.extension().is_some_and(|e| e == "aion") && !(skip_helpers && name.starts_with('_')) {
            out.push(p);
        }
    }
}

/// `aion test [path…]`: run every program in the `tests/` folder of the project (or of this directory), or
/// the given files and folders.  A test passes when its program exits with status 0.
pub fn test(args: &[String]) -> ExitCode {
    let mut paths: Vec<PathBuf> = args.iter().map(PathBuf::from).collect();
    if paths.is_empty() {
        let root = project::find(Path::new("."))
            .and_then(|p| p.parent().map(Path::to_path_buf))
            .filter(|p| !p.as_os_str().is_empty())
            .unwrap_or_else(|| PathBuf::from("."));
        let dir = root.join("tests");
        if !dir.is_dir() {
            eprintln!("error: there is no `tests` folder in {}", root.display());
            eprintln!("hint: put test programs in tests/*.aion; each passes if it exits with status 0 (see `assert` and `assertEq`)");
            return ExitCode::FAILURE;
        }
        paths.push(dir);
    }
    let mut files = Vec::new();
    for p in &paths {
        if !p.exists() {
            eprintln!("error: {} does not exist", p.display());
            return ExitCode::FAILURE;
        }
        aion_files(p, true, &mut files);
    }
    if files.is_empty() {
        eprintln!("no tests found");
        return ExitCode::FAILURE;
    }

    let exe = std::env::current_exe().expect("the compiler knows where it is");
    let mut failed = 0;
    for file in &files {
        let started = Instant::now();
        let out = Command::new(&exe).arg("run").arg(file).output();
        let seconds = started.elapsed().as_secs_f32();
        match out {
            Ok(o) if o.status.success() => eprintln!("  ok    {} ({seconds:.1}s)", file.display()),
            Ok(o) => {
                eprintln!("  FAIL  {} ({seconds:.1}s)", file.display());
                for stream in [&o.stdout, &o.stderr] {
                    for line in String::from_utf8_lossy(stream).lines() {
                        eprintln!("          {line}");
                    }
                }
                failed += 1;
            }
            Err(e) => {
                eprintln!("  FAIL  {}: could not start the compiler: {e}", file.display());
                failed += 1;
            }
        }
    }
    eprintln!("\n{} passed, {failed} failed", files.len() - failed);
    if failed == 0 { ExitCode::SUCCESS } else { ExitCode::FAILURE }
}

/// `aion fmt [--check] [path…]`: tidy the whitespace of the given files and folders (default: this directory).
pub fn fmt(args: &[String]) -> ExitCode {
    let check = args.iter().any(|a| a == "--check");
    let mut paths: Vec<PathBuf> = args.iter().filter(|a| *a != "--check").map(PathBuf::from).collect();
    if paths.is_empty() {
        paths.push(PathBuf::from("."));
    }
    let mut files = Vec::new();
    for p in &paths {
        if !p.exists() {
            eprintln!("error: {} does not exist", p.display());
            return ExitCode::FAILURE;
        }
        aion_files(p, false, &mut files);
    }

    let (mut changed, mut broken) = (0, 0);
    for file in &files {
        let Ok(text) = std::fs::read_to_string(file) else { continue };
        match format::format(&text) {
            Ok(tidy) if tidy == text => {}
            Ok(tidy) => {
                changed += 1;
                if check {
                    eprintln!("would change {}", file.display());
                } else if let Err(e) = std::fs::write(file, tidy) {
                    eprintln!("error: cannot write {}: {e}", file.display());
                    return ExitCode::FAILURE;
                } else {
                    eprintln!("formatted {}", file.display());
                }
            }
            Err(message) => {
                broken += 1;
                eprintln!("skipped {}: {message}", file.display());
            }
        }
    }
    if changed == 0 && broken == 0 {
        eprintln!("{} file{} already tidy", files.len(), if files.len() == 1 { "" } else { "s" });
    }
    if (check && changed > 0) || broken > 0 { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}
