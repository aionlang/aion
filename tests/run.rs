//! End-to-end tests: compile and run every program under `tests/`.
//!
//! * `tests/programs/*.aion` are run; stdout must equal the sibling `.out`
//!   file.  Optional header comments: `// exit: N` (expected exit code,
//!   default 0), `// stderr: text` (stderr must contain `text`) and
//!   `// stress: skip` (don't repeat under `AION_GC_STRESS=1`).
//!   Every program is also run with a collection on every allocation and must
//!   produce identical output — that catches values the compiler failed to
//!   keep alive.
//! * `tests/errors/*.aion` must fail to compile with the diagnostic named by
//!   their `// error: text` header.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn aion(args: &[&str], file: &Path, stress: bool) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aion"));
    cmd.args(args).arg(file);
    if stress {
        cmd.env("AION_GC_STRESS", "1");
    }
    cmd.output().expect("failed to launch the aion binary")
}

fn sources(dir: &str) -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join(dir);
    let mut files: Vec<PathBuf> = std::fs::read_dir(root)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "aion"))
        .collect();
    files.sort();
    files
}

/// Value of a `// key: value` header line.
fn header(source: &str, key: &str) -> Option<String> {
    source.lines().find_map(|l| l.strip_prefix(&format!("// {key}:")).map(|v| v.trim().to_string()))
}

#[test]
fn programs_produce_expected_output() {
    let mut failures = Vec::new();
    for file in sources("programs") {
        let name = file.file_stem().unwrap().to_string_lossy().into_owned();
        let source = std::fs::read_to_string(&file).unwrap();
        let expected_code: i32 = header(&source, "exit").map_or(0, |v| v.parse().unwrap());
        let expected_out = std::fs::read_to_string(file.with_extension("out"))
            .unwrap_or_else(|_| panic!("missing tests/programs/{name}.out"));

        let mut modes = vec![false];
        if header(&source, "stress").as_deref() != Some("skip") {
            modes.push(true);
        }
        for stress in modes {
            let mode = if stress { "gc-stress" } else { "normal" };
            let out = aion(&["run"], &file, stress);
            let stdout = String::from_utf8_lossy(&out.stdout);
            let stderr = String::from_utf8_lossy(&out.stderr);
            if out.status.code() != Some(expected_code) {
                failures.push(format!("{name} [{mode}]: exit {:?}, expected {expected_code}\n{stderr}", out.status.code()));
            } else if stdout != expected_out {
                failures.push(format!("{name} [{mode}]: stdout differs\n--- expected\n{expected_out}--- actual\n{stdout}"));
            } else if let Some(want) = header(&source, "stderr")
                && !stderr.contains(&want)
            {
                failures.push(format!("{name} [{mode}]: stderr lacks `{want}`:\n{stderr}"));
            }
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

#[test]
fn invalid_programs_are_rejected_with_the_right_message() {
    let mut failures = Vec::new();
    for file in sources("errors") {
        let name = file.file_stem().unwrap().to_string_lossy().into_owned();
        let source = std::fs::read_to_string(&file).unwrap();
        let want = header(&source, "error").unwrap_or_else(|| panic!("tests/errors/{name}.aion has no `// error:` header"));

        // `emit-ir` stops after analysis, so these tests don't need clang.
        let out = aion(&["emit-ir"], &file, false);
        let stderr = String::from_utf8_lossy(&out.stderr);
        if out.status.success() {
            failures.push(format!("{name}: compiled, expected error `{want}`"));
        } else if !stderr.contains(&want) {
            failures.push(format!("{name}: expected `{want}` in:\n{stderr}"));
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

#[test]
fn diagnostics_point_at_the_source() {
    let file = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/errors/type_mismatch.aion");
    let out = aion(&["emit-ir"], &file, false);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("type_mismatch.aion:3:15"), "{stderr}");
    assert!(stderr.contains("x: Int := \"hello\""), "{stderr}");
    assert!(stderr.contains("^^^^^^^"), "{stderr}");
}

#[test]
fn circular_imports_are_reported() {
    let dir = std::env::temp_dir().join(format!("aion-cycle-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("a.aion"), "import b;\nfn fa() {}\n").unwrap();
    std::fs::write(dir.join("b.aion"), "import a;\nfn fb() {}\n").unwrap();
    std::fs::write(dir.join("main.aion"), "import a;\nfn main() {}\n").unwrap();
    let out = aion(&["emit-ir"], &dir.join("main.aion"), false);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("circular import"));
}

#[test]
fn hello_world_via_build_and_run() {
    // Exercises `aion build -o` and then runs the produced executable.
    let dir = std::env::temp_dir().join(format!("aion-build-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let src = dir.join("hello.aion");
    std::fs::write(&src, "fn main() { println(\"hi from a built binary\") }\n").unwrap();
    let exe = dir.join("hello_bin");
    let build = Command::new(env!("CARGO_BIN_EXE_aion"))
        .args(["build"])
        .arg(&src)
        .arg("-o")
        .arg(&exe)
        .output()
        .unwrap();
    assert!(build.status.success(), "{}", String::from_utf8_lossy(&build.stderr));
    let run = Command::new(&exe).output().unwrap();
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(String::from_utf8_lossy(&run.stdout), "hi from a built binary\n");
}
