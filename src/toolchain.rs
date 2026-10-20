//! Turns generated LLVM IR + C sources into a native executable using `clang`.
//!
//! One `clang` invocation compiles the `.ll`, the C runtime and any C halves
//! of imported stdlib modules, and links them together.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::diag::{Diagnostic, Result};
use crate::embedded;

pub struct Options {
    /// Passed through as `-O<level>`.
    pub opt_level: String,
    pub keep_temps: bool,
    /// Keep a stack trace (function and line) for a panic to print; costs some speed on call-heavy code.
    pub trace: bool,
}

/// Are the programs built for Windows?  True when the compiler itself runs on Windows; `AION_TARGET` naming a Windows
/// triple (`x86_64-w64-mingw32`) makes a build on another system aim at Windows too (the programs are then run with
/// `AION_RUNNER`, such as `wine`), which is how the Windows port is tested.
pub fn windows() -> bool {
    cfg!(windows) || std::env::var("AION_TARGET").is_ok_and(|t| t.contains("mingw") || t.contains("windows"))
}

/// The suffix of an executable file name.
pub fn exe_suffix() -> &'static str {
    if windows() { ".exe" } else { "" }
}

/// A C compiler to try: the program, and the arguments that go before the usual ones (`cc` for `zig cc`).
struct Compiler {
    program: String,
    leading: &'static [&'static str],
}

impl Compiler {
    fn new(program: impl Into<String>) -> Compiler {
        Compiler { program: program.into(), leading: &[] }
    }

    /// `zig cc`.  Zig turns on the undefined-behavior sanitizer for unoptimized C by default; clang does not,
    /// so it is switched off to build the same programs the same way.
    fn zig(program: impl Into<String>) -> Compiler {
        Compiler { program: program.into(), leading: &["cc", "-fno-sanitize=undefined"] }
    }
}

/// Where a Zig that was shipped with Aion would be: `zig/zig` next to the `aion` executable.
fn bundled_zig() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let path = exe.parent()?.join("zig").join(format!("zig{}", exe_suffix()));
    path.is_file().then_some(path)
}

/// Compilers to try, in order: clang (newest first), then `zig cc`, which needs no C toolchain installed.
/// `AION_CLANG` and `AION_ZIG` override and are then the only choice.
fn candidates() -> Vec<Compiler> {
    if let Ok(c) = std::env::var("AION_CLANG") {
        return vec![Compiler::new(c)];
    }
    if let Ok(z) = std::env::var("AION_ZIG") {
        return vec![Compiler::zig(z)];
    }
    let mut v = vec![Compiler::new("clang")];
    v.extend((15..=22).rev().map(|n| Compiler::new(format!("clang-{n}"))));
    if let Some(z) = bundled_zig() {
        v.push(Compiler::zig(z.to_string_lossy()));
    }
    v.push(Compiler::zig("zig"));
    v
}

pub fn build(
    ir: &str,
    c_sources: &[(String, String)],
    libs: &[String],
    output: &Path,
    opts: &Options,
) -> Result<()> {
    let nanos = SystemTime::now().duration_since(UNIX_EPOCH).map_or(0, |d| d.subsec_nanos());
    let dir: PathBuf = std::env::temp_dir().join(format!("aion-{}-{nanos}", std::process::id()));
    std::fs::create_dir_all(&dir).map_err(io_err("create a temporary directory"))?;

    let result = compile(&dir, ir, c_sources, libs, output, opts);
    if opts.keep_temps {
        eprintln!("kept intermediate files in {}", dir.display());
    } else {
        let _ = std::fs::remove_dir_all(&dir);
    }
    result
}

fn compile(
    dir: &Path,
    ir: &str,
    c_sources: &[(String, String)],
    libs: &[String],
    output: &Path,
    opts: &Options,
) -> Result<()> {
    let write = |name: &str, text: &str| -> Result<PathBuf> {
        let path = dir.join(name);
        std::fs::write(&path, text).map_err(io_err("write a temporary file"))?;
        Ok(path)
    };

    let mut inputs = vec![write("program.ll", ir)?];
    for (name, text) in embedded::RUNTIME {
        let path = write(name, text)?;
        if name.ends_with(".c") {
            inputs.push(path);
        }
    }
    for (name, text) in c_sources {
        inputs.push(write(&format!("std_{name}"), text)?);
    }

    let mut first_missing: Option<String> = None;
    for cc in candidates() {
        let mut cmd = Command::new(&cc.program);
        cmd.args(cc.leading)
            .arg(format!("-O{}", opts.opt_level))
            .arg("-w")
            .arg("-I")
            .arg(dir)
            .args(&inputs)
            .arg("-o")
            .arg(output)
            .arg("-lm");
        for lib in libs {
            // Windows has no libdl (the C code loads libraries with LoadLibrary there) and no separate libpthread
            if windows() && (lib == "dl" || lib == "pthread") {
                continue;
            }
            cmd.arg(format!("-l{lib}"));
        }
        if windows() {
            cmd.args(["-lws2_32", "-lbcrypt"]);
        } else {
            cmd.arg("-pthread");
        }
        match cmd.output() {
            Ok(out) if out.status.success() => return Ok(()),
            Ok(out) => {
                return Err(Diagnostic::general(format!(
                    "`{}` failed:\n{}",
                    cc.program,
                    String::from_utf8_lossy(&out.stderr).trim_end()
                ))
                .hint("this is probably a compiler bug; `aion emit-ir <file>` shows the generated IR"));
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                first_missing.get_or_insert(cc.program);
            }
            Err(e) => return Err(Diagnostic::general(format!("cannot run `{}`: {e}", cc.program))),
        }
    }
    Err(Diagnostic::general(format!(
        "no C compiler found (tried `{}` and others)",
        first_missing.unwrap_or_else(|| "clang".into())
    ))
    .hint("install clang 15 or newer (or zig), or point AION_CLANG or AION_ZIG at it"))
}

fn io_err(what: &'static str) -> impl Fn(std::io::Error) -> Diagnostic {
    move |e| Diagnostic::general(format!("could not {what}: {e}"))
}
