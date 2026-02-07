//! Linker — invokes the system C compiler to produce a native binary.
//!
//! Tries gcc, then clang (plus `cc` on Unix) and adds `-lm` for math.

use std::path::Path;
use std::process::Command;

use crate::errors::{self, Phase};

/// Link an object file + the Aion runtime into a native executable.
///
/// Adds `-lm` so that C math functions (sqrt, sin, …) are resolved.
/// When the sockets module is used, also links against the platform
/// socket library (ws2_32 on Windows).
pub fn link(object_path: &Path, runtime_lib: &Path, output_path: &Path, imported_modules: &[String]) {
    let obj = object_path.to_str().unwrap();
    let rt = runtime_lib.to_str().unwrap();
    let out = output_path.to_str().unwrap();

    let mut extra_libs: Vec<&str> = Vec::new();
    if cfg!(windows) && imported_modules.iter().any(|m| m == "sockets") {
        extra_libs.push("-lws2_32");
    }

    let build_args = |base: &[&str]| -> Vec<String> {
        let mut args: Vec<String> = base.iter().map(|s| s.to_string()).collect();
        for lib in &extra_libs {
            args.push(lib.to_string());
        }
        args
    };

    let candidates: Vec<(&str, Vec<String>)> = if cfg!(windows) {
        vec![
            ("gcc",   build_args(&[obj, rt, "-lm", "-o", out])),
            ("clang", build_args(&[obj, rt, "-lm", "-o", out])),
        ]
    } else {
        vec![
            ("cc",    build_args(&[obj, rt, "-lm", "-o", out])),
            ("gcc",   build_args(&[obj, rt, "-lm", "-o", out])),
            ("clang", build_args(&[obj, rt, "-lm", "-o", out])),
        ]
    };

    for (cmd, args) in &candidates {
        let result = Command::new(cmd).args(args).output();
        match result {
            Ok(output) if output.status.success() => return,
            Ok(output) => {
                let stderr = String::from_utf8_lossy(&output.stderr);
                eprintln!("[{cmd}] link failed:\n{stderr}");
            }
            Err(_) => continue,
        }
    }

    errors::fatal_with_hint(
        Phase::Linker,
        "No working linker found",
        Some("Install gcc or clang and make sure it's on your PATH".into()),
    );
}
