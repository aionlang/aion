//! Linker — invokes the system C compiler to produce a native binary.
//!
//! Tries gcc, then clang (plus `cc` on Unix) and adds `-lm` for math.

use std::path::Path;
use std::process::Command;

use crate::errors::{self, Phase};

/// Link an object file + the Aion runtime into a native executable.
///
/// Adds `-lm` so that C math functions (sqrt, sin, …) are resolved.
pub fn link(object_path: &Path, runtime_lib: &Path, output_path: &Path) {
    let obj = object_path.to_str().unwrap();
    let rt = runtime_lib.to_str().unwrap();
    let out = output_path.to_str().unwrap();

    let candidates: Vec<(&str, Vec<&str>)> = if cfg!(windows) {
        vec![
            ("gcc",   vec![obj, rt, "-lm", "-o", out]),
            ("clang", vec![obj, rt, "-lm", "-o", out]),
        ]
    } else {
        vec![
            ("cc",    vec![obj, rt, "-lm", "-o", out]),
            ("gcc",   vec![obj, rt, "-lm", "-o", out]),
            ("clang", vec![obj, rt, "-lm", "-o", out]),
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
