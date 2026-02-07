//! Runtime builder — compiles embedded C sources into a static library.
//!
//! All C sources (`aion_runtime.c`, `aion_math.c`, …) are baked into the
//! compiler binary via `include_str!()` so that `aion.exe` is fully
//! self-contained.  The compiled `.a` is cached in a temp directory.

use std::path::PathBuf;
use std::process::Command;

use crate::errors::{self, Phase};

/// The core runtime C source.
const RUNTIME_SRC: &str = include_str!("../../runtime/aion_runtime.c");

/// The math module C source.
const MATH_SRC: &str = include_str!("../../runtime/aion_math.c");

/// The sockets module C source.
const SOCKETS_SRC: &str = include_str!("../../runtime/aion_sockets.c");

/// Build the Aion runtime (core + stdlib modules) into a static
/// library and return its path.
///
/// `imported_modules` lists which `aion.*` C modules are needed
/// (e.g. `["math"]`).
pub fn build(imported_modules: &[String]) -> PathBuf {
    let cache_dir = std::env::temp_dir().join("aion-runtime-cache");
    std::fs::create_dir_all(&cache_dir).expect("failed to create runtime cache dir");

    // ── determine which C sources to compile ────────────────────


    let mut sources: Vec<(&str, &str)> = vec![("aion_runtime.c", RUNTIME_SRC)];

    for m in imported_modules {
        match m.as_str() {
            "math" => sources.push(("aion_math.c", MATH_SRC)),
            "sockets" => sources.push(("aion_sockets.c", SOCKETS_SRC)),
            other  => errors::warn(Phase::Compiler, format!("no C source for module '{other}'")),
        }
    }

    // ── write sources and check if rebuild needed ───────────────
    let mut obj_paths = Vec::new();
    let mut needs_rebuild = false;

    for (filename, src) in &sources {
        let c_path = cache_dir.join(filename);
        let o_path = cache_dir.join(filename.replace(".c", ".o"));

        let source_changed = match std::fs::read_to_string(&c_path) {
            Ok(existing) => existing != *src,
            Err(_) => true,
        };

        if source_changed || !o_path.exists() {
            needs_rebuild = true;
            std::fs::write(&c_path, src).expect("failed to write runtime source");

            let status = Command::new("gcc")
                .args([
                    c_path.to_str().unwrap(),
                    "-c", "-O2",
                    "-o", o_path.to_str().unwrap(),
                ])
                .status()
                .expect("failed to run gcc for runtime");
            assert!(status.success(), "compilation of {} failed", filename);
        }

        obj_paths.push(o_path);
    }

    let rt_lib = cache_dir.join("libaion_runtime.a");

    if needs_rebuild || !rt_lib.exists() {
        let mut ar_args = vec!["rcs".to_string(), rt_lib.to_str().unwrap().to_string()];
        for o in &obj_paths {
            ar_args.push(o.to_str().unwrap().to_string());
        }

        let status = Command::new("ar")
            .args(&ar_args)
            .status()
            .expect("failed to run ar");
        assert!(status.success(), "ar failed");
    }

    rt_lib
}
