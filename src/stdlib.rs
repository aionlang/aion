//! Embedded Aion standard library registry.
//!
//! Aion source files under `stdlib/` are baked into the compiler binary
//! via `include_str!()`.  When the user writes `import std.io;` the
//! driver looks up the module name here instead of searching the disk.
//!
//! ## Adding a new stdlib module
//!
//! 1. Create `stdlib/<name>.aion`.
//! 2. Add an `include_str!()` constant below.
//! 3. Register it in [`get()`].

/// Return the embedded Aion source for a standard-library module, or
/// `None` if no such module exists.
///
/// Module names correspond to the last segment of the import path,
/// e.g. `import std.io;` → `name = "io"`.
pub fn get(name: &str) -> Option<&'static str> {
    match name {
        "io" => Some(include_str!("../stdlib/io.aion")),
        "name" => Some(include_str!("../stdlib/name.aion")),
        // ── add new modules here ──────────────────────────────
        // "strings" => Some(include_str!("../stdlib/strings.aion")),
        // "fmt"     => Some(include_str!("../stdlib/fmt.aion")),
        _ => None,
    }
}

/// List all available embedded stdlib module names.
#[allow(dead_code)]
pub fn available() -> &'static [&'static str] {
    &["io", "name"]
}
