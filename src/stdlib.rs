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

/// Return the embedded Aion prelude source.
///
/// The prelude is automatically loaded into every program — its
/// functions (e.g. `println()`) are available without any import.
pub fn prelude() -> &'static str {
    include_str!("../stdlib/prelude.aion")
}

// ═══════════════════════════════════════════════════════════════════
// Stub declarations for IDE / language-server tooling
// ═══════════════════════════════════════════════════════════════════

/// Return the embedded Aion stub source for an IDE-visible module.
///
/// These are **declaration-only** files used by the language server
/// to provide Go-to-Definition, hover documentation, and completions
/// for built-in and C-backed functions.
///
/// The compiler itself never compiles these — they exist purely for
/// developer tooling.
pub fn get_stub(name: &str) -> Option<&'static str> {
    match name {
        "builtins" => Some(include_str!("../stdlib/builtins.aion")),
        "math"     => Some(include_str!("../stdlib/math.aion")),
        _ => None,
    }
}

/// List all available stub module names (for IDE enumeration).
#[allow(dead_code)]
pub fn available_stubs() -> &'static [&'static str] {
    &["builtins", "math"]
}

/// List all available embedded stdlib module names.
#[allow(dead_code)]
pub fn available() -> &'static [&'static str] {
    &["io", "name"]
}
