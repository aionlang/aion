//! C-backed standard library function registry.
//!
//! Maps module names (e.g. `"math"`) to their C function descriptors.
//! Extend this file when adding new `import aion.*` modules.
//!
//! ## Adding a new module
//!
//! Use the `stdlib_module!` macro:
//!
//! ```ignore
//! "strings" => stdlib_module! {
//!     concat  => "aion_str_concat"  (2) -> f64,
//!     length  => "aion_str_length"  (1) -> f64,
//! }
//! ```

use std::collections::HashMap;

/// Describes one C function exposed from a standard-library module.
#[derive(Clone, Debug)]
pub struct StdlibFunc {
    /// The C symbol name, e.g. `"aion_math_sqrt"`.
    pub c_name: &'static str,
    /// Number of f64 parameters.
    pub arity: usize,
    /// Return type — for now all math functions return f64.
    pub returns_f64: bool,
}

/// Declaratively build a `HashMap` of stdlib function descriptors.
///
/// Syntax:  `name => "c_symbol" (arity) -> f64,`
macro_rules! stdlib_module {
    ( $( $name:ident => $c_name:literal ($arity:expr) -> f64 ),+ $(,)? ) => {{
        let mut m = HashMap::new();
        $(
            m.insert(
                stringify!($name),
                StdlibFunc { c_name: $c_name, arity: $arity, returns_f64: true },
            );
        )+
        Some(m)
    }};
}

/// Return the function registry for a given module name.
///
/// Extend this when you add new `import aion.*` modules backed by C.
pub fn registry(module: &str) -> Option<HashMap<&'static str, StdlibFunc>> {
    match module {
        "math" => stdlib_module! {
            sqrt  => "aion_math_sqrt"  (1) -> f64,
            abs   => "aion_math_abs"   (1) -> f64,
            sin   => "aion_math_sin"   (1) -> f64,
            cos   => "aion_math_cos"   (1) -> f64,
            floor => "aion_math_floor" (1) -> f64,
            ceil  => "aion_math_ceil"  (1) -> f64,
            pow   => "aion_math_pow"   (2) -> f64,
            max   => "aion_math_max"   (2) -> f64,
            min   => "aion_math_min"   (2) -> f64,
        },
        _ => None,
    }
}
