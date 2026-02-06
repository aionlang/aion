//! C-backed standard library function registry.
//!
//! Maps module names (e.g. `"math"`) to their C function descriptors.
//! Extend this file when adding new `import aion.*` modules.

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

/// Return the function registry for a given module name.
///
/// Extend this when you add new `import aion.*` modules backed by C.
pub fn registry(module: &str) -> Option<HashMap<&'static str, StdlibFunc>> {
    match module {
        "math" => {
            let mut m = HashMap::new();
            m.insert("sqrt",  StdlibFunc { c_name: "aion_math_sqrt",  arity: 1, returns_f64: true });
            m.insert("abs",   StdlibFunc { c_name: "aion_math_abs",   arity: 1, returns_f64: true });
            m.insert("sin",   StdlibFunc { c_name: "aion_math_sin",   arity: 1, returns_f64: true });
            m.insert("cos",   StdlibFunc { c_name: "aion_math_cos",   arity: 1, returns_f64: true });
            m.insert("floor", StdlibFunc { c_name: "aion_math_floor", arity: 1, returns_f64: true });
            m.insert("ceil",  StdlibFunc { c_name: "aion_math_ceil",  arity: 1, returns_f64: true });
            m.insert("pow",   StdlibFunc { c_name: "aion_math_pow",   arity: 2, returns_f64: true });
            Some(m)
        }
        _ => None,
    }
}
