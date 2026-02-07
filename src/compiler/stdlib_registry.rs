//! C-backed standard library function registry.
//!
//! Maps module names (e.g. `"math"`) to their C function descriptors.
//! Extend this file when adding new `import aion.*` modules.
//!
//! ## Adding a new module
//!
//! For all-f64 modules (like math), use the `stdlib_module!` macro:
//!
//! ```ignore
//! "strings" => stdlib_module! {
//!     concat  => "aion_str_concat"  (2) -> f64,
//!     length  => "aion_str_length"  (1) -> f64,
//! }
//! ```
//!
//! For modules with mixed types, use `stdlib_typed!`:
//!
//! ```ignore
//! "sockets" => stdlib_typed! {
//!     _raw_create => "aion_socket_raw_create" [Int, Int, Int] -> Int,
//!     _raw_recv   => "aion_socket_raw_recv"   [Int, Int]      -> Str,
//! }
//! ```

use std::collections::HashMap;

/// Parameter / return type for C-backed stdlib functions.
#[derive(Clone, Debug, PartialEq)]
pub enum CType {
    Int,    // i64  (long long in C)
    Float,  // f64  (double in C)
    Str,    // ptr  (const char * in C)
    Void,   // void (return only)
}

/// Describes one C function exposed from a standard-library module.
#[derive(Clone, Debug)]
pub struct StdlibFunc {
    /// The C symbol name, e.g. `"aion_math_sqrt"`.
    pub c_name: &'static str,
    /// Parameter types, in order.
    pub params: Vec<CType>,
    /// Return type.
    pub ret: CType,
}

/// Declaratively build a `HashMap` of stdlib function descriptors
/// where every parameter and return value is `f64`.
///
/// Syntax:  `name => "c_symbol" (arity) -> f64,`
macro_rules! stdlib_module {
    ( $( $name:ident => $c_name:literal ($arity:expr) -> f64 ),+ $(,)? ) => {{
        let mut m = HashMap::new();
        $(
            m.insert(
                stringify!($name),
                StdlibFunc {
                    c_name: $c_name,
                    params: vec![CType::Float; $arity],
                    ret: CType::Float,
                },
            );
        )+
        Some(m)
    }};
}

/// Declaratively build a `HashMap` with explicit per-parameter types.
///
/// Syntax:  `name => "c_symbol" [Type, Type, …] -> RetType,`
macro_rules! stdlib_typed {
    ( $( $name:ident => $c_name:literal [ $($pty:ident),* ] -> $rty:ident ),+ $(,)? ) => {{
        let mut m = HashMap::new();
        $(
            m.insert(
                stringify!($name),
                StdlibFunc {
                    c_name: $c_name,
                    params: vec![ $( CType::$pty ),* ],
                    ret: CType::$rty,
                },
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

        "sockets" => stdlib_typed! {
            // Raw primitives (prefixed with _ to discourage direct use)
            _raw_create   => "aion_socket_raw_create"   [Int, Int, Int]      -> Int,
            _raw_bind     => "aion_socket_raw_bind"     [Int, Int]           -> Int,
            _raw_connect  => "aion_socket_raw_connect"  [Int, Str, Int]      -> Int,
            _raw_listen   => "aion_socket_raw_listen"   [Int, Int]           -> Int,
            _raw_accept   => "aion_socket_raw_accept"   [Int]                -> Int,
            _raw_send     => "aion_socket_raw_send"     [Int, Str, Int]      -> Int,
            _raw_recv     => "aion_socket_raw_recv"     [Int, Int]           -> Str,
            _raw_close    => "aion_socket_raw_close"    [Int]                -> Int,
            _raw_shutdown => "aion_socket_raw_shutdown"  [Int, Int]           -> Int,
            _raw_setopt   => "aion_socket_raw_setopt"   [Int, Int, Int]      -> Int,
            _raw_error    => "aion_socket_raw_error"    []                   -> Int,
            _raw_is_valid => "aion_socket_raw_is_valid" [Int]                -> Int,
            _raw_cleanup  => "aion_socket_raw_cleanup"  []                   -> Void,
            _strlen       => "aion_socket_strlen"       [Str]                -> Int
        },

        _ => None,
    }
}
