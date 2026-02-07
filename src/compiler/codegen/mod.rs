//! Code generation — walks the AST and emits LLVM IR.

use std::collections::HashMap;
use inkwell::values::FunctionValue;

pub mod runtime;
pub mod stdlib;
pub mod expr;
pub mod func;

/// Lookup table for the declared Aion runtime functions in the LLVM module.
///
/// Keys are the C symbol names, e.g. `"aion_print"`, `"aion_panic"`.
pub type Runtime<'ctx> = HashMap<&'static str, FunctionValue<'ctx>>;

/// Type alias for the per-module function lookup table.
pub type ModuleFns<'ctx> = HashMap<String, HashMap<String, FunctionValue<'ctx>>>;

pub use runtime::declare_runtime;
pub use stdlib::declare_stdlib;
pub use func::{compile_functions, compile_user_module};
pub use expr::compile_expr;
