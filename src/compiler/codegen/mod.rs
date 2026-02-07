//! Code generation — walks the AST and emits LLVM IR.

use std::collections::HashMap;
use inkwell::values::FunctionValue;

pub mod runtime;
pub mod stdlib;
pub mod expr;
pub mod func;
pub mod types;

/// Lookup table for the declared Aion runtime functions in the LLVM module.
///
/// Keys are the C symbol names, e.g. `"aion_print"`, `"aion_panic"`.
pub type Runtime<'ctx> = HashMap<&'static str, FunctionValue<'ctx>>;

/// Type alias for the per-module function lookup table.
pub type ModuleFns<'ctx> = HashMap<String, HashMap<String, FunctionValue<'ctx>>>;

/// Information about a single user-defined type (struct) in the LLVM module.
pub struct TypeInfo<'ctx> {
    /// The LLVM struct type for this user-defined type.
    pub struct_type: inkwell::types::StructType<'ctx>,
    /// Ordered list of (field_name, field_llvm_type) pairs.
    pub fields: Vec<(String, inkwell::types::BasicTypeEnum<'ctx>)>,
    /// The constructor parameter names (determines which fields and their order).
    pub constructor_params: Vec<String>,
    /// `true` if the type has an explicit `constructor(…) { … }` block.
    pub has_explicit_constructor: bool,
}

/// Maps Aion type names to their compiled [`TypeInfo`].
pub type TypeRegistry<'ctx> = HashMap<String, TypeInfo<'ctx>>;

pub use runtime::declare_runtime;
pub use stdlib::declare_stdlib;
pub use func::{forward_declare_functions, compile_functions, compile_user_module};
pub use expr::compile_expr;
pub use types::{compile_type_defs, compile_constructors, compile_methods};
