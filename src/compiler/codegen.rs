//! Code generation — walks the AST and emits LLVM IR.

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue, ValueKind};

use crate::ast::{Expr, Function, Import, UserModule};
use crate::errors::{self, Phase};

use super::stdlib_registry;

// ═══════════════════════════════════════════════════════════════════
// Runtime function handles
// ═══════════════════════════════════════════════════════════════════

/// Lookup table for the declared Aion runtime functions in the LLVM module.
///
/// Keys are the C symbol names, e.g. `"aion_print"`, `"aion_panic"`.
pub type Runtime<'ctx> = HashMap<&'static str, FunctionValue<'ctx>>;

/// Describes the parameter types of a runtime function (no generics yet).
enum ParamKind { Ptr, I64, F64, None }

/// Describes the return type of a runtime function.
enum RetKind { Void, Ptr }

// ═══════════════════════════════════════════════════════════════════
// Runtime & stdlib declaration helpers
// ═══════════════════════════════════════════════════════════════════

/// Declare the external `aion_*` core runtime functions from a data table.
pub fn declare_runtime<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> Runtime<'ctx> {
    // (symbol, param, return)
    const TABLE: &[(&str, ParamKind, RetKind)] = &[
        ("aion_print",         ParamKind::Ptr, RetKind::Void),
        ("aion_print_int",     ParamKind::I64, RetKind::Void),
        ("aion_print_float",   ParamKind::F64, RetKind::Void),
        ("aion_println",       ParamKind::Ptr, RetKind::Void),
        ("aion_println_int",   ParamKind::I64, RetKind::Void),
        ("aion_println_float", ParamKind::F64, RetKind::Void),
        ("aion_panic",         ParamKind::Ptr, RetKind::Void),
        ("aion_alloc",         ParamKind::I64, RetKind::Ptr),
        ("aion_free",          ParamKind::Ptr, RetKind::Void),
    ];

    let void   = context.void_type();
    let ptr    = context.ptr_type(inkwell::AddressSpace::default());
    let i64_ty = context.i64_type();
    let f64_ty = context.f64_type();

    let mut rt = HashMap::new();

    for &(name, ref param, ref ret) in TABLE {
        let params: Vec<inkwell::types::BasicMetadataTypeEnum> = match param {
            ParamKind::Ptr  => vec![ptr.into()],
            ParamKind::I64  => vec![i64_ty.into()],
            ParamKind::F64  => vec![f64_ty.into()],
            ParamKind::None => vec![],
        };

        let fn_type = match ret {
            RetKind::Void => void.fn_type(&params, false),
            RetKind::Ptr  => ptr.fn_type(&params, false),
        };

        let fn_val = module.add_function(name, fn_type, Option::None);
        rt.insert(name, fn_val);
    }

    rt
}

/// Declare C-backed stdlib functions for each `import aion.*` module.
pub fn declare_stdlib<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    imports: &[Import],
) -> HashMap<String, HashMap<String, FunctionValue<'ctx>>> {
    let mut map = HashMap::new();
    let f64_ty = context.f64_type();

    for imp in imports {
        if !imp.is_stdlib() {
            continue;
        }

        let mod_name = imp.module_name().to_string();
        let registry = stdlib_registry::registry(&mod_name).unwrap_or_else(|| {
            errors::fatal_with_hint(
                Phase::Compiler,
                format!("Unknown standard-library module: '{}'", imp.path.join(".")),
                Some("Available modules: aion.math".into()),
            );
        });

        let mut fns = HashMap::new();
        for (name, info) in &registry {
            let param_types: Vec<inkwell::types::BasicMetadataTypeEnum> =
                (0..info.arity).map(|_| f64_ty.into()).collect();
            let fn_type = if info.returns_f64 {
                f64_ty.fn_type(&param_types, false)
            } else {
                context.void_type().fn_type(&param_types, false)
            };
            let fn_val = module.add_function(info.c_name, fn_type, None);
            fns.insert(name.to_string(), fn_val);
        }

        map.insert(mod_name, fns);
    }

    map
}

// ═══════════════════════════════════════════════════════════════════
// Expression & function compilation
// ═══════════════════════════════════════════════════════════════════

/// Type alias for the per-module function lookup table.
pub type ModuleFns<'ctx> = HashMap<String, HashMap<String, FunctionValue<'ctx>>>;

/// Emit a `print()` or `println()` call with type-based dispatch.
///
/// Picks the correct C runtime function based on the LLVM value type
/// (`ptr` → string, `i64` → int, else → float) and whether a trailing
/// newline is required.
fn emit_print_call<'ctx>(
    builder: &Builder<'ctx>,
    rt: &Runtime<'ctx>,
    val: BasicValueEnum<'ctx>,
    newline: bool,
) {
    let fn_name = if val.is_pointer_value() {
        if newline { "aion_println" } else { "aion_print" }
    } else if val.is_int_value() {
        if newline { "aion_println_int" } else { "aion_print_int" }
    } else {
        if newline { "aion_println_float" } else { "aion_print_float" }
    };

    let f = rt[fn_name];
    let a: [BasicMetadataValueEnum; 1] = [val.into()];
    builder.build_call(f, &a, "call").expect("build call");
}

/// The kind of function being compiled — determines naming and return type.
enum FnKind<'a> {
    /// Top-level `fn main()` — returns i32 0.
    TopLevel,
    /// A function inside a user module — void return, name is `module_func`.
    Module { module_name: &'a str },
}

/// Compile a single function body to LLVM IR.
///
/// This is the unified codegen path used for both top-level functions
/// and user-module functions.  The `kind` parameter controls the LLVM
/// function signature (return type) and the symbol name mangling.
fn compile_fn_body<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    func: &Function,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
    kind: &FnKind<'_>,
) -> FunctionValue<'ctx> {
    let (fn_name, fn_type, returns_value) = match kind {
        FnKind::TopLevel => {
            let i32_type = context.i32_type();
            (func.name.clone(), i32_type.fn_type(&[], false), true)
        }
        FnKind::Module { module_name } => {
            let void = context.void_type();
            (format!("{module_name}_{}", func.name), void.fn_type(&[], false), false)
        }
    };

    let fn_val = module.add_function(&fn_name, fn_type, None);
    let entry = context.append_basic_block(fn_val, "entry");
    builder.position_at_end(entry);

    let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
        HashMap::new();

    for expr in &func.body {
        compile_expr(context, builder, expr, rt, module_fns, &mut variables);
    }

    if returns_value {
        let i32_type = context.i32_type();
        builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .expect("build return");
    } else {
        builder.build_return(None).expect("build void return");
    }

    fn_val
}

/// Compile a single top-level function definition.
pub fn compile_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    func: &Function,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
) {
    compile_fn_body(context, module, builder, func, rt, module_fns, &FnKind::TopLevel);
}

/// Compile all functions in a user-written Aion module.
///
/// Each `fn greet()` in module `utils` becomes `utils_greet` in LLVM IR.
pub fn compile_user_module<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    user_mod: &UserModule,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
) -> HashMap<String, FunctionValue<'ctx>> {
    let mut fns = HashMap::new();
    let kind = FnKind::Module { module_name: &user_mod.name };

    for func in &user_mod.functions {
        let fn_val = compile_fn_body(context, module, builder, func, rt, module_fns, &kind);
        fns.insert(func.name.clone(), fn_val);
    }

    fns
}

/// Lower a single expression to LLVM IR.
///
/// Returns `Some(value)` when the expression produces a result,
/// or `None` for void statements like `print`.
pub fn compile_expr<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    expr: &Expr,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
    variables: &mut HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)>,
) -> Option<BasicValueEnum<'ctx>> {
    match expr {
        // ── string literal ───────────────────────────────────
        Expr::StringLiteral(text) => {
            let global = builder
                .build_global_string_ptr(text, "str")
                .expect("build global string");
            Some(global.as_pointer_value().into())
        }

        // ── module.func(args…) ──────────────────────────────────
        Expr::ModuleCall { module, func, args } => {
            let mod_fns = module_fns.get(module).unwrap_or_else(|| {
                errors::fatal_with_hint(
                    Phase::Compiler,
                    format!("Module '{module}' was not imported"),
                    Some(format!("Add 'import {module};' at the top of your file")),
                );
            });
            let llvm_fn = mod_fns.get(func.as_str()).unwrap_or_else(|| {
                errors::fatal_with_hint(
                    Phase::Compiler,
                    format!("Function '{func}' not found in module '{module}'"),
                    Some(format!("Check spelling or module documentation")),
                );
            });

            let llvm_args: Vec<BasicMetadataValueEnum> = args
                .iter()
                .map(|a| {
                    compile_expr(context, builder, a, rt, module_fns, variables)
                        .expect("argument must produce a value")
                        .into()
                })
                .collect();

            let call = builder
                .build_call(*llvm_fn, &llvm_args, "modcall")
                .expect("build module call");

            match call.try_as_basic_value() {
                ValueKind::Basic(val) => Some(val),
                ValueKind::Instruction(_) => None,
            }
        }

        // ── variable definition ─────────────────────────────────
        Expr::VarDef { name, type_annotation, value } => {
            // Determine the LLVM type and optionally compile the init expr.
            let (llvm_type, init_val) = match (type_annotation.as_deref(), value) {
                (Some("Int"), _) => {
                    let ty = context.i64_type().as_basic_type_enum();
                    let val = value.as_ref().map(|v| {
                        compile_expr(context, builder, v, rt, module_fns, variables)
                            .expect("initializer must produce a value")
                    });
                    (ty, val)
                }
                (Some("Float"), _) => {
                    let ty = context.f64_type().as_basic_type_enum();
                    let val = value.as_ref().map(|v| {
                        compile_expr(context, builder, v, rt, module_fns, variables)
                            .expect("initializer must produce a value")
                    });
                    (ty, val)
                }
                (None, Some(init_expr)) => {
                    let val = compile_expr(context, builder, init_expr, rt, module_fns, variables)
                        .expect("variable initializer must produce a value");
                    (val.get_type(), Some(val))
                }
                (None, None) => {
                    errors::fatal_with_hint(
                        Phase::Compiler,
                        format!("Variable '{name}' has no type and no initial value"),
                        Some(format!("Try: {name} : Int := 0  or  {name} ::= 0")),
                    );
                }
                (Some(other), _) => errors::fatal_with_hint(
                    Phase::Compiler,
                    format!("Unsupported type: '{other}'"),
                    Some("Supported types: Int, Float".into()),
                ),
            };

            let alloca = builder
                .build_alloca(llvm_type, name)
                .expect("build alloca");

            if let Some(val) = init_val {
                builder.build_store(alloca, val).expect("build store");
            }

            variables.insert(name.clone(), (alloca, llvm_type));
            None
        }

        // ── variable reference ──────────────────────────────────
        Expr::VarRef(name) => {
            let (ptr, ty) = variables.get(name)
                .unwrap_or_else(|| errors::fatal(
                    Phase::Compiler,
                    format!("Undefined variable: '{name}'"),
                ));
            let val = builder
                .build_load(*ty, *ptr, name)
                .expect("build load");
            Some(val)
        }

        // ── unqualified function call ───────────────────────────
        Expr::FuncCall { name, args } => {
            // ── built-in print() / println(): type-based dispatch ─
            if (name == "print" || name == "println") && args.len() == 1 {
                let is_ln = name == "println";
                let val = compile_expr(context, builder, &args[0], rt, module_fns, variables);
                match val {
                    Some(v) => emit_print_call(builder, rt, v, is_ln),
                    None => errors::fatal(Phase::Compiler, "print() argument must produce a value"),
                }
                return None;
            }
            // ── built-in println() with no args ─────────────────
            if name == "println" && args.is_empty() {
                let empty = builder.build_global_string_ptr("", "empty").expect("build empty str");
                let a: [BasicMetadataValueEnum; 1] = [empty.as_pointer_value().into()];
                builder.build_call(rt["aion_println"], &a, "call").expect("build call");
                return None;
            }

            // ── general function lookup ──────────────────────────
            // Search all modules for a matching function name.
            let mut found: Option<&FunctionValue<'ctx>> = None;
            for (_mod_name, fns) in module_fns {
                if let Some(f) = fns.get(name.as_str()) {
                    found = Some(f);
                    break;
                }
            }
            let llvm_fn = found.unwrap_or_else(|| {
                errors::fatal_with_hint(
                    Phase::Compiler,
                    format!("Undefined function: '{name}'"),
                    Some("Check spelling or make sure the function is defined in the prelude or an imported module".into()),
                );
            });

            let llvm_args: Vec<BasicMetadataValueEnum> = args
                .iter()
                .map(|a| {
                    compile_expr(context, builder, a, rt, module_fns, variables)
                        .expect("argument must produce a value")
                        .into()
                })
                .collect();

            let call = builder
                .build_call(*llvm_fn, &llvm_args, "call")
                .expect("build function call");

            match call.try_as_basic_value() {
                ValueKind::Basic(val) => Some(val),
                ValueKind::Instruction(_) => None,
            }
        }

        // ── float literal ───────────────────────────────────────
        Expr::FloatLiteral(v) => {
            Some(context.f64_type().const_float(*v).into())
        }

        // ── integer literal ─────────────────────────────────────
        Expr::IntLiteral(v) => {
            Some(context.i64_type().const_int(*v as u64, false).into())
        }
    }
}
