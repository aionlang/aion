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

/// Handles to the declared Aion runtime functions in the LLVM module.
#[allow(dead_code)]
pub struct Runtime<'ctx> {
    pub aion_print:       FunctionValue<'ctx>,
    pub aion_print_int:   FunctionValue<'ctx>,
    pub aion_print_float: FunctionValue<'ctx>,
    pub aion_panic:       FunctionValue<'ctx>,
    pub aion_alloc:       FunctionValue<'ctx>,
    pub aion_free:        FunctionValue<'ctx>,
}

// ═══════════════════════════════════════════════════════════════════
// Runtime & stdlib declaration helpers
// ═══════════════════════════════════════════════════════════════════

/// Declare the external `aion_*` core runtime functions.
pub fn declare_runtime<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> Runtime<'ctx> {
    let void = context.void_type();
    let ptr = context.ptr_type(inkwell::AddressSpace::default());
    let i64 = context.i64_type();
    let f64_type = context.f64_type();

    let aion_print = module.add_function(
        "aion_print",
        void.fn_type(&[ptr.into()], false),
        None,
    );
    let aion_print_int = module.add_function(
        "aion_print_int",
        void.fn_type(&[i64.into()], false),
        None,
    );
    let aion_print_float = module.add_function(
        "aion_print_float",
        void.fn_type(&[f64_type.into()], false),
        None,
    );
    let aion_panic = module.add_function(
        "aion_panic",
        void.fn_type(&[ptr.into()], false),
        None,
    );
    let aion_alloc = module.add_function(
        "aion_alloc",
        ptr.fn_type(&[i64.into()], false),
        None,
    );
    let aion_free = module.add_function(
        "aion_free",
        void.fn_type(&[ptr.into()], false),
        None,
    );

    Runtime {
        aion_print,
        aion_print_int,
        aion_print_float,
        aion_panic,
        aion_alloc,
        aion_free,
    }
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

/// Compile a single top-level function definition.
pub fn compile_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    func: &Function,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
) {
    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let fn_val = module.add_function(&func.name, fn_type, None);

    let entry = context.append_basic_block(fn_val, "entry");
    builder.position_at_end(entry);

    let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
        HashMap::new();

    for expr in &func.body {
        compile_expr(context, builder, expr, rt, module_fns, &mut variables);
    }

    builder
        .build_return(Some(&i32_type.const_int(0, false)))
        .expect("build return");
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
    let void = context.void_type();
    let mut fns = HashMap::new();

    for func in &user_mod.functions {
        let mangled = format!("{}_{}", user_mod.name, func.name);

        let fn_type = void.fn_type(&[], false);
        let fn_val = module.add_function(&mangled, fn_type, None);

        let entry = context.append_basic_block(fn_val, "entry");
        builder.position_at_end(entry);

        let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
            HashMap::new();

        for expr in &func.body {
            compile_expr(context, builder, expr, rt, module_fns, &mut variables);
        }

        builder.build_return(None).expect("build void return");
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
        // ── print("string literal") ─────────────────────────────
        Expr::PrintStr(text) => {
            let global = builder
                .build_global_string_ptr(text, "str")
                .expect("build global string");
            let args: [BasicMetadataValueEnum; 1] = [global.as_pointer_value().into()];
            builder
                .build_call(rt.aion_print, &args, "call")
                .expect("build call");
            None
        }

        // ── print(<expr>) ───────────────────────────────────────
        Expr::PrintExpr(inner) => {
            let val = compile_expr(context, builder, inner, rt, module_fns, variables)
                .expect("print argument must produce a value");

            if val.is_int_value() {
                let args: [BasicMetadataValueEnum; 1] = [val.into()];
                builder
                    .build_call(rt.aion_print_int, &args, "call")
                    .expect("build call to aion_print_int");
            } else {
                let args: [BasicMetadataValueEnum; 1] = [val.into()];
                builder
                    .build_call(rt.aion_print_float, &args, "call")
                    .expect("build call to aion_print_float");
            }
            None
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
