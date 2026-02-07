use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::{PointerValue, FunctionValue};

use crate::ast::{Function, Param, UserModule};
use super::{Runtime, ModuleFns};
use super::expr::compile_expr;

/// The kind of function being compiled — determines naming and return type.
enum FnKind<'a> {
    /// Top-level `fn main()` — returns i32 0.
    TopLevel,
    /// A function inside a user module — void return, name is `module_func`.
    Module { module_name: &'a str },
}

/// Map a user-facing type name to the corresponding LLVM type.
fn resolve_type<'ctx>(
    context: &'ctx Context,
    type_name: Option<&str>,
) -> Option<inkwell::types::BasicTypeEnum<'ctx>> {
    match type_name {
        Some("Int")    => Some(context.i64_type().into()),
        Some("Float")  => Some(context.f64_type().into()),
        Some("Bool")   => Some(context.bool_type().into()),
        Some("String") => Some(context.ptr_type(inkwell::AddressSpace::default()).into()),
        _ => None,
    }
}

/// Build the LLVM param types for a parameter list.
fn build_param_types<'ctx>(
    context: &'ctx Context,
    params: &[Param],
) -> Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> {
    params
        .iter()
        .map(|p| {
            resolve_type(context, Some(p.type_annotation.as_str()))
                .unwrap_or_else(|| {
                    crate::errors::fatal(
                        crate::errors::Phase::Compiler,
                        format!("Unsupported parameter type: '{}'", p.type_annotation),
                    )
                })
                .into()
        })
        .collect()
}

/// Compile a single function body to LLVM IR.
///
/// This is the unified codegen path used for both top-level functions
/// and user-module functions.  The `kind` parameter controls the LLVM
/// function signature (return type) and the symbol name mangling.
///
/// **Arrow functions** (`fn run() => expr`) with a value-producing
/// expression get an implicit return of that value.  Arrow functions
/// whose single expression is void (e.g. `println(…)`) behave like
/// normal block-body functions.
fn compile_fn_body<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    func: &Function,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
    kind: &FnKind<'_>,
) -> FunctionValue<'ctx> {
    // ── For arrow functions we first probe the single expression to
    //    decide whether it produces a value (and therefore the LLVM
    //    function should have a matching return type) or is void.
    let arrow_returns_value = func.is_arrow && func.body.len() == 1 && {
        // Peek: expressions that never produce a runtime value.
        !matches!(
            &func.body[0],
            crate::ast::Expr::FuncCall { name, .. }
                if name == "print" || name == "println"
        ) && !matches!(&func.body[0], crate::ast::Expr::VarDef { .. })
          && !matches!(&func.body[0], crate::ast::Expr::VarAssign { .. })
    };

    let (fn_name, fn_type, _returns_value) = match kind {
        FnKind::TopLevel => {
            let param_types = build_param_types(context, &func.params);
            let i32_type = context.i32_type();
            (func.name.clone(), i32_type.fn_type(&param_types, false), true)
        }
        FnKind::Module { module_name } => {
            let param_types = build_param_types(context, &func.params);
            if arrow_returns_value {
                let i64_type = context.i64_type();
                (format!("{module_name}_{}", func.name), i64_type.fn_type(&param_types, false), false)
            } else {
                let void = context.void_type();
                (format!("{module_name}_{}", func.name), void.fn_type(&param_types, false), false)
            }
        }
    };

    let fn_val = module.add_function(&fn_name, fn_type, None);
    compile_fn_body_inner(context, module, builder, func, rt, module_fns, kind, fn_val);
    fn_val
}

/// Compile the body of `func` into the *existing* LLVM function `fn_val`.
///
/// Used by `compile_functions` for top-level functions that have already
/// been forward-declared.
fn compile_fn_body_into<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    func: &Function,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
    kind: &FnKind<'_>,
) {
    let fn_val = module.get_function(&func.name)
        .expect("function must be forward-declared");
    compile_fn_body_inner(context, module, builder, func, rt, module_fns, kind, fn_val);
}

/// Shared implementation: emits the body into an already-created LLVM function.
fn compile_fn_body_inner<'ctx>(
    context: &'ctx Context,
    _module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    func: &Function,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
    kind: &FnKind<'_>,
    fn_val: FunctionValue<'ctx>,
) {
    let returns_value = matches!(kind, FnKind::TopLevel) && func.name == "main";

    let entry = context.append_basic_block(fn_val, "entry");
    builder.position_at_end(entry);

    let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
        HashMap::new();

    // ── Bind function parameters as local variables. ─────────────
    for (i, param) in func.params.iter().enumerate() {
        let llvm_ty = resolve_type(context, Some(param.type_annotation.as_str()))
            .unwrap_or_else(|| {
                crate::errors::fatal(
                    crate::errors::Phase::Compiler,
                    format!("Unsupported parameter type: '{}'", param.type_annotation),
                )
            });
        let alloca = builder.build_alloca(llvm_ty, &param.name).expect("build param alloca");
        let arg_val = fn_val.get_nth_param(i as u32).expect("param index");
        builder.build_store(alloca, arg_val).expect("store param");
        variables.insert(param.name.clone(), (alloca, llvm_ty));
    }

    // ── Arrow function: compile the single expression and maybe return it.
    if func.is_arrow && func.body.len() == 1 {
        let val = compile_expr(context, builder, &func.body[0], rt, module_fns, &mut variables);
        let is_main = matches!(kind, FnKind::TopLevel) && func.name == "main";
        let has_return_type = func.return_type.is_some();

        if is_main {
            // main() always returns i32 0 regardless of body.
            let i32_type = context.i32_type();
            builder
                .build_return(Some(&i32_type.const_int(0, false)))
                .expect("build return");
        } else if let Some(v) = val {
            if has_return_type {
                // Has a return type annotation → return the value.
                builder.build_return(Some(&v)).expect("build arrow return");
            } else {
                // No return type, void function that happened to produce a value.
                builder.build_return(None).expect("build void return");
            }
        } else {
            // The expression was void — emit a plain return.
            builder.build_return(None).expect("build void return");
        }
        return;
    }

    // ── Block function: compile every statement. ─────────────────
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
}

/// Forward-declare all top-level functions so they can call each other
/// regardless of definition order, then compile their bodies.
pub fn compile_functions<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    functions: &[Function],
    rt: &Runtime<'ctx>,
    module_fns: &mut ModuleFns<'ctx>,
) {
    // 1. Forward-declare every top-level function with the correct type.
    let mut top_fns: HashMap<String, FunctionValue<'ctx>> = HashMap::new();
    for func in functions {
        let param_types = build_param_types(context, &func.params);
        let fn_type = if func.name == "main" {
            // Entry point always returns i32.
            context.i32_type().fn_type(&param_types, false)
        } else if let Some(ret_ty) = resolve_type(context, func.return_type.as_deref()) {
            ret_ty.fn_type(&param_types, false)
        } else {
            // No return type annotation → void.
            context.void_type().fn_type(&param_types, false)
        };
        let fn_val = module.add_function(&func.name, fn_type, None);
        top_fns.insert(func.name.clone(), fn_val);
    }
    // Register them under a synthetic "__top" module so the expr
    // compiler can find them via the normal module_fns lookup.
    module_fns.insert("__top".to_string(), top_fns);

    // 2. Compile each body (the LLVM function already exists).
    for func in functions {
        compile_fn_body_into(
            context, module, builder, func, rt, module_fns, &FnKind::TopLevel,
        );
    }
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
