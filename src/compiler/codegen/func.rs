use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::{PointerValue, FunctionValue};

use crate::ast::{Function, FunctionKind, Param, TypeDef, UserModule};
use super::{Runtime, ModuleFns, TypeRegistry};
use super::expr::compile_expr;

/// The kind of function being compiled — determines naming and return type.
enum FnKind<'a> {
    /// Top-level `fn main()` — returns i32 0.
    TopLevel,
    /// A function inside a user module — void return, name is `module_func`.
    Module { module_name: &'a str },
}

/// Map a user-facing type name to the corresponding LLVM type.
///
/// Primitive types map to their LLVM equivalents; any other non-empty
/// name is assumed to be a user-defined struct type and is represented
/// as an opaque pointer (structs are heap-allocated).
fn resolve_type<'ctx>(
    context: &'ctx Context,
    type_name: Option<&str>,
) -> Option<inkwell::types::BasicTypeEnum<'ctx>> {
    match type_name {
        Some("Int")    => Some(context.i64_type().into()),
        Some("Float")  => Some(context.f64_type().into()),
        Some("Bool")   => Some(context.bool_type().into()),
        Some("String") => Some(context.ptr_type(inkwell::AddressSpace::default()).into()),
        // User-defined types are heap-allocated structs → pointer.
        Some(_)        => Some(context.ptr_type(inkwell::AddressSpace::default()).into()),
        None           => None,
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
    type_registry: &TypeRegistry<'ctx>,
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
    compile_fn_body_inner(context, module, builder, func, rt, module_fns, kind, fn_val, type_registry, &HashMap::new());
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
    type_registry: &TypeRegistry<'ctx>,
) {
    let fn_val = module.get_function(&func.name)
        .expect("function must be forward-declared");
    compile_fn_body_inner(context, module, builder, func, rt, module_fns, kind, fn_val, type_registry, &HashMap::new());
}

/// Shared implementation: emits the body into an already-created LLVM function.
///
/// `type_name_overrides` allows callers to override which concrete type
/// name a parameter is associated with — the key is monomorphization:
/// `animal: Animal` can be mapped to `Dog` so that `animal.bark()`
/// dispatches to `Dog__bark`.
fn compile_fn_body_inner<'ctx>(
    context: &'ctx Context,
    _module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    func: &Function,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
    kind: &FnKind<'_>,
    fn_val: FunctionValue<'ctx>,
    type_registry: &TypeRegistry<'ctx>,
    type_name_overrides: &HashMap<String, String>,
) {
    let _returns_value = matches!(kind, FnKind::TopLevel) && func.name == "main";

    let entry = context.append_basic_block(fn_val, "entry");
    builder.position_at_end(entry);

    let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
        HashMap::new();

    // Track which variables hold user-defined struct types (for field access).
    let mut var_type_names: HashMap<String, String> = HashMap::new();

    // ── Bind function parameters as local variables. ─────────────
    let mut ptr_allocas: Vec<PointerValue<'ctx>> = Vec::new();

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

        // If the parameter is a user-defined type, register it so that
        // `animal.bark()` is resolved as a method call, not a module call.
        let ty = param.type_annotation.as_str();
        if !matches!(ty, "Int" | "Float" | "Bool" | "String") {
            var_type_names.insert(param.name.clone(), ty.to_string());
        }

        // Collect pointer-typed allocas for GC root registration.
        if llvm_ty.is_pointer_type() {
            ptr_allocas.push(alloca);
        }
    }

    // Apply monomorphization overrides: e.g. map `animal` → `Dog`
    // so that method calls dispatch to the concrete type's methods.
    for (name, concrete_ty) in type_name_overrides {
        var_type_names.insert(name.clone(), concrete_ty.clone());
    }

    // Always enable shadow-stack GC so that both parameter and
    // local-variable gcroot calls are valid.
    fn_val.set_gc("shadow-stack");
    if !ptr_allocas.is_empty() {
        let gcroot = rt["llvm.gcroot"];
        let null_meta = context.ptr_type(inkwell::AddressSpace::default()).const_null();
        for alloca in &ptr_allocas {
            builder.build_call(gcroot, &[(*alloca).into(), null_meta.into()], "")
                .expect("emit gcroot");
        }
    }

    // ── Arrow function: compile the single expression and maybe return it.
    if func.is_arrow && func.body.len() == 1 {
        let val = compile_expr(context, builder, &func.body[0], rt, module_fns, &mut variables, type_registry, &mut var_type_names);
        let is_main = matches!(kind, FnKind::TopLevel) && func.name == "main";
        let has_return_type = func.return_type.is_some();

        if is_main {
            let i32_type = context.i32_type();
            builder
                .build_return(Some(&i32_type.const_int(0, false)))
                .expect("build return");
        } else if let Some(v) = val {
            if has_return_type {
                builder.build_return(Some(&v)).expect("build arrow return");
            } else {
                builder.build_return(None).expect("build void return");
            }
        } else {
            builder.build_return(None).expect("build void return");
        }
        return;
    }

    // ── Block function: compile every statement. ─────────────────
    for expr in &func.body {
        compile_expr(context, builder, expr, rt, module_fns, &mut variables, type_registry, &mut var_type_names);
    }

    // Add a default terminator if the last basic block needs one.
    let needs_terminator = builder.get_insert_block()
        .map(|bb| bb.get_terminator().is_none())
        .unwrap_or(false);
    if needs_terminator {
        let ret_ty = fn_val.get_type().get_return_type();
        match ret_ty {
            None => { builder.build_return(None).expect("build void return"); }
            Some(ty) if ty.is_int_type() => {
                let zero = ty.into_int_type().const_zero();
                builder.build_return(Some(&zero)).expect("build default int return");
            }
            Some(ty) if ty.is_float_type() => {
                let zero = ty.into_float_type().const_zero();
                builder.build_return(Some(&zero)).expect("build default float return");
            }
            Some(ty) if ty.is_pointer_type() => {
                let null = ty.into_pointer_type().const_null();
                builder.build_return(Some(&null)).expect("build default ptr return");
            }
            Some(ty) if ty.is_struct_type() => {
                let zero = ty.into_struct_type().const_zero();
                builder.build_return(Some(&zero)).expect("build default struct return");
            }
            _ => { builder.build_return(None).expect("build void return"); }
        }
    }
}

/// Forward-declare all top-level functions so they can be called from
/// constructors and methods that are compiled earlier.
///
/// Registers them under the synthetic `"__top"` module in `module_fns`.
pub fn forward_declare_functions<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    functions: &[Function],
    module_fns: &mut ModuleFns<'ctx>,
) {
    let mut top_fns: HashMap<String, FunctionValue<'ctx>> = HashMap::new();
    for func in functions {
        // Extern and abstract functions have no Aion body to compile;
        // extern symbols are resolved by the C linker, abstract ones
        // are only placeholders for override checks.
        if func.kind == FunctionKind::Extern || func.kind == FunctionKind::Abstract {
            continue;
        }
        let param_types = build_param_types(context, &func.params);
        let fn_type = if func.name == "main" {
            context.i32_type().fn_type(&param_types, false)
        } else if let Some(ret_ty) = resolve_type(context, func.return_type.as_deref()) {
            ret_ty.fn_type(&param_types, false)
        } else {
            context.void_type().fn_type(&param_types, false)
        };
        let fn_val = module.add_function(&func.name, fn_type, None);
        top_fns.insert(func.name.clone(), fn_val);
    }
    module_fns.insert("__top".to_string(), top_fns);
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
    type_registry: &TypeRegistry<'ctx>,
) {
    // 1. Forward-declare if not already done by forward_declare_functions.
    if !module_fns.contains_key("__top") {
        forward_declare_functions(context, module, functions, module_fns);
    }

    // 2. Compile each body (the LLVM function already exists).
    for func in functions {
        // Skip bodyless functions.
        if func.kind == FunctionKind::Extern || func.kind == FunctionKind::Abstract {
            continue;
        }
        compile_fn_body_into(
            context, module, builder, func, rt, module_fns, &FnKind::TopLevel, type_registry,
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
    type_registry: &TypeRegistry<'ctx>,
) -> HashMap<String, FunctionValue<'ctx>> {
    let mut fns = HashMap::new();
    let kind = FnKind::Module { module_name: &user_mod.name };

    for func in &user_mod.functions {
        let fn_val = compile_fn_body(context, module, builder, func, rt, module_fns, &kind, type_registry);
        fns.insert(func.name.clone(), fn_val);
    }

    fns
}

/// Generate specialised copies of functions whose parameters accept a
/// parent type, one per concrete child type.  This is the
/// **monomorphization** pass.
///
/// For example, given:
///
/// ```text
/// fn makeSound(animal: Animal) -> String { return animal.bark() }
/// type Dog: Animal { fn bark() -> String { return "Woof!" } }
/// ```
///
/// We generate `makeSound__Dog` whose body compiles `animal.bark()` as
/// a call to `Dog__bark` (not `Animal__bark`).
///
/// The specialised copies are registered under `"__top"` in `module_fns`
/// so that call-site code in `expr.rs` can find them.
pub fn monomorphize_functions<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    functions: &[Function],
    _type_defs: &[TypeDef],
    type_registry: &TypeRegistry<'ctx>,
    rt: &Runtime<'ctx>,
    module_fns: &mut ModuleFns<'ctx>,
) {
    // Build parent → [children] map from the type registry.
    let mut children_of: HashMap<String, Vec<String>> = HashMap::new();
    for (name, info) in type_registry.iter() {
        if let Some(ref parent) = info.parent {
            children_of.entry(parent.clone()).or_default().push(name.clone());
        }
    }

    if children_of.is_empty() {
        return; // nothing to monomorphize
    }

    for func in functions {
        if func.kind == FunctionKind::Extern || func.kind == FunctionKind::Abstract {
            continue;
        }

        // Collect (param_index, param_name, parent_type, [child_types])
        // for each polymorphic parameter.
        let mut poly_params: Vec<(usize, &str, &str, &Vec<String>)> = Vec::new();
        for (i, param) in func.params.iter().enumerate() {
            let ty = param.type_annotation.as_str();
            if let Some(kids) = children_of.get(ty) {
                poly_params.push((i, &param.name, ty, kids));
            }
        }

        if poly_params.is_empty() {
            continue;
        }

        // For simplicity, monomorphize over the first polymorphic
        // parameter.  (Multi-param monomorphization would require the
        // Cartesian product — future enhancement.)
        let (_idx, param_name, _parent_ty, child_types) = poly_params[0];

        for child_type in child_types {
            let spec_name = format!("{}__{}", func.name, child_type);

            // Build LLVM function type (same as the original).
            let param_types = build_param_types(context, &func.params);
            let fn_type = if func.name == "main" {
                context.i32_type().fn_type(&param_types, false)
            } else if let Some(ret_ty) = resolve_type(context, func.return_type.as_deref()) {
                ret_ty.fn_type(&param_types, false)
            } else {
                context.void_type().fn_type(&param_types, false)
            };
            let fn_val = module.add_function(&spec_name, fn_type, None);

            // Override: map the polymorphic param to the concrete child type.
            let mut overrides = HashMap::new();
            overrides.insert(param_name.to_string(), child_type.clone());

            compile_fn_body_inner(
                context, module, builder, func, rt, module_fns,
                &FnKind::TopLevel, fn_val, type_registry, &overrides,
            );

            // Register under __top.
            let top = module_fns.entry("__top".to_string()).or_default();
            top.insert(spec_name, fn_val);
        }
    }
}
