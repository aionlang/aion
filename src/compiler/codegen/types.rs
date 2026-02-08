//! Compile user-defined type definitions into LLVM struct types.

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::{FunctionValue, PointerValue};

use crate::ast::{Function, FunctionKind, TypeDef};
use super::{Runtime, ModuleFns, TypeInfo, TypeRegistry};
use super::expr::compile_expr;

/// Map a user-facing type name to an LLVM basic type (for struct fields).
///
/// `known_structs` maps type names to their opaque struct types,
/// allowing cross-references between types (e.g. `stream: TcpStream`).
fn resolve_field_type<'ctx>(
    context: &'ctx Context,
    type_name: &str,
    known_structs: &HashMap<String, inkwell::types::StructType<'ctx>>,
) -> inkwell::types::BasicTypeEnum<'ctx> {
    match type_name {
        "Int"    => context.i64_type().as_basic_type_enum(),
        "Float"  => context.f64_type().as_basic_type_enum(),
        "Bool"   => context.bool_type().as_basic_type_enum(),
        "String" => context.ptr_type(inkwell::AddressSpace::default()).as_basic_type_enum(),
        other => {
            if let Some(st) = known_structs.get(other) {
                st.as_basic_type_enum()
            } else {
                // User-defined types passed by pointer.
                context.ptr_type(inkwell::AddressSpace::default()).as_basic_type_enum()
            }
        }
    }
}

/// Build a map from type name → StructType from a TypeRegistry.
fn struct_types_from_registry<'ctx>(
    registry: &TypeRegistry<'ctx>,
) -> HashMap<String, inkwell::types::StructType<'ctx>> {
    registry.iter().map(|(name, info)| (name.clone(), info.struct_type)).collect()
}

/// Create LLVM struct types for all user-defined type definitions.
///
/// Uses a two-pass approach so types can reference each other:
///   1. Create opaque struct types for every `type` definition.
///   2. Resolve field types and set the struct body.
///
/// Child types (`type Dog: Animal`) inherit all parent fields — the
/// parent's fields are prepended so that field layout is compatible.
///
/// Returns a [`TypeRegistry`] that maps each type name to its
/// LLVM struct layout and field metadata.
pub fn compile_type_defs<'ctx>(
    context: &'ctx Context,
    type_defs: &[TypeDef],
) -> TypeRegistry<'ctx> {
    let mut registry = TypeRegistry::new();

    // ── Pass 1: create opaque struct types ────────────────────
    let mut struct_types: HashMap<String, inkwell::types::StructType<'ctx>> = HashMap::new();
    for td in type_defs {
        let struct_type = context.opaque_struct_type(&td.name);
        struct_types.insert(td.name.clone(), struct_type);
    }

    // Build a quick lookup: type name → TypeDef, for parent resolution.
    let td_map: HashMap<&str, &TypeDef> = type_defs.iter().map(|td| (td.name.as_str(), td)).collect();

    // ── Pass 2: resolve fields (with inheritance) and set struct bodies ──
    for td in type_defs {
        let struct_type = struct_types[&td.name];

        // Inherit parent fields first.
        let mut fields: Vec<(String, inkwell::types::BasicTypeEnum<'ctx>)> = Vec::new();
        if let Some(ref parent_name) = td.parent {
            if let Some(parent_td) = td_map.get(parent_name.as_str()) {
                for f in &parent_td.fields {
                    let llvm_ty = resolve_field_type(context, &f.type_name, &struct_types);
                    fields.push((f.name.clone(), llvm_ty));
                }
            }
        }

        // Then add own fields (skip if already inherited).
        for f in &td.fields {
            if !fields.iter().any(|(name, _)| name == &f.name) {
                let llvm_ty = resolve_field_type(context, &f.type_name, &struct_types);
                fields.push((f.name.clone(), llvm_ty));
            }
        }

        let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> =
            fields.iter().map(|(_, ty)| *ty).collect();
        struct_type.set_body(&field_types, false);

        // Constructor params: for child types without explicit constructors
        // or constructor_params, inherit from parent's constructor_params
        // so that `Dog("Fido")` maps the same params as `Animal("Fido")`.
        let constructor_params = if !td.constructor_params.is_empty() {
            td.constructor_params.clone()
        } else if td.constructor.is_some() {
            // explicit constructor — params come from the constructor block
            Vec::new()
        } else if let Some(ref parent_name) = td.parent {
            // Child without own constructor: inherit parent's constructor_params.
            if let Some(parent_td) = td_map.get(parent_name.as_str()) {
                if !parent_td.constructor_params.is_empty() {
                    // Parent uses shorthand constructor params.
                    parent_td.constructor_params.clone()
                } else if let Some(ref ctor) = parent_td.constructor {
                    // Parent has explicit constructor block — inherit its param names.
                    ctor.params.iter().map(|p| p.name.clone()).collect()
                } else {
                    fields.iter().map(|(name, _)| name.clone()).collect()
                }
            } else {
                fields.iter().map(|(name, _)| name.clone()).collect()
            }
        } else {
            fields.iter().map(|(name, _)| name.clone()).collect()
        };

        // Only mark has_explicit_constructor if THIS type has its own
        // constructor block.  Child types without their own constructor
        // use the shorthand path with inherited constructor_params.
        let has_explicit_ctor = td.constructor.is_some();

        registry.insert(td.name.clone(), TypeInfo {
            struct_type,
            fields,
            constructor_params,
            has_explicit_constructor: has_explicit_ctor,
            parent: td.parent.clone(),
        });
    }

    registry
}

/// Check whether `child_type` is a subtype of `parent_type` by walking
/// the parent chain.  Returns `true` if they are the same type too.
pub fn is_subtype_of(
    child_type: &str,
    parent_type: &str,
    type_registry: &HashMap<String, super::TypeInfo<'_>>,
) -> bool {
    if child_type == parent_type { return true; }
    let mut current = child_type;
    loop {
        match type_registry.get(current).and_then(|info| info.parent.as_deref()) {
            Some(parent) => {
                if parent == parent_type { return true; }
                current = parent;
            }
            None => return false,
        }
    }
}

/// Walk the type hierarchy to find a concrete (non-abstract) method
/// for `method_name` starting at `type_name`, then walking parents.
pub fn resolve_method_for_type<'ctx>(
    type_name: &str,
    method_name: &str,
    type_defs: &[TypeDef],
    module_fns: &super::ModuleFns<'ctx>,
) -> Option<FunctionValue<'ctx>> {
    let td_map: HashMap<&str, &TypeDef> = type_defs.iter().map(|td| (td.name.as_str(), td)).collect();
    resolve_method_inner(type_name, method_name, &td_map, module_fns)
}

fn resolve_method_inner<'ctx>(
    type_name: &str,
    method_name: &str,
    td_map: &HashMap<&str, &TypeDef>,
    module_fns: &super::ModuleFns<'ctx>,
) -> Option<FunctionValue<'ctx>> {
    let method_key = format!("__methods_{type_name}");
    if let Some(methods) = module_fns.get(&method_key) {
        if let Some(fn_val) = methods.get(method_name) {
            if let Some(td) = td_map.get(type_name) {
                let is_abstract = td.methods.iter()
                    .any(|m| m.name == method_name && m.kind == FunctionKind::Abstract);
                if !is_abstract {
                    return Some(*fn_val);
                }
            }
        }
    }
    if let Some(td) = td_map.get(type_name) {
        if let Some(ref parent_name) = td.parent {
            return resolve_method_inner(parent_name, method_name, td_map, module_fns);
        }
    }
    None
}

/// Compile explicit `constructor(…) { … }` blocks into LLVM functions.
///
/// Uses a two-pass approach:
///   1. Forward-declare all constructors so they can call each other.
///   2. Compile the constructor bodies.
///
/// Each constructor becomes a function named after the type (e.g. `Animal`)
/// that allocates the struct, executes the body, and returns it by value.
/// Returns a map of type_name → FunctionValue to add to `module_fns`.
pub fn compile_constructors<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    type_defs: &[TypeDef],
    type_registry: &TypeRegistry<'ctx>,
    rt: &Runtime<'ctx>,
    module_fns: &mut ModuleFns<'ctx>,
) -> HashMap<String, FunctionValue<'ctx>> {
    let mut ctor_fns = HashMap::new();
    let known_structs = struct_types_from_registry(type_registry);

    // ── Pass 1: forward-declare all constructors ─────────────
    let mut declared: Vec<(&TypeDef, FunctionValue<'ctx>)> = Vec::new();
    for td in type_defs {
        let ctor = match &td.constructor {
            Some(c) => c,
            None => continue,
        };
        let type_info = &type_registry[&td.name];

        let param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = ctor
            .params
            .iter()
            .map(|p| {
                resolve_field_type(context, &p.type_annotation, &known_structs).into()
            })
            .collect();

        let fn_type = type_info.struct_type.fn_type(&param_types, false);
        let fn_val = module.add_function(&td.name, fn_type, None);
        ctor_fns.insert(td.name.clone(), fn_val);
        declared.push((td, fn_val));
    }

    // Register forward-declared constructors so bodies can call them.
    module_fns.insert("__ctors".to_string(), ctor_fns.clone());

    // ── Pass 2: compile constructor bodies ───────────────────
    for (td, fn_val) in &declared {
        let ctor = td.constructor.as_ref().unwrap();
        let type_info = &type_registry[&td.name];

        // Enable LLVM's built-in shadow-stack GC strategy.
        // (set before emitting body, but gcroot calls come later)
        let entry = context.append_basic_block(*fn_val, "entry");
        builder.position_at_end(entry);

        let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
            HashMap::new();
        let mut var_type_names: HashMap<String, String> = HashMap::new();

        // Allocate the struct instance and bind `it` as a pointer to it.
        let struct_alloca = builder
            .build_alloca(type_info.struct_type, "it")
            .expect("alloca struct for constructor");
        variables.insert(
            "it".to_string(),
            (struct_alloca, type_info.struct_type.as_basic_type_enum()),
        );
        var_type_names.insert("it".to_string(), td.name.clone());

        // Bind constructor parameters as local variables.
        let mut ptr_allocas: Vec<PointerValue<'ctx>> = Vec::new();

        for (i, param) in ctor.params.iter().enumerate() {
            let llvm_ty = resolve_field_type(context, &param.type_annotation, &known_structs);
            let alloca = builder
                .build_alloca(llvm_ty, &param.name)
                .expect("alloca ctor param");
            let arg_val = fn_val.get_nth_param(i as u32).expect("ctor param index");
            builder.build_store(alloca, arg_val).expect("store ctor param");
            variables.insert(param.name.clone(), (alloca, llvm_ty));

            // Collect pointer-typed allocas for GC root registration.
            if llvm_ty.is_pointer_type() {
                ptr_allocas.push(alloca);
            }
        }

        // Always enable shadow-stack GC so local-variable gcroots are valid.
        fn_val.set_gc("shadow-stack");
        if !ptr_allocas.is_empty() {
            let gcroot = rt["llvm.gcroot"];
            let null_meta = context.ptr_type(inkwell::AddressSpace::default()).const_null();
            for alloca in &ptr_allocas {
                builder.build_call(gcroot, &[(*alloca).into(), null_meta.into()], "")
                    .expect("emit gcroot");
            }
        }

        // Compile the constructor body.
        for expr in &ctor.body {
            compile_expr(
                context, builder, expr, rt, module_fns,
                &mut variables, type_registry, &mut var_type_names,
            );
        }

        // Return the fully initialized struct.
        let struct_val = builder
            .build_load(type_info.struct_type, struct_alloca, "ret_struct")
            .expect("load struct for return");
        builder.build_return(Some(&struct_val)).expect("return struct");
    }

    ctor_fns
}

/// Compile methods for user-defined types into LLVM functions.
///
/// Uses a two-pass approach:
///   1. Forward-declare all methods so they can call each other.
///   2. Compile the method bodies.
///
/// Handles both:
/// - Inline methods defined inside `type Animal { fn bark() { … } }`
/// - Impl methods defined outside: `fn Animal::speak() { … }`
///
/// Each method is compiled as `TypeName__method_name` in LLVM IR and
/// receives a hidden first parameter `it: ptr` (pointer to the struct
/// instance).
///
/// Inserts `__methods_TypeName` entries into `module_fns` before bodies
/// are compiled, so method bodies can call methods on other types.
pub fn compile_methods<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    type_defs: &[TypeDef],
    impl_methods: &[(String, Function)],
    type_registry: &TypeRegistry<'ctx>,
    rt: &Runtime<'ctx>,
    module_fns: &mut ModuleFns<'ctx>,
) -> HashMap<String, HashMap<String, FunctionValue<'ctx>>> {
    let mut all_methods: HashMap<String, HashMap<String, FunctionValue<'ctx>>> = HashMap::new();
    let known_structs = struct_types_from_registry(type_registry);

    // Collect methods: (type_name, &Function) from both sources.
    let mut pairs: Vec<(String, &Function)> = Vec::new();

    for td in type_defs {
        for method in &td.methods {
            pairs.push((td.name.clone(), method));
        }
    }
    for (type_name, func) in impl_methods {
        pairs.push((type_name.clone(), func));
    }

    // ── Pass 1: forward-declare all methods ──────────────────
    let mut declared: Vec<(String, &Function, FunctionValue<'ctx>)> = Vec::new();

    for (type_name, func) in &pairs {
        let type_info = match type_registry.get(type_name.as_str()) {
            Some(ti) => ti,
            None => crate::errors::fatal(
                crate::errors::Phase::Compiler,
                format!("Cannot define method on unknown type '{type_name}'"),
            ),
        };

        let ptr_type = context.ptr_type(inkwell::AddressSpace::default());
        let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            vec![ptr_type.into()];
        for p in &func.params {
            param_types.push(resolve_field_type(context, &p.type_annotation, &known_structs).into());
        }

        let fn_type = match func.return_type.as_deref() {
            Some("Int")    => context.i64_type().fn_type(&param_types, false),
            Some("Float")  => context.f64_type().fn_type(&param_types, false),
            Some("Bool")   => context.bool_type().fn_type(&param_types, false),
            Some("String") => ptr_type.fn_type(&param_types, false),
            Some(other) => {
                if let Some(ti) = type_registry.get(other) {
                    ti.struct_type.fn_type(&param_types, false)
                } else {
                    crate::errors::fatal(
                        crate::errors::Phase::Compiler,
                        format!("Unsupported return type '{other}' for method '{}'", func.name),
                    )
                }
            }
            None => context.void_type().fn_type(&param_types, false),
        };

        let llvm_name = format!("{type_name}__{}", func.name);
        let fn_val = module.add_function(&llvm_name, fn_type, None);

        all_methods
            .entry(type_name.clone())
            .or_default()
            .insert(func.name.clone(), fn_val);
        declared.push((type_name.clone(), func, fn_val));
    }

    // Register all forward-declared methods so bodies can call them.
    for (type_name, methods) in &all_methods {
        module_fns.insert(format!("__methods_{type_name}"), methods.clone());
    }

    // ── Pass 2: compile method bodies ────────────────────────
    for (type_name, func, fn_val) in &declared {
        let type_info = &type_registry[type_name.as_str()];

        // Enable LLVM's built-in shadow-stack GC strategy.
        // (set after collecting pointer allocas)
        let entry = context.append_basic_block(*fn_val, "entry");
        builder.position_at_end(entry);

        let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
            HashMap::new();
        let mut var_type_names: HashMap<String, String> = HashMap::new();

        let it_ptr = fn_val.get_nth_param(0).unwrap().into_pointer_value();
        variables.insert(
            "it".to_string(),
            (it_ptr, type_info.struct_type.as_basic_type_enum()),
        );
        var_type_names.insert("it".to_string(), type_name.clone());

        let mut ptr_allocas: Vec<PointerValue<'ctx>> = Vec::new();

        for (i, param) in func.params.iter().enumerate() {
            let llvm_ty = resolve_field_type(context, &param.type_annotation, &known_structs);
            let alloca = builder
                .build_alloca(llvm_ty, &param.name)
                .expect("alloca method param");
            let arg_val = fn_val.get_nth_param((i + 1) as u32).expect("method param index");
            builder.build_store(alloca, arg_val).expect("store method param");
            variables.insert(param.name.clone(), (alloca, llvm_ty));

            // Collect pointer-typed allocas for GC root registration.
            if llvm_ty.is_pointer_type() {
                ptr_allocas.push(alloca);
            }
        }

        // Always enable shadow-stack GC so local-variable gcroots are valid.
        fn_val.set_gc("shadow-stack");
        if !ptr_allocas.is_empty() {
            let gcroot = rt["llvm.gcroot"];
            let null_meta = context.ptr_type(inkwell::AddressSpace::default()).const_null();
            for alloca in &ptr_allocas {
                builder.build_call(gcroot, &[(*alloca).into(), null_meta.into()], "")
                    .expect("emit gcroot");
            }
        }

        let is_arrow = func.is_arrow && func.body.len() == 1;
        if is_arrow {
            let val = compile_expr(
                context, builder, &func.body[0], rt, module_fns,
                &mut variables, type_registry, &mut var_type_names,
            );
            if func.return_type.is_some() {
                if let Some(v) = val {
                    builder.build_return(Some(&v)).expect("build arrow return");
                } else {
                    builder.build_return(None).expect("build void return");
                }
            } else {
                builder.build_return(None).expect("build void return");
            }
        } else {
            for expr in &func.body {
                compile_expr(
                    context, builder, expr, rt, module_fns,
                    &mut variables, type_registry, &mut var_type_names,
                );
            }
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
    }

    all_methods
}
