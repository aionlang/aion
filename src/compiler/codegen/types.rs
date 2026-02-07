//! Compile user-defined type definitions into LLVM struct types.

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::{FunctionValue, PointerValue};

use crate::ast::{Function, TypeDef};
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
                crate::errors::fatal(
                    crate::errors::Phase::Compiler,
                    format!("Unsupported field type: '{other}'"),
                )
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

    // ── Pass 2: resolve fields and set struct bodies ──────────
    for td in type_defs {
        let struct_type = struct_types[&td.name];

        let fields: Vec<(String, inkwell::types::BasicTypeEnum<'ctx>)> = td
            .fields
            .iter()
            .map(|f| {
                let llvm_ty = resolve_field_type(context, &f.type_name, &struct_types);
                (f.name.clone(), llvm_ty)
            })
            .collect();

        let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> =
            fields.iter().map(|(_, ty)| *ty).collect();
        struct_type.set_body(&field_types, false);

        let constructor_params = if td.constructor_params.is_empty() {
            fields.iter().map(|(name, _)| name.clone()).collect()
        } else {
            td.constructor_params.clone()
        };

        registry.insert(td.name.clone(), TypeInfo {
            struct_type,
            fields,
            constructor_params,
            has_explicit_constructor: td.constructor.is_some(),
        });
    }

    registry
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
        for (i, param) in ctor.params.iter().enumerate() {
            let llvm_ty = resolve_field_type(context, &param.type_annotation, &known_structs);
            let alloca = builder
                .build_alloca(llvm_ty, &param.name)
                .expect("alloca ctor param");
            let arg_val = fn_val.get_nth_param(i as u32).expect("ctor param index");
            builder.build_store(alloca, arg_val).expect("store ctor param");
            variables.insert(param.name.clone(), (alloca, llvm_ty));
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

        for (i, param) in func.params.iter().enumerate() {
            let llvm_ty = resolve_field_type(context, &param.type_annotation, &known_structs);
            let alloca = builder
                .build_alloca(llvm_ty, &param.name)
                .expect("alloca method param");
            let arg_val = fn_val.get_nth_param((i + 1) as u32).expect("method param index");
            builder.build_store(alloca, arg_val).expect("store method param");
            variables.insert(param.name.clone(), (alloca, llvm_ty));
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
                builder.build_return(None).expect("build void return");
            }
        }
    }

    all_methods
}
