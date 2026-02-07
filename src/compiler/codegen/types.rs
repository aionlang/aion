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
fn resolve_field_type<'ctx>(
    context: &'ctx Context,
    type_name: &str,
) -> inkwell::types::BasicTypeEnum<'ctx> {
    match type_name {
        "Int"    => context.i64_type().as_basic_type_enum(),
        "Float"  => context.f64_type().as_basic_type_enum(),
        "Bool"   => context.bool_type().as_basic_type_enum(),
        "String" => context.ptr_type(inkwell::AddressSpace::default()).as_basic_type_enum(),
        other => crate::errors::fatal(
            crate::errors::Phase::Compiler,
            format!("Unsupported field type: '{other}'"),
        ),
    }
}

/// Create LLVM struct types for all user-defined type definitions.
///
/// Returns a [`TypeRegistry`] that maps each type name to its
/// LLVM struct layout and field metadata.
pub fn compile_type_defs<'ctx>(
    context: &'ctx Context,
    type_defs: &[TypeDef],
) -> TypeRegistry<'ctx> {
    let mut registry = TypeRegistry::new();

    for td in type_defs {
        // Build the ordered list of (field_name, llvm_type).
        let fields: Vec<(String, inkwell::types::BasicTypeEnum<'ctx>)> = td
            .fields
            .iter()
            .map(|f| {
                let llvm_ty = resolve_field_type(context, &f.type_name);
                (f.name.clone(), llvm_ty)
            })
            .collect();

        // Create the LLVM struct type.
        let field_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> =
            fields.iter().map(|(_, ty)| *ty).collect();
        let struct_type = context.opaque_struct_type(&td.name);
        struct_type.set_body(&field_types, false);

        let constructor_params = if td.constructor_params.is_empty() {
            // Default: all fields in declaration order.
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
    module_fns: &ModuleFns<'ctx>,
) -> HashMap<String, FunctionValue<'ctx>> {
    let mut ctor_fns = HashMap::new();

    for td in type_defs {
        let ctor = match &td.constructor {
            Some(c) => c,
            None => continue,
        };
        let type_info = &type_registry[&td.name];

        // Build parameter types from the constructor's typed params.
        let param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = ctor
            .params
            .iter()
            .map(|p| {
                resolve_field_type(context, &p.type_annotation).into()
            })
            .collect();

        // fn TypeName(params…) -> StructType
        let fn_type = type_info.struct_type.fn_type(&param_types, false);
        let fn_val = module.add_function(&td.name, fn_type, None);

        let entry = context.append_basic_block(fn_val, "entry");
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
            let llvm_ty = resolve_field_type(context, &param.type_annotation);
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

        ctor_fns.insert(td.name.clone(), fn_val);
    }

    ctor_fns
}

/// Compile methods for user-defined types into LLVM functions.
///
/// Handles both:
/// - Inline methods defined inside `type Animal { fn bark() { … } }`
/// - Impl methods defined outside: `fn Animal::speak() { … }`
///
/// Each method is compiled as `TypeName__method_name` in LLVM IR and
/// receives a hidden first parameter `it: ptr` (pointer to the struct
/// instance).
///
/// Returns a nested map: `type_name → { method_name → FunctionValue }`.
pub fn compile_methods<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    type_defs: &[TypeDef],
    impl_methods: &[(String, Function)],
    type_registry: &TypeRegistry<'ctx>,
    rt: &Runtime<'ctx>,
    module_fns: &ModuleFns<'ctx>,
) -> HashMap<String, HashMap<String, FunctionValue<'ctx>>> {
    let mut all_methods: HashMap<String, HashMap<String, FunctionValue<'ctx>>> = HashMap::new();

    // Collect methods: (type_name, &Function) from both sources.
    let mut pairs: Vec<(String, &Function)> = Vec::new();

    // 1. Inline methods from type bodies.
    for td in type_defs {
        for method in &td.methods {
            pairs.push((td.name.clone(), method));
        }
    }

    // 2. Impl methods from top-level `fn Type::method()`.
    for (type_name, func) in impl_methods {
        pairs.push((type_name.clone(), func));
    }

    for (type_name, func) in &pairs {
        let type_info = match type_registry.get(type_name.as_str()) {
            Some(ti) => ti,
            None => crate::errors::fatal(
                crate::errors::Phase::Compiler,
                format!("Cannot define method on unknown type '{type_name}'"),
            ),
        };

        // Build param types: hidden `it: ptr` + declared params.
        let ptr_type = context.ptr_type(inkwell::AddressSpace::default());
        let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            vec![ptr_type.into()];
        for p in &func.params {
            param_types.push(resolve_field_type(context, &p.type_annotation).into());
        }

        // Determine return type.
        let fn_type = match func.return_type.as_deref() {
            Some("Int")    => context.i64_type().fn_type(&param_types, false),
            Some("Float")  => context.f64_type().fn_type(&param_types, false),
            Some("Bool")   => context.bool_type().fn_type(&param_types, false),
            Some("String") => ptr_type.fn_type(&param_types, false),
            Some(other) => {
                // Check for user-defined struct return type.
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

        let entry = context.append_basic_block(fn_val, "entry");
        builder.position_at_end(entry);

        let mut variables: HashMap<String, (PointerValue<'ctx>, inkwell::types::BasicTypeEnum<'ctx>)> =
            HashMap::new();
        let mut var_type_names: HashMap<String, String> = HashMap::new();

        // Bind `it` — the hidden first parameter (pointer to struct).
        let it_ptr = fn_val.get_nth_param(0).unwrap().into_pointer_value();
        variables.insert(
            "it".to_string(),
            (it_ptr, type_info.struct_type.as_basic_type_enum()),
        );
        var_type_names.insert("it".to_string(), type_name.clone());

        // Bind declared parameters.
        for (i, param) in func.params.iter().enumerate() {
            let llvm_ty = resolve_field_type(context, &param.type_annotation);
            let alloca = builder
                .build_alloca(llvm_ty, &param.name)
                .expect("alloca method param");
            let arg_val = fn_val.get_nth_param((i + 1) as u32).expect("method param index");
            builder.build_store(alloca, arg_val).expect("store method param");
            variables.insert(param.name.clone(), (alloca, llvm_ty));
        }

        // Compile body.
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
            // If no explicit return was emitted, add one.
            let needs_terminator = builder.get_insert_block()
                .map(|bb| bb.get_terminator().is_none())
                .unwrap_or(false);
            if needs_terminator {
                builder.build_return(None).expect("build void return");
            }
        }

        all_methods
            .entry(type_name.clone())
            .or_default()
            .insert(func.name.clone(), fn_val);
    }

    all_methods
}
