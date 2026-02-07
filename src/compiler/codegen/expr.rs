use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue, ValueKind, FunctionValue};
use inkwell::{IntPredicate, FloatPredicate};

use crate::ast::{Expr, BinOperator, AssignBinOperator, StringInterpolationPart};
use crate::errors::{self, Phase};

use super::{Runtime, ModuleFns, TypeRegistry};

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
    // Widen i1 (bool) to i64 so it matches aion_print_int's signature.
    let arg: BasicValueEnum = if val.is_int_value()
        && val.into_int_value().get_type().get_bit_width() < 64
    {
        builder
            .build_int_z_extend(val.into_int_value(), builder.get_insert_block().unwrap().get_context().i64_type(), "widen")
            .expect("widen bool to i64")
            .into()
    } else {
        val
    };
    let a: [BasicMetadataValueEnum; 1] = [arg.into()];
    builder.build_call(f, &a, "call").expect("build call");
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
    type_registry: &TypeRegistry<'ctx>,
    var_type_names: &mut HashMap<String, String>,
) -> Option<BasicValueEnum<'ctx>> {
    match expr {
        // ── string literal ───────────────────────────────────
        Expr::StringLiteral(text) => {
            let global = builder
                .build_global_string_ptr(text, "str")
                .expect("build global string");
            Some(global.as_pointer_value().into())
        }

        Expr::InterpolatedString { parts } => {
            let mut result: Option<BasicValueEnum<'ctx>> = None;

            for part in parts {
                let part_value = match part {
                    StringInterpolationPart::Literal(text) => {
                        let global = builder
                            .build_global_string_ptr(text, "str_part")
                            .expect("build string literal");
                        global.as_pointer_value().into()
                    }
                    StringInterpolationPart::Expr(expr) => {
                        let val = compile_expr(
                            context, builder, expr, rt, module_fns,
                            variables, type_registry, var_type_names,
                        ).expect("interpolated expression must produce a value");
                        string_from_value(context, builder, rt, val)
                    }
                };

                result = Some(if let Some(prev) = result {
                    string_concat(context, builder, rt, prev, part_value)
                } else {
                    part_value
                });
            }
            result
        }

        // ── module.func(args…) ──────────────────────────────────
        Expr::ModuleCall { module, func, args } => {
            // ── method call on a struct instance: a.speak() ─────
            if let Some(type_name) = var_type_names.get(module.as_str()) {
                let method_key = format!("__methods_{type_name}");
                if let Some(methods) = module_fns.get(&method_key) {
                    if let Some(method_fn) = methods.get(func.as_str()) {
                        // Pass the struct pointer as hidden first arg (`it`).
                        let (ptr, _ty) = variables.get(module.as_str()).unwrap();
                        let mut llvm_args: Vec<BasicMetadataValueEnum> = vec![(*ptr).into()];
                        for a in args {
                            let val = compile_expr(context, builder, a, rt, module_fns, variables, type_registry, var_type_names)
                                .expect("method argument must produce a value");
                            llvm_args.push(val.into());
                        }
                        let call = builder
                            .build_call(*method_fn, &llvm_args, "methodcall")
                            .expect("build method call");
                        return match call.try_as_basic_value() {
                            ValueKind::Basic(val) => Some(val),
                            ValueKind::Instruction(_) => None,
                        };
                    }
                }
            }

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
                .zip(llvm_fn.get_type().get_param_types().iter())
                .map(|(a, expected_ty)| {
                    let val = compile_expr(context, builder, a, rt, module_fns, variables, type_registry, var_type_names)
                        .expect("argument must produce a value");
                    // Implicit int → float coercion for stdlib math calls.
                    if val.is_int_value() && expected_ty.is_float_type() {
                        let coerced = builder
                            .build_signed_int_to_float(
                                val.into_int_value(),
                                context.f64_type(),
                                "i2f",
                            )
                            .expect("int to float coercion");
                        coerced.into()
                    } else {
                        val.into()
                    }
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
                        compile_expr(context, builder, v, rt, module_fns, variables, type_registry, var_type_names)
                            .expect("initializer must produce a value")
                    });
                    (ty, val)
                }
                (Some("Float"), _) => {
                    let ty = context.f64_type().as_basic_type_enum();
                    let val = value.as_ref().map(|v| {
                        compile_expr(context, builder, v, rt, module_fns, variables, type_registry, var_type_names)
                            .expect("initializer must produce a value")
                    });
                    (ty, val)
                }
                (Some("Bool"), _) => {
                    let ty = context.bool_type().as_basic_type_enum();
                    let val = value.as_ref().map(|v| {
                        compile_expr(context, builder, v, rt, module_fns, variables, type_registry, var_type_names)
                            .expect("initializer must produce a value")
                    });
                    (ty, val)
                }
                (None, Some(init_expr)) => {
                    let val = compile_expr(context, builder, init_expr, rt, module_fns, variables, type_registry, var_type_names)
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
                (Some(other), _) => {
                    // Check if it's a user-defined type.
                    if let Some(ti) = type_registry.get(other) {
                        let ty = ti.struct_type.as_basic_type_enum();
                        let val = value.as_ref().map(|v| {
                            compile_expr(context, builder, v, rt, module_fns, variables, type_registry, var_type_names)
                                .expect("initializer must produce a value")
                        });
                        (ty, val)
                    } else {
                        errors::fatal_with_hint(
                            Phase::Compiler,
                            format!("Unsupported type: '{other}'"),
                            Some("Supported types: Int, Float, Bool, String, or user-defined types".into()),
                        )
                    }
                },
            };

            let alloca = builder
                .build_alloca(llvm_type, name)
                .expect("build alloca");

            if let Some(val) = init_val {
                builder.build_store(alloca, val).expect("build store");
            }

            // Track the Aion type name for struct variables (for field access).
            if let Some(init_expr) = value {
                if let Expr::FuncCall { name: ctor_name, .. } = init_expr.as_ref() {
                    if type_registry.contains_key(ctor_name.as_str()) {
                        var_type_names.insert(name.clone(), ctor_name.clone());
                    }
                }
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

        // variable assignment ─────────────────────────────────────────────
        Expr::VarAssign { name, value, op } => {
            let (ptr, ty) = variables.get(name)
                .unwrap_or_else(|| errors::fatal(
                    Phase::Compiler,
                    format!("Undefined variable: '{name}'"),
                ));
            let ptr = *ptr;
            let ty = *ty;

            let new_val = compile_expr(context, builder, value, rt, module_fns, variables, type_registry, var_type_names)
                .expect("assignment value must produce a value");

            let store_val = match op {
                None => new_val,
                Some(assign_op) => {
                    let current = builder.build_load(ty, ptr, "cur")
                        .expect("load current value");
                    if current.is_int_value() {
                        let lhs = current.into_int_value();
                        let rhs = new_val.into_int_value();
                        let result = match assign_op {
                            AssignBinOperator::AddAssign => builder.build_int_add(lhs, rhs, "addtmp"),
                            AssignBinOperator::SubAssign => builder.build_int_sub(lhs, rhs, "subtmp"),
                            AssignBinOperator::MulAssign => builder.build_int_mul(lhs, rhs, "multmp"),
                            AssignBinOperator::DivAssign => builder.build_int_signed_div(lhs, rhs, "divtmp"),
                            AssignBinOperator::DotAssign => errors::fatal(
                                Phase::Compiler,
                                ".= is not supported on integer types".to_string(),
                            ),
                        };
                        result.expect("build int op").into()
                    } else {
                        let lhs = current.into_float_value();
                        let rhs = new_val.into_float_value();
                        let result = match assign_op {
                            AssignBinOperator::AddAssign => builder.build_float_add(lhs, rhs, "faddtmp"),
                            AssignBinOperator::SubAssign => builder.build_float_sub(lhs, rhs, "fsubtmp"),
                            AssignBinOperator::MulAssign => builder.build_float_mul(lhs, rhs, "fmultmp"),
                            AssignBinOperator::DivAssign => builder.build_float_div(lhs, rhs, "fdivtmp"),
                            AssignBinOperator::DotAssign => errors::fatal(
                                Phase::Compiler,
                                ".= is not supported on float types".to_string(),
                            ),
                        };
                        result.expect("build float op").into()
                    }
                }
            };

            builder.build_store(ptr, store_val).expect("build store");
            None
        }

        // while expression ─────────────────────────────────────
        Expr::WhileExpr { condition, body } => {
            let parent_fn = builder.get_insert_block().unwrap().get_parent().unwrap();

            let cond_bb = context.append_basic_block(parent_fn, "while_cond");
            let body_bb = context.append_basic_block(parent_fn, "while_body");
            let end_bb  = context.append_basic_block(parent_fn, "while_end");

            // Jump into the condition check.
            builder.build_unconditional_branch(cond_bb).expect("branch to cond");

            // ── condition block ──
            builder.position_at_end(cond_bb);
            let cond_val = compile_expr(context, builder, condition, rt, module_fns, variables, type_registry, var_type_names)
                .expect("while condition must produce a value");
            builder.build_conditional_branch(
                cond_val.into_int_value(),
                body_bb,
                end_bb,
            ).expect("build conditional branch");

            // ── body block ──
            builder.position_at_end(body_bb);
            for expr in body {
                compile_expr(context, builder, expr, rt, module_fns, variables, type_registry, var_type_names);
            }
            // Loop back to condition.
            builder.build_unconditional_branch(cond_bb).expect("branch back to cond");

            // ── continue after the loop ──
            builder.position_at_end(end_bb);
            None
        }

        // conditional expression ─────────────────────────────────────
        Expr::IfExpr { condition, then_branch, else_branch } => {
            // Compile the condition and create basic blocks for the branches.
            let cond_val = compile_expr(context, builder, condition, rt, module_fns, variables, type_registry, var_type_names)
                .expect("condition must produce a value");
            let parent_fn = builder.get_insert_block().unwrap().get_parent().unwrap();
            let then_bb = context.append_basic_block(parent_fn, "then");
            let else_bb = context.append_basic_block(parent_fn, "else");
            let merge_bb = context.append_basic_block(parent_fn, "ifcont");

            // Build the conditional branch.
            builder.build_conditional_branch(
                cond_val.into_int_value(),
                then_bb,
                else_bb,
            ).expect("build conditional branch");

            // Compile the then branch.
            builder.position_at_end(then_bb);
            for expr in then_branch {
                compile_expr(context, builder, expr, rt, module_fns, variables, type_registry, var_type_names);
            }
            builder.build_unconditional_branch(merge_bb).expect("build branch");

            // Compile the else branch (or just jump to merge if no else).
            builder.position_at_end(else_bb);
            if let Some(else_exprs) = else_branch {
                for expr in else_exprs {
                    compile_expr(context, builder, expr, rt, module_fns, variables, type_registry, var_type_names);
                }
            }
            builder.build_unconditional_branch(merge_bb).expect("build branch");

            // Continue at the merge block.
            builder.position_at_end(merge_bb);
            None
        }

        // ── unqualified function call ───────────────────────────
        Expr::FuncCall { name, args } => {
            // ── built-in print() / println(): type-based dispatch ─
            if (name == "print" || name == "println") && args.len() == 1 {
                let is_ln = name == "println";
                let val = compile_expr(context, builder, &args[0], rt, module_fns, variables, type_registry, var_type_names);
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

            // ── constructor call: TypeName(args…) ────────────────
            // Only for types WITHOUT an explicit constructor block.
            // Types WITH explicit constructors are compiled as LLVM functions
            // and found via the general function lookup above.
            if let Some(type_info) = type_registry.get(name.as_str()) {
                if !type_info.has_explicit_constructor {
                    // Allocate the struct on the stack.
                    let alloca = builder
                        .build_alloca(type_info.struct_type, "ctor_tmp")
                        .expect("build struct alloca");

                    // Map constructor args → fields using constructor_params order.
                    for (i, param_name) in type_info.constructor_params.iter().enumerate() {
                        if i >= args.len() {
                            errors::fatal(
                                Phase::Compiler,
                                format!(
                                    "Constructor for '{}' expects {} arguments, got {}",
                                    name, type_info.constructor_params.len(), args.len()
                                ),
                            );
                        }
                        // Find the field index for this constructor param.
                        let field_idx = type_info.fields.iter()
                            .position(|(fname, _)| fname == param_name)
                            .unwrap_or_else(|| errors::fatal(
                                Phase::Compiler,
                                format!("Constructor param '{param_name}' has no matching field in type '{name}'"),
                            ));

                        let val = compile_expr(context, builder, &args[i], rt, module_fns, variables, type_registry, var_type_names)
                            .expect("constructor argument must produce a value");
                        let field_ptr = builder
                            .build_struct_gep(type_info.struct_type, alloca, field_idx as u32, &format!("field_{param_name}"))
                            .expect("build struct gep");
                        builder.build_store(field_ptr, val).expect("build field store");
                    }

                    // Load the complete struct value.
                    let struct_val = builder
                        .build_load(type_info.struct_type, alloca, "struct_val")
                        .expect("load struct");
                    return Some(struct_val);
                }
            }


    
            // ── general function lookup ──────────────────────────
            // Search all modules for a matching function name.
            // This also finds explicit constructor functions (registered under "__ctors").
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
                    compile_expr(context, builder, a, rt, module_fns, variables, type_registry, var_type_names)
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

        // ── binary operation ─────────────────────────────────────
        Expr::BinaryOp { op, left, right } => {
            let lhs = compile_expr(context, builder, left, rt, module_fns, variables, type_registry, var_type_names)
                .expect("left operand must produce a value");
            let rhs = compile_expr(context, builder, right, rt, module_fns, variables, type_registry, var_type_names)
                .expect("right operand must produce a value");

            match op {
                BinOperator::Add => {
                    if lhs.is_int_value() {
                        Some(builder.build_int_add(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "add",
                        ).expect("build int add").into())
                    } else {
                        Some(builder.build_float_add(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fadd",
                        ).expect("build float add").into())
                    }
                }
                BinOperator::Sub => {
                    if lhs.is_int_value() {
                        Some(builder.build_int_sub(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "sub",
                        ).expect("build int sub").into())
                    } else {
                        Some(builder.build_float_sub(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fsub",
                        ).expect("build float sub").into())
                    }
                }
                BinOperator::Eq | BinOperator::Neq |
                BinOperator::Lt | BinOperator::Gt  |
                BinOperator::Lte | BinOperator::Gte => {
                    if lhs.is_int_value() {
                        let pred = match op {
                            BinOperator::Eq  => IntPredicate::EQ,
                            BinOperator::Neq => IntPredicate::NE,
                            BinOperator::Lt  => IntPredicate::SLT,
                            BinOperator::Gt  => IntPredicate::SGT,
                            BinOperator::Lte => IntPredicate::SLE,
                            BinOperator::Gte => IntPredicate::SGE,
                            _ => unreachable!(),
                        };
                        Some(builder.build_int_compare(
                            pred,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "cmp",
                        ).expect("build int compare").into())
                    } else {
                        let pred = match op {
                            BinOperator::Eq  => FloatPredicate::OEQ,
                            BinOperator::Neq => FloatPredicate::ONE,
                            BinOperator::Lt  => FloatPredicate::OLT,
                            BinOperator::Gt  => FloatPredicate::OGT,
                            BinOperator::Lte => FloatPredicate::OLE,
                            BinOperator::Gte => FloatPredicate::OGE,
                            _ => unreachable!(),
                        };
                        Some(builder.build_float_compare(
                            pred,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fcmp",
                        ).expect("build float compare").into())
                    }
                }
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

        // ── boolean literal ─────────────────────────────────────
        Expr::BooleanLiteral(b) => {
            let int_val = if *b { 1 } else { 0 };
            Some(context.bool_type().const_int(int_val, false).into())
        }

        // ── field access: a.name ────────────────────────────────
        Expr::FieldAccess { object, field } => {
            if let Expr::VarRef(var_name) = object.as_ref() {
                let type_name = var_type_names.get(var_name).unwrap_or_else(|| {
                    errors::fatal(
                        Phase::Compiler,
                        format!("Variable '{var_name}' is not a struct type (cannot access '.{field}')"),
                    )
                });
                let type_info = type_registry.get(type_name).unwrap_or_else(|| {
                    errors::fatal(
                        Phase::Compiler,
                        format!("Unknown type '{type_name}' for variable '{var_name}'"),
                    )
                });
                let (ptr, _ty) = variables.get(var_name).unwrap_or_else(|| {
                    errors::fatal(
                        Phase::Compiler,
                        format!("Undefined variable: '{var_name}'"),
                    )
                });

                let field_idx = type_info.fields.iter()
                    .position(|(fname, _)| fname == field)
                    .unwrap_or_else(|| errors::fatal(
                        Phase::Compiler,
                        format!("Type '{type_name}' has no field '{field}'"),
                    ));
                let (_fname, field_ty) = &type_info.fields[field_idx];

                let field_ptr = builder
                    .build_struct_gep(type_info.struct_type, *ptr, field_idx as u32, &format!("gep_{field}"))
                    .expect("build struct gep for field access");
                let val = builder
                    .build_load(*field_ty, field_ptr, &format!("load_{field}"))
                    .expect("load field value");
                Some(val)
            } else {
                errors::fatal(
                    Phase::Compiler,
                    "Field access on non-variable expressions is not yet supported".to_string(),
                )
            }
        }

        // ── field assignment: it.name = value ───────────────────
        Expr::FieldAssign { object, field, value } => {
            if let Expr::VarRef(var_name) = object.as_ref() {
                let type_name = var_type_names.get(var_name).unwrap_or_else(|| {
                    errors::fatal(
                        Phase::Compiler,
                        format!("Variable '{var_name}' is not a struct type (cannot assign '.{field}')"),
                    )
                }).clone();
                let type_info = type_registry.get(&type_name).unwrap_or_else(|| {
                    errors::fatal(
                        Phase::Compiler,
                        format!("Unknown type '{type_name}' for variable '{var_name}'"),
                    )
                });
                let (ptr, _ty) = variables.get(var_name).unwrap_or_else(|| {
                    errors::fatal(
                        Phase::Compiler,
                        format!("Undefined variable: '{var_name}'"),
                    )
                });
                let ptr = *ptr;
                let struct_type = type_info.struct_type;

                let field_idx = type_info.fields.iter()
                    .position(|(fname, _)| fname == field)
                    .unwrap_or_else(|| errors::fatal(
                        Phase::Compiler,
                        format!("Type '{type_name}' has no field '{field}'"),
                    ));

                let val = compile_expr(context, builder, value, rt, module_fns, variables, type_registry, var_type_names)
                    .expect("field assignment value must produce a value");
                let field_ptr = builder
                    .build_struct_gep(struct_type, ptr, field_idx as u32, &format!("set_{field}"))
                    .expect("build struct gep for field assignment");
                builder.build_store(field_ptr, val).expect("store field value");
                None
            } else {
                errors::fatal(
                    Phase::Compiler,
                    "Field assignment on non-variable expressions is not yet supported".to_string(),
                )
            }
        }

        // ── return statement ────────────────────────────────────
        Expr::ReturnExpr { value } => {
            if let Some(val_expr) = value {
                let val = compile_expr(context, builder, val_expr, rt, module_fns, variables, type_registry, var_type_names)
                    .expect("return value must produce a value");
                builder.build_return(Some(&val)).expect("build return");
            } else {
                builder.build_return(None).expect("build void return");
            }
            // After a return, create a new basic block for any dead code.
            let parent_fn = builder.get_insert_block().unwrap().get_parent().unwrap();
            let dead_bb = context.append_basic_block(parent_fn, "after_return");
            builder.position_at_end(dead_bb);
            None
        }
    }
}

fn string_from_value<'ctx>(
    _context: &'ctx Context,
    builder: &Builder<'ctx>,
    rt: &Runtime<'ctx>,
    val: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    if val.is_pointer_value() {
        // Already a string
        val
    } else if val.is_int_value() {
        let f = rt.get("aion_int_to_str").expect("aion_int_to_str not found");
        let call = builder
            .build_call(*f, &[val.into()], "int_to_str")
            .expect("build int_to_str call");
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => v,
            _ => panic!("aion_int_to_str returned void or instruction"),
        }
    } else if val.is_float_value() {
        let f = rt.get("aion_float_to_str").expect("aion_float_to_str not found");
        let call = builder
            .build_call(*f, &[val.into()], "float_to_str")
            .expect("build float_to_str call");
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => v,
            _ => panic!("aion_float_to_str returned void or instruction"),
        }
    } else {
        errors::fatal(
            Phase::Compiler,
            format!("Cannot convert {:?} to string in interpolation", val.get_type()),
        )
    }
}

fn string_concat<'ctx>(
    _context: &'ctx Context,
    builder: &Builder<'ctx>,
    rt: &Runtime<'ctx>,
    left: BasicValueEnum<'ctx>,
    right: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let f = rt.get("aion_concat").expect("aion_concat not found");
    let call = builder
        .build_call(*f, &[left.into(), right.into()], "concat")
        .expect("build concat call");
    match call.try_as_basic_value() {
        ValueKind::Basic(v) => v,
        _ => panic!("aion_concat returned void or instruction"),
    }
}
