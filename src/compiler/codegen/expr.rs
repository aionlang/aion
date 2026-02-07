use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue, ValueKind, FunctionValue};
use inkwell::{IntPredicate, FloatPredicate};

use crate::ast::{Expr, BinOperator, AssignBinOperator};
use crate::errors::{self, Phase};

use super::{Runtime, ModuleFns};

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
                .zip(llvm_fn.get_type().get_param_types().iter())
                .map(|(a, expected_ty)| {
                    let val = compile_expr(context, builder, a, rt, module_fns, variables)
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
                (Some("Bool"), _) => {
                    let ty = context.bool_type().as_basic_type_enum();
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

        // variable assignment ─────────────────────────────────────────────
        Expr::VarAssign { name, value, op } => {
            let (ptr, ty) = variables.get(name)
                .unwrap_or_else(|| errors::fatal(
                    Phase::Compiler,
                    format!("Undefined variable: '{name}'"),
                ));
            let ptr = *ptr;
            let ty = *ty;

            let new_val = compile_expr(context, builder, value, rt, module_fns, variables)
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
            let cond_val = compile_expr(context, builder, condition, rt, module_fns, variables)
                .expect("while condition must produce a value");
            builder.build_conditional_branch(
                cond_val.into_int_value(),
                body_bb,
                end_bb,
            ).expect("build conditional branch");

            // ── body block ──
            builder.position_at_end(body_bb);
            for expr in body {
                compile_expr(context, builder, expr, rt, module_fns, variables);
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
            let cond_val = compile_expr(context, builder, condition, rt, module_fns, variables)
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
                compile_expr(context, builder, expr, rt, module_fns, variables);
            }
            builder.build_unconditional_branch(merge_bb).expect("build branch");

            // Compile the else branch (or just jump to merge if no else).
            builder.position_at_end(else_bb);
            if let Some(else_exprs) = else_branch {
                for expr in else_exprs {
                    compile_expr(context, builder, expr, rt, module_fns, variables);
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

        // ── binary operation ─────────────────────────────────────
        Expr::BinaryOp { op, left, right } => {
            let lhs = compile_expr(context, builder, left, rt, module_fns, variables)
                .expect("left operand must produce a value");
            let rhs = compile_expr(context, builder, right, rt, module_fns, variables)
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
    }
}
