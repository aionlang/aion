use std::collections::HashMap;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::Module;

use super::Runtime;

/// Describes the parameter types of a runtime function (no generics yet).
enum ParamKind { Ptr, I64, F64, None, PtrPtr }

/// Describes the return type of a runtime function.
enum RetKind { Void, Ptr, I64 }

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
        ("aion_alloc",           ParamKind::I64, RetKind::Ptr),
        ("aion_free",            ParamKind::Ptr, RetKind::Void),
        ("aion_gc_collect",      ParamKind::None, RetKind::Void),
        ("aion_gc_safepoint",    ParamKind::None, RetKind::Void),
        ("aion_gc_heap_size",    ParamKind::None, RetKind::I64),
        ("aion_gc_object_count", ParamKind::None, RetKind::I64),
        ("aion_concat",          ParamKind::PtrPtr, RetKind::Ptr),
        ("aion_int_to_str",      ParamKind::I64, RetKind::Ptr),
        ("aion_float_to_str",    ParamKind::F64, RetKind::Ptr),
    ];

    let void   = context.void_type();
    let ptr    = context.ptr_type(inkwell::AddressSpace::default());
    let i64_ty = context.i64_type();
    let f64_ty = context.f64_type();

    let mut rt = HashMap::new();

    for &(name, ref param, ref ret) in TABLE {
        let params: Vec<inkwell::types::BasicMetadataTypeEnum> = match param {
            ParamKind::Ptr    => vec![ptr.into()],
            ParamKind::I64    => vec![i64_ty.into()],
            ParamKind::F64    => vec![f64_ty.into()],
            ParamKind::None   => vec![],
            ParamKind::PtrPtr => vec![ptr.into(), ptr.into()],
        };

        let fn_type = match ret {
            RetKind::Void => void.fn_type(&params, false),
            RetKind::Ptr  => ptr.fn_type(&params, false),
            RetKind::I64  => i64_ty.fn_type(&params, false),
        };

        let fn_val = module.add_function(name, fn_type, Option::None);
        rt.insert(name, fn_val);
    }

    // ── @llvm.gcroot intrinsic for built-in shadow-stack GC ──
    let gcroot = Intrinsic::find("llvm.gcroot")
        .expect("@llvm.gcroot intrinsic not found");
    let gcroot_fn = gcroot
        .get_declaration(module, &[])
        .expect("failed to declare @llvm.gcroot");
    rt.insert("llvm.gcroot", gcroot_fn);

    rt
}
