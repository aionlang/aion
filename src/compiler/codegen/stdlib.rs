use std::collections::HashMap;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::FunctionValue;

use crate::compiler::stdlib_registry::{self, CType};
use crate::ast::Import;
use crate::errors::{self, Phase};

/// Declare C-backed stdlib functions for each `import aion.*` module.
pub fn declare_stdlib<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    imports: &[Import],
) -> HashMap<String, HashMap<String, FunctionValue<'ctx>>> {
    let mut map = HashMap::new();
    let i64_ty = context.i64_type();
    let f64_ty = context.f64_type();
    let ptr_ty = context.ptr_type(inkwell::AddressSpace::default());
    let void_ty = context.void_type();

    for imp in imports {
        if !imp.is_stdlib() {
            continue;
        }

        let mod_name = imp.module_name().to_string();
        let registry = stdlib_registry::registry(&mod_name).unwrap_or_else(|| {
            errors::fatal_with_hint(
                Phase::Compiler,
                format!("Unknown standard-library module: '{}'", imp.path.join(".")),
                Some("Available modules: aion.math, aion.sockets".into()),
            );
        });

        let mut fns = HashMap::new();
        for (name, info) in &registry {
            let param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = info
                .params
                .iter()
                .map(|ct| match ct {
                    CType::Int   => i64_ty.into(),
                    CType::Float => f64_ty.into(),
                    CType::Str   => ptr_ty.into(),
                    CType::Void  => unreachable!("Void is not a valid parameter type"),
                })
                .collect();

            let fn_type = match info.ret {
                CType::Int   => i64_ty.fn_type(&param_types, false),
                CType::Float => f64_ty.fn_type(&param_types, false),
                CType::Str   => ptr_ty.fn_type(&param_types, false),
                CType::Void  => void_ty.fn_type(&param_types, false),
            };

            let fn_val = module.add_function(info.c_name, fn_type, None);
            fns.insert(name.to_string(), fn_val);
        }

        map.insert(mod_name, fns);
    }

    map
}
