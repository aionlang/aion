use std::collections::HashMap;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::FunctionValue;

use crate::compiler::stdlib_registry;
use crate::ast::Import;
use crate::errors::{self, Phase};

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
