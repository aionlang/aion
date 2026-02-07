//! Module import resolver.
//!
//! Resolves the three import flavours:
//!
//! - `import aion.math;`  — C-backed stdlib (skipped here; handled at codegen)
//! - `import std.io;`     — Aion stdlib, embedded in the compiler binary
//! - `import utils;`      — user-local `.aion` file (same dir or `lib/`)

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use crate::ast::{Program, UserModule};
use crate::errors::{self, Phase};
use crate::parser::Parser;
use crate::stdlib;

/// Walk every import in `program` and attach the resolved modules.
///
/// C-backed (`aion.*`) imports are left for LLVM codegen.
/// Aion-stdlib (`std.*`) and user-local modules are parsed and pushed
/// as `UserModule`s so they participate in compilation.
///
/// Multiple selective imports from the same module (e.g.
/// `import aion.sockets.TcpListener;` and `import aion.sockets.TcpStream;`)
/// load the module only once.
pub fn resolve_imports(program: &mut Program, source_dir: &Path) {
    let imports = program.imports.clone();
    let mut loaded: HashSet<String> = HashSet::new();

    for imp in &imports {
        let mod_name = imp.module_name();

        // C-backed modules — handled during LLVM codegen.
        // But if there is also a high-level Aion wrapper (hybrid module),
        // parse it and merge its types/functions into the program.
        if imp.is_stdlib() {
            if loaded.contains(mod_name) {
                continue;
            }
            loaded.insert(mod_name.to_string());

            if let Some(src) = stdlib::get(mod_name) {
                errors::info(format!("loading aion.{mod_name} hybrid wrapper (embedded)"));
                let mut mod_parser = Parser::new(src);
                let mod_program = mod_parser.parse_program();

                // Merge type definitions (TcpListener, TcpStream, etc.)
                program.type_defs.extend(mod_program.type_defs);

                // Merge impl methods (fn TcpListener::foo() defined outside type body)
                program.impl_methods.extend(mod_program.impl_methods);

                // Merge plain functions into program.functions so they are
                // forward-declared before constructors/methods that call them.
                program.functions.extend(mod_program.functions);
            }
            continue;
        }

        // ── embedded Aion stdlib ────────────────────────────────
        if imp.is_std() {
            if loaded.contains(mod_name) {
                continue;
            }
            loaded.insert(mod_name.to_string());

            let src = stdlib::get(mod_name).unwrap_or_else(|| {
                errors::fatal_with_hint(
                    Phase::Compiler,
                    format!("Unknown standard module 'std.{mod_name}'"),
                    Some(format!("Available: {}", stdlib::available().join(", "))),
                );
            });

            errors::info(format!("loading std.{mod_name} (embedded)"));

            let mut mod_parser = Parser::new(src);
            let mod_program = mod_parser.parse_program();

            program.user_modules.push(UserModule {
                name: mod_name.to_string(),
                functions: mod_program.functions,
            });
            continue;
        }

        // ── user-local module from disk ─────────────────────────
        if loaded.contains(mod_name) {
            continue;
        }
        loaded.insert(mod_name.to_string());
        let candidates = [
            source_dir.join(format!("{mod_name}.aion")),
            source_dir.join("lib").join(format!("{mod_name}.aion")),
        ];

        let mod_path = candidates.iter().find(|p| p.exists()).unwrap_or_else(|| {
            errors::fatal_with_hint(
                Phase::Compiler,
                format!("Could not find module '{mod_name}'"),
                Some(format!(
                    "Searched:\n  {}",
                    candidates
                        .iter()
                        .map(|p| p.display().to_string())
                        .collect::<Vec<_>>()
                        .join("\n  ")
                )),
            );
        });

        let mod_source = fs::read_to_string(mod_path).unwrap_or_else(|e| {
            errors::fatal(
                Phase::Compiler,
                format!("Could not read {}: {e}", mod_path.display()),
            );
        });

        errors::info(format!(
            "loading module '{mod_name}' from {}",
            mod_path.display()
        ));

        let mut mod_parser = Parser::new(&mod_source);
        let mod_program = mod_parser.parse_program();

        program.user_modules.push(UserModule {
            name: mod_name.to_string(),
            functions: mod_program.functions,
        });
    }
}
