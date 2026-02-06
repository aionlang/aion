//! LLVM-backed compiler for the Aion language.
//!
//! This is the top-level coordinator. The heavy lifting is split across:
//!
//! - [`codegen`]           — AST → LLVM IR lowering
//! - [`stdlib_registry`]   — C-backed stdlib function descriptors
//! - [`runtime`]           — embedded C source compilation & caching
//! - [`linker`]            — native binary linking

use std::path::Path;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;

use crate::ast::Program;

use super::codegen;

// ═══════════════════════════════════════════════════════════════════
// Compiler
// ═══════════════════════════════════════════════════════════════════

/// Holds LLVM state for a single compilation unit.
pub struct Compiler<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new compiler targeting the given LLVM module name.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self { context, module, builder }
    }

    // ── codegen entry point ─────────────────────────────────────

    /// Lower a full [`Program`] to LLVM IR.
    pub fn compile(&self, program: &Program) {
        let rt = codegen::declare_runtime(self.context, &self.module);

        let mut module_fns = codegen::declare_stdlib(
            self.context,
            &self.module,
            &program.imports,
        );

        for user_mod in &program.user_modules {
            let fns = codegen::compile_user_module(
                self.context, &self.module, &self.builder,
                user_mod, &rt, &module_fns,
            );
            module_fns.insert(user_mod.name.clone(), fns);
        }

        for func in &program.functions {
            codegen::compile_function(
                self.context, &self.module, &self.builder,
                func, &rt, &module_fns,
            );
        }
    }

    // ── output helpers ──────────────────────────────────────────

    /// Dump the LLVM IR to stderr.
    #[allow(dead_code)]
    pub fn dump_ir(&self) {
        self.module.print_to_stderr();
    }

    /// Return the LLVM IR as a string.
    pub fn ir_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Write a native object file (`.obj` on Windows, `.o` elsewhere).
    pub fn write_object_file(&self, path: &Path) {
        Target::initialize_native(&InitializationConfig::default())
            .expect("failed to initialise native target");

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).expect("unsupported target triple");
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("failed to create TargetMachine");

        machine
            .write_to_file(&self.module, FileType::Object, path)
            .expect("failed to write object file");
    }
}