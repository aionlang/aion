//! LLVM-backed compiler for the Aion language.
//!
//! Takes a [`Program`] AST and lowers it to native machine code via Inkwell
//! (Rust-friendly LLVM bindings).
//!
//! Pipeline:
//! 1. **Codegen** — walk the AST and emit LLVM IR.
//! 2. **Object**  — write a `.obj` / `.o` via TargetMachine.
//! 3. **Link**    — invoke the system C compiler to link the object file
//!                  with `libaion_runtime` into a native binary.
//!
//! The compiler emits calls to `aion_*` runtime functions (declared in
//! `runtime/aion_runtime.c`) so that built-in operations like `print`
//! are handled by our own runtime, not raw libc.
//!
//! ## Standard library modules
//!
//! When the source contains `import aion.math;`, the compiler declares
//! the corresponding C functions from `runtime/aion_math.c` and emits
//! calls to them for expressions like `math.sqrt(144.0)`.

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, ValueKind};
use inkwell::OptimizationLevel;

use crate::ast::{Expr, Function, Import, Program, UserModule};

// ═══════════════════════════════════════════════════════════════════
// Standard-library function registry
// ═══════════════════════════════════════════════════════════════════

/// Describes one C function exposed from a standard-library module.
#[derive(Clone, Debug)]
struct StdlibFunc {
    /// The C symbol name, e.g. `"aion_math_sqrt"`.
    c_name: &'static str,
    /// Number of f64 parameters.
    arity: usize,
    /// Return type — for now all math functions return f64.
    returns_f64: bool,
}

/// Return the function registry for a given module name.
///
/// Extend this when you add new standard-library modules.
fn stdlib_registry(module: &str) -> Option<HashMap<&'static str, StdlibFunc>> {
    match module {
        "math" => {
            let mut m = HashMap::new();
            m.insert("sqrt",  StdlibFunc { c_name: "aion_math_sqrt",  arity: 1, returns_f64: true });
            m.insert("abs",   StdlibFunc { c_name: "aion_math_abs",   arity: 1, returns_f64: true });
            m.insert("sin",   StdlibFunc { c_name: "aion_math_sin",   arity: 1, returns_f64: true });
            m.insert("cos",   StdlibFunc { c_name: "aion_math_cos",   arity: 1, returns_f64: true });
            m.insert("floor", StdlibFunc { c_name: "aion_math_floor", arity: 1, returns_f64: true });
            m.insert("ceil",  StdlibFunc { c_name: "aion_math_ceil",  arity: 1, returns_f64: true });
            m.insert("pow",   StdlibFunc { c_name: "aion_math_pow",   arity: 2, returns_f64: true });
            Some(m)
        }
        _ => None,
    }
}

// ═══════════════════════════════════════════════════════════════════
// Compiler
// ═══════════════════════════════════════════════════════════════════

/// Holds LLVM state for a single compilation unit.
pub struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new compiler.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self { context, module, builder }
    }

    // ── codegen ─────────────────────────────────────────────────────

    /// Lower a full [`Program`] to LLVM IR.
    pub fn compile(&self, program: &Program) {
        // Declare the core Aion runtime functions.
        let rt = self.declare_runtime();

        // Declare standard-library functions for each imported module.
        let mut module_fns: HashMap<String, HashMap<String, FunctionValue<'ctx>>> =
            self.declare_stdlib(&program.imports);

        // Compile user-written modules and collect their function handles.
        for user_mod in &program.user_modules {
            let fns = self.compile_user_module(user_mod, &rt, &module_fns);
            module_fns.insert(user_mod.name.clone(), fns);
        }

        for func in &program.functions {
            self.compile_function(func, &rt, &module_fns);
        }
    }

    /// Compile a single function definition.
    fn compile_function(
        &self,
        func: &Function,
        rt: &Runtime<'ctx>,
        stdlib: &HashMap<String, HashMap<String, FunctionValue<'ctx>>>,
    ) {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let fn_val = self.module.add_function(&func.name, fn_type, None);

        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);

        for expr in &func.body {
            self.compile_expr(expr, rt, stdlib);
        }

        // Return 0 from main.
        self.builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .expect("build return");
    }

    /// Compile all functions in a user-written Aion module.
    ///
    /// Each function `fn greet()` in module `utils` becomes a void LLVM
    /// function named `utils_greet`.  Returns the lookup table so callers
    /// can resolve `utils.greet()`.
    fn compile_user_module(
        &self,
        user_mod: &UserModule,
        rt: &Runtime<'ctx>,
        module_fns: &HashMap<String, HashMap<String, FunctionValue<'ctx>>>,
    ) -> HashMap<String, FunctionValue<'ctx>> {
        let void = self.context.void_type();
        let mut fns = HashMap::new();

        for func in &user_mod.functions {
            let mangled = format!("{}_{}", user_mod.name, func.name);

            // For now, user module functions take no args and return void.
            let fn_type = void.fn_type(&[], false);
            let fn_val = self.module.add_function(&mangled, fn_type, None);

            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);

            for expr in &func.body {
                self.compile_expr(expr, rt, module_fns);
            }

            self.builder.build_return(None).expect("build void return");

            fns.insert(func.name.clone(), fn_val);
        }

        fns
    }

    /// Lower a single expression.
    ///
    /// Returns `Some(value)` when the expression produces a result
    /// (e.g.  a module call that returns f64), or `None` for statements
    /// like `print`.
    fn compile_expr(
        &self,
        expr: &Expr,
        rt: &Runtime<'ctx>,
        stdlib: &HashMap<String, HashMap<String, FunctionValue<'ctx>>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            // ── print("string literal") ─────────────────────────
            Expr::PrintStr(text) => {
                let global = self
                    .builder
                    .build_global_string_ptr(text, "str")
                    .expect("build global string");
                let args: [BasicMetadataValueEnum; 1] =
                    [global.as_pointer_value().into()];
                self.builder
                    .build_call(rt.aion_print, &args, "call")
                    .expect("build call");
                None
            }

            // ── print(<expr>) — currently emits as print_float ──
            Expr::PrintExpr(inner) => {
                let val = self
                    .compile_expr(inner, rt, stdlib)
                    .expect("print argument must produce a value");
                let args: [BasicMetadataValueEnum; 1] = [val.into()];
                self.builder
                    .build_call(rt.aion_print_float, &args, "call")
                    .expect("build call to aion_print_float");
                None
            }

            // ── module.func(args…) ──────────────────────────────
            Expr::ModuleCall { module, func, args } => {
                let mod_fns = stdlib.get(module).unwrap_or_else(|| {
                    panic!("Module '{module}' was not imported");
                });
                let llvm_fn = mod_fns.get(func.as_str()).unwrap_or_else(|| {
                    panic!("Function '{func}' not found in module '{module}'");
                });

                let llvm_args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|a| {
                        self.compile_expr(a, rt, stdlib)
                            .expect("argument must produce a value")
                            .into()
                    })
                    .collect();

                let call = self
                    .builder
                    .build_call(*llvm_fn, &llvm_args, "modcall")
                    .expect("build module call");

                match call.try_as_basic_value() {
                    ValueKind::Basic(val) => Some(val),
                    ValueKind::Instruction(_) => None,
                }
            }

            // ── float literal ───────────────────────────────────
            Expr::FloatLiteral(v) => {
                let val = self.context.f64_type().const_float(*v);
                Some(val.into())
            }

            // ── integer literal ─────────────────────────────────
            Expr::IntLiteral(v) => {
                // Promote to f64 so it works seamlessly with math funcs.
                let val = self.context.f64_type().const_float(*v as f64);
                Some(val.into())
            }
        }
    }

    // ── stdlib declarations ─────────────────────────────────────────

    /// For each imported module, declare the corresponding C functions
    /// in the LLVM module and return a lookup table.
    fn declare_stdlib(
        &self,
        imports: &[Import],
    ) -> HashMap<String, HashMap<String, FunctionValue<'ctx>>> {
        let mut map = HashMap::new();
        let f64_ty = self.context.f64_type();

        for imp in imports {
            // Skip non-stdlib imports — they're handled as user modules.
            if !imp.is_stdlib() {
                continue;
            }

            let mod_name = imp.module_name().to_string();
            let registry = stdlib_registry(&mod_name).unwrap_or_else(|| {
                panic!("Unknown standard-library module: '{}'", imp.path.join("."));
            });

            let mut fns = HashMap::new();
            for (name, info) in &registry {
                // Build the LLVM function type: (f64, f64, …) -> f64
                let param_types: Vec<inkwell::types::BasicMetadataTypeEnum> =
                    (0..info.arity).map(|_| f64_ty.into()).collect();
                let fn_type = if info.returns_f64 {
                    f64_ty.fn_type(&param_types, false)
                } else {
                    self.context.void_type().fn_type(&param_types, false)
                };
                let fn_val = self.module.add_function(info.c_name, fn_type, None);
                fns.insert(name.to_string(), fn_val);
            }

            map.insert(mod_name, fns);
        }

        map
    }

    // ── core runtime declarations ───────────────────────────────────

    /// Declare the external `aion_*` runtime functions so LLVM knows their
    /// signatures.  The actual implementations live in `runtime/aion_runtime.c`.
    fn declare_runtime(&self) -> Runtime<'ctx> {
        let void = self.context.void_type();
        let ptr = self.context.ptr_type(inkwell::AddressSpace::default());
        let i64 = self.context.i64_type();
        let f64_type = self.context.f64_type();

        // void aion_print(const char *)
        let print_ty = void.fn_type(&[ptr.into()], false);
        let aion_print = self.module.add_function("aion_print", print_ty, None);

        // void aion_print_int(long long)
        let print_int_ty = void.fn_type(&[i64.into()], false);
        let aion_print_int = self.module.add_function("aion_print_int", print_int_ty, None);

        // void aion_print_float(double)
        let print_float_ty = void.fn_type(&[f64_type.into()], false);
        let aion_print_float = self.module.add_function("aion_print_float", print_float_ty, None);

        // void aion_panic(const char *)
        let panic_ty = void.fn_type(&[ptr.into()], false);
        let aion_panic = self.module.add_function("aion_panic", panic_ty, None);

        // void *aion_alloc(long long)
        let alloc_ty = ptr.fn_type(&[i64.into()], false);
        let aion_alloc = self.module.add_function("aion_alloc", alloc_ty, None);

        // void aion_free(void *)
        let free_ty = void.fn_type(&[ptr.into()], false);
        let aion_free = self.module.add_function("aion_free", free_ty, None);

        Runtime {
            aion_print,
            aion_print_int,
            aion_print_float,
            aion_panic,
            aion_alloc,
            aion_free,
        }
    }

    // ── output helpers ──────────────────────────────────────────────

    /// Dump the LLVM IR to stderr (handy for `--emit-ir`).
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

    // ── embedded runtime sources ────────────────────────────────────

    /// The core runtime C source, embedded at compile time.
    const RUNTIME_SRC: &'static str = include_str!("../../runtime/aion_runtime.c");

    /// The math module C source, embedded at compile time.
    const MATH_SRC: &'static str = include_str!("../../runtime/aion_math.c");

    /// Build the Aion runtime (core + stdlib modules) into a static
    /// library and return its path.
    ///
    /// All C sources are embedded in the binary so the compiler is
    /// fully self-contained.  The compiled library is cached in a
    /// temporary directory and reused across invocations.
    pub fn build_runtime(imported_modules: &[String]) -> std::path::PathBuf {
        let cache_dir = std::env::temp_dir().join("aion-runtime-cache");
        std::fs::create_dir_all(&cache_dir).expect("failed to create runtime cache dir");

        // ── determine which C sources to compile ────────────────
        let mut sources: Vec<(&str, &str)> = vec![
            ("aion_runtime.c", Self::RUNTIME_SRC),
        ];

        for m in imported_modules {
            match m.as_str() {
                "math" => sources.push(("aion_math.c", Self::MATH_SRC)),
                other  => eprintln!("[aion] warning: no C source for module '{other}'"),
            }
        }

        // ── write sources and check if rebuild needed ───────────
        let mut obj_paths = Vec::new();
        let mut needs_rebuild = false;

        for (filename, src) in &sources {
            let c_path = cache_dir.join(filename);
            let o_path = cache_dir.join(filename.replace(".c", ".o"));

            let source_changed = match std::fs::read_to_string(&c_path) {
                Ok(existing) => existing != *src,
                Err(_) => true,
            };

            if source_changed || !o_path.exists() {
                needs_rebuild = true;
                std::fs::write(&c_path, src).expect("failed to write runtime source");

                let status = Command::new("gcc")
                    .args([
                        c_path.to_str().unwrap(),
                        "-c", "-O2",
                        "-o", o_path.to_str().unwrap(),
                    ])
                    .status()
                    .expect("failed to run gcc for runtime");
                assert!(status.success(), "compilation of {} failed", filename);
            }

            obj_paths.push(o_path);
        }

        let rt_lib = cache_dir.join("libaion_runtime.a");

        if needs_rebuild || !rt_lib.exists() {
            // Archive all object files into a single static library.
            let mut ar_args = vec!["rcs".to_string(), rt_lib.to_str().unwrap().to_string()];
            for o in &obj_paths {
                ar_args.push(o.to_str().unwrap().to_string());
            }

            let status = Command::new("ar")
                .args(&ar_args)
                .status()
                .expect("failed to run ar");
            assert!(status.success(), "ar failed");
        }

        rt_lib
    }

    /// Link an object file + the Aion runtime into a native executable.
    ///
    /// Adds `-lm` so that C math functions (sqrt, sin, …) are resolved.
    pub fn link(object_path: &Path, runtime_lib: &Path, output_path: &Path) {
        let obj = object_path.to_str().unwrap();
        let rt = runtime_lib.to_str().unwrap();
        let out = output_path.to_str().unwrap();

        let candidates: Vec<(&str, Vec<&str>)> = if cfg!(windows) {
            vec![
                ("gcc",   vec![obj, rt, "-lm", "-o", out]),
                ("clang", vec![obj, rt, "-lm", "-o", out]),
            ]
        } else {
            vec![
                ("cc",    vec![obj, rt, "-lm", "-o", out]),
                ("gcc",   vec![obj, rt, "-lm", "-o", out]),
                ("clang", vec![obj, rt, "-lm", "-o", out]),
            ]
        };

        for (cmd, args) in &candidates {
            let result = Command::new(cmd).args(args).output();
            match result {
                Ok(output) if output.status.success() => return,
                Ok(output) => {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    eprintln!("[{cmd}] link failed:\n{stderr}");
                }
                Err(_) => continue,
            }
        }

        panic!(
            "No working linker found. Install gcc or clang on your PATH."
        );
    }
}

#[allow(dead_code)]
/// Handles to the declared Aion runtime functions in the LLVM module.
///
/// Extend this struct as you add new built-in functions to
/// `runtime/aion_runtime.c`.
struct Runtime<'ctx> {
    aion_print:       FunctionValue<'ctx>,
    aion_print_int:   FunctionValue<'ctx>,
    aion_print_float: FunctionValue<'ctx>,
    aion_panic:       FunctionValue<'ctx>,
    aion_alloc:       FunctionValue<'ctx>,
    aion_free:        FunctionValue<'ctx>,
}
