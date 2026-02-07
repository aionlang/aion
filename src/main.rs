//! Aion compiler driver.
//!
//! Usage:
//!   cargo run -- <file.aion>              # compiles to <file> / <file.exe>
//!   cargo run -- <file.aion> -o out.exe   # explicit output name
//!   cargo run -- <file.aion> --emit-ir    # print LLVM IR and exit
//!
//! Pipeline:  source → Lexer → Parser → AST → LLVM IR → .obj → link(+runtime) → native binary

mod ast;
mod compiler;
mod errors;
mod lexer;
mod parser;
mod resolver;
mod stdlib;

use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use compiler::Compiler;
use parser::Parser;
use ast::UserModule;
use errors::Phase;

fn main() {
    // ── CLI argument handling ────────────────────────────────────
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        errors::fatal_with_hint(
            Phase::Compiler,
            "No input file specified",
            Some("Usage: aion <source.aion> [-o output] [--emit-ir]".into()),
        );
    }

    let source_path = PathBuf::from(&args[1]);
    let emit_ir_only = args.iter().any(|a| a == "--emit-ir");

    let output_path = if let Some(pos) = args.iter().position(|a| a == "-o") {
        PathBuf::from(args.get(pos + 1).expect("expected output path after -o"))
    } else {
        let stem = source_path.file_stem().unwrap().to_str().unwrap();
        if cfg!(windows) {
            PathBuf::from(format!("{stem}.exe"))
        } else {
            PathBuf::from(stem)
        }
    };

    // ── Read source ─────────────────────────────────────────────
    let source = fs::read_to_string(&source_path).unwrap_or_else(|e| {
        errors::fatal(Phase::Compiler, format!("Could not read {}: {e}", source_path.display()));
    });

    errors::info(format!("compiling {}", source_path.display()));

    // ── Parse ───────────────────────────────────────────────────
    let mut parser = Parser::new(&source);
    let mut program = parser.parse_program();

    // ── Auto-load prelude ────────────────────────────────────────
    //
    // The prelude provides always-available functions like `println()`.
    // It is injected as a user module so its functions are compiled and
    // callable without any import statement.
    {
        let prelude_src = stdlib::prelude();
        let mut prelude_parser = Parser::new(prelude_src);
        let prelude_prog = prelude_parser.parse_program();
        program.user_modules.push(UserModule {
            name: "prelude".to_string(),
            functions: prelude_prog.functions,
        });
    }

    // ── Resolve module imports ───────────────────────────────────
    let source_dir = source_path.parent().unwrap_or_else(|| std::path::Path::new("."));
    resolver::resolve_imports(&mut program, source_dir);

    // Collect stdlib module names (e.g. "math") for runtime build.
    let imported_modules: Vec<String> = program
        .imports
        .iter()
        .filter(|i| i.is_stdlib())
        .map(|i| i.module_name().to_string())
        .collect();

    // ── LLVM codegen ────────────────────────────────────────────
    let context = inkwell::context::Context::create();
    let compiler = Compiler::new(&context, "aion");
    compiler.compile(&program);

    if emit_ir_only {
        print!("{}", compiler.ir_string());
        return;
    }

    // ── Emit object file ────────────────────────────────────────
    let obj_path = output_path.with_extension(if cfg!(windows) { "obj" } else { "o" });
    compiler.write_object_file(&obj_path);
    errors::info(format!("wrote object → {}", obj_path.display()));

    // ── Build runtime & link ────────────────────────────────────
    let rt_lib = compiler::runtime::build(&imported_modules);
    errors::info(format!("built runtime → {}", rt_lib.display()));

    compiler::linker::link(&obj_path, &rt_lib, &output_path);
    errors::success(format!("done → {}", output_path.display()));

    // ── Run the compiled binary ─────────────────────────────────
    eprintln!();
    let run_path = if output_path.is_relative() {
        std::env::current_dir().unwrap().join(&output_path)
    } else {
        output_path.clone()
    };
    let status = Command::new(&run_path)
        .status()
        .unwrap_or_else(|e| {
            errors::fatal(Phase::Linker, format!("Failed to run {}: {e}", run_path.display()));
        });

    std::process::exit(status.code().unwrap_or(1));
}