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
mod lexer;
mod parser;
mod stdlib;

use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use compiler::Compiler;
use parser::Parser;
use ast::UserModule;

fn main() {
    // ── CLI argument handling ────────────────────────────────────
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: aion <source.aion> [-o output] [--emit-ir]");
        std::process::exit(1);
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
        eprintln!("Could not read {}: {e}", source_path.display());
        std::process::exit(1);
    });

    println!("[aion] compiling {}", source_path.display());

    // ── Parse ───────────────────────────────────────────────────
    let mut parser = Parser::new(&source);
    let mut program = parser.parse_program();

    // ── Resolve module imports ───────────────────────────────────
    //
    // Three import flavours:
    //   import aion.math;   → C-backed stdlib (compiler declares externs)
    //   import std.io;      → Aion stdlib, embedded in the compiler binary
    //   import utils;       → user-local .aion file (same dir or lib/)
    //
    let source_dir = source_path.parent().unwrap_or_else(|| std::path::Path::new("."));

    for imp in &program.imports {
        if imp.is_stdlib() {
            continue; // C-backed modules — handled during LLVM codegen
        }

        let mod_name = imp.module_name();

        // ── embedded Aion stdlib ────────────────────────────────
        if imp.is_std() {
            let src = stdlib::get(mod_name).unwrap_or_else(|| {
                eprintln!("Unknown standard module 'std.{mod_name}'.");
                eprintln!("Available: {}", stdlib::available().join(", "));
                std::process::exit(1);
            });

            println!("[aion] loading std.{mod_name} (embedded)");

            let mut mod_parser = Parser::new(src);
            let mod_program = mod_parser.parse_program();

            program.user_modules.push(UserModule {
                name: mod_name.to_string(),
                functions: mod_program.functions,
            });
            continue;
        }

        // ── user-local module from disk ─────────────────────────
        let candidates = [
            source_dir.join(format!("{mod_name}.aion")),
            source_dir.join("lib").join(format!("{mod_name}.aion")),
        ];

        let mod_path = candidates.iter().find(|p| p.exists()).unwrap_or_else(|| {
            eprintln!(
                "Could not find module '{mod_name}'. Searched:\n  {}",
                candidates.iter().map(|p| p.display().to_string()).collect::<Vec<_>>().join("\n  ")
            );
            std::process::exit(1);
        });

        let mod_source = fs::read_to_string(mod_path).unwrap_or_else(|e| {
            eprintln!("Could not read {}: {e}", mod_path.display());
            std::process::exit(1);
        });

        println!("[aion] loading module '{mod_name}' from {}", mod_path.display());

        let mut mod_parser = Parser::new(&mod_source);
        let mod_program = mod_parser.parse_program();

        program.user_modules.push(UserModule {
            name: mod_name.to_string(),
            functions: mod_program.functions,
        });
    }

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
    println!("[aion] wrote object → {}", obj_path.display());

    // ── Build runtime & link ────────────────────────────────────
    let rt_lib = Compiler::build_runtime(&imported_modules);
    println!("[aion] built runtime → {}", rt_lib.display());

    Compiler::link(&obj_path, &rt_lib, &output_path);
    println!("[aion] done → {}", output_path.display());

    // ── Run the compiled binary ─────────────────────────────────
    println!();
    let run_path = if output_path.is_relative() {
        std::env::current_dir().unwrap().join(&output_path)
    } else {
        output_path.clone()
    };
    let status = Command::new(&run_path)
        .status()
        .unwrap_or_else(|e| {
            eprintln!("Failed to run {}: {e}", run_path.display());
            std::process::exit(1);
        });

    std::process::exit(status.code().unwrap_or(1));
}