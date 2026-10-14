//! The Aion compiler driver.
//!
//! ```text
//! source ─▶ lexer/parser ─▶ loader (imports) ─▶ sema (types) ─▶ codegen (LLVM IR) ─▶ clang
//! ```

mod codegen;
mod diag;
mod embedded;
mod loader;
mod sema;
mod syntax;
mod toolchain;

use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use diag::{Diagnostic, SourceMap};

const USAGE: &str = "\
Aion compiler

usage:
  aion [run] <file.aion>             compile and run
  aion build <file.aion> [-o <out>]  compile to a native executable
  aion emit-ir <file.aion>           print the generated LLVM IR

options:
  -O0..-O3       optimisation level (default -O2)
  --keep-temps   keep intermediate files
  -h, --help     show this help
  -V, --version  show the version

Needs `clang` (15 or newer) on PATH; set AION_CLANG to use another compiler.";

enum Mode {
    Run,
    Build,
    EmitIr,
}

struct Cli {
    mode: Mode,
    input: PathBuf,
    output: Option<PathBuf>,
    opts: toolchain::Options,
}

fn parse_args(args: &[String]) -> Result<Cli, String> {
    let mut mode = None;
    let (mut input, mut output) = (None, None);
    let mut opts = toolchain::Options { opt_level: "2".into(), keep_temps: false };

    let mut it = args.iter();
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "run" | "build" | "emit-ir" if mode.is_none() && input.is_none() => {
                mode = Some(match arg.as_str() {
                    "run" => Mode::Run,
                    "build" => Mode::Build,
                    _ => Mode::EmitIr,
                });
            }
            "--emit-ir" => mode = Some(Mode::EmitIr), // pre-0.2 spelling
            "-o" => output = Some(PathBuf::from(it.next().ok_or("`-o` needs a file name")?)),
            "--keep-temps" => opts.keep_temps = true,
            "-h" | "--help" => return Err(USAGE.into()),
            "-V" | "--version" => return Err(format!("aion {}", env!("CARGO_PKG_VERSION"))),
            o if o.starts_with("-O") && o.len() == 3 && o.as_bytes()[2].is_ascii_digit() => {
                opts.opt_level = o[2..].to_string();
            }
            o if o.starts_with('-') => return Err(format!("unknown option `{o}`\n\n{USAGE}")),
            _ if input.is_none() => input = Some(PathBuf::from(arg)),
            _ => return Err(format!("unexpected argument `{arg}`")),
        }
    }
    let input = input.ok_or_else(|| format!("no input file\n\n{USAGE}"))?;
    let mode = mode.unwrap_or(Mode::Run);
    if output.is_some() && !matches!(mode, Mode::Build) {
        return Err("`-o` only makes sense with `aion build`".into());
    }
    Ok(Cli { mode, input, output, opts })
}

/// Generated LLVM IR plus the C sources it must be linked with.
type Compiled = (String, Vec<(String, &'static str)>);

/// Everything up to (but not including) invoking the C toolchain.
fn compile(map: &mut SourceMap, input: &Path) -> Result<Compiled, Diagnostic> {
    let loaded = loader::load(map, input)?;
    let program = sema::analyze(&loaded)?;
    Ok((codegen::emit(&program), loaded.c_sources))
}

fn default_output(input: &Path) -> PathBuf {
    let stem = input.file_stem().map_or("a".into(), |s| s.to_os_string());
    let mut out = PathBuf::from(stem);
    if cfg!(windows) {
        out.set_extension("exe");
    }
    out
}

fn run() -> Result<ExitCode, (Diagnostic, Option<SourceMap>)> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let cli = match parse_args(&args) {
        Ok(c) => c,
        Err(msg) => {
            let is_error = !(msg.starts_with("Aion compiler") || msg.starts_with("aion "));
            eprintln!("{msg}");
            return Ok(if is_error { ExitCode::from(2) } else { ExitCode::SUCCESS });
        }
    };

    let mut map = SourceMap::default();
    let (ir, c_sources) = match compile(&mut map, &cli.input) {
        Ok(v) => v,
        Err(d) => return Err((d, Some(map))),
    };

    if let Mode::EmitIr = cli.mode {
        print!("{ir}");
        return Ok(ExitCode::SUCCESS);
    }

    let output = match (&cli.mode, &cli.output) {
        (_, Some(o)) => o.clone(),
        (Mode::Build, None) => default_output(&cli.input),
        _ => std::env::temp_dir().join(format!("aion-run-{}{}", std::process::id(), std::env::consts::EXE_SUFFIX)),
    };
    toolchain::build(&ir, &c_sources, &output, &cli.opts).map_err(|d| (d, None))?;

    if let Mode::Build = cli.mode {
        eprintln!("built {}", output.display());
        return Ok(ExitCode::SUCCESS);
    }

    let status = Command::new(&output).status();
    let _ = std::fs::remove_file(&output);
    match status {
        Ok(s) => Ok(ExitCode::from(s.code().unwrap_or(1) as u8)),
        Err(e) => Err((Diagnostic::general(format!("could not run the compiled program: {e}")), None)),
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(code) => code,
        Err((diag, map)) => {
            eprint!("{}", diag.render(&map.unwrap_or_default()));
            ExitCode::FAILURE
        }
    }
}
