//! The Aion compiler driver.
//!
//! ```text
//! source ─▶ lexer/parser ─▶ loader (imports, deps) ─▶ sema (types) ─▶ codegen (LLVM IR) ─▶ clang
//! ```

mod cache;
mod codegen;
mod diag;
mod embedded;
mod loader;
mod project;
mod sema;
mod syntax;
mod toolchain;

use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use diag::{Diagnostic, SourceMap};

const USAGE: &str = "\
Aion compiler

usage:
  aion [run] [file.aion] [args…]      compile and run (default: the project's entry)
  aion build [file.aion] [-o <out>]   compile to a native executable
  aion emit-ir [file.aion]            print the generated LLVM IR
  aion new <name>                     create a project directory
  aion fetch                          download the project's dependencies
  aion clean                          delete the build cache

Arguments after the file are passed to the program (`args()` in Aion); put `--`
before them if they look like options.

options:
  -O0..-O3       optimisation level (default -O2)
  --keep-temps   keep intermediate files
  -h, --help     show this help
  -V, --version  show the version

A project is a folder with an `aion.project` file; see docs/projects.md.
Needs `clang` (15 or newer) on PATH; set AION_CLANG to use another compiler.
Set AION_NO_CACHE=1 to rebuild even when a cached binary exists.";

enum Subcommand {
    Run,
    Build,
    EmitIr,
    New,
    Fetch,
    Clean,
}

struct Cli {
    command: Subcommand,
    input: Option<PathBuf>,
    output: Option<PathBuf>,
    opts: toolchain::Options,
    /// What follows the file: handed to the program.
    program_args: Vec<String>,
}

fn parse_args(args: &[String]) -> Result<Cli, String> {
    let mut command = None;
    let (mut input, mut output) = (None, None);
    let mut opts = toolchain::Options { opt_level: "2".into(), keep_temps: false };
    let mut program_args = Vec::new();

    let mut it = args.iter();
    while let Some(arg) = it.next() {
        match arg.as_str() {
            // `--` ends our options: everything after it belongs to the program.
            "--" => {
                program_args.extend(it.by_ref().cloned());
                break;
            }
            "run" | "build" | "emit-ir" | "new" | "fetch" | "clean" if command.is_none() && input.is_none() => {
                command = Some(match arg.as_str() {
                    "run" => Subcommand::Run,
                    "build" => Subcommand::Build,
                    "emit-ir" => Subcommand::EmitIr,
                    "new" => Subcommand::New,
                    "fetch" => Subcommand::Fetch,
                    _ => Subcommand::Clean,
                });
            }
            "--emit-ir" => command = Some(Subcommand::EmitIr), // pre-0.2 spelling
            "-o" => output = Some(PathBuf::from(it.next().ok_or("`-o` needs a file name")?)),
            "--keep-temps" => opts.keep_temps = true,
            "-h" | "--help" => return Err(USAGE.into()),
            "-V" | "--version" => return Err(format!("aion {}", env!("CARGO_PKG_VERSION"))),
            o if o.starts_with("-O") && o.len() == 3 && o.as_bytes()[2].is_ascii_digit() => {
                opts.opt_level = o[2..].to_string();
            }
            // After the file, anything we don't know is the program's.
            _ if input.is_some() => program_args.push(arg.clone()),
            o if o.starts_with('-') => return Err(format!("unknown option `{o}`\n\n{USAGE}")),
            _ => input = Some(PathBuf::from(arg)),
        }
    }
    let command = command.unwrap_or(Subcommand::Run);
    if output.is_some() && !matches!(command, Subcommand::Build) {
        return Err("`-o` only makes sense with `aion build`".into());
    }
    if matches!(command, Subcommand::New) && input.is_none() {
        return Err("usage: aion new <name>".into());
    }
    if !program_args.is_empty() && !matches!(command, Subcommand::Run) {
        return Err("arguments for the program only make sense when running it".into());
    }
    Ok(Cli { command, input, output, opts, program_args })
}

type Failure = (Diagnostic, Option<SourceMap>);

/// The entry file and the project it belongs to (if any).
fn locate(input: Option<&Path>) -> Result<(PathBuf, Option<project::Project>), Diagnostic> {
    match input {
        Some(file) => {
            let dir = file.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
            let proj = project::find(dir).map(|p| project::load(&p)).transpose()?;
            Ok((file.to_path_buf(), proj))
        }
        None => {
            let found = project::find(Path::new("."))
                .ok_or_else(|| Diagnostic::general("no input file, and no `aion.project` in this directory or above")
                    .hint("run `aion file.aion`, or create a project with `aion new <name>`"))?;
            let proj = project::load(&found)?;
            Ok((proj.entry.clone(), Some(proj)))
        }
    }
}

/// Generated LLVM IR plus the C sources it must be linked with.
type Compiled = (String, Vec<(String, &'static str)>);

/// Everything up to (but not including) invoking the C toolchain.
fn compile(map: &mut SourceMap, entry: &Path, project: Option<&project::Project>) -> Result<Compiled, Failure> {
    let dir = entry.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
    let packages = project::resolve(project, dir).map_err(|d| (d, None))?;
    let result = (|| {
        let loaded = loader::load(map, entry, packages)?;
        let program = sema::analyze(&loaded)?;
        Ok::<_, Diagnostic>((codegen::emit(&program), loaded.c_sources))
    })();
    result.map_err(|d| (d, None))
}

fn exe_name(stem: &str) -> String {
    format!("{stem}{}", std::env::consts::EXE_SUFFIX)
}

/// Build (or reuse) the program for `ir` and run it.
fn run_program(
    ir: &str,
    c_sources: &[(String, &'static str)],
    opts: &toolchain::Options,
    program_args: &[String],
) -> Result<ExitCode, Diagnostic> {
    let cached = std::env::var_os("AION_NO_CACHE").is_none();
    let exe = if cached {
        let mut parts: Vec<&[u8]> = vec![env!("CARGO_PKG_VERSION").as_bytes(), opts.opt_level.as_bytes(), ir.as_bytes()];
        // The runtime is linked into every program, so a new compiler's runtime must not reuse an old build.
        for (name, text) in embedded::RUNTIME {
            parts.push(name.as_bytes());
            parts.push(text.as_bytes());
        }
        for (name, text) in c_sources {
            parts.push(name.as_bytes());
            parts.push(text.as_bytes());
        }
        let bin_dir = cache::dir().join("bin");
        let exe = bin_dir.join(exe_name(&format!("{:016x}", cache::hash(&parts))));
        if !exe.is_file() {
            std::fs::create_dir_all(&bin_dir)
                .map_err(|e| Diagnostic::general(format!("cannot create {}: {e}", bin_dir.display())))?;
            // Build beside the final name, then rename, so a crash never leaves a half-written binary.
            let partial = bin_dir.join(exe_name(&format!("partial-{}", std::process::id())));
            toolchain::build(ir, c_sources, &partial, opts)?;
            std::fs::rename(&partial, &exe)
                .map_err(|e| Diagnostic::general(format!("cannot store the build: {e}")))?;
            cache::trim(&bin_dir, 128);
        }
        exe
    } else {
        let exe = std::env::temp_dir().join(exe_name(&format!("aion-run-{}", std::process::id())));
        toolchain::build(ir, c_sources, &exe, opts)?;
        exe
    };

    let status = Command::new(&exe).args(program_args).status();
    if !cached {
        let _ = std::fs::remove_file(&exe);
    }
    match status {
        Ok(s) => Ok(ExitCode::from(s.code().unwrap_or(1) as u8)),
        Err(e) => Err(Diagnostic::general(format!("could not run the compiled program: {e}"))),
    }
}

fn new_project(name: &str) -> Result<(), Diagnostic> {
    let dir = Path::new(name);
    if dir.exists() {
        return Err(Diagnostic::general(format!("`{name}` already exists")));
    }
    let stem = dir.file_name().map_or(name.to_string(), |s| s.to_string_lossy().into_owned());
    let write = |file: &str, text: String| {
        std::fs::write(dir.join(file), text).map_err(|e| Diagnostic::general(format!("cannot write {file}: {e}")))
    };
    std::fs::create_dir_all(dir).map_err(|e| Diagnostic::general(format!("cannot create {name}: {e}")))?;
    write(project::FILE_NAME, format!("name = \"{stem}\"\nversion = \"0.1.0\"\nentry = \"main.aion\"\n\n[dependencies]\n"))?;
    write("main.aion", format!("println(\"Hello from {stem}!\")\n"))?;
    eprintln!("created {name}/  — try: cd {name} && aion");
    Ok(())
}

fn run() -> Result<ExitCode, Failure> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let cli = match parse_args(&args) {
        Ok(c) => c,
        Err(msg) => {
            let is_error = !(msg.starts_with("Aion compiler") || msg.starts_with("aion "));
            eprintln!("{msg}");
            return Ok(if is_error { ExitCode::from(2) } else { ExitCode::SUCCESS });
        }
    };
    let general = |d: Diagnostic| (d, None);

    match cli.command {
        Subcommand::New => {
            new_project(cli.input.as_deref().unwrap().to_str().unwrap()).map_err(general)?;
            return Ok(ExitCode::SUCCESS);
        }
        Subcommand::Clean => {
            let dir = cache::dir();
            let _ = std::fs::remove_dir_all(&dir);
            eprintln!("removed {}", dir.display());
            return Ok(ExitCode::SUCCESS);
        }
        Subcommand::Fetch => {
            let (_, proj) = locate(cli.input.as_deref()).map_err(general)?;
            let proj = proj.ok_or_else(|| general(Diagnostic::general("no `aion.project` found")))?;
            let packages = project::resolve(Some(&proj), &proj.root).map_err(general)?;
            eprintln!("{} dependenc{} ready", packages.len() - 1, if packages.len() == 2 { "y" } else { "ies" });
            return Ok(ExitCode::SUCCESS);
        }
        _ => {}
    }

    let (entry, proj) = locate(cli.input.as_deref()).map_err(general)?;
    let mut map = SourceMap::default();
    let (ir, c_sources) = compile(&mut map, &entry, proj.as_ref()).map_err(|(d, _)| (d, Some(map)))?;

    match cli.command {
        Subcommand::EmitIr => {
            print!("{ir}");
            Ok(ExitCode::SUCCESS)
        }
        Subcommand::Build => {
            let stem = proj.as_ref().filter(|_| cli.input.is_none()).map(|p| p.name.clone()).unwrap_or_else(|| {
                entry.file_stem().map_or("a".into(), |s| s.to_string_lossy().into_owned())
            });
            let output = cli.output.clone().unwrap_or_else(|| PathBuf::from(exe_name(&stem)));
            toolchain::build(&ir, &c_sources, &output, &cli.opts).map_err(general)?;
            eprintln!("built {}", output.display());
            Ok(ExitCode::SUCCESS)
        }
        _ => run_program(&ir, &c_sources, &cli.opts, &cli.program_args).map_err(general),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(args: &[&str]) -> Result<Cli, String> {
        parse_args(&args.iter().map(|a| a.to_string()).collect::<Vec<_>>())
    }

    #[test]
    fn arguments_after_the_file_belong_to_the_program() {
        let cli = parse(&["script.aion", "one", "-x", "--flag=2"]).unwrap();
        assert_eq!(cli.input.unwrap(), PathBuf::from("script.aion"));
        assert_eq!(cli.program_args, ["one", "-x", "--flag=2"]);

        // our own options still work after the file, and `--` hands over everything
        let cli = parse(&["run", "script.aion", "-O3", "a", "--", "-O0", "b"]).unwrap();
        assert_eq!(cli.opts.opt_level, "3");
        assert_eq!(cli.program_args, ["a", "-O0", "b"]);

        // before the file an unknown option is still an error, and builds take no arguments
        assert!(parse(&["-x", "script.aion"]).is_err());
        assert!(parse(&["build", "script.aion", "extra"]).is_err());
    }
}
