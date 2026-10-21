//! The Aion compiler driver.
//!
//! ```text
//! source ─▶ lexer/parser ─▶ loader (imports, deps) ─▶ sema (types) ─▶ codegen (LLVM IR) ─▶ clang
//! ```

mod banner;
mod cache;
mod codegen;
mod diag;
mod doc;
mod embedded;
mod format;
mod loader;
mod manage;
mod project;
mod reflect;
mod repl;
mod sema;
mod syntax;
mod testing;
mod toolchain;
#[cfg(feature = "bundled-zig")]
mod zigbundle;

use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use diag::{Diagnostic, SourceMap};

const USAGE: &str = concat!(
    "Aion compiler ", env!("CARGO_PKG_VERSION"), "\n",
    "\n",
    "usage:\n",
    "  aion [run] [file.aion] [args…]      compile and run (default: the project's entry)\n",
    "  aion build [file.aion] [-o <out>]   compile to a native executable\n",
    "  aion emit-ir [file.aion]            print the generated LLVM IR\n",
    "  aion check [file.aion]              type-check without building anything\n",
    "  aion repl                           an interactive prompt (builds at -O0 unless you give -O1 or -O3)\n",
    "  aion help                           show this text\n",
    "  aion test [path…]                   run the programs in tests/ (each passes if it exits with status 0)\n",
    "  aion fmt [--check] [path…]          tidy whitespace in .aion files\n",
    "  aion doc [path…] [-o <file>]        write the API reference (Markdown) from the /** */ comments\n",
    "  aion new <name>                     create a project directory\n",
    "  aion add <source> [--as NAME] [--tag T|--branch B|--rev R] [--script FILE] [--yes]\n",
    "                                      add a dependency (a git repo like github.com/me/http@v1, or a path)\n",
    "  aion install [<source>] [--packages DIR]\n",
    "                                      like add; a new project asks where to keep its packages (default\n",
    "                                      aion-dependencies/). With no source: download what the project needs\n",
    "  aion remove <name>                  remove a dependency\n",
    "  aion update [name]                  fetch newer commits for git dependencies\n",
    "  aion tree [FILE]                    show the dependency tree\n",
    "  aion fetch                          download the project's dependencies\n",
    "  aion clean                          delete the build cache\n",
    "  aion warmup                         prepare the C compiler that comes with aion now (the installer does)\n",
    "\n",
    "Arguments after the file are passed to the program (`args()` in Aion); put `--`\n",
    "before them if they look like options.\n",
    "\n",
    "options:\n",
    "  -O0..-O3       optimisation level (default -O2)\n",
    "  --project <p>  use this aion.project (or the folder holding it) instead of searching upward\n",
    "  --no-trace     leave out the stack trace a panic prints (a little faster)\n",
    "  --silent-empty-main  do not warn when the program has no `fn main()` and no top-level statements\n",
    "  --keep-temps   keep intermediate files\n",
    "  -h, --help     show this help\n",
    "  -V, --version  show the version\n",
    "\n",
    "A project is a folder with an `aion.project` file; see docs/projects.md.\n",
    "Programs are built with a C compiler: the Windows installer and the release builds of aion carry their own;\n",
    "otherwise aion uses clang 15 or newer, or zig, from the PATH (AION_CLANG or AION_ZIG name another one).\n",
    "Set AION_NO_CACHE=1 to rebuild even when a cached binary exists.",
);

enum Subcommand {
    Run,
    Build,
    EmitIr,
    Check,
    New,
    Fetch,
    Clean,
    Repl,
}

struct Cli {
    command: Subcommand,
    input: Option<PathBuf>,
    output: Option<PathBuf>,
    /// `--project`: use this project instead of searching.
    project: Option<PathBuf>,
    /// `--silent-empty-main`: say nothing when there is nothing to run.
    silent_empty_main: bool,
    opts: toolchain::Options,
    /// What follows the file: handed to the program.
    program_args: Vec<String>,
}

fn parse_args(args: &[String]) -> Result<Cli, String> {
    let mut command = None;
    let (mut input, mut output) = (None, None);
    // `AION_OPT=0` makes builds faster and slower-running by default (the test suite uses it); `-O<n>` wins.
    let default_opt = std::env::var("AION_OPT").ok().filter(|o| matches!(o.as_str(), "0" | "1" | "2" | "3" | "s" | "z"));
    let mut opts = toolchain::Options { opt_level: default_opt.unwrap_or_else(|| "2".into()), keep_temps: false, trace: true };
    let mut program_args = Vec::new();
    let mut project = None;
    let mut silent_empty_main = false;

    let mut it = args.iter();
    while let Some(arg) = it.next() {
        match arg.as_str() {
            // `--` ends our options: everything after it belongs to the program.
            "--" => {
                program_args.extend(it.by_ref().cloned());
                break;
            }
            "run" | "build" | "emit-ir" | "check" | "new" | "fetch" | "clean" | "repl"
                if command.is_none() && input.is_none() =>
            {
                command = Some(match arg.as_str() {
                    "run" => Subcommand::Run,
                    "build" => Subcommand::Build,
                    "emit-ir" => Subcommand::EmitIr,
                    "check" => Subcommand::Check,
                    "new" => Subcommand::New,
                    "fetch" => Subcommand::Fetch,
                    "repl" => Subcommand::Repl,
                    _ => Subcommand::Clean,
                });
            }
            "--emit-ir" => command = Some(Subcommand::EmitIr), // pre-0.2 spelling
            "-o" => output = Some(PathBuf::from(it.next().ok_or("`-o` needs a file name")?)),
            "--keep-temps" => opts.keep_temps = true,
            "--no-trace" => opts.trace = false,
            "--silent-empty-main" => silent_empty_main = true,
            "--project" => project = Some(PathBuf::from(it.next().ok_or("`--project` needs a path")?)),
            "-h" | "--help" => return Err(USAGE.into()),
            "-V" | "--version" => return Err(format!("aion {}", env!("CARGO_PKG_VERSION"))),
            o if o.starts_with("-O") && o.len() == 3 && o.as_bytes()[2].is_ascii_digit() => {
                opts.opt_level = o[2..].to_string();
            }
            // After the file, anything we don't know is the program's.
            _ if input.is_some() => program_args.push(arg.clone()),
            o if o.starts_with('-') => return Err(format!("unknown option `{o}`\n\n{USAGE}")),
            "help" if command.is_none() => return Err(USAGE.into()),
            // `aion new <name>`: the name of the project to create
            _ if matches!(command, Some(Subcommand::New)) => input = Some(PathBuf::from(arg)),
            // anything else that is not a file of ours is not a command
            file if file.ends_with(".aion") => input = Some(PathBuf::from(file)),
            other => return Err(format!("error: unknown command `{other}`; type `aion help` for help")),
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
    Ok(Cli { command, input, output, project, silent_empty_main, opts, program_args })
}

type Failure = (Diagnostic, Option<SourceMap>);

/// The entry file and the project it belongs to (if any).
fn locate(input: Option<&Path>, explicit: Option<&Path>) -> Result<(PathBuf, Option<project::Project>), Diagnostic> {
    if let Some(p) = explicit {
        let proj = project::explicit(p)?;
        let entry = input.map_or_else(|| proj.entry.clone(), Path::to_path_buf);
        return Ok((entry, Some(proj)));
    }
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

/// The `// dep:` lines of a loose script, and the lock file for them (`script.aion.lock`).
/// A script inside a project keeps its dependencies in `aion.project`; its lines are ignored.
fn inline_dependencies(
    entry: &Path,
    project: Option<&project::Project>,
) -> Result<(Vec<project::Dependency>, Option<PathBuf>), Diagnostic> {
    let Ok(text) = std::fs::read_to_string(entry) else { return Ok((Vec::new(), None)) };
    let dir = entry.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
    let deps = project::inline_deps(&text, dir, entry)?;
    if deps.is_empty() {
        return Ok((deps, None));
    }
    if project.is_some() {
        eprintln!("warning: the `// dep:` lines in {} are ignored: this script belongs to a project", entry.display());
        eprintln!("hint: put them in aion.project (`aion add <source>`)");
        return Ok((Vec::new(), None));
    }
    let mut lock = entry.as_os_str().to_owned();
    lock.push(".lock");
    Ok((deps, Some(PathBuf::from(lock))))
}

/// Generated LLVM IR plus the C sources and system libraries it must be linked with.
struct Compiled {
    ir: String,
    c_sources: Vec<(String, String)>,
    libs: Vec<String>,
    vendors: Vec<String>,
}

/// Everything up to (but not including) invoking the C toolchain.
fn compile(
    map: &mut SourceMap,
    entry: &Path,
    project: Option<&project::Project>,
    trace: bool,
    silent_empty_main: bool,
) -> Result<Compiled, Failure> {
    let dir = entry.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
    let (inline, lock) = inline_dependencies(entry, project).map_err(|d| (d, None))?;
    let packages = project::resolve(project, dir, &inline, lock.as_deref()).map_err(|d| (d, None))?;
    let result = (|| {
        let mut loaded = loader::load(map, entry, packages)?;
        reflect::instrument(&mut loaded, map)?;
        for w in &loaded.warnings {
            eprint!("{}", w.render(map));
        }
        let program = sema::analyze(&loaded, map, trace)?;
        if program.nothing_to_run && !silent_empty_main {
            let nothing = Diagnostic { warning: true, ..Diagnostic::general("no procedures to execute") }
                .hint("write statements in the file or define `fn main() { … }`; `--silent-empty-main` hides this warning");
            eprint!("{}", nothing.render(map));
        }
        Ok::<_, Diagnostic>(Compiled { ir: codegen::emit(&program), c_sources: loaded.c_sources, libs: loaded.libs, vendors: loaded.vendors })
    })();
    result.map_err(|d| (d, None))
}

fn exe_name(stem: &str) -> String {
    format!("{stem}{}", toolchain::exe_suffix())
}

/// Start a built program, through `AION_RUNNER` (`wine`, say) when that is set.
fn program_command(exe: &Path) -> Command {
    match std::env::var("AION_RUNNER") {
        Ok(runner) if !runner.is_empty() => {
            let mut cmd = Command::new(runner);
            cmd.arg(exe);
            cmd
        }
        _ => Command::new(exe),
    }
}

/// Build (or reuse) the program for `ir`: the executable, and whether it is a temporary one to delete after use.
fn build_program(program: &Compiled, opts: &toolchain::Options) -> Result<(PathBuf, bool), Diagnostic> {
    let Compiled { ir, c_sources, libs, vendors } = program;
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
        for lib in libs {
            parts.push(lib.as_bytes());
        }
        // A bundled library is part of the program: a new compiler may carry a new version of it.
        let vendor_sizes: Vec<String> = vendors.iter().map(|v| toolchain::vendor_fingerprint(v)).collect();
        for v in &vendor_sizes {
            parts.push(v.as_bytes());
        }
        let bin_dir = cache::dir().join("bin");
        let exe = bin_dir.join(exe_name(&format!("{:016x}", cache::hash(&parts))));
        if !exe.is_file() {
            std::fs::create_dir_all(&bin_dir)
                .map_err(|e| Diagnostic::general(format!("cannot create {}: {e}", bin_dir.display())))?;
            // Build beside the final name, then rename, so a crash never leaves a half-written binary.
            let partial = bin_dir.join(exe_name(&format!("partial-{}", std::process::id())));
            toolchain::build(ir, c_sources, libs, vendors, &partial, opts)?;
            std::fs::rename(&partial, &exe)
                .map_err(|e| Diagnostic::general(format!("cannot store the build: {e}")))?;
            cache::trim(&bin_dir, 128);
        }
        exe
    } else {
        let exe = std::env::temp_dir().join(exe_name(&format!("aion-run-{}", std::process::id())));
        toolchain::build(ir, c_sources, libs, vendors, &exe, opts)?;
        exe
    };
    Ok((exe, !cached))
}

/// Build (or reuse) the program for `ir` and run it.
fn run_program(
    program: &Compiled,
    opts: &toolchain::Options,
    program_args: &[String],
) -> Result<ExitCode, Diagnostic> {
    let (exe, temporary) = build_program(program, opts)?;
    let status = program_command(&exe).args(program_args).status();
    if temporary {
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
    manage::ignore_packages(dir, manage::DEFAULT_PACKAGES).map_err(Diagnostic::general)?;
    eprintln!("created {name}/  — try: cd {name} && aion");
    Ok(())
}

/// `aion add / remove / update / tree`.
fn manage_command(command: &str, args: &[String]) -> ExitCode {
    let result = (|| {
        let opts = manage::parse_opts(args)?;
        let cwd = std::env::current_dir().map_err(|e| e.to_string())?;
        let home = std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")).map(PathBuf::from);
        let mut prompt = manage::Terminal { yes: opts.yes };
        match command {
            "add" => manage::add(&opts, &cwd, home.as_deref(), &mut prompt),
            "install" => manage::install(&opts, &cwd, home.as_deref(), &mut prompt),
            "remove" => manage::remove(&opts, &cwd, home.as_deref(), &mut prompt),
            "update" => manage::update(&opts, &cwd, home.as_deref(), &mut prompt),
            _ => manage::tree(&opts, &cwd, home.as_deref(), &mut prompt),
        }
    })();
    match result {
        Ok(message) => {
            if command == "tree" { print!("{message}") } else { eprintln!("{message}") }
            ExitCode::SUCCESS
        }
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::FAILURE
        }
    }
}

/// `aion warmup`: unpack the C compiler that comes with aion and build what a program needs first (its C library and
/// the bundled libraries), so that the first real program does not wait for it.  The Windows installer runs this.
fn warmup() -> ExitCode {
    let dir = std::env::temp_dir().join(format!("aion-warmup-{}", std::process::id()));
    let file = dir.join("warmup.aion");
    let source = "import sqlite\nimport zlib\nimport unicode\nprintln(\"aion is ready\")\n";
    let done = std::fs::create_dir_all(&dir).and_then(|()| std::fs::write(&file, source)).is_ok()
        && std::env::current_exe().and_then(|exe| Command::new(exe).arg(&file).status()).is_ok_and(|s| s.success());
    let _ = std::fs::remove_dir_all(&dir);
    if done { ExitCode::SUCCESS } else { ExitCode::FAILURE }
}

fn run() -> Result<ExitCode, Failure> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    // Just `aion`, with no project to run: show the banner, instead of an error.
    if args.is_empty() && project::find(Path::new(".")).is_none() {
        banner::show(banner::Stream::Stdout);
        return Ok(ExitCode::SUCCESS);
    }
    match args.first().map(String::as_str) {
        Some("warmup") => return Ok(warmup()),
        Some("test") => return Ok(testing::test(&args[1..])),
        Some("fmt") => return Ok(testing::fmt(&args[1..])),
        Some("doc") => return Ok(doc::run(&args[1..])),
        _ => {}
    }
    if let Some(command) = args.first().map(String::as_str).filter(|c| ["add", "install", "remove", "update", "tree"].contains(c)) {
        return Ok(manage_command(command, &args[1..]));
    }
    let cli = match parse_args(&args) {
        Ok(c) => c,
        Err(msg) => {
            let is_error = !(msg.starts_with("Aion compiler") || msg.starts_with("aion "));
            if msg.starts_with("error: unknown command") {
                banner::show(banner::Stream::Stderr);
            } else if is_error {
                eprintln!("{msg}");
            } else {
                // The help and the version are what was asked for, so they go to the standard output (`aion --help | more`).
                println!("{msg}");
            }
            return Ok(if is_error { ExitCode::from(2) } else { ExitCode::SUCCESS });
        }
    };
    let general = |d: Diagnostic| (d, None);

    match cli.command {
        Subcommand::New => {
            new_project(cli.input.as_deref().unwrap().to_str().unwrap()).map_err(general)?;
            return Ok(ExitCode::SUCCESS);
        }
        Subcommand::Repl => return Ok(repl::run(&cli.opts)),
        Subcommand::Clean => {
            let dir = cache::dir();
            let _ = std::fs::remove_dir_all(&dir);
            eprintln!("removed {}", dir.display());
            return Ok(ExitCode::SUCCESS);
        }
        Subcommand::Fetch => {
            let (entry, proj) = locate(cli.input.as_deref(), cli.project.as_deref()).map_err(general)?;
            let packages = match &proj {
                Some(p) => project::resolve(Some(p), &p.root, &[], None),
                None if cli.input.is_some() => {
                    let (inline, lock) = inline_dependencies(&entry, None).map_err(general)?;
                    let dir = entry.parent().filter(|d| !d.as_os_str().is_empty()).unwrap_or(Path::new("."));
                    project::resolve(None, dir, &inline, lock.as_deref())
                }
                None => Err(Diagnostic::general("no `aion.project` found")),
            }
            .map_err(general)?;
            eprintln!("{} dependenc{} ready", packages.len() - 1, if packages.len() == 2 { "y" } else { "ies" });
            return Ok(ExitCode::SUCCESS);
        }
        _ => {}
    }

    let (entry, proj) = locate(cli.input.as_deref(), cli.project.as_deref()).map_err(general)?;
    let mut map = SourceMap::default();
    let program = compile(&mut map, &entry, proj.as_ref(), cli.opts.trace, cli.silent_empty_main).map_err(|(d, _)| (d, Some(map)))?;

    match cli.command {
        Subcommand::Check => {
            eprintln!("ok");
            Ok(ExitCode::SUCCESS)
        }
        Subcommand::EmitIr => {
            print!("{}", program.ir);
            Ok(ExitCode::SUCCESS)
        }
        Subcommand::Build => {
            let stem = proj.as_ref().filter(|_| cli.input.is_none()).map(|p| p.name.clone()).unwrap_or_else(|| {
                entry.file_stem().map_or("a".into(), |s| s.to_string_lossy().into_owned())
            });
            let output = cli.output.clone().unwrap_or_else(|| PathBuf::from(exe_name(&stem)));
            toolchain::build(&program.ir, &program.c_sources, &program.libs, &program.vendors, &output, &cli.opts).map_err(general)?;
            eprintln!("built {}", output.display());
            Ok(ExitCode::SUCCESS)
        }
        _ => run_program(&program, &cli.opts, &cli.program_args).map_err(general),
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

    #[test]
    fn only_a_name_ending_in_aion_is_a_file() {
        let err = |args: &[&str]| parse(args).err().unwrap();
        assert_eq!(err(&["frobnicate"]), "error: unknown command `frobnicate`; type `aion help` for help");
        assert!(matches!(parse(&["repl"]).unwrap().command, Subcommand::Repl));
        assert!(err(&["run", "script"]).contains("unknown command `script`"));
        assert!(err(&["help"]).starts_with("Aion compiler"));
        assert_eq!(parse(&["new", "myapp"]).unwrap().input.unwrap(), PathBuf::from("myapp"));
        assert_eq!(parse(&["dir/x.aion"]).unwrap().input.unwrap(), PathBuf::from("dir/x.aion"));
        // the program's own arguments are not looked at
        assert_eq!(parse(&["x.aion", "repl"]).unwrap().program_args, ["repl"]);
    }

    #[test]
    fn silent_empty_main_is_an_option() {
        assert!(!parse(&["script.aion"]).unwrap().silent_empty_main);
        assert!(parse(&["--silent-empty-main", "script.aion"]).unwrap().silent_empty_main);
    }
}
