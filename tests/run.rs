//! End-to-end tests: compile and run every program under `tests/`.
//!
//! * `tests/programs/*.aion` are run; stdout must equal the sibling `.out`
//!   file.  Optional header comments: `// exit: N` (expected exit code,
//!   default 0), `// stderr: text` (stderr must contain `text`) and
//!   `// stress: skip` (don't repeat under `AION_GC_STRESS=1`).
//!   Every program is also run with a collection on every allocation and must
//!   produce identical output — that catches values the compiler failed to
//!   keep alive.
//! * `tests/errors/*.aion` must fail to compile with the diagnostic named by
//!   their `// error: text` header.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// Where compiled programs and cloned dependencies go during tests.
fn test_cache() -> PathBuf {
    Path::new(env!("CARGO_TARGET_TMPDIR")).join("aion-cache")
}

fn aion(args: &[&str], file: &Path, stress: bool) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aion"));
    cmd.args(args).arg(file).env("AION_CACHE", test_cache());
    if stress {
        cmd.env("AION_GC_STRESS", "1");
    }
    cmd.output().expect("failed to launch the aion binary")
}

fn sources(dir: &str) -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join(dir);
    let mut files: Vec<PathBuf> = std::fs::read_dir(root)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "aion"))
        .collect();
    files.sort();
    files
}

/// Value of a `// key: value` header line.
fn header(source: &str, key: &str) -> Option<String> {
    source.lines().find_map(|l| l.strip_prefix(&format!("// {key}:")).map(|v| v.trim().to_string()))
}

#[test]
fn programs_produce_expected_output() {
    let mut failures = Vec::new();
    for file in sources("programs") {
        let name = file.file_stem().unwrap().to_string_lossy().into_owned();
        let source = std::fs::read_to_string(&file).unwrap();
        let expected_code: i32 = header(&source, "exit").map_or(0, |v| v.parse().unwrap());
        let expected_out = std::fs::read_to_string(file.with_extension("out"))
            .unwrap_or_else(|_| panic!("missing tests/programs/{name}.out"));

        let mut modes = vec![false];
        if header(&source, "stress").as_deref() != Some("skip") {
            modes.push(true);
        }
        for stress in modes {
            let mode = if stress { "gc-stress" } else { "normal" };
            let out = aion(&["run"], &file, stress);
            let stdout = String::from_utf8_lossy(&out.stdout);
            let stderr = String::from_utf8_lossy(&out.stderr);
            if out.status.code() != Some(expected_code) {
                failures.push(format!("{name} [{mode}]: exit {:?}, expected {expected_code}\n{stderr}", out.status.code()));
            } else if stdout != expected_out {
                failures.push(format!("{name} [{mode}]: stdout differs\n--- expected\n{expected_out}--- actual\n{stdout}"));
            } else if let Some(want) = header(&source, "stderr")
                && !stderr.contains(&want)
            {
                failures.push(format!("{name} [{mode}]: stderr lacks `{want}`:\n{stderr}"));
            }
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

#[test]
fn invalid_programs_are_rejected_with_the_right_message() {
    let mut failures = Vec::new();
    for file in sources("errors") {
        let name = file.file_stem().unwrap().to_string_lossy().into_owned();
        let source = std::fs::read_to_string(&file).unwrap();
        let want = header(&source, "error").unwrap_or_else(|| panic!("tests/errors/{name}.aion has no `// error:` header"));

        // `emit-ir` stops after analysis, so these tests don't need clang.
        let out = aion(&["emit-ir"], &file, false);
        let stderr = String::from_utf8_lossy(&out.stderr);
        if out.status.success() {
            failures.push(format!("{name}: compiled, expected error `{want}`"));
        } else if !stderr.contains(&want) {
            failures.push(format!("{name}: expected `{want}` in:\n{stderr}"));
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

#[test]
fn diagnostics_point_at_the_source() {
    let file = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/errors/type_mismatch.aion");
    let out = aion(&["emit-ir"], &file, false);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("type_mismatch.aion:3:15"), "{stderr}");
    assert!(stderr.contains("x: Int := \"hello\""), "{stderr}");
    assert!(stderr.contains("^^^^^^^"), "{stderr}");
}

#[test]
fn circular_imports_are_reported() {
    let dir = std::env::temp_dir().join(format!("aion-cycle-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("a.aion"), "import b;\nfn fa() {}\n").unwrap();
    std::fs::write(dir.join("b.aion"), "import a;\nfn fb() {}\n").unwrap();
    std::fs::write(dir.join("main.aion"), "import a;\nfn main() {}\n").unwrap();
    let out = aion(&["emit-ir"], &dir.join("main.aion"), false);
    let _ = std::fs::remove_dir_all(&dir);
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("circular import"));
}

#[test]
fn hello_world_via_build_and_run() {
    // Exercises `aion build -o` and then runs the produced executable.
    let dir = std::env::temp_dir().join(format!("aion-build-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let src = dir.join("hello.aion");
    std::fs::write(&src, "fn main() { println(\"hi from a built binary\") }\n").unwrap();
    let exe = dir.join("hello_bin");
    let build = Command::new(env!("CARGO_BIN_EXE_aion"))
        .args(["build"])
        .arg(&src)
        .arg("-o")
        .arg(&exe)
        .output()
        .unwrap();
    assert!(build.status.success(), "{}", String::from_utf8_lossy(&build.stderr));
    let run = Command::new(&exe).output().unwrap();
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(String::from_utf8_lossy(&run.stdout), "hi from a built binary\n");
}

#[test]
fn a_panic_prints_the_calls_that_led_to_it_unless_told_not_to() {
    let root = scratch("trace");
    write(&root.join("boom.aion"), "fn inner(n: Int) = [1][n]\nfn outer() = inner(5) + 1\nprintln(outer())\n");
    let out = aion_in(&root, &["boom.aion"]);
    let text = stderr(&out);
    assert_eq!(out.status.code(), Some(1), "{text}");
    assert!(text.contains("panic: index 5 is out of bounds for size 1"), "{text}");
    assert!(text.contains("at inner (boom.aion:1)"), "{text}");
    assert!(text.contains("at outer (boom.aion:2)"), "{text}");
    assert!(text.contains("at <top level> (boom.aion:3)"), "{text}");

    let quiet = stderr(&aion_in(&root, &["--no-trace", "boom.aion"]));
    assert!(quiet.contains("panic: index 5"), "{quiet}");
    assert!(!quiet.contains("    at "), "{quiet}");
}

#[test]
fn folders_are_packages_and_imports_can_start_at_the_root_or_the_current_folder() {
    let root = scratch("folders");
    write(&root.join("util.aion"), "fn double(n: Int) = n * 2\n");
    write(&root.join("shapes/lib.aion"), "import .circle\nimport @util\nfn name() = \"shapes\"\nfn big(n: Int) = util.double(circle.area(n))\n");
    write(&root.join("shapes/circle.aion"), "fn area(r: Int) = r * r * 3\n");
    write(&root.join("shapes/deep/x.aion"), "from ..circle import area\nfrom @util import double\nfn go() = double(area(1))\n");
    write(&root.join("only/only.aion"), "fn hi() = \"only\"\n");
    write(
        &root.join("main.aion"),
        "import shapes\nimport shapes.circle\nimport shapes.deep.x as dx\nimport only\n\
         println(shapes.name(), shapes.big(2), circle.area(2), dx.go(), only.hi())\n",
    );
    let out = aion_in(&root, &["main.aion"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out), "shapes 24 12 6 only\n");

    // a relative import cannot climb out of the package
    write(&root.join("pkg/aion.project"), "name = \"pkg\"\n");
    write(&root.join("pkg/main.aion"), "import ...util\n");
    let out = aion_in(&root.join("pkg"), &[]);
    assert!(stderr(&out).contains("cannot leave the package"), "{}", stderr(&out));
    write(&root.join("nope.aion"), "import @missing.thing\n");
    let out = aion_in(&root, &["nope.aion"]);
    assert!(stderr(&out).contains("cannot find module `missing.thing`"), "{}", stderr(&out));
    assert!(stderr(&out).contains("the package root"), "{}", stderr(&out));
}

#[test]
fn names_starting_with_an_underscore_are_private_to_their_file() {
    let root = scratch("private");
    write(
        &root.join("lib.aion"),
        "fn _hidden() = 1\nfn shown() = _hidden() + 1\ntype _Node(x: Int)\n_count := 5\nfn count() = _count\n",
    );
    let run = |body: &str| {
        write(&root.join("t.aion"), &format!("import lib\n{body}\n"));
        aion_in(&root, &["t.aion"])
    };
    let ok = run("println(lib.shown(), lib.count())");
    assert_eq!(stdout(&ok), "2 5\n", "{}", stderr(&ok));
    for body in ["println(lib._hidden())", "x := lib._Node(1)", "println(lib._count)", "lib._count = 3"] {
        let out = run(body);
        assert!(stderr(&out).contains("is private to module `lib`"), "{body}: {}", stderr(&out));
    }
    write(&root.join("t.aion"), "from lib import _hidden\n");
    assert!(stderr(&aion_in(&root, &["t.aion"])).contains("is private to module `lib`"));
}

#[test]
fn shadowing_the_standard_library_warns_and_missing_modules_name_the_project() {
    let root = scratch("shadow");
    write(&root.join("math.aion"), "fn sqrt(x: Float) = 42.0\n");
    write(&root.join("main.aion"), "import math\nprintln(math.sqrt(1.0))\n");
    let out = aion_in(&root, &["main.aion"]);
    assert_eq!(stdout(&out), "42\n", "{}", stderr(&out));
    assert!(stderr(&out).contains("warning: `math.aion` shadows the standard library module `math`"), "{}", stderr(&out));

    write(&root.join("n.aion"), "import nothing\n");
    assert!(stderr(&aion_in(&root, &["n.aion"])).contains("no aion.project (running as a single script)"));
    write(&root.join("p/aion.project"), "name = \"p\"\n");
    write(&root.join("p/main.aion"), "import nothing\n");
    let text = stderr(&aion_in(&root.join("p"), &[]));
    assert!(text.contains("p/aion.project") || text.contains("project:"), "{text}");
}

// ── projects & dependencies ─────────────────────────────────────────

/// A scratch directory unique to one test.
fn scratch(name: &str) -> PathBuf {
    let dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join(format!("scratch-{name}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn write(path: &Path, text: &str) {
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, text).unwrap();
}

/// Run `aion <args>` with `dir` as the working directory.
fn aion_in(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_aion"))
        .args(args)
        .current_dir(dir)
        .env("AION_CACHE", test_cache())
        .output()
        .unwrap()
}

fn stdout(o: &Output) -> String {
    String::from_utf8_lossy(&o.stdout).into_owned()
}

fn stderr(o: &Output) -> String {
    String::from_utf8_lossy(&o.stderr).into_owned()
}

#[test]
fn a_project_runs_its_entry_and_resolves_nested_path_dependencies() {
    let root = scratch("deps");
    // text: no dependencies, default `lib.aion`, plus a nested module.
    write(&root.join("text/aion.project"), "name = \"text\"\n");
    write(&root.join("text/lib.aion"), "import strings\nfn shout(s: String) = strings.upper(s) + \"!\"\n");
    write(&root.join("text/extra/pad.aion"), "fn pad(s: String) = \"[$s]\"\n");
    // greeter: custom `lib`, depends on text.
    write(&root.join("greeter/aion.project"), "name = \"greeter\"\nlib = \"greeter.aion\"\n[dependencies]\ntext = { path = \"../text\" }\n");
    write(&root.join("greeter/greeter.aion"), "import text\nfn hello(who: String) = text.shout(\"hello, \" + who)\n");
    // app: depends on both; entry is main.aion.
    write(
        &root.join("app/aion.project"),
        "name = \"app\"\n[dependencies]\ngreeter = { path = \"../greeter\" }\ntext = { path = \"../text\" }\n",
    );
    write(
        &root.join("app/main.aion"),
        "import greeter\nimport text.extra.pad as p\nfrom text import shout\nprintln(greeter.hello(\"world\"))\nprintln(p.pad(\"x\"), shout(\"y\"))\n",
    );

    let out = aion_in(&root.join("app"), &[]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out), "HELLO, WORLD!\n[x] Y!\n");

    // A package only sees the dependencies *it* declares.
    write(&root.join("greeter/bad.aion"), "import app_only\n");
    let out = aion_in(&root.join("greeter"), &["emit-ir", "bad.aion"]);
    assert!(!out.status.success());
    assert!(stderr(&out).contains("cannot find module `app_only`"), "{}", stderr(&out));
}

#[test]
fn aion_new_creates_a_runnable_project() {
    let root = scratch("new");
    let out = aion_in(&root, &["new", "hello_app"]);
    assert!(out.status.success(), "{}", stderr(&out));
    let run = aion_in(&root.join("hello_app"), &[]);
    assert_eq!(stdout(&run), "Hello from hello_app!\n", "{}", stderr(&run));
    // Refuses to overwrite.
    assert!(!aion_in(&root, &["new", "hello_app"]).status.success());
}

#[test]
fn project_files_are_validated() {
    let root = scratch("badproject");
    let check = |toml: &str, expect: &str| {
        write(&root.join("aion.project"), toml);
        write(&root.join("main.aion"), "println(1)\n");
        let out = aion_in(&root, &["emit-ir"]);
        assert!(!out.status.success(), "{toml}");
        assert!(stderr(&out).contains(expect), "wanted `{expect}` in:\n{}", stderr(&out));
    };
    check("version = \"1\"\n", "missing `name`");
    check("name = \"a\"\nbogus = \"x\"\n", "unknown key `bogus`");
    check("name = \"a\"\n[dependencies]\nd = { }\n", "needs exactly one of `path` or `git`");
    check("name = \"a\"\n[dependencies]\nd = { git = \"u\", tag = \"a\", rev = \"b\" }\n", "only one of tag, branch, rev");
    check("name = \"a\"\n[dependencies]\nd = { path = \"../nowhere\" }\n", "dependency `d`");
    check("name = \"a\"\nthis is not toml\n", "aion.project:2");
}

#[test]
fn git_dependencies_are_pinned_by_the_lock_file() {
    if Command::new("git").arg("--version").output().is_err() {
        eprintln!("skipping: git is not installed");
        return;
    }
    let root = scratch("git");
    let repo = root.join("lib_repo");
    std::fs::create_dir_all(&repo).unwrap();
    let git = |dir: &Path, args: &[&str]| {
        let ok = Command::new("git")
            .args(["-c", "user.name=t", "-c", "user.email=t@t", "-c", "init.defaultBranch=main"])
            .args(args)
            .current_dir(dir)
            .output()
            .unwrap()
            .status
            .success();
        assert!(ok, "git {args:?} failed");
    };
    git(&repo, &["init", "-q"]);
    write(&repo.join("lib.aion"), "fn version() = \"1.0\"\n");
    git(&repo, &["add", "-A"]);
    git(&repo, &["commit", "-qm", "one"]);
    git(&repo, &["tag", "v1"]);

    let app = root.join("app");
    write(
        &app.join("aion.project"),
        &format!("name = \"app\"\n[dependencies]\nlib = {{ git = \"{}\", tag = \"v1\" }}\n", repo.display()),
    );
    write(&app.join("main.aion"), "import lib\nprintln(lib.version())\n");

    // Fresh cache so the first run really clones.
    let cache = root.join("cache");
    let run = |dir: &Path| {
        Command::new(env!("CARGO_BIN_EXE_aion")).current_dir(dir).env("AION_CACHE", &cache).output().unwrap()
    };
    let first = run(&app);
    assert_eq!(stdout(&first), "1.0\n", "{}", stderr(&first));
    let lock = std::fs::read_to_string(app.join("aion.lock")).unwrap();
    assert!(lock.contains("#tag:v1\" = \""), "{lock}");

    // Upstream moves the tag and the cached clone disappears: the lock still wins.
    write(&repo.join("lib.aion"), "fn version() = \"2.0\"\n");
    git(&repo, &["commit", "-qam", "two"]);
    git(&repo, &["tag", "-f", "v1"]);
    let _ = std::fs::remove_dir_all(cache.join("git"));
    let second = run(&app);
    assert_eq!(stdout(&second), "1.0\n", "{}", stderr(&second));
}

#[test]
fn unchanged_programs_are_not_rebuilt() {
    let root = scratch("cache");
    write(&root.join("a.aion"), "println(\"cached\")\n");
    let cache = root.join("cache");
    let run = || {
        Command::new(env!("CARGO_BIN_EXE_aion")).arg("a.aion").current_dir(&root).env("AION_CACHE", &cache).output().unwrap()
    };
    let count = || std::fs::read_dir(cache.join("bin")).map_or(0, |d| d.count());
    assert_eq!(stdout(&run()), "cached\n");
    assert_eq!(count(), 1);
    let before = std::fs::metadata(std::fs::read_dir(cache.join("bin")).unwrap().next().unwrap().unwrap().path())
        .unwrap()
        .modified()
        .unwrap();
    assert_eq!(stdout(&run()), "cached\n");
    assert_eq!(count(), 1, "the second run reuses the first binary");
    let after = std::fs::metadata(std::fs::read_dir(cache.join("bin")).unwrap().next().unwrap().unwrap().path())
        .unwrap()
        .modified()
        .unwrap();
    assert_eq!(before, after, "the cached binary was not rewritten");
    // A change in the source produces a different binary.
    write(&root.join("a.aion"), "println(\"changed\")\n");
    assert_eq!(stdout(&run()), "changed\n");
    assert_eq!(count(), 2);
}

#[test]
fn a_script_with_a_shebang_line_runs() {
    let root = scratch("shebang");
    write(&root.join("s.aion"), "#!/usr/bin/env aion\nx := 20\nprintln(\"answer\", x + 22)\n");
    assert_eq!(stdout(&aion_in(&root, &["s.aion"])), "answer 42\n");
}

/// Run `aion <args>` in `dir` with `input` on standard input.
fn aion_with_input(dir: &Path, args: &[&str], input: &str) -> Output {
    use std::io::Write;
    use std::process::Stdio;
    let mut child = Command::new(env!("CARGO_BIN_EXE_aion"))
        .args(args)
        .current_dir(dir)
        .env("AION_CACHE", test_cache())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(input.as_bytes()).unwrap();
    child.wait_with_output().unwrap()
}

#[test]
fn programs_can_read_standard_input() {
    let root = scratch("stdin");
    write(
        &root.join("lines.aion"),
        "print(\"name? \")\nname := readLine() ?: \"nobody\"\nprintln(\"hello\", name)\n\
         total := 0\nline := readLine()\nwhile line != null {\n    total += line.toIntOr(0)\n    line = readLine()\n}\n\
         println(\"sum\", total, readLine())\n",
    );
    let out = aion_with_input(&root, &["lines.aion"], "Ada\n1\n2\r\nx\n39");
    assert_eq!(stdout(&out), "name? hello Ada\nsum 42 null\n", "{}", stderr(&out));
    // an empty input is not an error
    let out = aion_with_input(&root, &["lines.aion"], "");
    assert_eq!(stdout(&out), "name? hello nobody\nsum 0 null\n", "{}", stderr(&out));

    write(&root.join("all.aion"), "text := readText()\nprintln(text.length(), text.split(\"\\n\").size())\n");
    let out = aion_with_input(&root, &["all.aion"], "a\nb\nc");
    assert_eq!(stdout(&out), "5 3\n", "{}", stderr(&out));
}

#[test]
fn arguments_after_the_script_reach_the_program() {
    let root = scratch("args");
    write(
        &root.join("a.aion"),
        "xs := args()\nprintln(xs.size(), xs)\nfor a in xs { println(\"arg:\", a) }\n",
    );
    let out = aion_in(&root, &["a.aion", "one", "two words", "-x"]);
    assert_eq!(stdout(&out), "3 [one, two words, -x]\narg: one\narg: two words\narg: -x\n", "{}", stderr(&out));
    // no arguments: an empty list; `--` passes things that look like options
    assert_eq!(stdout(&aion_in(&root, &["a.aion"])).lines().next().unwrap(), "0 []");
    let out = aion_in(&root, &["a.aion", "--", "-O0"]);
    assert_eq!(stdout(&out).lines().next().unwrap(), "1 [-O0]");
}

#[test]
fn projects_can_link_system_libraries() {
    let root = scratch("link");
    // zlibCompileFlags() lives in libz, which is not linked unless the project asks for it
    let source = "extern fn zlibCompileFlags() -> Int\nprintln(zlibCompileFlags() > 0)\n";
    write(&root.join("main.aion"), source);
    write(&root.join("aion.project"), "name = \"z\"\nlink = [\"z\"]\n");
    let out = aion_in(&root, &[]);
    assert_eq!(stdout(&out), "true\n", "{}", stderr(&out));

    write(&root.join("aion.project"), "name = \"z\"\n");
    let out = aion_in(&root, &[]);
    assert!(!out.status.success(), "without `link` the symbol is undefined");

    write(&root.join("aion.project"), "name = \"z\"\nlink = [\"-lz\"]\n");
    let out = aion_in(&root, &[]);
    assert!(stderr(&out).contains("is not a library name"), "{}", stderr(&out));
    write(&root.join("aion.project"), "name = \"z\"\nlink = \"z\"\n");
    let out = aion_in(&root, &[]);
    assert!(stderr(&out).contains("`link` must be a list"), "{}", stderr(&out));
}

// ── documentation ───────────────────────────────────────────────────

/// `(first line, info string, body)` of every fenced code block in `text`.
fn code_blocks(text: &str) -> Vec<(usize, String, String)> {
    let mut blocks = Vec::new();
    let mut open: Option<(usize, String, String)> = None;
    for (i, line) in text.lines().enumerate() {
        let trimmed = line.trim_start();
        match (&mut open, trimmed.strip_prefix("```")) {
            (None, Some(info)) => open = Some((i + 2, info.trim().to_string(), String::new())),
            (Some(_), Some(_)) => blocks.push(open.take().unwrap()),
            (Some((_, _, body)), None) => {
                body.push_str(line);
                body.push('\n');
            }
            (None, None) => {}
        }
    }
    blocks
}

#[test]
fn code_in_the_documentation_works() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = vec![root.join("README.md")];
    let mut docs: Vec<PathBuf> = std::fs::read_dir(root.join("docs"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "md"))
        .collect();
    docs.sort();
    files.extend(docs);

    let dir = scratch("docs");
    let (mut failures, mut checked) = (Vec::new(), 0);
    for file in files {
        let text = std::fs::read_to_string(&file).unwrap();
        for (line, info, body) in code_blocks(&text) {
            // ```aion runs; ```aion no_run only compiles; anything else is not Aion.
            let mode = match info.as_str() {
                "aion" => "run",
                "aion no_run" => "emit-ir",
                _ => continue,
            };
            checked += 1;
            let src = dir.join(format!("block_{checked}.aion"));
            write(&src, &body);
            let out = aion_in(&dir, &[mode, src.to_str().unwrap()]);
            if !out.status.success() {
                failures.push(format!(
                    "{}:{line}\n{body}\n{}",
                    file.strip_prefix(root).unwrap().display(),
                    stderr(&out)
                ));
            }
        }
    }
    assert!(checked > 10, "expected to find documentation examples, found {checked}");
    assert!(failures.is_empty(), "\n{}", failures.join("\n---\n"));
}

/// The https client talks TLS to a real server (`openssl s_server`), checking certificates unless told not to.
#[test]
fn https_requests_verify_the_servers_certificate() {
    let have = |program: &str| Command::new(program).arg("version").output().is_ok_and(|o| o.status.success());
    if !have("openssl") {
        eprintln!("skipped: the openssl command is not installed");
        return;
    }
    let dir = scratch("https");
    let (cert, key) = (dir.join("cert.pem"), dir.join("key.pem"));
    let made = Command::new("openssl")
        .args(["req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1", "-subj", "/CN=localhost"])
        .args(["-addext", "subjectAltName=DNS:localhost"])
        .arg("-keyout")
        .arg(&key)
        .arg("-out")
        .arg(&cert)
        .output()
        .unwrap();
    assert!(made.status.success(), "{}", String::from_utf8_lossy(&made.stderr));

    let port = std::net::TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port();
    let mut server = Command::new("openssl")
        .args(["s_server", "-quiet", "-www", "-accept", &port.to_string()])
        .arg("-cert")
        .arg(&cert)
        .arg("-key")
        .arg(&key)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .stdin(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    std::thread::sleep(std::time::Duration::from_millis(500));

    let program = format!(
        r#"import http
trusted := http.get("https://localhost:{port}/", null, http.Options(caFile: "{cert}"))
println(trusted != null, trusted?.status)
untrusted := http.get("https://localhost:{port}/", null, http.Options(useProxy: false))
println(untrusted, http.lastError().contains("certificate"))
unchecked := http.get("https://localhost:{port}/", null, http.Options(verify: false))
println(unchecked?.status, unchecked?.text()?.contains("s_server"))
wrong := http.get("https://127.0.0.1:{port}/", null, http.Options(caFile: "{cert}"))
println(wrong, http.lastError().contains("certificate"))
"#,
        cert = cert.display()
    );
    write(&dir.join("main.aion"), &program);
    let out = aion_in(&dir, &["main.aion"]);
    let _ = server.kill();
    let _ = server.wait();
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        stdout(&out),
        "true 200\nnull true\n200 true\nnull true\n",
        "stderr: {}",
        stderr(&out)
    );
}

/// `term.readKey` names the keys arriving on standard input, including escape sequences.
#[test]
fn keys_are_read_from_standard_input() {
    let dir = scratch("keys");
    write(
        &dir.join("main.aion"),
        r#"import term
keys := List<String>()
while true {
    key := term.readKey(1000)
    if key == null { break }
    keys.add(key)
}
println(keys.join(" "))
"#,
    );
    let input = "ab\r\t \x7f\x03\x1b[A\x1b[B\x1b[C\x1b[D\x1b[H\x1b[F\x1b[3~\x1b[5~\x1b[6~é中\x01z\x1b";
    let out = aion_with_input(&dir, &["main.aion"], input);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        stdout(&out).trim_end(),
        "a b enter tab   backspace ctrl-c up down right left home end delete pageup pagedown é 中 ctrl-a z escape"
    );
}

/// A project's `c = [...]` files are compiled into the program, and so are those of its dependencies.
#[test]
fn packages_can_bundle_c_code() {
    let root = scratch("bundled-c");
    write(
        &root.join("aion.project"),
        "name = \"app\"\nc = [\"native/scale.c\"]\n\n[dependencies]\nadder = { path = \"adder\" }\n",
    );
    write(
        &root.join("native/scale.c"),
        "#include \"aion.h\"\n#include <stdio.h>\nint64_t app_scale(int64_t x) { return x * 10; }\nconst char *app_greeting(const char *name) {\n    char buf[64];\n    snprintf(buf, sizeof buf, \"hello, %s\", name);\n    return aion_string(buf);\n}\n",
    );
    write(&root.join("adder/aion.project"), "name = \"adder\"\nlib = \"lib.aion\"\nc = [\"add.c\"]\n");
    write(&root.join("adder/add.c"), "#include \"aion.h\"\nint64_t adder_add(int64_t a, int64_t b) { return a + b; }\n");
    write(&root.join("adder/lib.aion"), "extern fn add(a: Int, b: Int) -> Int = \"adder_add\"\n");
    write(
        &root.join("main.aion"),
        "import adder\nextern fn scale(x: Int) -> Int = \"app_scale\"\nextern fn greeting(name: String) -> String = \"app_greeting\"\nprintln(scale(adder.add(2, 3)), greeting(\"Ann\"))\n",
    );
    let out = aion_in(&root, &[]);
    assert_eq!(stdout(&out), "50 hello, Ann\n", "{}", stderr(&out));

    for (value, message) in [
        ("[\"/etc/x.c\"]", "must be a .c file inside the project"),
        ("[\"../x.c\"]", "must be a .c file inside the project"),
        ("[\"native/scale.h\"]", "must be a .c file inside the project"),
        ("\"native/scale.c\"", "`c` must be a list"),
        ("[\"native/missing.c\"]", "cannot read"),
    ] {
        write(&root.join("aion.project"), &format!("name = \"app\"\nc = {value}\n\n[dependencies]\nadder = {{ path = \"adder\" }}\n"));
        let out = aion_in(&root, &[]);
        assert!(!out.status.success() && stderr(&out).contains(message), "{value}: {}", stderr(&out));
    }
}

// ── examples ────────────────────────────────────────────────────────

/// The lines of a comment block such as `// stdin: text`, joined by line breaks.
fn directive(source: &str, key: &str) -> Option<String> {
    let prefix = format!("// {key}:");
    let lines: Vec<&str> = source.lines().filter_map(|l| l.strip_prefix(prefix.as_str())).map(str::trim_start).collect();
    (!lines.is_empty()).then(|| lines.join("\n"))
}

/// What an example says it prints: its trailing `// > ` lines.
fn expected_output(source: &str) -> Option<String> {
    let lines: Vec<&str> =
        source.lines().filter_map(|l| l.strip_prefix("// >").map(|rest| rest.strip_prefix(' ').unwrap_or(rest))).collect();
    (!lines.is_empty()).then(|| lines.join("\n") + "\n")
}

/// Run one example (a file, or a project folder) in its own directory and compare with what it promises.
fn check_example(path: &Path, entry: &Path, project: bool) -> Result<(), String> {
    let source = std::fs::read_to_string(entry).map_err(|e| format!("cannot read {}: {e}", entry.display()))?;
    let dir = if project { path } else { path.parent().unwrap() };
    let args: Vec<String> = directive(&source, "args").map(|a| a.split_whitespace().map(String::from).collect()).unwrap_or_default();
    let name = path.strip_prefix(env!("CARGO_MANIFEST_DIR")).unwrap_or(path).display().to_string();

    let Some(expected) = expected_output(&source) else {
        // no promised output (a server, or something too slow to run): it must at least compile
        let out = Command::new(env!("CARGO_BIN_EXE_aion"))
            .arg("emit-ir")
            .arg(entry)
            .current_dir(dir)
            .env("AION_CACHE", test_cache())
            .output()
            .unwrap();
        return if out.status.success() { Ok(()) } else { Err(format!("{name} does not compile:\n{}", stderr(&out))) };
    };

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aion"));
    if !project {
        cmd.arg(entry);
    }
    cmd.args(&args).current_dir(dir).env("AION_CACHE", test_cache());
    let input = directive(&source, "stdin").map(|s| s + "\n").unwrap_or_default();
    let out = {
        use std::io::Write;
        cmd.stdin(std::process::Stdio::piped()).stdout(std::process::Stdio::piped()).stderr(std::process::Stdio::piped());
        let mut child = cmd.spawn().unwrap();
        child.stdin.take().unwrap().write_all(input.as_bytes()).unwrap();
        child.wait_with_output().unwrap()
    };
    if !out.status.success() {
        return Err(format!("{name} exited with {:?}:\n{}", out.status.code(), stderr(&out)));
    }
    // spaces at the end of a line do not count (editors strip them from the comments)
    let tidy = |text: &str| text.lines().map(|l| l.trim_end().to_string()).collect::<Vec<_>>().join("\n");
    let actual = tidy(&stdout(&out));
    let expected = tidy(&expected);
    if actual != expected {
        let (a, e): (Vec<&str>, Vec<&str>) = (actual.lines().collect(), expected.lines().collect());
        let at = a.iter().zip(&e).position(|(x, y)| x != y).unwrap_or(a.len().min(e.len()));
        return Err(format!(
            "{name} printed something else than it promises, from line {}:\n  expected: {:?}\n  actual:   {:?}",
            at + 1,
            e.get(at),
            a.get(at)
        ));
    }
    Ok(())
}

/// Every program in `examples/` ends with the output it prints (`// > ` lines); a project folder keeps them in
/// its `main.aion`.  Running them all keeps the examples working as the language changes.
#[test]
fn examples_print_what_they_promise() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples");
    let mut failures = Vec::new();
    let mut checked = 0;
    for file in sources_in(&root) {
        checked += 1;
        if let Err(e) = check_example(&file, &file, false) {
            failures.push(e);
        }
    }
    let mut folders: Vec<PathBuf> = std::fs::read_dir(root.join("projects"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.is_dir())
        .collect();
    folders.sort();
    for folder in folders {
        checked += 1;
        let project = folder.join("aion.project").is_file();
        let main = folder.join("main.aion");
        let result = if main.is_file() {
            check_example(&folder, &main, project)
        } else {
            // a library package has no program of its own, only tests
            let tests = folder.join("tests/check.aion");
            check_example(&tests, &tests, false)
        };
        if let Err(e) = result {
            failures.push(e);
        }
    }
    assert!(checked >= 10, "expected to find examples, found {checked}");
    assert!(failures.is_empty(), "{} of {checked} examples failed:\n\n{}", failures.len(), failures.join("\n\n"));
}

fn sources_in(dir: &Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "aion"))
        .collect();
    files.sort();
    files
}
