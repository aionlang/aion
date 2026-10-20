//! End-to-end tests: compile and run every program under `tests/`.
//!
//! * `tests/programs/*.aion` are run; stdout must equal the sibling `.out`
//!   file.  Optional header comments: `// exit: N` (expected exit code,
//!   default 0), `// stderr: text` (stderr must contain `text`) and
//!   `// stress: skip` (don't repeat under `AION_GC_STRESS=1`) or `// stress: N` (collect on every Nth
//!   allocation instead of every one, for programs that allocate a lot).
//!   Every program is also run with a collection on every allocation and must
//!   produce identical output — that catches values the compiler failed to
//!   keep alive.
//! * Programs are built at `-O0` (builds are several times faster; the tests check behaviour, not speed) and
//!   run side by side on all cores.  `AION_TEST_OPT=2 cargo test` builds them optimised instead.
//! * `tests/errors/*.aion` must fail to compile with the diagnostic named by
//!   their `// error: text` header.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// Where compiled programs and cloned dependencies go during tests.
fn test_cache() -> PathBuf {
    Path::new(env!("CARGO_TARGET_TMPDIR")).join("aion-cache")
}

/// The optimisation level the test programs are built at.  Tests check what programs do, not how fast they run,
/// and `-O0` builds take a fraction of the time; `AION_TEST_OPT=2 cargo test` checks the optimised build.
fn test_opt() -> String {
    std::env::var("AION_TEST_OPT").unwrap_or_else(|_| "0".into())
}

fn aion(args: &[&str], file: &Path, stress: Option<&str>) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aion"));
    cmd.args(args).arg(file).env("AION_CACHE", test_cache()).env("AION_OPT", test_opt());
    if let Some(every) = stress {
        cmd.env("AION_GC_STRESS", every);
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

/// Run `job` for every item on all the cores, and gather the failure messages the jobs return, sorted.
/// (Each test program is compiled and run on its own, so they can go side by side.)
fn in_parallel<T: Sync>(items: &[T], job: impl Fn(&T) -> Vec<String> + Sync) -> Vec<String> {
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicUsize, Ordering};
    let next = AtomicUsize::new(0);
    let failures = Mutex::new(Vec::new());
    let threads = std::thread::available_parallelism().map_or(4, |n| n.get());
    std::thread::scope(|scope| {
        for _ in 0..threads {
            scope.spawn(|| {
                while let Some(item) = items.get(next.fetch_add(1, Ordering::SeqCst)) {
                    let found = job(item);
                    failures.lock().unwrap().extend(found);
                }
            });
        }
    });
    let mut all = failures.into_inner().unwrap();
    all.sort();
    all
}

/// Value of a `// key: value` header line.
fn header(source: &str, key: &str) -> Option<String> {
    source.lines().find_map(|l| l.strip_prefix(&format!("// {key}:")).map(|v| v.trim().to_string()))
}

#[test]
fn programs_produce_expected_output() {
    let failures = in_parallel(&sources("programs"), |file| {
        let mut failures = Vec::new();
        let file = file.clone();
        let name = file.file_stem().unwrap().to_string_lossy().into_owned();
        let source = std::fs::read_to_string(&file).unwrap();
        let expected_code: i32 = header(&source, "exit").map_or(0, |v| v.parse().unwrap());
        let expected_out = std::fs::read_to_string(file.with_extension("out"))
            .unwrap_or_else(|_| panic!("missing tests/programs/{name}.out"));

        // `// stress: skip` runs it once; `// stress: N` collects on every Nth allocation instead of every one
        let mut modes: Vec<Option<String>> = vec![None];
        match header(&source, "stress").as_deref() {
            Some("skip") => {}
            Some(every) if every.parse::<u32>().is_ok() => modes.push(Some(every.to_string())),
            _ => modes.push(Some("1".to_string())),
        }
        for stress in modes {
            let mode = if stress.is_some() { "gc-stress" } else { "normal" };
            let out = aion(&["run"], &file, stress.as_deref());
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
        failures
    });
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
        let out = aion(&["emit-ir"], &file, None);
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
    let out = aion(&["emit-ir"], &file, None);
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
    let out = aion(&["emit-ir"], &dir.join("main.aion"), None);
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

#[test]
fn the_project_search_stops_before_the_home_folder() {
    let root = scratch("home");
    let home = root.join("home");
    let script = |dir: &Path| {
        write(&dir.join("x.aion"), "import nothing\n");
        stderr(&aion_in_env(dir, &["x.aion"], &[("HOME", home.as_path())]))
    };
    // a project above $HOME, or in $HOME itself, is not picked up from below
    write(&root.join("aion.project"), "name = \"outer\"\n");
    write(&home.join("aion.project"), "name = \"mine\"\n");
    assert!(script(&home.join("work/deep")).contains("no aion.project"));
    // one between the script and $HOME is
    write(&home.join("work/aion.project"), "name = \"work\"\n");
    assert!(script(&home.join("work/deep")).contains("work/aion.project"));
    // outside $HOME the search still goes up
    let other = root.join("elsewhere");
    write(&other.join("aion.project"), "name = \"e\"\n");
    write(&other.join("sub/x.aion"), "import nothing\n");
    let text = stderr(&aion_in_env(&other.join("sub"), &["x.aion"], &[("HOME", home.as_path())]));
    assert!(text.contains("elsewhere/aion.project"), "{text}");
    // --project overrides the search
    let text = stderr(&aion_in_env(&home.join("work/deep"), &["--project", home.to_str().unwrap(), "x.aion"], &[("HOME", home.as_path())]));
    assert!(text.contains("home/aion.project"), "{text}");
}

#[test]
fn a_script_can_declare_dependencies_in_its_header() {
    if Command::new("git").arg("--version").output().is_err() {
        eprintln!("skipping: git is not installed");
        return;
    }
    let root = scratch("inline");
    write(&root.join("tools/lib.aion"), "fn tool() = \"a tool\"\n");
    write(
        &root.join("a.aion"),
        "#!/usr/bin/env aion\n// dep: tools = ./tools\nimport tools\nprintln(tools.tool())\n",
    );
    let out = aion_in(&root, &["a.aion"]);
    assert_eq!(stdout(&out), "a tool\n", "{}", stderr(&out));

    // a git dependency is pinned in `<script>.lock`
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
    write(
        &root.join("b.aion"),
        &format!("// dep: lib = file://{}@v1\nimport lib\nprintln(lib.version())\n", repo.display()),
    );
    let out = aion_in(&root, &["b.aion"]);
    assert_eq!(stdout(&out), "1.0\n", "{}", stderr(&out));
    assert!(std::fs::read_to_string(root.join("b.aion.lock")).unwrap().contains("lib_repo"), "no lock file");

    // inside a project the lines are ignored, with a warning
    write(&root.join("p/aion.project"), "name = \"p\"\n");
    write(&root.join("p/main.aion"), "// dep: tools = ../tools\nprintln(1)\n");
    let out = aion_in(&root.join("p"), &[]);
    assert!(stderr(&out).contains("belongs to a project"), "{}", stderr(&out));
}

#[test]
fn aion_add_remove_update_and_tree_manage_dependencies() {
    if Command::new("git").arg("--version").output().is_err() {
        eprintln!("skipping: git is not installed");
        return;
    }
    let root = scratch("manage");
    let home = root.join("home");
    let env = [("HOME", home.as_path())];
    let repo = root.join("http_repo");
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
    let url = format!("file://{}", repo.display());

    // into the project in the current folder (no terminal: no questions)
    let app = home.join("app");
    write(&app.join("aion.project"), "name = \"app\"  # mine\n");
    write(&app.join("main.aion"), "import http\nprintln(http.version())\n");
    let out = aion_in_env(&app, &["add", &format!("{url}@v1"), "--as", "http"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    let manifest = std::fs::read_to_string(app.join("aion.project")).unwrap();
    assert!(manifest.contains("# mine") && manifest.contains("http = { git = \"file://"), "{manifest}");
    assert_eq!(stdout(&aion_in_env(&app, &[], &env)), "1.0\n");
    assert!(stderr(&aion_in_env(&app, &["add", &format!("{url}@v1"), "--as", "http"], &env)).contains("already a dependency"));

    // from a sub-folder: the project above is used
    write(&app.join("sub/x.aion"), "println(1)\n");
    let out = aion_in_env(&app.join("sub"), &["add", "../../../tools_dir", "--as", "tools"], &env);
    assert!(!out.status.success());
    write(&home.join("tools_dir/lib.aion"), "fn t() = 1\n");
    let out = aion_in_env(&app.join("sub"), &["add", "../../tools_dir"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(std::fs::read_to_string(app.join("aion.project")).unwrap().contains("tools_dir = { path = \"../tools_dir\" }"));

    // the tree, then removing
    let tree = stdout(&aion_in_env(&app, &["tree"], &env));
    assert!(tree.starts_with("app"), "{tree}");
    assert!(tree.contains("http") && tree.contains("git file://") && tree.contains("tools_dir"), "{tree}");
    let out = aion_in_env(&app, &["remove", "tools_dir"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(!std::fs::read_to_string(app.join("aion.project")).unwrap().contains("tools_dir"));
    assert!(!aion_in_env(&app, &["remove", "tools_dir"], &env).status.success());

    // a script keeps its dependency in its header
    write(&home.join("s.aion"), "println(1)\n");
    let out = aion_in_env(&home, &["add", &format!("{url}@v1"), "--as", "http", "--script", "s.aion"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(std::fs::read_to_string(home.join("s.aion")).unwrap().starts_with("// dep: http = file://"));
    assert!(home.join("s.aion.lock").is_file());

    // no project anywhere: one is created
    let loose = root.join("elsewhere");
    std::fs::create_dir_all(&loose).unwrap();
    let out = aion_in_env(&loose, &["add", &url, "--as", "http", "--tag", "v1"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(std::fs::read_to_string(loose.join("aion.project")).unwrap().contains("tag = \"v1\""));

    // update after a new commit on the default branch moves the pin
    write(&repo.join("lib.aion"), "fn version() = \"2.0\"\n");
    git(&repo, &["commit", "-qam", "two"]);
    let branch = loose.join("b");
    write(&branch.join("aion.project"), "name = \"b\"\n");
    write(&branch.join("main.aion"), "import http\nprintln(http.version())\n");
    assert!(aion_in_env(&branch, &["add", &url, "--as", "http"], &env).status.success());
    assert_eq!(stdout(&aion_in_env(&branch, &[], &env)), "2.0\n");
    write(&repo.join("lib.aion"), "fn version() = \"3.0\"\n");
    git(&repo, &["commit", "-qam", "three"]);
    assert_eq!(stdout(&aion_in_env(&branch, &[], &env)), "2.0\n");
    assert!(aion_in_env(&branch, &["update", "http"], &env).status.success());
    assert_eq!(stdout(&aion_in_env(&branch, &[], &env)), "3.0\n");

    // a failing clone leaves the file as it was
    let before = std::fs::read_to_string(branch.join("aion.project")).unwrap();
    let out = aion_in_env(&branch, &["add", "file:///definitely/not/there", "--as", "ghost"], &env);
    assert!(!out.status.success());
    assert_eq!(std::fs::read_to_string(branch.join("aion.project")).unwrap(), before);
}

/// `aion install` makes a project that keeps its packages in `aion-dependencies/`, next to a `.gitignore`.
#[test]
fn aion_install_downloads_packages_into_the_project() {
    if Command::new("git").arg("--version").output().is_err() {
        eprintln!("skipping: git is not installed");
        return;
    }
    let root = scratch("install");
    let home = root.join("home");
    let cache = root.join("cache");
    let env = [("HOME", home.as_path()), ("AION_CACHE", cache.as_path())];
    let repo = root.join("http_repo");
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
    let url = format!("file://{}", repo.display());

    // no project yet and nobody to ask: the defaults make one, with the packages in aion-dependencies
    let app = home.join("app");
    std::fs::create_dir_all(&app).unwrap();
    write(&app.join("main.aion"), "import http\nprintln(http.version())\n");
    let out = aion_in_env(&app, &["install", &format!("{url}@v1"), "--as", "http"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    let text = stderr(&out);
    assert!(text.contains("packages are downloaded into aion-dependencies") && text.contains(".gitignore"), "{text}");

    let manifest = std::fs::read_to_string(app.join("aion.project")).unwrap();
    assert!(manifest.contains("packages = \"aion-dependencies\"") && manifest.contains("http = {"), "{manifest}");
    let ignore = std::fs::read_to_string(app.join(".gitignore")).unwrap();
    assert!(ignore.contains("/aion-dependencies/"), "{ignore}");

    let folders: Vec<_> = std::fs::read_dir(app.join("aion-dependencies"))
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    let package = folders.iter().find(|f| f.starts_with("http-")).unwrap_or_else(|| panic!("{folders:?}"));
    assert!(app.join("aion-dependencies").join(package).join("lib.aion").is_file());
    assert!(!app.join("aion-dependencies").join(package).join(".git").exists(), "just the files");
    assert!(!cache.join("git").exists(), "nothing went to the shared cache");
    assert_eq!(stdout(&aion_in_env(&app, &[], &env)).trim(), "1.0");

    // a missing folder is downloaded again, to the same place
    std::fs::remove_dir_all(app.join("aion-dependencies")).unwrap();
    let out = aion_in_env(&app, &["install"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(stderr(&out).contains("1 package installed"), "{}", stderr(&out));
    assert!(app.join("aion-dependencies").join(package).join("lib.aion").is_file());

    // `aion fmt` and `aion test` leave the downloaded code alone
    write(&app.join("aion-dependencies").join(package).join("theirs.aion"), "x := 1   \n");
    write(&app.join("mine.aion"), "x := 1   \n");
    let out = aion_in_env(&app, &["fmt", "--check"], &env);
    let report = format!("{}{}", stdout(&out), stderr(&out));
    assert!(report.contains("mine.aion") && !report.contains("theirs.aion"), "{report}");

    // the other answer: one copy in the shared cache, and nothing to ignore
    let shared = home.join("shared");
    std::fs::create_dir_all(&shared).unwrap();
    let out = aion_in_env(&shared, &["install", &format!("{url}@v1"), "--as", "http", "--packages", "cache"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(!std::fs::read_to_string(shared.join("aion.project")).unwrap().contains("packages"));
    assert!(!shared.join("aion-dependencies").exists() && !shared.join(".gitignore").exists());
    assert!(cache.join("git").is_dir());

    // `aion new` starts with a .gitignore too
    let out = aion_in_env(&home, &["new", "fresh"], &env);
    assert!(out.status.success(), "{}", stderr(&out));
    assert!(std::fs::read_to_string(home.join("fresh/.gitignore")).unwrap().contains("/aion-dependencies/"));
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
        .env("AION_OPT", test_opt())
        .output()
        .unwrap()
}

/// Like [`aion_in`], with extra environment variables.
fn aion_in_env(dir: &Path, args: &[&str], env: &[(&str, &Path)]) -> Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aion"));
    cmd.args(args).current_dir(dir).env("AION_CACHE", test_cache()).env("AION_OPT", test_opt());
    for (k, v) in env {
        cmd.env(k, v);
    }
    cmd.output().unwrap()
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

/// What `aion repl` showed for each entry: the text after each `>>> ` prompt, without the banner.
fn repl_answers(out: &Output) -> Vec<String> {
    let text = stdout(out);
    text.split(">>> ").skip(1).map(|a| a.trim().trim_start_matches("... ").to_string()).filter(|a| !a.is_empty()).collect()
}

#[test]
fn the_repl_keeps_the_session_between_entries() {
    let root = scratch("repl");
    let session = "1 + 2\nx := 10\nx * 2\nfn add(n: Int) = n + x\nadd(1)\nx = 20\nadd(1)\n\
                   fn add(n: Int) = n * x\nadd(2)\nnums := [3, 1, 2]\nnums.add(0)\nnums.sorted()\n\
                   if x > 5 {\n    println(\"big\")\n}\n:quit\n";
    let out = aion_with_input(&root, &["repl"], session);
    assert_eq!(repl_answers(&out), ["3", "20", "11", "21", "40", "[0, 1, 2, 3]", "big"], "{}", stderr(&out));
}

#[test]
fn a_failed_entry_leaves_the_repl_session_as_it_was() {
    let root = scratch("repl_errors");
    let out = aion_with_input(&root, &["repl"], "x := 1\ny := nothing + 1\n10 / (x - 1)\nx + 1\n");
    assert_eq!(repl_answers(&out), ["2"]);
    let errors = stderr(&out);
    assert!(errors.contains("undefined variable `nothing`"), "{errors}");
    assert!(errors.contains("division by zero"), "{errors}");
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
    // (where it is, the code, how to check it, a number for its file)
    let mut blocks = Vec::new();
    for file in files {
        let text = std::fs::read_to_string(&file).unwrap();
        for (line, info, body) in code_blocks(&text) {
            // ```aion runs; ```aion no_run only compiles; anything else is not Aion.
            let mode = match info.as_str() {
                "aion" => "run",
                "aion no_run" => "emit-ir",
                _ => continue,
            };
            let at = format!("{}:{line}", file.strip_prefix(root).unwrap().display());
            blocks.push((at, body, mode, blocks.len() + 1));
        }
    }
    let checked = blocks.len();
    let failures = in_parallel(&blocks, |(at, body, mode, number)| {
        let src = dir.join(format!("block_{number}.aion"));
        write(&src, body);
        let out = aion_in(&dir, &[mode, src.to_str().unwrap()]);
        if out.status.success() { Vec::new() } else { vec![format!("{at}\n{body}\n{}", stderr(&out))] }
    });
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

/// `http2` against nghttpd, a reference implementation: HTTP/2 without TLS and with TLS (ALPN), a large body,
/// and several requests at once on one connection.
#[test]
fn http2_interoperates_with_nghttpd() {
    let have = |program: &str| Command::new(program).arg("--version").output().is_ok_and(|o| o.status.success());
    if !have("nghttpd") || !have("openssl") {
        eprintln!("skipped: the nghttpd or openssl command is not installed");
        return;
    }
    let dir = scratch("h2");
    let www = dir.join("www");
    write(&www.join("hello.txt"), "hello from nghttpd");
    write(&www.join("big.txt"), &"0123456789abcdef".repeat(25000));
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
    let free_port = || std::net::TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port();
    let (plain_port, tls_port) = (free_port(), free_port());
    let start = |args: Vec<String>| {
        Command::new("nghttpd")
            .args(args)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()
            .unwrap()
    };
    let docs = www.display().to_string();
    let mut plain = start(vec!["--no-tls".into(), "-d".into(), docs.clone(), plain_port.to_string()]);
    let mut secure = start(vec![
        "-d".into(),
        docs,
        tls_port.to_string(),
        key.display().to_string(),
        cert.display().to_string(),
    ]);
    std::thread::sleep(std::time::Duration::from_millis(700));

    let program = format!(
        r#"import http2
r := http2.get("http://127.0.0.1:{plain_port}/hello.txt")
println(r?.status, r?.text(), r?.header("content-length"))
t := http2.get("https://localhost:{tls_port}/hello.txt", verify: false)
println(t?.status, t?.text())
println(http2.get("http://127.0.0.1:{plain_port}/missing")?.status)
big := http2.get("http://127.0.0.1:{plain_port}/big.txt")
println(big?.status, big?.body?.size())
conn := http2.connect("http://127.0.0.1:{plain_port}")!!
jobs := List<Job>()
sizes := Map<Int, Int>()
for i in 0..6 {{
    jobs.add(launch {{
        a := conn.request("GET", i % 2 == 0 ? "/hello.txt" : "/big.txt")
        sizes[i] = a == null ? -1 : a.body.size()
    }})
}}
for j in jobs {{ j.join() }}
println(sizes)
conn.close()
println(http2.get("https://localhost:{plain_port}/hello.txt", verify: false), http2.lastError().length() > 0)
"#
    );
    write(&dir.join("main.aion"), &program);
    let out = aion_in(&dir, &["main.aion"]);
    let _ = plain.kill();
    let _ = secure.kill();
    let _ = plain.wait();
    let _ = secure.wait();
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        stdout(&out),
        "200 hello from nghttpd 18\n200 hello from nghttpd\n404\n200 400000\n{0: 18, 1: 400000, 2: 18, 3: 400000, 4: 18, 5: 400000}\nnull true\n",
        "stderr: {}",
        stderr(&out)
    );
}

/// Types declared in other modules are registered with `reflect` whichever order the modules are imported in.
#[test]
fn reflection_sees_the_types_of_every_module() {
    let dir = scratch("reflect_modules");
    write(&dir.join("animals.aion"), "type Dog(name: String, age: Int = 1) {\n    fn bark(times: Int) = \"woof \".repeat(times).trim()\n}\n");
    write(&dir.join("cats.aion"), "type Cat(name: String) {\n    fn purr() = \"purr\"\n}\n");
    write(
        &dir.join("first.aion"),
        "import animals\nimport reflect\nimport cats\nd := reflect.create(\"Dog\", [\"Rex\"])!!\nprintln(reflect.fields(d), reflect.call(d, \"bark\", [2]), reflect.typeNames())\nprintln(reflect.call(reflect.create(\"Cat\", [\"Tom\"])!!, \"purr\"))\n",
    );
    write(
        &dir.join("second.aion"),
        "import reflect\nimport cats\nimport animals\nprintln(reflect.typeNames(), reflect.get(animals.Dog(\"Rex\", 5), \"age\"))\n",
    );
    let out = aion_in(&dir, &["first.aion"]);
    assert_eq!(stdout(&out), "[name: String, age: Int] woof woof [Dog, Cat]\npurr\n", "{}", stderr(&out));
    let out = aion_in(&dir, &["second.aion"]);
    assert_eq!(stdout(&out), "[Cat, Dog] 5\n", "{}", stderr(&out));
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
    cmd.args(&args).current_dir(dir).env("AION_CACHE", test_cache()).env("AION_OPT", test_opt());
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
    // (what to point the compiler at, the program, is it a project?)
    let mut jobs: Vec<(PathBuf, PathBuf, bool)> = sources_in(&root).into_iter().map(|f| (f.clone(), f, false)).collect();
    let mut folders: Vec<PathBuf> = std::fs::read_dir(root.join("projects"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.is_dir())
        .collect();
    folders.sort();
    for folder in folders {
        let project = folder.join("aion.project").is_file();
        let main = folder.join("main.aion");
        if main.is_file() {
            jobs.push((folder, main, project));
        } else {
            // a library package has no program of its own, only tests
            let tests = folder.join("tests/check.aion");
            jobs.push((tests.clone(), tests, false));
        }
    }
    let checked = jobs.len();
    let failures = in_parallel(&jobs, |(target, program, project)| check_example(target, program, *project).err().into_iter().collect());
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

#[test]
fn enums_work_across_modules() {
    let root = scratch("enums");
    write(&root.join("lib.aion"), "enum Color { Red, Green }\nfn favourite() = Color.Green\nprintln(\"lib loaded\")\n");
    write(
        &root.join("main.aion"),
        "import lib\nfrom lib import Color\n\
         println(lib.Color.Red, Color.Green, lib.favourite() == Color.Green, lib.Color.values())\n\
         println(when lib.favourite() { Color.Red -> 1\n Color.Green -> 2 })\n",
    );
    let out = aion_in(&root, &["main.aion"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out), "lib loaded\nRed Green true [Red, Green]\n2\n");
}

#[test]
fn synonyms_work_across_modules() {
    let root = scratch("synonyms");
    write(
        &root.join("lib.aion"),
        "synonym Stock = Map<String, Int>\nsynonym Pair<A> = (A, A)\nfn fresh() -> Stock { s := Stock()\n s[\"a\"] = 1\n return s }\n",
    );
    write(
        &root.join("main.aion"),
        "import lib\nfrom lib import Stock\n\
         a: Stock = lib.fresh()\nb: lib.Stock = lib.Stock()\nb[\"z\"] = 2\n\
         p: lib.Pair<Int> = (1, 2)\n\
         println(a.size(), b.size(), p)\n",
    );
    let out = aion_in(&root, &["main.aion"]);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out), "1 1 (1, 2)\n");
}

#[test]
fn aion_check_aion_fmt_and_aion_test() {
    let root = scratch("tooling");
    write(&root.join("good.aion"), "println(\"hi\")\n");
    write(&root.join("bad.aion"), "x: Int := \"no\"\n");
    let ok = aion_in(&root, &["check", "good.aion"]);
    assert!(ok.status.success(), "{}", stderr(&ok));
    assert_eq!(stdout(&ok), "", "check must not run the program");
    let bad = aion_in(&root, &["check", "bad.aion"]);
    assert!(!bad.status.success());
    assert!(stderr(&bad).contains("type_mismatch") || stderr(&bad).contains("bad.aion"), "{}", stderr(&bad));

    // fmt
    write(&root.join("messy.aion"), "fn f() {  \n\treturn 1\n}\n\n\n\nx := f()");
    let check = aion_in(&root, &["fmt", "--check", "messy.aion"]);
    assert!(!check.status.success(), "a messy file fails --check");
    let before = "fn f() {  \n\treturn 1\n}\n\n\n\nx := f()";
    assert_eq!(std::fs::read_to_string(root.join("messy.aion")).unwrap(), before, "--check must not write");
    let done = aion_in(&root, &["fmt", "messy.aion"]);
    assert!(done.status.success(), "{}", stderr(&done));
    assert_eq!(std::fs::read_to_string(root.join("messy.aion")).unwrap(), "fn f() {\n    return 1\n}\n\nx := f()\n");
    assert!(aion_in(&root, &["fmt", "--check", "messy.aion"]).status.success());

    // test
    write(&root.join("tests/passes.aion"), "assertEq(1 + 1, 2)\n");
    write(&root.join("tests/_helper.aion"), "panic(\"a helper is not a test\")\n");
    write(&root.join("tests/more/fails.aion"), "assertEq(1 + 1, 3, \"sum\")\n");
    let run = aion_in(&root, &["test", "tests/passes.aion"]);
    assert!(run.status.success(), "{}", stderr(&run));
    assert!(stderr(&run).contains("1 passed, 0 failed"), "{}", stderr(&run));
    let all = aion_in(&root, &["test"]);
    let text = stderr(&all);
    assert!(!all.status.success(), "{text}");
    assert!(text.contains("ok") && text.contains("FAIL") && text.contains("sum: expected 3 but got 2"), "{text}");
    assert!(text.contains("1 passed, 1 failed") && !text.contains("helper"), "{text}");
}

/// Just `aion`, in a folder with no project, says what Aion is instead of failing; `--help` writes to the standard output.
#[test]
fn aion_with_no_arguments_shows_the_welcome() {
    let dir = std::env::temp_dir().join(format!("aion-welcome-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let run = |args: &[&str]| {
        Command::new(env!("CARGO_BIN_EXE_aion")).args(args).current_dir(&dir).env("AION_CACHE", test_cache()).output().unwrap()
    };

    let welcome = run(&[]);
    let text = String::from_utf8_lossy(&welcome.stdout).into_owned();
    assert!(welcome.status.success(), "{}", stderr(&welcome));
    assert!(text.contains(&format!("aion v{}", env!("CARGO_PKG_VERSION"))) && text.contains("aion --help"), "{text}");
    assert!(!text.contains('\x1b'), "colour only on a terminal");

    // a command that does not exist gets the same banner on the standard error, and a failing exit status
    let unknown = run(&["frobnicate"]);
    assert_eq!(unknown.status.code(), Some(2));
    assert!(stderr(&unknown).contains(&format!("aion v{}", env!("CARGO_PKG_VERSION"))), "{}", stderr(&unknown));
    assert!(unknown.stdout.is_empty());

    let help = run(&["--help"]);
    assert!(help.status.success());
    assert!(String::from_utf8_lossy(&help.stdout).starts_with(&format!("Aion compiler {}", env!("CARGO_PKG_VERSION"))));
    let _ = std::fs::remove_dir_all(&dir);
}
