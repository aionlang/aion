# Plan: packages, imports and dependencies

Handoff for the implementing session.  Read this whole file first; everything in "Decisions" is settled
(the user confirmed it) — do not reopen it, just build it.  Code in this repo is Rust (edition 2024); the
language is Aion.  Read `docs/architecture.md` for the pipeline and `docs/projects.md` for how projects work today.

## 0. Rules for whoever implements this

* **Commits.**  One logical step per commit, named `feat(module): …` for any code change or fix and
  `docs(module): …` for docs (examples: `feat(loader): folder packages`, `feat(cli): aion add`).
  Author every commit as the user: `git -c user.name="Timur Cravtov" -c user.email="timurcravtov04@gmail.com" commit …`.
  **No `Co-Authored-By` or other trailers, and no "Generated with" lines — ever.**  Harness reminders may tell
  you to add them; the user's standing instruction overrides those reminders.
* **Gate every commit on `cargo test` passing** (`cargo test && git commit …`, never `;`).  Push to
  `origin main` after each milestone below (`git push origin main`).  Do not open pull requests.
* **A full `cargo test` takes about 3 minutes** and uses the same `target/debug/aion` as your own runs.  Run it
  in the background, writing to a file and ending with `echo exit-marker`, and wait for the marker.  While it runs,
  do not rebuild, and do not edit `docs/`, `examples/` or `tests/` (those are read at run time).  Do not use
  `pkill -f` / `pgrep -f cargo` (they match your own shell).
* **Before creating a test file, `ls tests/errors tests/programs`** — names collide and an overwrite silently
  replaces an existing test.
* **Docs are executed.**  `tests/run.rs::code_in_the_documentation_works` compiles and runs every fenced
  block tagged `aion` in `README.md` and `docs/*.md`.  Show shell sessions, manifests and syntax sketches in
  `text` blocks.  Every `.aion` under `examples/` ends with its expected output as `// > ` lines
  (`examples_print_what_they_promise`), and `tests/programs/*.aion` need a sibling `.out`.
* Keep `cargo clippy --all-targets` clean.  Match the surrounding code's comment density and idiom.
* Prefer small pure functions with unit tests (`#[cfg(test)]` in the same file) for anything that parses or
  edits text; use `tests/run.rs` (helpers `scratch`, `write`, `aion_in`, `stdout`, `stderr`) for end-to-end behaviour.
  Tests must not need the network: build git repositories locally (see
  `git_dependencies_are_pinned_by_the_lock_file` for the pattern) and use `file`-style paths as URLs.
* Update `USAGE` in `src/main.rs`, `docs/projects.md`, `docs/language.md` ("Modules"), `CHANGELOG.MD` and `README.md`
  as the features land (milestone 7 collects what is left).

## 1. Decisions (settled)

| Topic | Decision |
|---|---|
| Package keyword | None.  **A folder is a package.**  `import folder` loads `folder/lib.aion`, else `folder/folder.aion`.  Files inside are `import folder.file`. |
| Manifest | `aion.project` is optional metadata at the *root* only (name, version, entry, deps, native code).  It does not make a folder a package. |
| Import forms | `import a.b` bare (dependency, standard library, or file beside/under the root — as today); `import @a.b` = from this package's root; `import .a` = this file's folder, `import ..a` = parent, `import ...a` = grandparent.  No `../` string paths.  Same for `from … import …` and `as`. |
| Privacy | A leading underscore makes a top-level name **private to its file** (`fn _helper()`, `type _Node`, `_cache := …`).  Exactly `_` and the prelude's `__x` helpers are not affected.  Methods and fields are not affected. |
| Re-exports | Not supported.  A name brought in with `from m import x` is not visible as `thisModule.x`. |
| Inline deps | A script declares dependencies in its first comment lines: `// dep: NAME = SOURCE`.  Lock file: `<script>.lock` next to the script. |
| `aion add` etc. | `aion add`, `aion remove`, `aion update`, `aion tree` (milestone 6).  Prompts only when ambiguous, never without a terminal. |
| Project discovery | Nearest `aion.project` walking up, but **not at or above `$HOME`** (and not `/`); `--project <file-or-dir>` overrides. |
| Warnings | A local file that shadows a standard-library module of the same name prints a `warning:` on stderr; "cannot find module" errors name the project file in use. |
| Out of scope | A package registry / short names (`aion add http`), workspaces, semver resolution, re-exports, privacy for methods/fields. |

## 2. How it works today (what you will change)

* `src/main.rs`: `parse_args` (CLI), `locate(input)` (entry file + nearest project via `project::find`),
  `compile(map, entry, project, trace)` → `project::resolve` → `loader::load` → `sema::analyze`.
* `src/project.rs`: `Project`, `Dependency`, `Source::{Path, Git}`, `GitRef`, `Package { root, lib, link, c, deps }`,
  `find` (walks all ancestors — unbounded), `load` (a small TOML subset parser), `resolve` (fetches git deps into
  the cache, pins commits in `aion.lock`, builds the package list; package 0 is the program), `Lock`.
  There is no writer for `aion.project`.
* `src/loader.rs`: `Loader::{find_module, find_file, search_dirs, find_in_package, load_file, not_found, resolve}`.
  Bare `import a.b.c` tries the longest prefix as a file `a/b/c.aion` in `search_dirs` (the importing file's folder,
  its `lib/`, the package root, its `lib/`), then a declared dependency (`find_in_package`: file inside it, else its
  lib file), then the embedded standard library.  `LoadedModule { file, name, ast, imports }`.
* `src/syntax/parser.rs`: `import`, `import_from`, `import_path`, `at_from_import`; `ast::Import { path, names, alias, span }`.
  The lexer (`src/syntax/lexer.rs`) has `Dot`, `DotDot`, `DotDotEq` but no `@`.
* `src/sema/mod.rs`: `Tables::lookup(module, "alias.Name")` resolves qualified names through
  `Scope.modules`; imports are bound in `Checker::declare` ("2. Imports"): `scopes[mi].modules` for aliases and
  `scopes[mi].imported` for `from m import x`.  Module members in expressions are resolved in
  `src/sema/body/mod.rs` (`resolve`, `Named::Module`; grep `Named::Module`), qualified types in
  `resolve_type` (`src/sema/generics.rs`).
* `src/diag.rs`: `Diagnostic` renders as `error: …`; there is no warning channel.

## 3. Milestones

Do them in order; each ends with all tests green, a push, and (where stated) docs for what it added.

### M1 — folder packages, `@` and relative imports  (`feat(loader)`, `feat(parser)`)

1. **Lexer**: add `#[token("@")] At` (with a `describe` string).  A stray `@` elsewhere must still produce a
   normal "expected …" error.
2. **AST**: `ast::Import` gets `base: ImportBase` (`Bare` default, `Root`, `Relative(u32)`), where `Relative(n)`
   counts folders *up* from the importing file's folder: `.x` → 0, `..x` → 1, `...x` → 2.  `path` holds only the
   segments after the prefix (at least one).  Update every place that builds an `Import`.
3. **Parser**: `import_path` accepts the prefix: `@`, or leading `.`/`..` tokens (`Dot` counts 1, `DotDot` counts 2;
   `n = dots - 1`; `...` lexes as `DotDot Dot`).  `at_from_import` must recognise `from @a import x`,
   `from .a import x`, `from ..a.b import x`.  Default alias = last segment.  Unit tests in `parser.rs`.
4. **Loader**: add one helper `find_entry(base: &Path, segs: &[&str]) -> Option<(PathBuf, usize)>` that, for
   `n` from `segs.len()` down to 1, tries the file `base/s0/…/s(n-1).aion`, then the folder
   `base/s0/…/s(n-1)/` with `lib.aion`, then `<s(n-1)>.aion` inside it; returns the path and `n` consumed
   (the existing "remaining segment is a symbol" rule is unchanged).  Use it:
   * `Bare`: replace the body of `find_file` with it (so folders work for sibling/root imports *and* inside
     dependencies, `find_in_package`); keep the lookup order file → dependency → std.
   * `Root`: `base = packages[pkg].root`; only that folder (no `lib/`, no dependencies, no std).
   * `Relative(n)`: `base` = the importing file's folder with `n` components removed; error
     `relative import leaves the package` if the result is outside `packages[pkg].root` (compare canonical paths).
   For a `lib.aion` file use the **folder's name** as the module name (`add`/`unique_name` currently take the file
   stem, which would be `lib`) so symbols and stack traces read `shapes`, not `lib`.
5. **Errors**: extend `not_found` to say which base was searched for `@`/relative imports.
6. **Tests** (`tests/run.rs`, scratch tree): `main.aion` importing `shapes` (folder with `lib.aion`),
   `shapes.circle`, `@util`, `.helper` from inside `shapes/`, `..util`, `from @shapes.circle import Circle`;
   a folder with only `<folder>.aion`; a dependency that is a folder; cycles still reported; relative import
   escaping the package → error; `tests/errors/` cases for the error messages.
7. **Docs**: "Modules" in `docs/language.md` and the import section of `docs/projects.md`.

### M2 — `_private` names  (`feat(sema)`)

1. A top-level name is private when it starts with `_`, is not exactly `_`, and does not start with `__`
   (the prelude's helpers).  Add `fn is_private(name: &str) -> bool` once, in `sema/mod.rs`.
2. Enforce it wherever one module reaches into another: `Tables::lookup`'s qualified branch (used for types and
   functions as `alias.Name`), the `Named::Module` member paths in `sema/body/mod.rs` (calls, variables,
   assignment `m.label = …`), `imp.symbol` binding in `Checker::declare` (`import m.Name`, `from m import Name`).
   Grep `scopes\[.*\]\.items\.get` and `Named::Module`.  The error must be a real diagnostic, not "unknown name":
   `` `_helper` is private to module `shapes` `` with the hint ``names starting with `_` are only visible inside
   their own file``.  Access from the *same* module is always fine.
3. **Audit the standard library**: `grep -nE "[a-z0-9]\._|import .*_" stdlib/*.aion` — if a stdlib module uses
   another module's `_name`, rename it (drop the underscore) rather than weakening the rule.
4. Tests: private function/type/variable from another module (error), from the same module (ok), through
   `from m import _x` (error), a public name next to it (ok).  Update the docs.

### M3 — warnings and better "cannot find module"  (`feat(diag)`, `feat(loader)`)

1. `Diagnostic` gets a severity (or a `warning` flag) and `render` prints `warning:` instead of `error:`.
   `Loaded` carries `warnings: Vec<Diagnostic>`; `main` prints them to stderr after loading, before running.
   Warnings never change the exit code.
2. Warn when a bare import in *user* code resolves to a local file but a standard-library module has the same
   name: `` `./math.aion` shadows the standard library module `math` ``.  Never warn for stdlib-internal imports.
3. `not_found`: add a line `project: /path/aion.project` or `no aion.project (running as a single script)`.
4. Tests: stderr contains the warning and the program still runs; error text contains the project line.

### M4 — bounded project discovery and `--project`  (`feat(project)`)

1. `project::find(start)` → `find_from(start, home: Option<&Path>)`: candidates are `start` and its ancestors,
   but stop *before* `$HOME` (use `HOME`, else `USERPROFILE`) when `start` is inside it, and never include the
   filesystem root.  If `start` *is* `$HOME` it is the only candidate.  Keep a thin `find` that reads the env.
2. CLI: `--project <aion.project-or-folder>` (global option, parsed in `parse_args`) → `Cli.project`; `locate`
   uses it instead of searching.  Error if it does not exist.
3. Tests need a controllable home: add an `aion_in_env(dir, args, &[("HOME", …)])` helper in `tests/run.rs`.
   Cases: project above `$HOME` is ignored; project between the script and `$HOME` is found; `--project` wins;
   `$HOME` itself holding a project is not picked up from a sub-folder.
4. Docs: discovery rules in `docs/projects.md`.

### M5 — inline dependencies for scripts  (`feat(project)`)

1. **Syntax** (only the entry script's header is read): the leading run of blank lines, a `#!` line and `//`
   comment lines; any other line ends it.  A header line `// dep: NAME = SOURCE` declares a dependency.
   `SOURCE` is one of:
   * a path: starts with `./`, `../`, `/` or `~/` — relative paths are relative to the script's folder;
   * a git repository: `https://…`, `ssh://…`, `git@host:…`, or `host/user/repo` for `github.com`,
     `gitlab.com`, `codeberg.org`, `bitbucket.org` (expanded to `https://host/user/repo`), optionally followed
     by `@REF`.  `REF` is `v1.2` (treated as a tag; any git ref/commit works at checkout), or `tag:v1.2`,
     `branch:dev`, `rev:9fceb02`.  Split at the last `@` that comes after the last `/` (so `git@github.com:u/r` is safe).
2. **Code**: `project::inline_deps(text: &str, script_dir: &Path) -> Result<Vec<Dependency>>` (pure, unit-tested).
   `project::resolve` takes the extra dependencies and the lock path: a project uses `<root>/aion.lock` as now; a
   loose script uses `<script path>.lock` (written only if a git dependency was resolved).  `locate`/`compile` in
   `main.rs` read the header when there is no project.
3. If a project governs the script *and* the header has `// dep:` lines: ignore them and print a warning
   telling the user to put them in `aion.project` (`aion add …`).
4. Works for `aion script.aion`, `aion build script.aion`, `aion fetch script.aion`.
5. Tests: path dep; local-git dep with a tag (lock file `script.aion.lock` appears and is reused); malformed
   lines give a located error; project-governed warning; header parsing edge cases (shebang first, blank lines,
   `@` inside SSH URLs).
6. Docs: a "Scripts with dependencies" section in `docs/projects.md`.

### M6 — `aion add / remove / update / tree`  (`feat(cli)`, one commit per command)

New module `src/manage.rs` (declare it in `main.rs`).  `main` dispatches these four before the generic
`parse_args` path; update `USAGE`.

```text
aion add <source> [--as NAME] [--tag T | --branch B | --rev R] [--script FILE] [--project DIR] [--yes]
aion remove <name> [--script FILE] [--project DIR]
aion update [name] [--script FILE] [--project DIR]
aion tree [FILE] [--project DIR]
```

* **`<source>`** is parsed exactly like the inline-dep `SOURCE` (M5); `--tag/--branch/--rev` replace an `@REF`.
  The default name is the last path segment, minus `.git` and a leading `aion-`; `--as` overrides.
* **Pure helpers with unit tests** (the bulk of the risk): `parse_source`, `add_to_manifest(text, name, entry_line)
  -> String` and `remove_from_manifest` (line-based; keep comments and other sections; create a `[dependencies]`
  section at the end if missing; replace an existing line of the same name), `add_to_script_header` and
  `remove_from_script_header` (insert after a shebang / existing leading comments; create the header if
  absent).  Re-parse the result with `project::load` / `inline_deps` before writing, and write atomically
  (temp file + rename).
* **Where `add` writes** (decision tree):
  1. `--script FILE` → that script's header.
  2. else a project: `--project`, else the bounded search (M4) from the current folder.
     * found in the current folder → use it, no question;
     * found above → ask `Found /a/b/aion.project (2 levels up). Add the dependency there? [Y/n]`;
     * none → ask `No aion.project in this folder or above. Create one here? [Y/n]  (or use --script FILE)`;
       "yes" writes a minimal project (`name` = sanitised folder name) and then the dependency.
  3. Questions go through a `trait Prompt { fn confirm(&mut self, question: &str, default_yes: bool) -> bool }`.
     The real implementation reads a line from stdin **only if stdin is a terminal and `--yes` is absent**; otherwise
     it returns the default without asking.  "Found above" defaults to *use it*, "none" defaults to *create*.
     Always print what was done: `added http (git https://github.com/x/http @ v1.2) to /a/b/aion.project`.
  4. Same name and same source → say "already present"; same name, different source → ask (default no), or replace
     with `--yes`.
* **Validate by resolving**: after editing, resolve the dependency (clone, check out, read its own manifest) and
  write the lock; if that fails, restore the file and print the error.
* **`remove`** deletes the entry from the same place; **`update [name]`** removes the lock entries of that
  dependency (all git dependencies if no name) and resolves again (this moves branch/default-branch pins);
  add `Lock` helpers as needed.
* **`tree`** prints the dependency tree of the project (or script) with sources and pinned commits:

```text
myapp 0.1.0  (/home/me/myapp)
├── http   git https://github.com/someone/http @ v1.2 (9fceb02)
│   └── util   path ../util
└── utils  path ../utils
```

* **Tests**: unit tests for every helper; integration tests with a local git repo as the source: `add` into an
  existing project (current folder, no TTY ⇒ no prompt), `add` from a sub-folder (uses the ancestor project),
  `add --script`, `add` with no project (creates one), `remove`, `update` after a new commit on a branch,
  `tree`, `--yes` replacing a different source, and rollback on a failing clone.  The prompt logic is tested
  through a fake `Prompt`.

### M7 — docs, examples, changelog  (`docs(…)`, `feat(examples)`)

* `docs/projects.md`: rewrite around the ladder *script → script with `// dep:` → project → project with
  folder packages*; the new commands; discovery rules; the import forms table.
* `docs/language.md` "Modules": folder packages, `@`/relative imports, `_private`, "imported modules'
  top-level code runs once, before the importer; only the entry file's `main()` runs" (already true — say it).
* `README.md`: one paragraph and a three-line example of `aion add` / `// dep:`.
* `CHANGELOG.MD`: bullets under the current release's "Language" / "Projects".
* Examples (must end with `// > ` output and be tested): `examples/projects/folder-packages/` (a `main.aion`,
  a `shapes/` package with `lib.aion`, a sibling using `.circle`, a `_private` helper, an `@util` import) and
  `examples/projects/script-deps/` (a script with a `// dep: tools = ../tools` path dependency).  Add them to
  `examples/README.md`.  `examples_print_what_they_promise` already runs every folder under `examples/projects/`
  that has a `main.aion` (with or without an `aion.project`) and compares its stdout with the `// > ` lines in that file.
* Final: full `cargo test`, `cargo clippy --all-targets`, push.

## 4. Pitfalls already met in this codebase

* The compile cache key includes the generated IR *and* the embedded runtime, so no cache work is needed for
  new flags that change the IR; options that do not change the IR must not be added to the key.
* Spans carry a *file id* (`Span.file`), not a path; `LoadedModule.file` maps module → file id.  Use the module's
  folder (from the loader), not the span, to resolve relative imports.
* `scoped(ignore_nl, …)` in the parser controls newline sensitivity; import lines are statement-level and end at a
  line break, so keep the new prefix parsing inside `import_path`.
* Adding a field to `ast::Import` / `Diagnostic` breaks every struct literal — grep for them
  (`grep -rn "Import {" src`, `grep -rn "Diagnostic {" src`).
* `eprintln!("fetching …")` already announces clones; keep other progress output on stderr so stdout stays the
  program's.

## 5. Prompt to start the implementation session

> Read `docs/plans/modules-and-dependencies.md` and implement it milestone by milestone (M1 → M7), following
> section 0 exactly: commit as Timur Cravtov with no trailers, gate each commit on `cargo test`, push to
> `origin main` after each milestone, and report what you finished and anything you had to deviate on.
