# Contributing to Aion

Thank you for wanting to help.  Aion is in beta: the language and the standard library can still change, so for anything
larger than a small fix, please open an issue first and agree on the idea before writing the code.

By taking part you agree to follow the [code of conduct](CODE_OF_CONDUCT.md).  Contributions are under the project's
[Apache License 2.0](LICENSE.md).

## Reporting a bug

Open an issue with the bug report template.  The most useful report has:

- the smallest program that shows the problem, and what you expected instead of what happened;
- the output of `aion --version`, your operating system, and the clang version (`clang --version`);
- for a crash of the compiler itself, the full message.

Security problems go through [SECURITY.md](SECURITY.md), not a public issue.

## Suggesting a feature

Open an issue with the feature template.  Say what you want to write and cannot (or can only write awkwardly) today; an
example program is worth more than a description.  Features for the standard library are easier to accept than changes to
the syntax.

## Setting up

[docs/LOCAL-BUILDING.MD](docs/LOCAL-BUILDING.MD) covers Linux, macOS and Windows.  Without Rust or clang on the machine, build
and test in Docker, as in [docs/docker.md](docs/docker.md):

```powershell
docker run --rm -v "${PWD}:/src:ro" -v aion-target:/work/target rust:1-slim-trixie bash /src/scripts/docker-test.sh
```

How the compiler is put together is in [docs/architecture.md](docs/architecture.md).

## Making a change

1. Fork the repository and make a branch from `main`.
2. Make the change, with a test (below) and, if it changes what a user can do, the documentation in `docs/` and a line
   in [CHANGELOG.MD](CHANGELOG.MD).
3. Run the whole test suite.  It takes about five minutes, and it must pass before you open the pull request: a new name
   in the prelude, for example, can break a program or a documentation example that defines the same name.
4. Open a pull request that says what changed and why.  Keep it to one subject; several small pull requests are
   reviewed faster than one large one.

### Tests

| Change | Test |
| --- | --- |
| a language feature or a standard-library function | a program in `tests/programs/name.aion` with its expected output in `name.out` |
| an error message, or something that must be rejected | a program in `tests/errors/` whose first line is `// error: the message` |
| code in `docs/*.md` | the fenced `aion` blocks are compiled and run by the suite, so they must be correct |
| behaviour that differs on Windows | a program in `tests/windows/` |

The header comments a test program can have (`// exit: N`, `// stderr: text`, `// stress: skip`) are listed at the top of
[tests/run.rs](tests/run.rs).  Every program also runs with a collection on every allocation and must print the same, which
catches values the compiler failed to keep alive.

Changes to the C parts (`runtime/`, `stdlib/*.c`) are checked on Windows too, with `scripts/docker-test-windows.sh`
(see [docs/docker.md](docs/docker.md#testing-the-windows-port)); keep the Windows branches (`#ifdef _WIN32`) working.

### Style

- Write code that reads like the code around it: its naming, its comments (few, saying why rather than what) and its idioms.
- Rust: no new compiler warnings.  Aion code: `aion fmt`.
- Standard-library modules are `stdlib/<name>.aion`, with `stdlib/<name>.c` when they need C, and are documented in
  [docs/stdlib.md](docs/stdlib.md).

### Commit messages

`feat|fix|docs(module): description`, where the description starts with a verb in the imperative and says what the commit
does:

```
feat(time): add formatting with strftime patterns
fix(sema): report the error of an argument instead of a problem with choosing among versions
docs(stdlib): describe the reflect module
```

One logical change per commit; several commits for one feature are fine.  The module is the part of the project the
commit touches (`sema`, `codegen`, `tasks`, `net`, `readme`, ...).
