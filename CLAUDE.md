# Aion

## Git

- Do not add a `Co-Authored-By: Claude …` trailer (or any other Claude attribution line) to commit messages or pull request descriptions.

- If asked to make a commit, follow the convention: feat/fix/docs(module_name): description

## Testing with Docker

There is no Rust or clang on this machine, so build and test in Docker. Use PowerShell: in Git Bash the `$PWD` path is rewritten and the mount comes up empty.

- Run an Aion file (the compiler and standard library are baked into the image, so rebuild it after any change to `src/`, `stdlib/` or `runtime/`):

  ```powershell
  docker build -t aion .
  docker run --rm -v "${PWD}:/work" aion examples/hello.aion
  ```

  Add `emit-ir` before the file to check a `tests/errors/` program: it stops after type checking.

- Rust unit tests only (seconds once the `aion-target` volume is warm):

  ```powershell
  docker run --rm -v "${PWD}:/src" -v aion-target:/src/target -w /src rust:1-slim-trixie cargo test --bin aion
  ```

- The whole suite (programs, errors, examples, documentation blocks, projects), about five minutes; arguments after the script go to `cargo test`:

  ```powershell
  docker run --rm -v "${PWD}:/src:ro" -v aion-target:/work/target rust:1-slim-trixie bash /src/scripts/docker-test.sh
  ```

  The script copies the tree with LF line endings first, because the Windows checkout has CRLF and the tests compare output byte for byte. A failure from `code_in_the_documentation_works` names the `docs/*.md` file and line of the block; one from `programs_produce_expected_output` names the program and whether it failed normally or under GC stress.

Run the whole suite before pushing a change to `src/`, `stdlib/` or `tests/`: a prelude addition can break a program or documentation example that defines the same name.
