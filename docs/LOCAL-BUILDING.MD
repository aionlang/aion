# Building Aion from source

The compiler is a plain Rust program.  At *run* time it also needs `clang`
(version 15 or newer) to compile the generated LLVM IR and the C runtime.  You do
**not** need LLVM development packages, `llvm-config`, or any environment
variables.

## Requirements

| Tool  | Version        | Why                                              |
|-------|----------------|--------------------------------------------------|
| Rust  | stable, 2024 edition | builds the compiler                        |
| clang | 15 or newer    | compiles generated IR + the C runtime, and links |

## Linux

```bash
# Debian / Ubuntu
sudo apt install -y build-essential clang curl
# Fedora
sudo dnf install -y clang gcc

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## macOS

```bash
xcode-select --install        # provides clang
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## Windows

1. Install Rust from <https://rustup.rs> (with the Visual Studio Build Tools).
2. Install LLVM/clang, e.g. `winget install LLVM.LLVM`, and make sure
   `clang.exe` is on `PATH`.

> The Windows path (`-lws2_32` for sockets, `.exe` naming) is written but has not
> been exercised since the 0.2 rewrite; please report problems.

`aion.exe` itself can be built without a Windows machine, by cross-compiling in a Linux container
(`scripts/build-windows.sh` has the command; the result is `dist/aion.exe`).  It runs the compiler's own commands
(`check`, `emit-ir`, `fmt`, `new`, `help`, ...).  Compiling and running programs on Windows also needs clang, and the
C parts of the standard library are still POSIX only (the prelude's `memmem`, and `fs`, `os`, `time`, `net`, `term`
and the modules that load a library with `dlopen`), so use the Docker image or WSL for that for now.

## Build, test, install

```bash
git clone <repo-url> aion && cd aion

cargo build --release            # ./target/release/aion
cargo test                       # end-to-end suite (needs clang)
cargo install --path .           # puts `aion` on your PATH
```

Try it:

```bash
aion examples/hello.aion         # compile and run
aion build examples/hello.aion   # leaves ./hello
aion new demo && cd demo && aion # a project
```

## Environment variables

| Variable          | Effect                                                         |
|-------------------|----------------------------------------------------------------|
| `AION_CLANG`      | the C compiler to use instead of `clang` / `clang-N`           |
| `AION_CACHE`      | where built programs and git dependencies are cached           |
| `AION_NO_CACHE`   | rebuild every run instead of reusing a cached binary           |
| `AION_GC_STRESS`  | when *running a compiled program*: collect on every allocation |

## Troubleshooting

* **`no C compiler found`** — install clang 15+ or set `AION_CLANG`.
* **`clang failed` with IR errors** — your clang is older than 15 (no opaque
  pointers); upgrade it.
* **Generated code looks wrong** — `aion emit-ir file.aion` prints the LLVM IR,
  and `--keep-temps` keeps the intermediate files.
