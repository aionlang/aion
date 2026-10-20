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

## No clang? Use Zig

When no `clang` is found, Aion falls back to `zig cc` (`zig` on `PATH`, a `zig/` folder next to the `aion`
executable, or `AION_ZIG`).  Zig bundles a C compiler, a linker and the C library headers, so nothing else has to be
installed.  Libraries that a standard library module links (`sqlite3`, `z`) must still be installed.

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

`Dockerfile.macos` cross-compiles a universal `aion-macos` binary in Docker (see [docker.md](docker.md#a-macos-executable)).

## Windows

1. Install Rust from <https://rustup.rs> (with the Visual Studio Build Tools).
2. Install a clang that targets MinGW (the GNU runtime), such as [llvm-mingw](https://github.com/mstorsjo/llvm-mingw) or
   the `clang64` environment of MSYS2, and make sure `clang.exe` is on `PATH`.

`aion.exe` itself can be built with nothing but Docker, by cross-compiling in a Linux container:
`docker build -f Dockerfile.windows --output dist .` leaves `dist/aion.exe` (see [docker.md](docker.md#a-windows-executable)).

Programs built on Windows use the same runtime and standard library as on Linux: the C parts have Windows branches
(`CreateProcess` for `os`, Winsock for `net`, fibers for tasks, the console API for `term`, `BCryptGenRandom` for `random`,
and so on), and `os.platform()` returns `"windows"`.  The modules that load an optional library at run time look for its DLL
next to the program or on the `PATH`: `libcrypto-3-x64.dll` and `libssl-3-x64.dll` (OpenSSL: `net` TLS, `crypto`, `smtp`),
`libgmp-10.dll` (`bigint`), `libutf8proc-3.dll` (`unicode`), `libexpat-1.dll` (`xml`) and `libarchive-13.dll` (`archive`).
`sqlite` and `zlib` are linked, so their import libraries must be installed for the compiler to find.

Windows is tested by compiling every program of `tests/programs` for MinGW and running it under Wine
(`scripts/docker-test-windows.sh`, see [docker.md](docker.md#testing-the-windows-port)); all of them pass there except two that are
left out: `os_info` (it runs POSIX tools such as `tr` and `wc`) and `archive_formats` (the test image has no libarchive).
It has not been run on a real Windows machine yet, so reports of problems are welcome.  Shell commands go through `cmd /c`
(`os.shell`), and file paths use `/` or `\`.

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
| `AION_ZIG`        | the `zig` to build with (`zig cc`) instead of searching        |
| `AION_CACHE`      | where built programs and git dependencies are cached           |
| `AION_NO_CACHE`   | rebuild every run instead of reusing a cached binary           |
| `AION_GC_STRESS`  | when *running a compiled program*: collect on every allocation |

## Troubleshooting

* **`no C compiler found`** — install clang 15+ (or Zig), or set `AION_CLANG` / `AION_ZIG`.
* **`clang failed` with IR errors** — your clang is older than 15 (no opaque
  pointers); upgrade it.
* **Generated code looks wrong** — `aion emit-ir file.aion` prints the LLVM IR,
  and `--keep-temps` keeps the intermediate files.
