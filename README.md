# Aion Language

## TURING COMLETE 

A compiled programming language that targets native binaries via LLVM.



## Quick start

```bash
# Install the compiler (puts `aion` on your PATH)
cargo install --path .

# Compile and run an Aion program
aion main.aion              # produces main.exe (Windows) / main (Linux/macOS)
aion main.aion -o hello.exe # explicit output name
aion main.aion --emit-ir    # print LLVM IR instead of compiling

# Or run without installing
cargo run -- main.aion
```

**Example** — `main.aion`:
```
fn main() {
    print("Hello from Aion!")
}
```

## How it works

```
source.aion → Lexer → Parser → AST → LLVM IR → .obj → Linker → native binary
                                                          ↑
                                                   libaion_runtime
```

| Stage | What happens |
|-------|-------------|
| **Lexer** | Tokenises source using the [Logos](https://github.com/maciejhirsz/logos) crate |
| **Parser** | Recursive-descent parser builds an AST from the token stream |
| **LLVM codegen** | Walks the AST and emits LLVM IR via [Inkwell](https://github.com/TheDan64/inkwell) |
| **Object file** | LLVM's `TargetMachine` writes a native `.obj` / `.o` |
| **Runtime** | `runtime/aion_runtime.c` is compiled to a static library (`libaion_runtime.a`) |
| **Link** | GCC or Clang links the object file + runtime into the final binary |

See [docs/architecture.md](docs/architecture.md) for a deeper walkthrough.

## Setting up on a new machine

Full step-by-step guide: **[docs/LOCAL-BUILDING.MD](docs/LOCAL-BUILDING.MD)**

### Short version (Windows)

1. Install **Rust** — <https://rustup.rs>
2. Install **Visual Studio Build Tools** (MSVC linker)
3. Install **GCC** — `scoop install tdm-gcc` (or MinGW-W64)
4. Download the **LLVM 21 SDK** — the `clang+llvm-*-x86_64-pc-windows-msvc.tar.xz` archive
5. Extract it (e.g. `C:\llvm-dev\clang+llvm-21.1.8-x86_64-pc-windows-msvc`)
6. Set the environment variable:
   ```powershell
   [System.Environment]::SetEnvironmentVariable("LLVM_SYS_211_PREFIX", "C:\llvm-dev\clang+llvm-21.1.8-x86_64-pc-windows-msvc", "User")
   ```
7. Make sure `LLVM-C.dll` is on your PATH (add the SDK's `bin/` folder)
8. `cargo install --path .` — installs `aion.exe` to `~/.cargo/bin/`

### Short version (Linux / macOS)

1. Install Rust, GCC/Clang, and LLVM 21 dev packages
2. `export LLVM_SYS_211_PREFIX=/path/to/llvm-21`
3. `cargo install --path .`

## Project structure

```
aion/
├── .cargo/config.toml      # Forces dynamic LLVM linking on Windows
├── build.rs                 # Compiles LLVM target stubs
├── Cargo.toml
├── main.aion                # Sample program
├── runtime/
│   └── aion_runtime.c       # Built-in functions (print, alloc, panic, …)
├── stubs/
│   └── llvm_target_stubs.c  # Stubs for LLVM targets not in the pre-built SDK
├── src/
│   ├── main.rs              # Compiler driver & CLI
│   ├── ast/                 # AST types (Expr, Function, Program)
│   ├── lexer/               # Logos-based tokeniser
│   ├── parser/              # Recursive-descent parser
│   └── compiler/            # LLVM codegen + linking
└── docs/
    ├── architecture.md      # Pipeline deep-dive
    └── LOCAL-BUILDING.MD    # Full setup instructions
```

## License

See [LICENSE.md](LICENSE.md).