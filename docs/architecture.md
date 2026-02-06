# Aion Compiler — Architecture

This document explains what happens when you run `aion main.aion -o hello.exe`.

---

## Pipeline overview

```
┌──────────┐    ┌────────┐    ┌─────┐    ┌──────────┐    ┌───────┐    ┌────────┐
│ Source    │──▶│ Lexer  │──▶│ AST │──▶│ LLVM IR  │──▶│ .obj  │──▶│ Binary │
│ .aion    │    │ (Logos)│    │     │    │ (Inkwell)│    │       │    │ .exe   │
└──────────┘    └────────┘    └─────┘    └──────────┘    └───────┘    └────────┘
                                                                        ↑
                                                               libaion_runtime.a
```

## Stage 1 — Lexing (`src/lexer/`)

The lexer uses the **Logos** derive macro to turn source text into a flat stream
of tokens.  Each token carries its type (`Fn`, `Print`, `LParen`, `Str`, …) and
the corresponding slice of source text (the *lexeme*).

```
fn main() { print("Hello") }
 ↓
[Fn, Main, LParen, RParen, LBrace, Print, LParen, Str("Hello"), RParen, RBrace]
```

Logos generates a fast DFA at compile time — no hand-written state machine needed.

### Key file
- `src/lexer/lexer.rs` — `Token` enum with `#[derive(Logos)]`

---

## Stage 2 — Parsing (`src/parser/`)

A **recursive-descent parser** consumes the token stream and builds an
Abstract Syntax Tree (AST).

The parser pre-lexes the entire source into a `Vec<SpannedToken>` (token +
lexeme pairs) so it can peek ahead without re-scanning.

### Grammar (current)

```
program    = function*
function   = "fn" IDENT "(" ")" "{" statement* "}"
statement  = print_stmt
print_stmt = "print" "(" STRING ")"
```

### Key files
- `src/parser/parser.rs` — `Parser` struct with `peek()`, `advance()`, `expect()`
- `src/ast/ast.rs` — `Expr`, `Function`, `Program` types

---

## Stage 3 — LLVM code generation (`src/compiler/`)

The compiler walks the AST and emits **LLVM IR** using
[Inkwell](https://github.com/TheDan64/inkwell), a safe Rust wrapper around the
LLVM C API.

For `print("Hello from Aion!")` the generated IR looks like:

```llvm
@str = private unnamed_addr constant [17 x i8] c"Hello from Aion!\00", align 1

declare void @aion_print(ptr)

define i32 @main() {
entry:
  call void @aion_print(ptr @str)
  ret i32 0
}
```

Note how `print()` becomes a call to `@aion_print` — a function provided by the
**Aion runtime** rather than libc `puts` directly.  This lets us add formatting,
buffering, or Unicode handling later without changing the compiler.

### Key structures

| Type | Purpose |
|------|---------|
| `Compiler<'ctx>` | Owns the LLVM `Context`, `Module`, and `Builder` |
| `Runtime<'ctx>` | Holds `FunctionValue` handles for every runtime function |

---

## Stage 4 — Object file emission

LLVM's `TargetMachine` compiles the IR into a native **object file**
(`.obj` on Windows, `.o` on Linux/macOS) for the host architecture.

```rust
machine.write_to_file(&self.module, FileType::Object, path)
```

Only the **native target** (x86-64 on your PC) needs to be initialised.

---

## Stage 5 — Runtime compilation

Built-in functions live in `runtime/aion_runtime.c`.  The source is **embedded
into the compiler binary** at build time via `include_str!()`, so the installed
`aion` executable is fully self-contained — no need to ship the C file separately.

At compile time the compiler:
1. Writes the embedded C source to a cache directory (`%TEMP%/aion-runtime-cache/`)
2. Shells out to GCC to compile it:
   ```
   gcc -c -O2 aion_runtime.c -o aion_runtime.o
   ar rcs libaion_runtime.a aion_runtime.o
   ```
3. Skips recompilation if the cached library already exists and the source hasn't changed

### Current runtime functions

| Function | Signature | Purpose |
|----------|-----------|---------|
| `aion_print` | `void (const char *)` | Print a string + newline |
| `aion_print_int` | `void (long long)` | Print a 64-bit integer |
| `aion_print_float` | `void (double)` | Print a double |
| `aion_panic` | `void (const char *)` | Print error + `exit(1)` |
| `aion_alloc` | `void *(long long)` | `malloc` wrapper |
| `aion_free` | `void (void *)` | `free` wrapper |

---

## Stage 6 — Linking

The compiler invokes **GCC** (or Clang) to link:

```
gcc hello.obj runtime/libaion_runtime.a -o hello.exe
```

The result is a fully standalone native binary that depends only on the
system C runtime (no LLVM or Rust runtime needed at execution time).

---

## LLVM integration details (Windows)

The pre-built LLVM SDK for Windows ships with **static** `.lib` files compiled
with `/MT` (static MSVC CRT).  Rust links with `/MD` (dynamic CRT).  Mixing
these CRTs causes access-violation crashes at runtime.

### How we solve this

1. **Dynamic linking** — `.cargo/config.toml` overrides the `llvm-sys` link
   directives to link against `LLVM-C.dll` (the shared library) instead of
   the static `.lib` files.  The DLL handles its own CRT internally.

2. **Target stubs** — Inkwell references initialiser functions for *every*
   LLVM backend (Mips, Sparc, PowerPC, …), but the pre-built SDK only
   includes a subset (X86, AArch64, ARM, RISCV, WebAssembly, NVPTX, BPF).
   `stubs/llvm_target_stubs.c` provides no-op stubs for the missing targets.

3. **Wrapper functions** — The `LLVM_InitializeAll*` and
   `LLVM_InitializeNative*` functions are `static inline` in the LLVM headers
   and not exported by `LLVM-C.dll`.  Our stubs file implements them by
   calling into the individual target init functions from the DLL.

4. **build.rs** — The `cc` crate compiles `stubs/llvm_target_stubs.c` into a
   static lib that gets linked automatically by Cargo.

On **Linux/macOS** this is simpler: install the LLVM dev packages from your
distribution and static linking works out of the box (no CRT mismatch).

---

## Extending the compiler

### Adding a new built-in function

1. Add the C implementation in `runtime/aion_runtime.c`
2. Declare it in `Compiler::declare_runtime()` → add to the `Runtime` struct
3. Emit a call to it from `Compiler::compile_expr()`

### Adding a new AST node

1. Add a variant to `Expr` in `src/ast/ast.rs`
2. Parse it in `Parser::parse_statement()`
3. Lower it in `Compiler::compile_expr()`

### Adding a new CLI flag

Edit the argument handling in `src/main.rs` — it's plain `std::env::args()`
for now (no dependency on clap or similar).
