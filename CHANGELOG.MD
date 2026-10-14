# Changelog

## 0.2.0 — rewrite

The compiler was rewritten around a typed intermediate representation, and the
LLVM library dependency was dropped.

### Build & tooling
- Generates LLVM IR as text and finishes with `clang` (15+). **No LLVM
  development libraries, `llvm-sys`, Inkwell, `LLVM_SYS_*` variables, target
  stubs or `.cargo/config.toml` are needed any more.**
- CLI: `aion <file>` still compiles and runs, but no longer leaves files behind;
  use `aion build <file> [-o out]` for an executable and `aion emit-ir <file>`
  for the IR (`--emit-ir` is still accepted). `-O0`…`-O3` select the
  optimisation level; the old `-O1` cap is gone.
- Errors are real diagnostics with `file:line:col`, a source excerpt and hints.
  Previously many mistakes were silently skipped or crashed the compiler.

### C bindings
- New `extern fn name(params) -> T [= "c_symbol"]`. A C-backed function is now a
  single line in a `.aion` file; the registry macros, the separate IDE stub
  files and the per-module `include_str!` wiring are gone. `stdlib/<m>.aion`
  (+ optional `<m>.c`) is discovered automatically by `build.rs`.

### Language
- Proper static checking: conditions must be `Bool`, no implicit `Float`→`Int`,
  unknown names/fields/methods/modules are errors, missing `return` is an error.
- User types are now **references** (heap objects) instead of by-value structs.
  `b := a` aliases; objects can be returned, stored in fields and passed around.
- Added `*` `/` `%` `&&` `||` `!`, `*=` `/=`, `else if`, parenthesised
  expressions, string `==`/`!=` by value, string `+`, `Bool` printing as
  `true`/`false`, `fn main() -> Int` for exit codes.
- `std.` and `aion.` imports both name the bundled standard library.
- Removed: `.=` (never worked) and `type A: B` inheritance syntax (was parsed and
  ignored; now an error).
- New modules: `std.strings`, `std.gc`.  `math.max`/`math.min` now take and
  return `Int` as documented (`fmax`/`fmin` for floats).

### Runtime
- New precise mark-sweep garbage collector: traces through object fields,
  handles string literals safely, works at every optimisation level (it uses its
  own shadow stack instead of LLVM's `gc "shadow-stack"` pass).
  `AION_GC_STRESS=1` collects on every allocation.
- Null-reference and division-by-zero checks.

### Standard library
- `std.sockets`: `send` now sends the whole string (`send_all` removed); the
  `_raw_*` primitives are `raw_*`; connection flags are `Bool`.

### Tests
- End-to-end suite in `tests/`: 15 programs (each also run under GC stress, bar
  one slow one) and 35 compile-error cases, plus parser/diagnostic unit tests. Benchmarks moved to `benchmarks/`; samples to `examples/`.
