# Changelog

## 0.3.0 — scripting syntax, closures, projects

A new surface syntax (Kotlin / Go / Python flavoured), first-class functions, and
project files with dependencies.  **Source-incompatible with 0.2**: see the
migration notes at the end of this section.

### Language
- **Scripts.** Top-level statements run in order; top-level `x := …` defines a
  module variable that functions can read and assign. `fn main()` is optional (and
  runs after the top-level code). A `#!` first line is ignored. Imported modules'
  top-level code runs once, before the importer.
- **Newline-aware parsing.** No terminators; a line break ends a statement except
  inside parentheses, after an operator, or before `.` `&&` `||`.
- **Functions.** `fn f(x: Int) = x * 2` with return-type inference; default
  parameters (`age: Int = 0`) and named arguments (`f(a, flag: true)`), evaluated
  in the order written.
- **Types.** Primary constructors `type User(name: String, age: Int = 0)`, field
  initialisers, `init { }` blocks, implicit `this` (fields and methods in scope by
  bare name), `fn User.label()` for methods outside the type, and a generated
  `toString` so objects can be printed and interpolated.
- **Expressions.** `if` and `when` are expressions; `x in 1..10`; `for i in a..b`
  and `a..=b` with `break` / `continue`; `$name` interpolation; `print`/`println`
  take several arguments.
- **Function values.** Function types `(Int) -> Int` (`Unit` = no value), lambdas
  `{ x -> … }` with implicit `it`, trailing lambdas `repeat(3) { … }`, closures
  that share mutable captured variables, and declared functions / constructors /
  module members usable as values.
- **Imports.** `import math`, `from sockets import TcpListener`, `import x as y`.
  `std.` / `aion.` prefixes still work. A local file shadows a stdlib module.
- **Generics.** `fn identity<T>(x: T)`, `type Box<T>(value: T)`, generic methods
  (`fn map<R>(f: (T) -> R)`), type arguments inferred from arguments, expected
  types and lambdas, or written out (`Box<Int>(1)`, `f<Int>(x)`). Monomorphized;
  members of an instance are only checked when used.
- **Collections.** Built-in `Array<T>` (bounds-checked) and prelude `List<T>`,
  `Map<K, V>`, `Set<T>`, `Entry<K, V>` written in Aion, with `map filter fold
  reduce forEach any all count sortedWith join …`. List literals `[1, 2]`, map
  literals `{"a": 1}`, `xs[i]` / `xs[i] = v`, `x in xs`, `for x in xs`, all as sugar
  over `get` / `set` / `contains` / `size` / `iter`, so user types can join in.
  `hashOf(value)` for map keys.
- **Nullability.** `T?` types, the `null` literal, `x == null`, `x!!`, `x ?: y`,
  `x?.f` / `x?.f()` and smart casts after null checks (`if x != null`, `if x == null
  { return }`, `&&`/`||`, loops that walk `node = node.next`). `Int?`/`Float?`/`Bool?`
  are boxed; optional strings, objects and functions are plain pointers. Library:
  `getOrNull`, `firstOrNull`, `lastOrNull`, `find`, `Map.getOrNull`, `toIntOrNull`.
- **Interfaces.** `interface Shape { fn area() -> Float }`, satisfied by shape (no
  `implements`), converted automatically, dispatched at run time; lists of mixed
  kinds work and print through each real type's `toString`. Bounds on type
  parameters: `fn total<T: Shape>(…)`, `type Holder<T: Shape>`.
- **`defer` and `try` / `catch`.** `defer cleanup()` / `defer { … }` runs when the
  enclosing block is left (end, `return`, `break`, `continue`, panic); `try { … }
  catch e { … }` is an expression that catches `panic(...)` and runtime errors
  (the message is a String, or `it` without a name).
- **Methods on built-in types.** `fn String.shout() = "$this!"`, `fn Int.double()`;
  `String` gains `length() contains() startsWith() indexOf() substring() trim()
  replace() split() toIntOr() upper() lower()` as methods.
- **Projects.** `aion.project` (name, entry, lib, `[dependencies]` with `path` or
  `git` + `tag`/`branch`/`rev`), `aion.lock` pinning git commits, and the
  commands `aion new`, `aion fetch`, `aion clean`. `aion` with no file runs the
  project. See docs/projects.md.
- **Faster re-runs.** `aion file.aion` caches the built binary by a hash of the
  generated program: ≈4 ms for an unchanged script instead of ≈180 ms.

### Compiler
- Return types of `= expr` functions and types of top-level variables are inferred
  on demand, with cycle detection.
- Closures are GC objects (`{ code, captured… }`); captured variables that are also
  reassigned live in shared heap cells. Module variables are GC roots.
- Every ```` ```aion ```` block in the documentation is compiled and run by the tests.

### Migrating from 0.2
| 0.2                               | 0.3                                   |
|-----------------------------------|---------------------------------------|
| `it` (receiver), `it.field`       | `this` / bare field name              |
| `fn T::method()`                  | `fn T.method()`                       |
| `fn f() => expr`                  | `fn f() = expr`                       |
| `type P(it.x, it.y) { x: Int … }` | `type P(x: Int, y: Int)`              |
| type with no constructor `T(a, b)`| declare the fields in `type T(a: …, b: …)`; a plain body-only type now has a zero-argument constructor |
| `import std.math;`                | `import math` (old form still works)  |
| `x: Int := 1`                     | `x: Int = 1` (old form still works)   |

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
