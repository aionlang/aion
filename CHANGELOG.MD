# Changelog

## 0.3.0 — scripting syntax, closures, generics, collections, nullability, interfaces, projects

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
- **Tasks and channels.** Cooperative tasks on their own stacks: `launch { }` (a `Job`),
  `async { value }` (a `Task<T>` with `await()`), `sleep(ms)`, `yield()`, `waitAll()`, and
  `Channel<T>` (unbuffered or buffered, `send`/`receive`/`close`); deadlocks are reported.
- **Inheritance.** `open type`, `type Dog(…) : Animal(args)`, `open fn` / `override
  fn` with dynamic dispatch, `super.method()`, subtype conversion, `x is T`, `x !is T`,
  `x as T`, `x as? T`, smart casts after `is`, and polymorphic printing.
- **Generic interfaces and ordering.** `interface Container<T>`, bounds that mention
  the parameter (`T: Comparable<T>`), the standard `Comparable<T>`, `sorted()`,
  `sortedBy`, `maxOrNull`, `minOrNull`; `sortedWith` is now a stable merge sort.
- **Value equality and hashing.** A type with `equals(other)` is compared by value
  by `==`, `contains` and friends; with `hashCode()` too it can be a `Map` key or
  `Set` member.
- **Slices.** `xs[a..b]`, `xs[a..]`, `xs[..b]`, `xs[a..=b]` and `xs[..]` on lists and
  strings (and any type with `slice(from, to)`), with checked bounds.
- **Characters.** The `Char` type: `'a'` literals with escapes and `\u{…}`,
  comparison and `c + 1` / `c - 'a'` arithmetic, ranges (`'a'..='z'`), `Char`
  methods, `text.chars()`, `text[i]` and `for c in text`.
- **Input and arguments.** `readLine()` (a `String?`, null at the end of the input),
  `readText()`, and `args()`; `aion script.aion a b c` passes `a b c` to the program.
- **Methods on built-in types.** `fn String.shout() = "$this!"`, `fn Int.double()`;
  `String` gains `length() contains() startsWith() indexOf() substring() trim()
  replace() split() toIntOr() upper() lower()` as methods.
- **Projects.** `aion.project` (name, entry, lib, `[dependencies]` with `path` or
  `git` + `tag`/`branch`/`rev`), `aion.lock` pinning git commits, and the
  commands `aion new`, `aion fetch`, `aion clean`. `aion` with no file runs the
  project. See docs/projects.md.
- **Faster re-runs.** `aion file.aion` caches the built binary by a hash of the
  generated program: ≈4 ms for an unchanged script instead of ≈180 ms.
- **Numbers.** Hex, binary and octal literals (`0xFF`, `0b1010`, `0o17`), `1_000_000`,
  exponents (`1.5e-3`); bit operations as `Int` methods (`and or xor inv shl shr ushr`);
  `Float` values print as the shortest text that reads back exactly (`0.1`,
  `0.30000000000000004`); conversions and helpers (`toFloat toInt round floor ceil format
  roundTo pow gcd clamp toHex toRadix parseInt …`).
- **Strings.** `"""triple-quoted"""` strings (multi-line, may hold quotes, interpolation
  still works), raw strings `r"C:\dir"` / `r"""…"""`, `\u{…}` escapes, and many more methods
  (`endsWith lastIndexOf trimStart trimEnd repeat padLeft padRight lines isBlank charCount
  toFloatOrNull indexOf(needle, from)`); `StringBuilder`; `eprint` / `eprintln`.
- **Bytes.** A `Bytes` type for binary data (`Bytes(n)`, `bytesOf([…])`, `b[i]`, slices, `+`,
  `==`, `Map` keys, `toHex`/`fromHex`, `toText`/`toBytes`, `find`, `copyFrom`) and `BytesBuilder`;
  `extern fn` can pass `Bytes`, `Bytes?` and `Char`.
- **Language details.** Optional parameters may default to `null`; a module's types can be
  written `module.Type`; indexing, `for`, slices, `==` and `hashOf` on an `open` type now
  dispatch to the real kind's method (they called the parent's before).
- **Packages.** `link = ["sqlite3"]` and `c = ["native/x.c"]` in `aion.project` (system libraries
  and bundled C code); `// link: name` in stdlib modules. See docs/packages.md.

### Standard library
New modules, all written for this release:
- `fs`, `path` (files, directories, temporary directories, path arithmetic), `os`
  (environment, process info, running programs with captured output), `time` (clocks,
  `DateTime`, formatting and parsing), `random` (seedable generator, secure bytes), `math`
  (constants, logs, trigonometry, rounding).
- `hash` (MD5, SHA-1, SHA-2, HMAC, CRC-32), `encoding` (Base64, percent-encoding, UUIDs),
  `json` (a family of types built with inheritance), `regex` (a self-contained linear-time
  engine with groups, named groups, lazy quantifiers and flags), `csv`, `cli` (argument parsing
  with usage text), `term` (ANSI styles, terminal size, key input).
- `net` (TCP, TLS clients through a run-time-loaded OpenSSL; every wait lets other tasks run)
  and `http` (client with redirects, proxies and TLS; a small multi-connection server).
- `sqlite` (prepared statements, transactions) and `zlib` (gzip, zlib, raw deflate with a
  decompression-bomb guard); they need the system development packages and declare
  their `-l` flags themselves.

### Compiler
- Return types of `= expr` functions and types of top-level variables are inferred
  on demand, with cycle detection.
- Closures are GC objects (`{ code, captured… }`); captured variables that are also
  reassigned live in shared heap cells. Module variables are GC roots.
- Every ```` ```aion ```` block in the documentation is compiled and run by the tests.
- Tasks can wait for file descriptors (`aion_wait_fd`), which `net` and `http` use so a
  server can handle many connections at once.

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
