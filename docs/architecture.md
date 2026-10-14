# Compiler architecture

```
 aion.project ─▶ project.rs        fetch dependencies, build the package list
                    │
 source.aion        ▼
     │ syntax/lexer.rs, parser.rs      tokens → syntax tree (ast.rs)
     ▼
 loader.rs                              find + parse every imported module
     ▼
 sema/mod.rs   declare                  types, functions, globals, signatures
 sema/body.rs  check                    type-check bodies → typed IR (hir.rs)
     ▼
 codegen.rs                             typed IR → LLVM IR (text)
     ▼
 toolchain.rs                           clang: program.ll + runtime/*.c + stdlib/*.c → exe
     ▼
 main.rs  (cache.rs)                    run it, reusing a cached build when possible
```

`main.rs` wires these together and is the only place that prints errors.

## Design principles

* **Every stage returns `Result<_, Diagnostic>`.**  Nothing calls
  `process::exit` deep in the compiler; errors carry a `Span` and are rendered
  with a source excerpt by `diag.rs`.
* **Analysis and code generation are separate.**  `sema` decides what every name
  and expression *means* and what type it has; `codegen` only translates.  The
  typed IR (`sema/hir.rs`) makes implicit conversions explicit (`IntToFloat`,
  `ToStr`), points calls at function ids, and resolves fields to indices — the
  code generator never infers or looks anything up by name.
* **LLVM as text.**  Emitting `.ll` text and invoking `clang` removes the
  dependency on a specific LLVM version, on LLVM development packages, and the
  need for any platform-specific linking tricks.  Any clang that understands
  opaque pointers (LLVM 15+) works.
* **One declaration, one place.**  C-backed functions are `extern fn`
  declarations in `.aion` files; `build.rs` discovers stdlib files, so there is
  no registry to keep in sync.
* **Desugar early.**  `when`, `x in a..b` and `for` loops are rewritten into
  `if`/`while` in the parser or checker, so later stages only know the small core.

## Stages

### Lexer & parser (`src/syntax/`)

[Logos](https://github.com/maciejhirsz/logos) tokenises; every token records
whether a line break precedes it.  Strings are scanned by a small hand-written
routine because `${...}` may nest quotes and braces.

The parser is recursive descent with precedence climbing.  Statements have no
terminator; the parser ends an expression at a line break unless it is inside
`(...)`, follows an operator, or the next line starts with `.`, `&&` or `||`.
Two flags handle the one real ambiguity — is `{` a block or a trailing lambda?
In an `if`/`while`/`for`/`when` header `{` opens the body (`no_brace`), and a `{`
on a new line never continues the previous expression.

Assignment targets are parsed as ordinary expressions and validated afterwards.
String interpolations are re-parsed with a sub-parser whose offsets point into
the original file, so errors inside `${...}` are located correctly.  `when` is
desugared here into `{ $w := subject; if … else if … }`.

### Loader & projects (`src/loader.rs`, `src/project.rs`)

`project.rs` reads `aion.project`, clones git dependencies into the cache (pinning
commits in `aion.lock`) and returns a list of **packages**: where each one's files
live and which dependencies it declares.  `loader.rs` resolves each `import`
against that list — a file beside the importer, then a declared dependency, then
the embedded stdlib — detects cycles, and returns modules in dependency order
(prelude first, entry last).  Every module gets a unique symbol-safe name used to
mangle its functions (`utils__greet`, `sockets__TcpStream__send`).

### Semantic analysis (`src/sema/`)

*Declare* walks all modules in order and builds `Tables`: types with their field
layouts, every function/method/constructor with a resolved signature, a global
for each top-level `x := …`, an *init function* for each module that has
top-level statements, and a `Scope` per module (own items, imports, prelude as a
fallback).  *Check* then type-checks every body.

Two kinds of facts can't be known from declarations alone: the return type of
`fn f() = expr` and the type of an un-annotated top-level `x := expr`.  They are
inferred **on demand**: whoever first needs one asks the `Checker`, which checks
the defining body right then and reports a cycle if it depends on itself.  Module
initialisers are checked first so globals get their types in source order.

Names resolve as: local variable → field of `this` → module items (own, imported,
prelude) → module alias.  Inside a lambda, a name found in an enclosing function
is **captured** (see below).

Notable rules: `Int` widens to `Float`; conditions must be `Bool`; a function
with a return type must return on every path; lambdas take their parameter types
from the type they are expected to have.

### Code generation (`src/codegen.rs`)

Each HIR function becomes an LLVM function.  Values:

| Aion | LLVM | |
|------|------|-|
| Int / Float / Bool | `i64` / `double` / `i1` | locals are `alloca`s (promoted to registers by `-O2`) |
| String / object / function | `ptr` | to GC memory preceded by a header |

Objects are arrays of 8-byte slots, one per field (`Bool` is stored as `i64`).
`extern` calls pass `Bool` with `zeroext`, matching the C ABI.  Field access and
calls through function values check for null; integer `/` and `%` check for a zero
divisor.  Module-level variables are `internal global`s; `main` calls every
module's init function in order, then the entry's `fn main()`.

### Closures

A function value is a pointer to a GC object `{ code, captured… }`.  A call loads
`code` from slot 0 and passes the object itself as a hidden first argument; the
lambda's prologue copies its captured variables out of the object.

Which variables need sharing is decided per source variable while checking:

* captured and never reassigned → **copied** into the closure;
* captured **and** reassigned anywhere → **boxed**: the variable lives in a
  one-slot heap cell, and the creator and every closure hold the cell.

Capturing through nested lambdas creates a hidden local at each level that shares
the original's identity, so mutation anywhere marks the root variable.  The
decision (`hir::Local::boxed`) is made once the outermost function is checked,
when all assignments are known.  Declared functions used as values get a
generated adapter (`thunk__…`) with the closure calling convention.

### Runtime (`runtime/`)

`aion.h` defines the object header and the C API; `core.c` implements the
collector, string helpers and printing.  Both are embedded in the compiler and
compiled together with the program on every build (a hello-world build takes
≈0.15 s; a cached re-run ≈4 ms).

## Garbage collection

A precise, non-moving **mark-sweep** collector.

* **Headers.**  Every String and object is preceded by a 24-byte header
  `{next, size, flags, ptr_mask}`.  `ptr_mask` has bit *i* set when 8-byte slot
  *i* of an object holds a pointer, so tracing is exact — no scanning of the C
  stack, no guessing whether a word is a pointer.
* **Static strings.**  Literals are emitted by the compiler as constant objects
  with the same header and the `STATIC` flag, so every pointer the collector
  sees has a valid header and statics are skipped when marking.
* **Roots: an explicit shadow stack and the globals.**  A function that handles
  GC pointers puts a frame `[prev, n, slot₀ … slotₙ₋₁]` on the machine stack and
  links it into `aion_frame_top`.  *Every* GC-typed variable lives in a slot, and
  so does every freshly produced temporary (call results, concatenations, field
  loads).  Module-level variables holding pointers are listed in a table the
  compiler emits (`aion_roots`).  The collector walks both.  This is plain memory
  traffic, so it is correct at every optimisation level and needs no LLVM GC
  support.
* **When.**  Only inside an allocation, when the heap exceeds a threshold
  (initially 4 MiB, afterwards twice the live size).  Because everything live is
  rooted at that point, no safepoints are needed.
* **Marking** uses an explicit stack, so long linked lists cannot overflow the C
  stack.

`AION_GC_STRESS=1` makes every allocation collect and poisons freed memory; the
test-suite runs every program this way too, which makes a missed root show up
as garbage output rather than a rare crash.

## Toolchain & cache

`toolchain.rs` writes `program.ll`, the embedded runtime files, and the C halves
of imported stdlib modules to a temp directory and runs one

```text
clang -O2 -w -I <dir> program.ll core.c [stdlib C files] -lm -o <exe>
```

It tries `clang`, then `clang-22 … clang-15`; `AION_CLANG` overrides.  `aion run`
hashes the generated IR and C sources and stores the executable in the cache
(`cache.rs`), so an unchanged program is not recompiled.

## Tests

`cargo test` runs `tests/run.rs` and the unit tests:

* `tests/programs/*.aion` — compiled and run, stdout compared with `.out`, then
  run again under `AION_GC_STRESS=1` with identical expectations.
* `tests/errors/*.aion` — each must be rejected with the message in its
  `// error:` header (these only run the front end).
* projects, dependencies (including git, with a moved tag), the run cache, shebang
  scripts, import cycles, diagnostics rendering and `aion build`.
* every ```` ```aion ```` block in the documentation is compiled and run.

## Extending the language

1. **Syntax** — add tokens in `syntax/lexer.rs`, nodes in `ast.rs`, parsing in
   `parser.rs`.
2. **Meaning** — handle the new node in `sema/body.rs`, lowering it to existing
   HIR nodes where possible (`else if` is just a nested `if`).
3. **Machine code** — only if HIR itself needs a new node, add it to `hir.rs`
   and teach `codegen.rs` to emit it.
4. **Test** — add a program to `tests/programs/` (with its `.out`) and, for new
   errors, a file in `tests/errors/`.
