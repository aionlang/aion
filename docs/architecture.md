# Compiler architecture

```
 source.aion
     │ syntax/lexer.rs, parser.rs      tokens → syntax tree (ast.rs)
     ▼
 loader.rs                              finds + parses every imported module
     ▼
 sema/mod.rs   declare                  collect types/functions/signatures
 sema/body.rs  check                    type-check bodies → typed IR (hir.rs)
     ▼
 codegen.rs                             typed IR → LLVM IR (text)
     ▼
 toolchain.rs                           clang: program.ll + runtime/*.c + stdlib/*.c → exe
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

## Stages

### Lexer & parser (`src/syntax/`)

[Logos](https://github.com/maciejhirsz/logos) tokenises; strings are scanned by
a small hand-written routine because `${...}` may nest quotes and braces.  The
parser is recursive descent with precedence climbing for binary operators.
Assignment targets are parsed as ordinary expressions and validated afterwards,
which avoids lookahead special cases.  String interpolations are re-parsed with
a sub-parser whose offsets point into the original file, so errors inside
`${...}` are located correctly.

### Loader (`src/loader.rs`)

Resolves each `import` (stdlib from the embedded table, user modules from disk),
detects cycles, and returns modules in dependency order: prelude first, entry
module last.  Every module gets a unique symbol-safe name used to mangle its
functions (`utils__greet`, `sockets__TcpStream__send`).

### Semantic analysis (`src/sema/`)

*Declare* walks all modules in order and builds `Tables`: types with their field
layouts, every function/method/constructor with a resolved signature, and a
`Scope` per module (own items, `import m` aliases, selective imports, with the
prelude as a fallback).  *Check* then type-checks every body against those
tables.  Because signatures are known up front, functions may call each other in
any order.

Notable rules: `Int` widens to `Float`; conditions must be `Bool`; a function
with a return type must return on every path; locals are lexically scoped.

### Code generation (`src/codegen.rs`)

Each HIR function becomes an LLVM function.  Values:

| Aion | LLVM | |
|------|------|-|
| Int / Float / Bool | `i64` / `double` / `i1` | locals are `alloca`s (promoted to registers by `-O2`) |
| String / object | `ptr` | to GC memory preceded by a header |

Objects are arrays of 8-byte slots, one per field (`Bool` is stored as `i64`).
`extern` calls pass `Bool` with `zeroext`, matching the C ABI.  Field access and
method calls check for null; integer `/` and `%` check for a zero divisor.

### Runtime (`runtime/`)

`aion.h` defines the object header and the C API; `core.c` implements the
collector, string helpers and printing.  Both are embedded in the compiler and
compiled together with the program on every build (a whole hello-world build takes ≈0.15 s).

## Garbage collection

A precise, non-moving **mark-sweep** collector.

* **Headers.**  Every String and object is preceded by a 24-byte header
  `{next, size, flags, ptr_mask}`.  `ptr_mask` has bit *i* set when 8-byte slot
  *i* of an object holds a pointer, so tracing is exact — no scanning of the C
  stack, no guessing whether a word is a pointer.
* **Static strings.**  Literals are emitted by the compiler as constant objects
  with the same header and the `STATIC` flag, so every pointer the collector
  sees has a valid header and statics are skipped when marking.
* **Roots: an explicit shadow stack.**  A function that handles GC pointers puts
  a frame `[prev, n, slot₀ … slotₙ₋₁]` on the machine stack and links it into
  `aion_frame_top`.  *Every* GC-typed variable lives in a slot, and so does every
  freshly produced temporary (call results, concatenations, field loads).  The
  collector walks the chain of frames.  This is plain memory traffic, so it is
  correct at every optimisation level and needs no LLVM GC support.
* **When.**  Only inside an allocation, when the heap exceeds a threshold
  (initially 4 MiB, afterwards twice the live size).  Because everything live is
  in a slot at that point, no safepoints are needed.
* **Marking** uses an explicit stack, so long linked lists cannot overflow the C
  stack.

`AION_GC_STRESS=1` makes every allocation collect and poisons freed memory; the
test-suite runs every program this way too, which makes a missed root show up
as garbage output rather than a rare crash.

## Toolchain

`toolchain.rs` writes `program.ll`, the embedded runtime files, and the C halves
of imported stdlib modules to a temp directory and runs one

```
clang -O2 -w -I <dir> program.ll core.c [stdlib C files] -lm -o <exe>
```

It tries `clang`, then `clang-22 … clang-15`; `AION_CLANG` overrides.

## Tests

`cargo test` runs `tests/run.rs`:

* `tests/programs/*.aion` — compiled and run, stdout compared with `.out`, then
  run again under `AION_GC_STRESS=1` with identical expectations.
* `tests/errors/*.aion` — each must be rejected with the message in its
  `// error:` header (these only run the front end).
* a few direct tests for diagnostics rendering, import cycles and `aion build`.

## Extending the language

1. **Syntax** — add tokens in `syntax/lexer.rs`, nodes in `ast.rs`, parsing in
   `parser.rs`.
2. **Meaning** — handle the new node in `sema/body.rs`, lowering it to existing
   HIR nodes where possible (`else if` is just a nested `if`).
3. **Machine code** — only if HIR itself needs a new node, add it to `hir.rs`
   and teach `codegen.rs` to emit it.
4. **Test** — add a program to `tests/programs/` (with its `.out`) and, for new
   errors, a file in `tests/errors/`.
