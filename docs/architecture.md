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

### Generics (`src/sema/generics.rs`, `body/generic_calls.rs`)

Generics are **monomorphized**.  A generic type or function stays a template (its
syntax tree) until it is used with concrete type arguments; each distinct argument
list produces a separate `TypeId` / `FnId` that is declared and checked like any
non-generic item, with the type parameters bound in a `Subst`.  Instances are
cached per argument list, a self-referential type finds itself because it is
registered before its fields are resolved, and nesting is capped (`f<T>` calling
`f<List<T>>` stops with an error instead of looping).  Nothing after `sema` knows
generics exist, which keeps the garbage collector's pointer maps exact.

Type arguments are inferred by `unify`-ing each written parameter type against the
checked argument's concrete type, then from the expected type, and last from
lambdas (checked with the partially known parameter types, so `xs.map { it + 1 }`
learns `R` from the lambda's result).  An error inside an instance is annotated
with its bindings (`while checking the instance with T = String`).

Members of generic instances are **lazy**: they are declared (signatures only) when
the instance is created but only type-checked when something calls them, so
`List<Fn>` is fine until you ask for `join`.  Unused ones become
`FuncKind::Unused` placeholders to keep ids stable.  Signatures are still eager,
so a method whose signature mentions an ever-larger instance of its own type is
cut off by the nesting cap (this is why `setOf` is a function, not a `List`
method).

A `static fn` is an ordinary `FuncKind::Free` function with no receiver, kept in
`TypeInfo::statics` instead of `methods`; `Work::Fn { owner }` lets its body call its
siblings bare.  `Type.name(…)` is resolved in the body checker before the receiver is
evaluated as a value (`static_receiver`), and `Checker::static_of` walks up `parent`.

Methods on built-in types (`fn String.shout()`) live in `Checker::ext_methods`,
keyed by type and name; `extern` ones take the receiver as their first C argument.

### Interfaces

An interface is a `TypeInfo` with `iface = Some(required methods)`, and the type
of a value of it is an ordinary `Ty::Obj`, so interface values are plain pointers.
`Checker::satisfies(ty, iface)` compares names, parameter types and result types
structurally.  `coerce` turns a conforming `T` into the interface with an
`Upcast` node (a no-op at run time) and records the pair (`T`, interface); a call
on an interface-typed receiver becomes `CallIface`.  Every object header carries
the object's type id + 1 above the four flag bits; for each interface method the
program gets a constant table indexed by that id, filled for the (type, interface)
pairs seen by `coerce` (and with `toString` implementations when an interface
value is ever printed).  The call loads the id, indexes the table and calls
through the pointer.  Bounds (`<T: Shape>`) reuse `satisfies` when a generic is
instantiated.

### Inheritance

`TypeInfo` records `parent`, `open`, an `init` function, the methods it `inherited`,
and `virtuals`.  A subtype's field list is the parent's followed by its own (a
constructor parameter named like an inherited field only forwards), and its method
table starts as a copy of the parent's, so inherited methods are ordinary calls
whose receiver is retyped (`Upcast`, free at run time).  Constructors of open types
and subtypes are split: `Type.init(this, params…)` runs the parent's `init` with the
arguments after the colon, then the type's own field initialisers, `init` blocks
and constructor body; `Type.new` allocates and calls it.

`open fn` makes a method *virtual*: the type that first declares it owns a
dispatch table (`Program.dispatch`, the same structure interfaces use) with an
entry for itself and each descendant, indexed by the type id in the object header.
Calls to a virtual method, including bare calls on `this`, become `CallIface`;
`super.m()` stays a direct call.  Overrides are validated after type checking
(`check_overrides`), when inferred return types are known.  A printed open type
dispatches `toString` through another slot of the same table.  `is` and `as` use a
constant per-target byte table indexed by type id (`instance_checks`), and a test
on a variable narrows it like a null check does (`narrow.rs`).

### Tasks (`runtime/tasks.c`)

A task is a closure with its own `mmap`'ed C stack, switched with `swapcontext`.  Everything
that is per-task lives in `AionTaskState` — the shadow stack of GC roots, the innermost
`try` and the `defer` stack — and core.c swaps it on every switch, so a task is
unaware of the others.  The collector reaches suspended tasks through
`aion_tasks_mark`.  Scheduling is round-robin over a list, with sleepers woken
earliest deadline first.  Waiting is `aion_park` (sleep until some `aion_notify`, which
wakes every parked task to re-check its condition) or `aion_wait_fd` (a task parks
with a file descriptor, an event mask and an optional deadline; the scheduler `poll`s
every waiting task's descriptor before picking the next task and, when nothing else can
run, blocks in `poll` until the earliest deadline).  If nothing is runnable, sleeping or
waiting for a descriptor, that is a deadlock and the program panics.  This is what lets
`std.net` and `std.http` serve many connections from one thread.  `Job`, `Task<T>` and
`Channel<T>` are ordinary Aion types in the prelude built on `__spawn`, `__park` and
`__notify`.

### `defer` and `try` (`body/mod.rs`, `runtime/core.c`)

Both are built from closures and a few runtime functions declared in the prelude
(`__defer_push`, `__defer_mark`, `__defer_run`, `__try_*`).  `defer body` is a
lambda pushed on a runtime stack of GC roots; the first `defer` of a block also
records the stack depth in a hidden local, and every way out of the block —
falling off the end, `return` (value computed into a temporary first), `break`,
`continue` — gets a `__defer_run(depth)` call from the checker, which tracks the
open blocks per function.  `try` checks the body and handler as lambdas and calls
`aion_try_*` (one per result kind: Unit, Int, Float, Bool, pointer), which runs the
body under `setjmp`.  `aion_panic` pops the innermost `try`, runs the defers
registered since, restores the shadow stack and `longjmp`s; with no `try` it runs
all defers and exits.  Doing it in C keeps `setjmp` away from optimised LLVM
locals.

### Optional types (`narrow.rs`)

`T?` is `Ty::Opt(i)`, an interned wrapper (an optional of an optional is the same
optional).  Representation is chosen by the inner type: a `String`, object or
function optional **is the pointer** (null = absent, so `Wrap` is free), while
`Int?`, `Float?` and `Bool?` point to a one-slot GC box.  The checker has three
small HIR nodes for it — `Wrap` (T → T?), `Unwrap` (null-checked T? → T) and
`IsNull` — and everything else is desugared onto them: `a ?: b`, `a?.f()`,
`a == b` between optionals, and printing an optional are `Block`/`Cond` trees over
a temporary.  `null` has its own type `Ty::Null`, which converts to any optional
and is rejected anywhere it would have to be stored (`x := null`).  `if` branches
and `?:` join a value and `null` into an optional.

Smart casts are purely syntactic.  `facts(cond, outcome)` reads `x != null`,
`x == null`, `&&`, `||` and `!` and yields the variables known non-null; they are
pushed while checking the branch (or the right operand of `&&` / `||`), and an
`if c { return }` guard pushes the negation for the rest of the block.  A
narrowing is keyed by the *variable*, not the local slot, so a lambda that
captures it still sees it narrowed.  It is dropped before the first statement that
syntactically assigns the variable (a plain `x = e` still sees it while `e`
runs), and never granted to a variable assigned inside a lambda or to a module
variable assigned by any function.  The narrowed reference becomes an `Unwrap`,
so even a wrong guess would be a clean panic rather than memory corruption.

### Arrays and collections

`Array<T>` is the one collection the compiler knows: a `TypeInfo` with
`array_elem = Some(T)`, created by `Tables::array_of`.  `Array(n, fill)`,
`a[i]`, `a[i] = v` and `a.size()` are HIR nodes (`ArrayNew`, `Elem`, `SetElem`,
`ArrayLen`) that codegen turns into an allocation, an inline bounds check
(`aion_panic_index`) and loads/stores of 8-byte slots.  A pointer array carries
the `PTRS` header flag so the collector traces every slot.

Everything else is library code.  Indexing, `in`, `for` and the literals are
sugar over methods (`get`/`set`/`contains`/`size`/`iter`/`add`) that the checker
resolves by name, so user types can opt in; `List`, `Map` and `Set` are generic
types written in Aion in the prelude.  `x in coll` is rewritten by the parser to
`coll.contains(x)` unless the right side is a range.

### Bytes

`Bytes` is a built-in type (`Ty::Bytes`) represented like a `String`: a pointer to a GC
object whose payload is the data, with the length in the header (so it may hold any byte,
unlike a NUL-terminated `String`) and no pointers to scan.  It is never null: the zero value
is the static object `@aion_empty_bytes` (`ExprKind::EmptyBytes`), and `Bytes?` is the
same pointer with `NULL` for absent.  `b[i]` and `b[i] = v` go through the `get` and `set`
methods the prelude adds to the type (extension methods on built-in types work for
indexing, slicing, `for`, and `+`), `==` and `hashOf` call prelude functions that compare and
hash the contents, and `extern fn` accepts `Bytes` / `Bytes?` so C code can produce and
consume them (`aion_bytes_new`, `aion_bytes_size`, `aion_bytes_data`).

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
* **Stack traces.**  A function of the program's own code (not the standard library) also keeps a
  record `{prev, info, line}` on the machine stack, linked into `aion_trace_top`; `info` names the
  function and file, and the compiled code stores the current line before each statement.  A fatal
  panic walks the chain and prints it.  `try` and the task switch save and restore the top, like the
  shadow stack.  `--no-trace` compiles all of it out.
* **When.**  Only inside an allocation, when the heap exceeds a threshold
  (initially 4 MiB, afterwards twice the live size).  Because everything live is
  rooted at that point, no safepoints are needed.
* **Marking** uses an explicit stack, so long linked lists cannot overflow the C
  stack.
* **Arrays** are objects whose payload is the elements; the `PTRS` flag says
  every slot is a pointer (an `Array<Int>` has no `SCAN` flag and is never traced).

`AION_GC_STRESS=1` makes every allocation collect and poisons freed memory; the
test-suite runs every program this way too, which makes a missed root show up
as garbage output rather than a rare crash.

## Toolchain & cache

`toolchain.rs` writes `program.ll`, the embedded runtime files, and the C halves
of imported stdlib modules to a temp directory and runs one

```text
clang -O2 -w -I <dir> program.ll core.c tasks.c [stdlib C files] [package C files] -lm [-l<lib> …] -o <exe>
```

The libraries come from `link = [...]` in `aion.project` and `// link: name` comments at the top
of stdlib modules; a package's `c = [...]` files are compiled in beside the stdlib's.

A `// vendor: name` comment asks for a C library that comes with Aion (`vendor/`, see its README): SQLite, zlib and
utf8proc.  Their sources are embedded in the compiler.  The first program that needs one compiles its `.c` files
with the same compiler (`-c -O2`, flags from the folder's `FLAGS` file) into `<cache>/vendor/<name>-<hash>/`, and
every program after that only links those object files, so SQLite's 9 MB amalgamation is compiled once, not per
program.  The hash covers the sources, the compiler and the target.

It tries `clang`, then `clang-22 … clang-15`, then `zig cc` (see docs/LOCAL-BUILDING.MD); `AION_CLANG` overrides.  `aion run`
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
* every ```` ```aion ```` block in the documentation is compiled and run;
* `https` against a local `openssl s_server`, keys from a piped terminal, a package that bundles C code.

## Extending the language

1. **Syntax** — add tokens in `syntax/lexer.rs`, nodes in `ast.rs`, parsing in
   `parser.rs`.
2. **Meaning** — handle the new node in `sema/body.rs`, lowering it to existing
   HIR nodes where possible (`else if` is just a nested `if`).
3. **Machine code** — only if HIR itself needs a new node, add it to `hir.rs`
   and teach `codegen.rs` to emit it.
4. **Test** — add a program to `tests/programs/` (with its `.out`) and, for new
   errors, a file in `tests/errors/`.
