# Aion Compiler — Design Document

Everything about how Aion source code becomes a native binary.

---

## Table of Contents

1. [Compilation Pipeline](#1-compilation-pipeline)
2. [Stage 1 — Lexing](#2-stage-1--lexing)
3. [Stage 2 — Parsing](#3-stage-2--parsing)
4. [Stage 3 — AST](#4-stage-3--ast)
5. [Stage 4 — Prelude & Import Resolution](#5-stage-4--prelude--import-resolution)
6. [Stage 5 — LLVM Code Generation](#6-stage-5--llvm-code-generation)
7. [Stage 6 — Runtime Compilation](#7-stage-6--runtime-compilation)
8. [Stage 7 — Linking](#8-stage-7--linking)
9. [Stage 8 — Execution](#9-stage-8--execution)
10. [The Aion Language (Current Subset)](#10-the-aion-language-current-subset)
11. [How To: Add a New Built-in Function](#11-how-to-add-a-new-built-in-function)
12. [How To: Add a New C-Backed Stdlib Module](#12-how-to-add-a-new-c-backed-stdlib-module)
13. [How To: Add a New Aion Stdlib Module](#13-how-to-add-a-new-aion-stdlib-module)
14. [How To: Add a New Type](#14-how-to-add-a-new-type)
15. [How To: Add New Syntax](#15-how-to-add-new-syntax)
16. [Project Layout](#16-project-layout)
17. [Error Reporting](#17-error-reporting)
18. [IDE Stubs](#18-ide-stubs)

---

## 1. Compilation Pipeline

When you run `aion main.aion`, the following stages execute in order:

```
 source.aion
     │
     ▼
 ┌──────────────────────────────────┐
 │  1. Lexer (Logos)                │  source text → tokens
 │  2. Parser (recursive-descent)   │  tokens → AST
 │  3. Prelude injection            │  inject always-available fns
 │  4. Import resolution            │  resolve imports → more AST nodes
 │  5. LLVM codegen (Inkwell)       │  AST → LLVM IR
 │  6. Object emission              │  LLVM IR → .obj
 │  7. Runtime build (gcc + ar)     │  C sources → libaion_runtime.a
 │  8. Linking (gcc/clang)          │  .obj + .a → native binary
 │  9. Auto-run                     │  execute the binary
 └──────────────────────────────────┘
     │
     ▼
 main.exe (native binary)
```

The entire compiler is a single Rust binary. There is no interpreter — Aion
compiles directly to machine code via LLVM.

---

## 2. Stage 1 — Lexing

**File:** `src/lexer/lexer.rs`

The lexer uses the [Logos](https://github.com/maciejhirsz/logos) derive macro
on a `Token` enum. Logos generates a DFA at Rust compile time — no hand-written
state machine.

### Token types

| Category    | Tokens                                              |
|-------------|-----------------------------------------------------|
| Keywords    | `fn`, `import`                                      |
| Punctuation | `(`, `)`, `{`, `}`, `.`, `;`, `,`, `:`, `:=`, `::=`|
| Literals    | `FloatLit`, `IntLit`, `Str`                         |
| Identifiers | `Ident` (catches everything else: names, types)     |

Key design decisions:

- **No keyword tokens for types.** `Int`, `Float`, and any future type names
  are just `Ident` tokens. The parser checks the lexeme string. This means
  adding a type like `Bool` or `String` requires zero lexer changes.
- **No keyword token for `print`.** `print` and `println` are regular
  identifiers parsed as function calls, then special-cased during codegen.
- **Whitespace and `//` comments** are silently skipped by Logos.

### Example

```
fn main() { print("Hello") }
```

Becomes:

```
[Fn, Ident("main"), LParen, RParen, LBrace,
 Ident("print"), LParen, Str("\"Hello\""), RParen,
 RBrace]
```

---

## 3. Stage 2 — Parsing

**File:** `src/parser/parser.rs`

A **recursive-descent parser** consumes tokens and builds an AST.

The entire source is pre-lexed into a `Vec<SpannedToken>` (token + lexeme pairs)
so the parser has random access for lookahead.

### Grammar (current subset)

```
program    = import* function* EOF
import     = "import" IDENT ("." IDENT)* ";"
function   = "fn" IDENT "(" ")" "{" statement* "}"
statement  = var_def | expr_stmt
var_def    = IDENT "::" "=" expr              // infer type
           | IDENT ":" IDENT ":=" expr        // explicit type + value
           | IDENT ":" IDENT                  // explicit type, no value
expr       = func_call | module_call | literal | var_ref
func_call  = IDENT "(" args ")"
module_call= IDENT "." IDENT "(" args ")"
args       = expr ("," expr)*
literal    = INT_LIT | FLOAT_LIT | STRING_LIT
var_ref    = IDENT
```

### How statements are disambiguated

When the parser sees an `Ident` at statement position, it peeks one token
ahead. If the next token is `:`, `:=`, or `::=`, it's a variable definition.
Otherwise it's an expression statement (function call, etc.).

### How expressions are disambiguated

When the parser sees an `Ident` in expression position:

- If the next token is `.` → **module call** (`math.sqrt(x)`)
- If the next token is `(` → **function call** (`print("hello")`)
- Otherwise → **variable reference** (`x`)

---

## 4. Stage 3 — AST

**File:** `src/ast/ast.rs`

The AST is a tree of plain Rust structs and enums — no spans or source
locations yet.

### Node types

```
Program
├── imports: Vec<Import>            // import aion.math; import std.io;
├── functions: Vec<Function>        // fn main() { ... }
└── user_modules: Vec<UserModule>   // resolved imports (filled later)

Import { path: Vec<String> }
  └── helpers: is_stdlib(), is_std(), is_user(), module_name()

Function { name: String, body: Vec<Expr> }

UserModule { name: String, functions: Vec<Function> }

Expr (enum)
├── StringLiteral(String)
├── IntLiteral(i64)
├── FloatLiteral(f64)
├── VarDef { name, type_annotation: Option<String>, value: Option<Box<Expr>> }
├── VarRef(String)
├── FuncCall { name, args: Vec<Expr> }
└── ModuleCall { module, func, args: Vec<Expr> }
```

### Import categories

Imports are classified by their first path segment:

| Pattern            | Kind         | Resolution              |
|--------------------|--------------|-------------------------|
| `import aion.math` | C-backed     | Handled at LLVM codegen |
| `import std.io`    | Aion stdlib  | Embedded in binary      |
| `import utils`     | User module  | Loaded from disk        |

---

## 5. Stage 4 — Prelude & Import Resolution

**Files:** `src/main.rs`, `src/resolver.rs`, `src/stdlib.rs`

### Prelude

Before resolving imports, the compiler injects the **prelude** — a special
Aion source file (`stdlib/prelude.aion`) that is automatically available in
every program without any import statement.

The prelude source is embedded in the compiler binary via `include_str!()` and
parsed into a `UserModule` named `"prelude"`. Its functions become accessible
from any user code.

Currently `print()` and `println()` are compiler built-ins (special-cased in
codegen), so the prelude is mostly a documentation placeholder. Future
functions — like `assert()` or string helpers — will live here.

### Import resolution (`resolver.rs`)

The resolver walks every `Import` node and fills the program's `user_modules`:

1. **`aion.*` (C-backed)** — Skipped. These are declared as LLVM externs
   during codegen and linked from `libaion_runtime.a`.

2. **`std.*` (Aion stdlib)** — The compiler looks up the module name in
   `stdlib.rs`, which returns embedded source via `include_str!()`. That
   source is parsed and added as a `UserModule`.

3. **User modules** — The resolver searches two locations on disk:
   - `<source_dir>/<name>.aion`
   - `<source_dir>/lib/<name>.aion`

   The first match wins. The file is read, parsed, and added as a `UserModule`.

---

## 6. Stage 5 — LLVM Code Generation

**Files:** `src/compiler/codegen.rs`, `src/compiler/compiler.rs`,
`src/compiler/stdlib_registry.rs`

This is where the AST becomes LLVM IR via the **Inkwell** crate (safe Rust
bindings to the LLVM C API).

### 6.1 — Runtime declaration

The compiler first declares all Aion runtime C functions as LLVM externals.
This is done from a **data table** — a const array of `(symbol, param, return)`
tuples:

```rust
const TABLE: &[(&str, ParamKind, RetKind)] = &[
    ("aion_print",         ParamKind::Ptr, RetKind::Void),
    ("aion_print_int",     ParamKind::I64, RetKind::Void),
    ("aion_println",       ParamKind::Ptr, RetKind::Void),
    ("aion_panic",         ParamKind::Ptr, RetKind::Void),
    ("aion_alloc",         ParamKind::I64, RetKind::Ptr),
    // ...
];
```

The result is a `HashMap<&str, FunctionValue>` — looked up by C symbol name
at codegen time. To add a new runtime function, you just add a row to the
table.

### 6.2 — Stdlib declaration

For each `import aion.*`, the compiler queries
`stdlib_registry::registry(module_name)`. This returns a HashMap of
`StdlibFunc` descriptors (C symbol, arity, return type). Each function is
declared as an LLVM extern.

The registry uses a declarative macro for concise definitions:

```rust
"math" => stdlib_module! {
    sqrt  => "aion_math_sqrt"  (1) -> f64,
    pow   => "aion_math_pow"   (2) -> f64,
}
```

### 6.3 — Function compilation

Both top-level functions and user-module functions go through the same
`compile_fn_body()` codegen path. A `FnKind` enum controls the differences:

| `FnKind`    | LLVM name    | Return type   |
|-------------|-------------|---------------|
| `TopLevel`  | `main`      | `i32` (returns 0) |
| `Module`    | `utils_greet` | `void`        |

### 6.4 — Expression compilation

`compile_expr()` is a big match over `Expr` variants:

| Expr                | LLVM IR produced                         |
|---------------------|------------------------------------------|
| `StringLiteral`     | Global string constant → pointer         |
| `IntLiteral`        | `i64` constant                           |
| `FloatLiteral`      | `f64` constant                           |
| `VarDef`            | `alloca` + optional `store`              |
| `VarRef`            | `load` from alloca'd pointer             |
| `FuncCall("print")` | Type-dispatched call to C runtime        |
| `FuncCall(other)`   | Lookup in module_fns → LLVM `call`       |
| `ModuleCall`        | Qualified lookup → LLVM `call`           |

### 6.5 — print/println type dispatch

`print()` and `println()` are **compiler built-ins** — not regular functions.
When codegen sees a `FuncCall` named `"print"` or `"println"`, it inspects the
LLVM value type of the argument and dispatches to the correct C function:

```
                  ┌──── is_pointer_value() ──→ aion_print / aion_println
argument value ───┼──── is_int_value()     ──→ aion_print_int / aion_println_int
                  └──── else (float)       ──→ aion_print_float / aion_println_float
```

- `print(x)` — prints without a trailing newline
- `println(x)` — prints with a trailing newline
- `println()` — prints just a newline (zero-arg special case)

This logic lives in the `emit_print_call()` helper function.

### 6.6 — Object file emission

After codegen, the LLVM module is written to a native object file
(`.obj` on Windows, `.o` on Unix) using LLVM's `TargetMachine`.

---

## 7. Stage 6 — Runtime Compilation

**File:** `src/compiler/runtime.rs`

The Aion C runtime (`runtime/aion_runtime.c`, `runtime/aion_math.c`) is
**embedded in the compiler binary** via `include_str!()`. This makes `aion.exe`
fully self-contained — it doesn't need any external files at runtime.

When compiling, the runtime builder:

1. Writes the C sources to a temp directory (`aion-runtime-cache/`)
2. Compiles each with `gcc -c -O2`
3. Archives into `libaion_runtime.a` with `ar rcs`
4. **Caches** the result — only recompiles if the embedded C source changes

Only the C modules actually imported are compiled. If the user doesn't
`import aion.math;`, `aion_math.c` is not compiled.

---

## 8. Stage 7 — Linking

**File:** `src/compiler/linker.rs`

The linker invokes the system C compiler to produce the final native binary:

```
gcc main.obj libaion_runtime.a -lm -o main.exe
```

It tries multiple linkers in order: `gcc`, `clang` (and `cc` on Unix).
The `-lm` flag is always passed so C math functions resolve correctly.

---

## 9. Stage 8 — Execution

After linking, the compiler **automatically runs** the produced binary and
exits with its exit code. This makes the developer loop:

```
cargo run -- main.aion
```

…which compiles and immediately runs the Aion program.

Use `--emit-ir` to print LLVM IR instead of compiling:

```
cargo run -- main.aion --emit-ir
```

---

## 10. The Aion Language (Current Subset)

### Hello World

```aion
fn main() {
    println("Hello, world!")
}
```

### Variables

```aion
fn main() {
    // Inferred type:
    x ::= 42          // Int
    y ::= 3.14        // Float
    name ::= "Alice"  // String (pointer)

    // Explicit type:
    a : Int := 10
    b : Float := 2.5
}
```

Three forms:
- `x ::= value` — infer type from value
- `x : Type := value` — explicit type with value
- `x : Type` — explicit type, no initial value (uninitialized)

Supported types: `Int` (i64), `Float` (f64). Strings are pointers.

### Functions

```aion
fn main() {
    println("Hello")
}
```

All functions currently take no parameters and return nothing (except `main`,
which returns i32 0 to the OS).

### Imports and modules

```aion
import aion.math;       // C-backed math functions
import std.io;          // Aion stdlib module
import utils;           // user module from utils.aion or lib/utils.aion

fn main() {
    math.sqrt(144.0)    // qualified call
}
```

### Built-in functions

| Function                 | Description                      |
|--------------------------|----------------------------------|
| `print("text")`         | Print string, no newline         |
| `print(42)`             | Print integer, no newline        |
| `print(3.14)`           | Print float, no newline          |
| `println("text")`       | Print string + newline           |
| `println(42)`           | Print integer + newline          |
| `println()`             | Print just a newline             |

### Comments

```aion
// This is a comment
fn main() {
    println("hi")  // inline comment
}
```

---

## 11. How To: Add a New Built-in Function

Example: adding `panic("message")`.

### Step 1 — C runtime

Add the C implementation to `runtime/aion_runtime.c`:

```c
void aion_panic(const char *msg) {
    fprintf(stderr, "[aion] panic: %s\n", msg);
    exit(1);
}
```

### Step 2 — Declare in codegen

Add a row to the `TABLE` in `codegen.rs` → `declare_runtime()`:

```rust
("aion_panic", ParamKind::Ptr, RetKind::Void),
```

This makes the function available as `rt["aion_panic"]` during codegen.

### Step 3 — Special-case in compile_expr (if needed)

If the function needs special handling (like print's type dispatch), add a
branch in the `FuncCall` arm of `compile_expr()`:

```rust
if name == "panic" && args.len() == 1 {
    let val = compile_expr(..., &args[0], ...);
    let f = rt["aion_panic"];
    builder.build_call(f, &[val.into()], "call");
    return None;
}
```

If it's a normal function with no special cases, skip this step — it will be
found via the general function lookup.

### Step 4 — IDE stub (optional)

Add the declaration to `stdlib/builtins.aion`:

```aion
@builtin
fn panic(msg) {}
```

---

## 12. How To: Add a New C-Backed Stdlib Module

Example: adding `import aion.strings;` with a `concat()` function.

### Step 1 — Write the C source

Create `runtime/aion_strings.c`:

```c
#include <string.h>
#include <stdlib.h>

const char* aion_str_concat(const char* a, const char* b) {
    size_t la = strlen(a), lb = strlen(b);
    char* result = malloc(la + lb + 1);
    memcpy(result, a, la);
    memcpy(result + la, b, lb + 1);
    return result;
}
```

### Step 2 — Register in stdlib_registry.rs

Add a new arm to the `registry()` match using the `stdlib_module!` macro:

```rust
"strings" => stdlib_module! {
    concat => "aion_str_concat" (2) -> f64,
}
```

### Step 3 — Embed the C source

In `runtime.rs`, add:

```rust
const STRINGS_SRC: &str = include_str!("../../runtime/aion_strings.c");
```

And in the `build()` function's module match:

```rust
"strings" => sources.push(("aion_strings.c", STRINGS_SRC)),
```

### Step 4 — IDE stub (optional)

Create `stdlib/strings.aion`:

```aion
@runtime("aion_str_concat")
fn concat(a, b) {}
```

That's it. Users can now write:

```aion
import aion.strings;

fn main() {
    strings.concat("hello", " world")
}
```

---

## 13. How To: Add a New Aion Stdlib Module

These are pure Aion modules embedded in the compiler binary.

Example: adding `import std.greet;`.

### Step 1 — Write the Aion source

Create `stdlib/greet.aion`:

```aion
fn hello() {
    println("Hello from the greet module!")
}
```

### Step 2 — Register in stdlib.rs

Add the `include_str!()` line in the `get()` function:

```rust
"greet" => Some(include_str!("../stdlib/greet.aion")),
```

And add it to the `available()` list:

```rust
&["io", "name", "greet"]
```

Done. Users can now write:

```aion
import std.greet;

fn main() {
    greet.hello()
}
```

---

## 14. How To: Add a New Type

Example: adding `Bool`.

### Step 1 — No lexer changes needed

Types are just `Ident` tokens. `Bool` will be lexed as `Ident("Bool")`.

### Step 2 — Handle in codegen

In `compile_expr()`, in the `VarDef` match arm, add a branch:

```rust
(Some("Bool"), _) => {
    let ty = context.bool_type().as_basic_type_enum();
    let val = value.as_ref().map(|v| {
        compile_expr(...)
    });
    (ty, val)
}
```

### Step 3 — Add literal parsing (if applicable)

If the type needs its own literal syntax (e.g., `true`/`false`), add:

1. A new `Expr` variant: `Expr::BoolLiteral(bool)`
2. Parser logic in `parse_expr()` to recognize `Ident("true")`/`Ident("false")`
3. Codegen in `compile_expr()`: `context.bool_type().const_int(v as u64, false)`

---

## 15. How To: Add New Syntax

General steps for adding any new language construct:

### 1. AST node

Add a variant to the `Expr` enum in `src/ast/ast.rs`:

```rust
pub enum Expr {
    // ...existing variants...
    IfElse {
        condition: Box<Expr>,
        then_body: Vec<Expr>,
        else_body: Vec<Expr>,
    },
}
```

### 2. Lexer tokens (if needed)

If the syntax uses new keywords or operators, add them to `Token` in
`src/lexer/lexer.rs`:

```rust
#[token("if")]
If,

#[token("else")]
Else,
```

### 3. Parser rule

Add a parsing method in `src/parser/parser.rs` and call it from
`parse_statement()` or `parse_expr()`:

```rust
fn parse_if(&mut self) -> Expr {
    self.expect(Token::LParen, "...");
    let cond = self.parse_expr();
    self.expect(Token::RParen, "...");
    self.expect(Token::LBrace, "...");
    let then_body = self.parse_body();
    // ... etc
}
```

### 4. Codegen

Handle the new AST node in `compile_expr()` in `src/compiler/codegen.rs`:

```rust
Expr::IfElse { condition, then_body, else_body } => {
    // emit LLVM basic blocks, conditional branch, etc.
}
```

### 5. Test

Create or update a `.aion` test file and run:

```
cargo run -- test.aion
```

---

## 16. Project Layout

```
aion/
├── src/
│   ├── main.rs              # CLI driver — arg parsing, pipeline orchestration
│   ├── errors.rs            # Coloured error/warning/info reporting
│   ├── stdlib.rs            # Embedded Aion stdlib registry (include_str!)
│   ├── resolver.rs          # Module import resolution
│   ├── lexer/
│   │   ├── mod.rs
│   │   └── lexer.rs         # Token enum with #[derive(Logos)]
│   ├── ast/
│   │   ├── mod.rs
│   │   └── ast.rs           # AST node types (Expr, Function, Program, …)
│   ├── parser/
│   │   ├── mod.rs
│   │   └── parser.rs        # Recursive-descent parser
│   └── compiler/
│       ├── mod.rs
│       ├── compiler.rs       # LLVM Context/Module/Builder orchestrator
│       ├── codegen.rs        # AST → LLVM IR lowering (the big file)
│       ├── stdlib_registry.rs# C-backed stdlib function descriptors
│       ├── runtime.rs        # Embedded C compilation + caching
│       └── linker.rs         # Native binary linking (gcc/clang)
├── runtime/
│   ├── aion_runtime.c        # Core C runtime (print, panic, alloc, …)
│   ├── aion_math.c           # Math module C implementation
│   └── aion_sockets.c        # (future) Networking
├── stdlib/
│   ├── prelude.aion           # Auto-loaded (no import needed)
│   ├── io.aion                # import std.io
│   ├── name.aion              # import std.name
│   ├── builtins.aion          # IDE stubs for built-ins
│   └── math.aion              # IDE stubs for aion.math
├── lib/
│   └── greeter.aion           # User module: import greeter
├── docs/
│   ├── architecture.md        # High-level architecture overview
│   ├── design.md              # This file
│   └── concepts/
│       └── oop.aion           # OOP design concepts
└── main.aion                  # Example Aion program
```

---

## 17. Error Reporting

**File:** `src/errors.rs`

All compiler errors go through a centralized reporting system with:

- **Phase tagging** — each error knows where it happened: `lex`, `parse`,
  `compile`, or `link`
- **Hints** — optional suggestions shown below the error
- **ANSI colours** — errors in red, warnings in yellow, info in blue,
  success in green

Functions:

| Function            | Description                              |
|---------------------|------------------------------------------|
| `fatal(phase, msg)` | Print error + exit(1)                    |
| `fatal_with_hint()` | Print error with hint + exit(1)          |
| `warn()`            | Print yellow warning, continue           |
| `info()`            | Print blue info message                  |
| `success()`         | Print green success message              |

Example output:

```
[aion] compiling main.aion
error[compile]: Unknown standard-library module: 'aion.strings'
  hint: Available modules: aion.math
```

---

## 18. IDE Stubs

**Files:** `stdlib/builtins.aion`, `stdlib/math.aion`

Stub files provide **declaration-only** function signatures annotated with
`@builtin` or `@runtime("c_symbol")`. They are never compiled — they exist
purely for future language-server support:

- Go-to-Definition on `print(...)` would jump to `stdlib/builtins.aion`
- Hover on `math.sqrt()` would show the declared signature from `stdlib/math.aion`

The stubs are accessible via `stdlib::get_stub(name)` in `src/stdlib.rs`.
