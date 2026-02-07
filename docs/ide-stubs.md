# Aion Stub Declarations — IDE Architecture

This document explains how Aion provides "Go to Definition" support for
built-in and standard-library functions, similar to how Python uses `.pyi`
stub files and TypeScript uses `.d.ts` declaration files.

---

## The Problem

Built-in functions like `print()` and C-backed stdlib functions like
`math.sqrt()` are implemented in C (`runtime/aion_runtime.c`,
`runtime/aion_math.c`).  A developer using an IDE wants to press
**"Go to Definition"** on `print(...)` and land in an `.aion` file that
shows the function's signature, parameters, and documentation — not raw C.

## The Solution: Stub Files

Every built-in or C-backed function has a **stub declaration** in an
`.aion` file under `stdlib/`:

```
stdlib/
├── builtins.aion   ← print, panic, alloc, free  (always available)
├── math.aion       ← sqrt, abs, sin, cos, …     (import aion.math;)
├── io.aion         ← hello, newline, …           (import std.io;)
└── name.aion       ← name                        (import std.name;)
```

### Stub format

Stubs use structured comment annotations that a language server can parse:

```aion
// @builtin
// @runtime "aion_print"
// @param message String
// @return Void
//
// Prints a string to standard output, followed by a newline.
//
fn print(message: String) {}
```

| Annotation             | Meaning                                  |
|------------------------|------------------------------------------|
| `@builtin`             | Compiler-intrinsic, not user-defined      |
| `@runtime "<symbol>"`  | The C function symbol backing this        |
| `@param <name> <Type>` | Parameter name and type                   |
| `@return <Type>`       | Return type (`Void` if omitted)           |

The empty body `{}` makes the stub parseable by the current Aion parser
(which doesn't support `extern` syntax yet).  The language server should
treat `@builtin` functions as declaration-only and never attempt to
compile them.

---

## How the Compiler Uses Stubs

The compiler itself does **not** compile stub files.  The mapping is:

| Function category         | Compiler source of truth       | Stub file (IDE)        |
|---------------------------|-------------------------------|------------------------|
| Built-ins (`print`, etc.) | `codegen.rs` → C runtime      | `stdlib/builtins.aion` |
| `import aion.math;`       | `stdlib_registry.rs` → C libs | `stdlib/math.aion`     |
| `import std.io;`          | `stdlib.rs` → embedded `.aion`| `stdlib/io.aion`       |

Stubs are embedded into the binary via `stdlib.rs`:

```rust
pub fn get_stub(name: &str) -> Option<&'static str> {
    match name {
        "builtins" => Some(include_str!("../stdlib/builtins.aion")),
        "math"     => Some(include_str!("../stdlib/math.aion")),
        _ => None,
    }
}
```

---

## Language Server Integration

When building an IDE extension or language server, use this flow:

### Go to Definition

```
User clicks "Go to Definition" on print(...)
  │
  ├─ Is it a built-in keyword?  (print, panic)
  │   └─ Look up in stdlib/builtins.aion → jump to the fn declaration
  │
  ├─ Is it a module call?  (math.sqrt)
  │   └─ Resolve module name → look up stdlib/<module>.aion → jump to fn
  │
  └─ Is it a user function?  (utils.greet)
      └─ Resolve to the user's .aion file → jump to fn definition
```

### Hover Documentation

Parse the `@param`, `@return`, and free-text comments above each `fn` in
the stub file.  Display them in a hover tooltip:

```
fn print(message: String) -> Void

Prints a string to standard output, followed by a newline.
```

### Auto-Complete

1. **Unqualified context** — offer all functions from `builtins.aion`
2. **After `math.`** — offer all functions from `math.aion`
3. **After `io.`** — offer all functions from `io.aion`

---

## Adding a New Built-in or Stdlib Function

### New built-in (always-available, no import needed)

1. Implement in C: add the function to `runtime/aion_runtime.c`
2. Declare in LLVM: add to `codegen::declare_runtime()`
3. **Add stub** to `stdlib/builtins.aion` with `@builtin` + `@runtime` annotations
4. Wire up in the parser/codegen as needed

### New C-backed stdlib function (requires `import aion.<module>;`)

1. Implement in C: add to `runtime/aion_<module>.c`
2. Register in `compiler/stdlib_registry.rs`
3. **Add stub** to `stdlib/<module>.aion` with annotations
4. Register in `stdlib.rs :: get_stub()`

### New pure-Aion stdlib function (requires `import std.<module>;`)

1. Write the function directly in `stdlib/<module>.aion`
2. Register in `stdlib.rs :: get()`
3. No separate stub needed — the `.aion` file IS the definition

---

## Future: `extern fn` Syntax

Once the parser supports an `extern` keyword, stubs can use a cleaner
declaration syntax without empty bodies:

```aion
extern fn print(message: String) -> Void
extern fn sqrt(x: Float) -> Float
```

This eliminates the need for `@builtin` comment annotations and makes
the stubs feel like a natural part of the language grammar.
