<div align="center">
<img src="docs/img/aion_banner.png" alt="aion logo" width=300>
</div>

<div align="center">

<img src="https://img.shields.io/badge/status-Turing%20Complete-yellow">
<a href="https://github.com/aionlang/aion">
<img src="https://img.shields.io/badge/github-Aion Repository-blue?logo=github">
</a>
</div>

## Aion language

A small, statically typed, garbage-collected language that compiles to native
binaries through LLVM.

```aion
import std.math;

type Animal(it.name, it.age) {
    name: String
    age: Int

    fn describe() -> String => "${it.name} is ${it.age}"
}

fn main() {
    pet := Animal("Fido", 5)
    println(pet.describe())
    println("sqrt(2) = ${math.sqrt(2)}")
}
```

## Quick start

You need **Rust** and **clang 15+** (no LLVM development libraries).

```bash
cargo install --path .       # puts `aion` on your PATH

aion examples/hello.aion          # compile and run
aion build examples/hello.aion    # …or just produce ./hello
aion emit-ir examples/hello.aion  # print the generated LLVM IR
```

More programs live in [`examples/`](examples). Run the test-suite with
`cargo test`.

## How it works

```
 source.aion
     │  lexer + parser        src/syntax/
     ▼
   syntax tree
     │  loader (imports)      src/loader.rs
     ▼
   modules
     │  semantic analysis     src/sema/       names, types, implicit conversions
     ▼
   typed IR (HIR)
     │  code generation       src/codegen.rs  → LLVM IR as text
     ▼
   program.ll  +  runtime/*.c  +  stdlib/*.c
     │  clang                 src/toolchain.rs
     ▼
   native executable
```

The compiler emits LLVM IR *as text* and lets `clang` finish the job, so there
is no dependency on a particular LLVM version or on LLVM development packages.
The runtime (garbage collector, strings, printing) is a small C library that is
embedded in the compiler binary.

See **[docs/architecture.md](docs/architecture.md)** for the details.

## Documentation

| | |
|---|---|
| [docs/language.md](docs/language.md) | the language: syntax, types, modules |
| [docs/stdlib.md](docs/stdlib.md) | the standard library, and how to add C-backed functions with `extern fn` |
| [docs/architecture.md](docs/architecture.md) | compiler internals, memory model, garbage collector |
| [docs/LOCAL-BUILDING.MD](docs/LOCAL-BUILDING.MD) | setting up a development machine |
| [CHANGELOG.MD](CHANGELOG.MD) | what changed between versions |

## Project layout

```
aion/
├── src/
│   ├── main.rs        CLI driver
│   ├── diag.rs        spans, source map, error rendering
│   ├── syntax/        lexer, AST, parser
│   ├── loader.rs      finds and parses imported modules
│   ├── sema/          name resolution + type checking → typed IR
│   ├── codegen.rs     typed IR → LLVM IR text
│   └── toolchain.rs   runs clang
├── runtime/           always-linked C runtime (GC, strings, printing)
├── stdlib/            standard library: <module>.aion [+ <module>.c]
├── examples/          sample programs
├── benchmarks/        count-to-100M in Aion, C, Node and Python
└── tests/             end-to-end tests (programs/, errors/)
```

## License

See [LICENSE.md](LICENSE.md).
