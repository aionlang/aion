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

A statically typed, garbage-collected language that reads like a script and
compiles to a native binary through LLVM.

```aion
import math

type Shape(name: String, sides: Int = 0) {
    fn describe() = if sides == 0 { "$name (round)" } else { "$name with $sides sides" }
}

println(Shape("square", sides: 4).describe())

for r in 1..=3 {
    println("radius", r, "area", math.pi * r * r)
}

fn twice(f: (Int) -> Int) = { x: Int -> f(f(x)) }
println(twice { it * 3 }(2))        // 18
```

No `main`, no semicolons, no type noise: types are only written on parameters and
fields.  Generics, collections, nullable types, interfaces, `defer` and `try` are
there when you need them:

```aion
interface Shape { fn area() -> Float }
type Circle(r: Float) { fn area() = 3.14159 * r * r }
type Rect(w: Float, h: Float) { fn area() = w * h }

shapes: List<Shape> = [Circle(1), Rect(2, 3)]
println(shapes.map { it.area() })

ages := {"ann": 31, "bob": 25}
println(ages.entries().filter { it.value > 30 }.map { it.key })

fn lookup(name: String) -> Int? = ages.getOrNull(name)
println(lookup("bob") ?: -1, lookup("zed") ?: -1)

try { println(10 / (ages["bob"] - 25)) } catch e { println("oops:", e) }
```

## Quick start

You need **Rust** and **clang 15+** (no LLVM development libraries).

```bash
cargo install --path .       # puts `aion` on your PATH

aion examples/tour.aion            # compile and run (re-runs are instant)
aion new myapp && cd myapp && aion # a project with an aion.project file
aion build examples/hello.aion     # produce ./hello
aion emit-ir examples/hello.aion   # print the generated LLVM IR
```

More than forty programs live in [`examples/`](examples) — algorithms, language tours, small
applications and sample projects, each ending with the output it prints (see
[examples/README.md](examples/README.md)). Run the test-suite, which runs them all, with `cargo test`.

## Batteries

The standard library covers what scripts and small tools need: text and numbers
(`StringBuilder`, `regex`, `json`, `csv`, `encoding`, `hash`), files and the machine (`fs`, `path`,
`os`, `time`, `random`, `cli`, `term`), the network (`net`, `http` client and server, TLS) and
data (`sqlite`, `zlib`, `Bytes`). Each is one `import` away; see [docs/stdlib.md](docs/stdlib.md).
Anything missing can be bound from C with one `extern fn` line, or shipped as a
[package](docs/packages.md).

## How it works

```
 source.aion
     │  lexer + parser        src/syntax/
     ▼
   syntax tree
     │  loader (imports, deps) src/loader.rs, src/project.rs
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
| [docs/language.md](docs/language.md) | the language: syntax, types, lambdas, modules |
| [docs/projects.md](docs/projects.md) | `aion.project`, dependencies, the build cache |
| [docs/packages.md](docs/packages.md) | writing, testing and publishing packages; bundling C code |
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
│   ├── project.rs     aion.project, dependencies, aion.lock
│   ├── cache.rs       build cache
│   ├── sema/          name resolution + type checking → typed IR
│   ├── codegen.rs     typed IR → LLVM IR text
│   └── toolchain.rs   runs clang
├── runtime/           always-linked C runtime (GC, strings, printing)
├── stdlib/            standard library: <module>.aion [+ <module>.c]
├── examples/          over 40 programs and 7 sample projects, each with its expected output
├── benchmarks/        count-to-100M in Aion, C, Node and Python
└── tests/             end-to-end tests (programs/, errors/)
```

## License

See [LICENSE.md](LICENSE.md).
