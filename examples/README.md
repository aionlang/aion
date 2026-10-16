# Examples

Every file here runs with `aion <file>`; run them from anywhere:

```text
aion examples/tour.aion
aion examples/fibonacci.aion
```

Each example ends with the output it prints, as `// > ` comment lines, so you can read a program
and see what it does without running it. `cargo test` runs all of them and fails if the output
changes (`// stdin:` lines in a header feed the program input, `// args:` gives it arguments).

## Start here

| File | What it shows |
|------|---------------|
| [hello.aion](hello.aion) | the smallest program |
| [tour.aion](tour.aion) | the whole language in about 80 lines: variables, `if`/`when`, loops, functions, types, lambdas, nulls, errors |
| [script.aion](script.aion) | a script with no `main`: top-level statements, `when` as an expression |

## Algorithms and puzzles

| File | What it shows |
|------|---------------|
| [fibonacci.aion](fibonacci.aion) | recursion, loops, memoisation with a `Map`, how `Int` wraps around |
| [primes.aion](primes.aion) | trial division, the sieve of Eratosthenes (`Array`), factorisation |
| [fizzbuzz.aion](fizzbuzz.aion) | `when`, a rule table as data |
| [sorting.aion](sorting.aion) | bubble, insertion, quick and merge sort; generic sorting with `Comparable`; sorting objects |
| [bigint.aion](bigint.aion) | arbitrary-precision arithmetic built from a list of "limbs" (100!, 2^1000, fib 1000) |
| [collatz.aion](collatz.aion) | loops, a cache, a histogram |
| [hanoi.aion](hanoi.aion) | recursion and extension methods |
| [n_queens.aion](n_queens.aion) | backtracking with closures |
| [matrix.aion](matrix.aion) | linear algebra: multiply, determinant, solve equations, fast Fibonacci |
| [game_of_life.aion](game_of_life.aion) | Conway's Game of Life on a wrapping grid |
| [mandelbrot.aion](mandelbrot.aion) | the Mandelbrot set in ASCII |
| [maze.aion](maze.aion) | breadth-first search through a maze that is a triple-quoted string |
| [tictactoe.aion](tictactoe.aion) | minimax: a perfect player |
| [brainfuck.aion](brainfuck.aion) | an interpreter for a tiny language |
| [calculator.aion](calculator.aion) | tokenizer, recursive-descent parser, syntax tree with inheritance, evaluator |
| [simulations.aion](simulations.aion) | seeded random numbers: dice, shuffling, estimating pi, the birthday paradox |

## The language, one topic at a time

| File | Topic |
|------|-------|
| [closures.aion](closures.aion) | lambdas, closures that remember variables, composition, currying, memoising, `map`/`filter`/`fold` |
| [functions.aion](functions.aion) | `cond ? a : b`, defaults that use earlier parameters, `vararg` and `*spread`, functions inside functions |
| [tuples.aion](tuples.aion) | `(a, b)` values, `a, b := pair`, swapping, `for (k, v) in …`, `zip`/`enumerate`, tuples as map keys |
| [data_types.aion](data_types.aion) | `data type` (equality, `copy`, destructuring, map keys), `?: return` / `break` / `continue` |
| [ranges.aion](ranges.aion) | ranges as values: `1..=5`, `step`, `downTo`, `map`/`filter` over a range |
| [operators.aion](operators.aion) | `+ - * / < ==` on your own types (`plus`, `times`, `compareTo`, `equals`): fractions and vectors |
| [generics.aion](generics.aion) | `Stack<T>`, `Queue<T>`, `Pair<A, B>`, constraints (`T: Comparable<T>`), generic interfaces |
| [interfaces.aion](interfaces.aion) | interfaces satisfied by shape, dispatch at run time, mixed lists |
| [inheritance.aion](inheritance.aion) | `open`, `override`, `super`, `is`, `as` |
| [nullability.aion](nullability.aion) | `T?`, `?.`, `?:`, `!!`, smart casts |
| [errors.aion](errors.aion) | `panic`, `try`/`catch`, `defer`, retries, validation |
| [tasks.aion](tasks.aion) | `launch`, `async`/`await`, channels, pipelines, worker pools, deadlock detection |
| [strings.aion](strings.aion) | string and `Char` methods, ciphers, run-length encoding, Roman numerals |
| [collections.aion](collections.aion) | `List`, `Map`, `Set`, `Array`, slicing, grouping, sorting |
| [numbers.aion](numbers.aion) | `Int` vs `Float`, formatting, overflow, bit tricks, the `math` module |

## Programs that use the standard library

| File | Modules |
|------|---------|
| [bank.aion](bank.aion) | a bank with all-or-nothing transfers (types, `try`, money in cents) |
| [sales_report.aion](sales_report.aion) | `csv` and `json`: parse a table, summarise, export |
| [log_analyzer.aion](log_analyzer.aion) | `regex` with named groups, `time` |
| [sqlite_library.aion](sqlite_library.aion) | `sqlite`: tables, joins, parameters, transactions *(needs libsqlite3-dev)* |
| [http_notes.aion](http_notes.aion) | `http` and `json`: a REST API and its client in one program |
| [checksums.aion](checksums.aion) | `hash`, `encoding`, `zlib` *(needs zlib1g-dev)* |
| [dates.aion](dates.aion) | `time`: formatting, parsing, calendar arithmetic, a month calendar |
| [files.aion](files.aion) | `fs` and `path`: a temporary workspace, a recursive tree walk |
| [cli_tool.aion](cli_tool.aion) | `cli`: options, flags, repeatable options, `--help` |
| [guess_number.aion](guess_number.aion) | reading input with `readLine()`, `random` |
| [wordcount.aion](wordcount.aion) | collections and lambdas in a short script |
| [animals.aion](animals.aion) | a type with a constructor and methods added from outside |
| [echo_server.aion](echo_server.aion) | the older `sockets` module (runs until stopped) |
| [gc_stress.aion](gc_stress.aion) | a million short-lived objects; try it with `AION_GC_STRESS=1` and a smaller count |

## Projects

A project is a folder with an `aion.project` file; run it from inside with `aion`.
See [docs/projects.md](../docs/projects.md) and [docs/packages.md](../docs/packages.md).

| Folder | What it shows |
|--------|---------------|
| [projects/todo](projects/todo) | an application over several modules in `lib/`, JSON persistence, a command-line interface |
| [projects/adventure](projects/adventure) | a text game whose world is a JSON data file |
| [projects/geometry](projects/geometry) | a *library package* with its own tests (`aion tests/check.aion`) |
| [projects/geometry-demo](projects/geometry-demo) | a program that depends on that package |
| [projects/c-extension](projects/c-extension) | C code compiled into the program (`c = [...]`), bound with `extern fn` |
| [projects/greeter](projects/greeter) | the smallest project with a dependency |
| [projects/modules](projects/modules) | plain imports between files, with no project file |
