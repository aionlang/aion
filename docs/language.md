# The Aion language

Aion reads like a scripting language and compiles to a native binary.  Every
`aion` example on this page is compiled by the test-suite.

## Scripts

A file is a list of statements, run top to bottom.  There is no `main` to write.

```aion
println("Hello from Aion!")
name := "world"
println("Hello, $name!")
```

`fn main()` still works and, if present, runs after the top-level statements.
A first line starting with `#!` is ignored, so a file can be an executable
script (`#!/usr/bin/env aion`).  Statements need no terminator; a line break ends
a statement (see [Line breaks](#line-breaks)).

## Types

| Type          | Meaning                                       | Default |
|---------------|-----------------------------------------------|---------|
| `Int`         | 64-bit signed integer (wraps on overflow)     | `0`     |
| `Float`       | 64-bit floating point                         | `0.0`   |
| `Bool`        | `true` / `false`                              | `false` |
| `String`      | immutable UTF-8 text                          | `""`    |
| `Point`       | an instance of a user-defined `type Point`    | null    |
| `(Int) -> Int`| a function value; `Unit` means "no value"    | null    |

An `Int` is accepted wherever a `Float` is expected and widens automatically.
There are no other implicit conversions.

## Variables

```aion
a := 12            // type inferred
b: Int = 12        // explicit type
c: Float           // declared, starts at 0.0
a = 5              // assignment
a += 1             // also -=  *=  /=
println(a, b, c)
```

`x: T := v` is accepted too.  A variable declared at the **top level** of a file
is a module variable: every function in the file can read and assign it.

```aion
count := 0
fn bump() { count += 1 }
bump()
bump()
println(count)     // 2
```

Top-level code runs in order, so a top-level statement can only use variables
defined above it.  Functions may be defined anywhere.

Blocks open a new scope; an inner declaration shadows an outer one, and declaring
the same name twice in one scope is an error.

## Operators

From loosest to tightest binding:

| Operators          | Operands                                              |
|--------------------|-------------------------------------------------------|
| `a..b`  `a..=b`    | Int ranges, only in `for` and after `in`              |
| `\|\|`             | Bool                                                  |
| `&&`               | Bool                                                  |
| `==` `!=`          | numbers, Bool, String (by value), objects (by identity)|
| `<` `<=` `>` `>=` `in` | numbers; `x in a..b` tests a range                |
| `+` `-`            | numbers; `+` also joins two Strings                   |
| `*` `/` `%`        | numbers                                               |
| unary `-` `!`      | number / Bool                                         |

`&&` and `||` short-circuit.  Mixing `Int` and `Float` computes in `Float`.
Integer division by zero stops the program with a panic.

```aion
println(3 in 1..5, 5 in 1..5, 5 in 1..=5)   // true false true
println(7 / 2, 7 % 2, 7 / 2.0)              // 3 1 3.5
```

## Control flow

`if` and `when` are expressions: the value of a branch is its last expression.

```aion
n := 7
if n < 0 {
    println("negative")
} else if n == 0 {
    println("zero")
} else {
    println("positive")
}

kind := if n % 2 == 0 { "even" } else { "odd" }

size := when n {
    0 -> "zero"
    1, 2, 3 -> "small"
    in 4..10 -> "medium"
    else -> "large"
}
println(kind, size)     // odd medium
```

`when` without a subject takes conditions: `when { n < 0 -> "neg"  else -> "pos" }`.
Used as a value, `if` and `when` need an `else`.  Parentheses around conditions
are optional.

```aion
i := 0
while i < 3 {
    i += 1
}

total := 0
for k in 1..=4 { total += k }          // 1, 2, 3, 4
for k in 0..10 {
    if k % 2 == 0 { continue }         // skip evens
    if k > 7 { break }
    print(k, "")                       // 1 3 5 7
}
println(i, total)
```

`a..b` excludes `b`; `a..=b` includes it.  The loop variable is fresh on every
iteration.

## Functions

```aion
fn add(a: Int, b: Int) -> Int {
    return a + b
}

fn double(x: Int) = x * 2                       // one-liner; return type inferred
fn greet(name: String, greeting: String = "Hello") = "$greeting, $name!"

println(add(1, 2), double(21))
println(greet("Ann"))                           // Hello, Ann!
println(greet("Bob", greeting: "Hi"))           // Hi, Bob!
println(greet(greeting: "Yo", name: "Cy"))      // named arguments, any order
```

* Parameters need types; defaults must be literals.
* After a named argument, all following arguments must be named.  Arguments are
  evaluated in the order they are written.
* `= expr` infers the return type; write `-> Type` to state it.  With a block
  body, no `->` means "returns nothing", and a function that declares `-> T`
  must return on every path.
* A function that calls itself (or two that call each other) needs `-> Type` on
  at least one of them.

`Unit` names "no value" and is only valid as a return type, mainly in function
types: `(Int) -> Unit`.

## Strings

```aion
name := "Aion"
n := 3
println("Hello, $name! ${n} + 1 = ${n + 1}")
println("tab:[\t] quote:[\"] backslash:[\\] dollar:[\$]")
println("nested: ${"inner ${name}"}")
```

`$name` interpolates a variable and `${...}` any expression, including strings
of its own.  Escapes: `\n \t \r \0 \\ \" \$`.  A `$` not followed by a name or `{`
is just a dollar sign.  `Int`, `Float`, `Bool` and objects convert to text in
`${}`; build text with interpolation, not `+`.

## Types

```aion
type User(name: String, age: Int = 0) {      // parameters are also fields
    active: Bool = true                      // more fields, with initialisers
    log: String = ""

    init { log = "created $name" }           // runs after the fields are set

    fn birthday() {
        age += 1                             // fields are in scope by name
        log += "; birthday"
    }
    fn isAdult() = age >= 18
}

fn User.label() = "$name($age)"              // methods can also be written outside

u := User("ann", 17)
u.birthday()
println(u.isAdult(), u.label())              // true ann(18)
println(User("bob").age)                     // 0
println(u)                                   // User(name=ann, age=18, active=true, log=created ann; birthday)
```

* **Fields** come from the parenthesised list and from `name: Type [= value]`
  lines in the body.  Fields not initialised keep their default.
* Inside a method, `this` is the instance and its fields and methods are in
  scope by bare name (`age`, not `this.age`; use `this.age` if a parameter has
  the same name).
* Without a primary list you can write your own constructor:

```aion
type Counter {
    count: Int
    label: String

    constructor(label: String) {
        this.label = label
    }
    fn inc() { count += 1 }
    fn show() = "$label=$count"
}

c := Counter("hits")
c.inc()
println(c.show())                            // hits=1
```

  A type with a primary list cannot also declare `constructor`.  A bare `return`
  inside a constructor ends construction early.
* Instances are **references**: `b := a` makes both names refer to the same
  object, and a function receiving an object can modify it.
* **Printing**: `println(u)` and `"$u"` use the type's own `toString()` method if
  it has one, otherwise `Name(field=value, …)`.  Object fields that were never
  set print as `null`.
* A field holding an object starts out null; using it before assigning it stops
  the program with a `null reference` panic.  `Type?` is accepted but nullability
  is not tracked yet.
* Inheritance is not supported.

## Function values and lambdas

A function type is written `(Int, String) -> Bool`.  A lambda is `{ params -> body }`;
its value is the last expression.

```aion
fn apply(f: (Int) -> Int, x: Int) = f(x)

println(apply({ x -> x * x }, 7))        // 49
println(apply({ it + 1 }, 41))           // 42: `it` names the only parameter
println(apply({ x: Int -> x - 1 }, 10))  // 9: parameters can be annotated

fn repeat(n: Int, action: (Int) -> Unit) {
    for i in 0..n { action(i) }
}
repeat(3) { println("step", it) }        // a lambda after the parentheses

fn double(n: Int) = n * 2
println(apply(double, 21))               // declared functions are values too
```

A lambda gets its parameter types from the function type it is passed to or
assigned to; `{ it + 1 }` only works where one is expected.  Write the types
yourself otherwise (`{ x: Int -> x + 1 }`).  Use `{ -> … }` for a lambda with no
parameters.  `return` inside a lambda returns from the lambda.

Lambdas are **closures**: they remember the variables they use, and share them.

```aion
fn counter() -> () -> Int {
    n := 0
    return { n += 1; n }          // this `n` belongs to this particular counter
}
a := counter()
b := counter()
println(a(), a(), a(), b())       // 1 2 3 1

fn adder(k: Int) = { x: Int -> x + k }
println(adder(5)(1))              // 6
```

A variable that a closure captures and anything reassigns is shared between the
closure and the code that created it; one that is never reassigned is copied.
Each loop iteration has its own loop variable, so closures created in a loop see
the value from their own iteration.  Function values can be stored in variables,
fields and module variables and called like functions: `onClick(3)`,
`button.onClick(3)`, `make()(3)`.

## Line breaks

A line break ends a statement, with these exceptions:

* inside parentheses, newlines are ignored;
* a line may continue after an operator (`total = a +` ⏎ `b`);
* a line may **start** with `.`, `&&` or `||` and continue the previous one;
* `else` may start a line after a closing brace.

```aion
x := 10
-3                          // a separate statement, not `10 - 3`
ok := x > 5
    && x < 20               // continues the line above
text := "abc"
println(x, ok, text)
```

A `(` or `{` at the start of a line begins a new statement; it is never a call
or a trailing lambda.

## Modules

```text
import utils               // utils.aion beside this file, or in lib/
import utils as u          // …under another name
import sockets.TcpListener // the module, plus TcpListener on its own
from sockets import TcpListener, TcpStream   // only those names
import math                // the standard library
import std.math            // …explicitly (aion.math works too)
```

* `import m` makes `m.name(...)` and `m.variable` available.
* An import is looked up as a file beside the importing file (or in its `lib/`
  folder, or the project root), then among the project's
  [dependencies](projects.md), then in the standard library.  A local file
  therefore shadows a standard-library module of the same name.
* A module's top-level statements run once, before the importing program, in
  dependency order.  Its variables are visible to importers as `m.name`, and can
  be assigned (`m.label = "x"`).
* Everything a module defines is public.  Circular imports are an error.

## Built-in functions

| Call                       | Effect                                                  |
|----------------------------|---------------------------------------------------------|
| `print(a, b, …)`           | write the values separated by spaces, no newline        |
| `println(a, b, …)`         | …with a trailing newline; `println()` is just a newline |
| `panic(message)`           | print `[aion] panic: message` to stderr and exit 1      |

Arguments may be Int, Float, Bool, String or objects.  Floats print with C's
`%g` (`3.0` prints as `3`).

## Calling C

`extern fn` declares a function implemented in C — see
[stdlib.md](stdlib.md#extern-fn).

```aion
extern fn llabs(n: Int) -> Int      // bind a libc function straight from your program
println(llabs(-5))
```

## Runtime errors

The program stops with exit code 1 and a message on stderr for: division by
zero, using a null object or function, out of memory, and `panic(...)`.

## Not (yet) in the language

Arrays and maps, generics, a `null` literal, `defer`, interfaces, inheritance,
`char`, reading input.
