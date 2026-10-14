# The Aion language

A tour of what the compiler accepts today.  Every snippet here compiles.

## Basics

```aion
// a comment
fn main() {
    println("Hello from Aion!")
}
```

Programs start at `fn main()`.  It may return nothing, or an `Int` that becomes
the process exit code.  Statements are separated by newlines (a `;` is also
allowed).  Functions can be defined in any order.

## Types

| Type     | Meaning                                   | Default |
|----------|-------------------------------------------|---------|
| `Int`    | 64-bit signed integer (wraps on overflow) | `0`     |
| `Float`  | 64-bit floating point                     | `0.0`   |
| `Bool`   | `true` / `false`                          | `false` |
| `String` | immutable UTF-8 text                      | `""`    |
| `Foo`    | an instance of a user-defined `type Foo`  | null    |

An `Int` is accepted wherever a `Float` is expected and widens automatically.
There are no other implicit conversions.

## Variables

```aion
a := 12            // type inferred
b: Int := 12       // explicit type
c: Float           // declared, starts at 0.0
d ::= "text"       // same as :=

a = 5              // assignment
a += 1             // also -=  *=  /=
```

Blocks (`{ ... }`) open a new scope.  An inner `x := ...` shadows an outer `x`;
declaring the same name twice in one scope is an error.

## Operators

From loosest to tightest binding:

| Operators            | Operands                                  |
|----------------------|-------------------------------------------|
| `\|\|`               | Bool                                      |
| `&&`                 | Bool                                      |
| `==` `!=`            | numbers, Bool, String (by value), objects (by identity) |
| `<` `<=` `>` `>=`    | numbers                                   |
| `+` `-`              | numbers; `+` also joins two Strings       |
| `*` `/` `%`          | numbers                                   |
| unary `-` `!`        | number / Bool                             |

`&&` and `||` short-circuit.  Mixing `Int` and `Float` computes in `Float`.
Integer division by zero stops the program with a panic.

## Control flow

```aion
if (n < 0) {
    println("negative")
} else if (n == 0) {
    println("zero")
} else {
    println("positive")
}

while (i < 10) {
    i += 1
}
```

Conditions must be `Bool`; the parentheses are optional.

## Functions

```aion
fn add(a: Int, b: Int) -> Int {
    return a + b
}

fn double(x: Int) -> Int => x * 2      // arrow form: the expression is returned
fn log(msg: String) => println(msg)    // no `->`: returns nothing
```

Every parameter needs a type.  A function with `-> T` must return on every
path; the compiler rejects one that can fall off the end.

## Strings

```aion
name := "Aion"
println("Hello, ${name}! 1 + 2 = ${1 + 2}")
println("tab:\t quote:\" backslash:\\ dollar:\$")
```

`${...}` may contain any expression, including strings of its own.  Escapes:
`\n \t \r \0 \\ \" \$`.  `Int`, `Float` and `Bool` convert to text inside
`${}`; to build text out of other things use interpolation, not `+`.

## Types (user-defined)

```aion
type Counter {
    count: Int
    label: String

    constructor(label: String) {
        it.label = label
    }

    fn inc() {
        it.count += 1
    }

    fn show() -> String => "${it.label}=${it.count}"
}

fn Counter::reset() {          // methods can also be written outside the type
    it.count = 0
}

fn main() {
    c := Counter("hits")
    c.inc()
    println(c.show())          // hits=1
}
```

* Inside a method or constructor, `it` is the instance.
* **Constructors**:
  * an explicit `constructor(...) { ... }`;
  * the shorthand `type Point(it.x, it.y) { x: Int  y: Int }`;
  * with neither, every field in declaration order: `type Pair { a: Int  b: Int }`
    is built with `Pair(1, 2)`.
  Fields not assigned by the constructor keep their default value.  A bare
  `return` inside a constructor finishes construction early.
* Instances are **references**: `b := a` makes both names refer to the same
  object, and passing an object to a function lets it modify the original.
* Fields can hold other objects (`a: Point`) and be chained: `line.b.x = 3`.
* A field holding an object starts out null; using it before assigning it
  stops the program with a `null reference` panic.
* `name: Type?` is accepted but nullability is not tracked yet.
* Inheritance is not supported.

## Modules

```aion
import std.math;                  // standard library:  math.sqrt(2.0)
import std.sockets.TcpListener;   // …and use TcpListener unqualified
import utils;                     // utils.aion next to this file, or in lib/
```

* `std.` and `aion.` both name the bundled standard library.
* A user module is searched for as `<name>.aion` beside the importing file, then
  in its `lib/` folder (and the same two places beside the entry file).
* `import m;` makes `m.name(...)` available; `import m.Name;` additionally makes
  `Name` usable on its own.  Dotted paths such as `import tools.text;` look for
  `tools/text.aion`.
* Everything a module defines is public.  Circular imports are an error.

## Built-in functions

| Call                        | Effect                                                |
|-----------------------------|-------------------------------------------------------|
| `print(x)`                  | write `x` (Int, Float, Bool or String), no newline    |
| `println(x)` / `println()`  | …with a trailing newline / just a newline             |
| `panic(message)`            | print `[aion] panic: message` to stderr and exit 1    |

Floats print with C's `%g` (`3.0` prints as `3`).

## Calling C

`extern fn` declares a function implemented in C — see
[stdlib.md](stdlib.md#extern-fn).

```aion
extern fn llabs(n: Int) -> Int      // bind a libc function straight from your program
```

## Runtime errors

The program stops with exit code 1 and a message on stderr for: division by
zero, using a null object reference, out of memory, and `panic(...)`.

## Not (yet) in the language

`break`/`continue`, `for`, arrays/maps, a `null` literal, generics, closures,
inheritance, `char`, user-visible access control, reading input.
