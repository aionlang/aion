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
| `?:`               | optional `?:` fallback (see [Nullability](#nullability)) |
| `+` `-`            | numbers; `+` also joins two Strings                   |
| `*` `/` `%`        | numbers                                               |
| unary `-` `!`      | number / Bool                                         |
| postfix `!!` `?.` `.` `()` `[]` | optional unwrap, safe access, member access, call, index |

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
of its own (`"hi".length()`, `.contains(s)`, `.upper()` and friends are
[methods](#methods-on-built-in-types)).  Escapes: `\n \t \r \0 \\ \" \$`.  A `$` not followed by a name or `{`
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
  the program with a `null reference` panic.  Declare such a field `Type?` to say
  it may be null (see [Nullability](#nullability)); a plain `Type` is still not
  checked against being unset.
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

## Generics

Functions and types can take **type parameters**.  They are inferred from the
arguments, from the type that is expected, or written out.

```aion
fn identity<T>(x: T) = x
fn biggest<T>(a: T, b: T) = if a > b { a } else { b }
fn apply<T, R>(x: T, f: (T) -> R) = f(x)

println(identity(5), identity("s"), biggest(3, 9), biggest("apple", "pear"))
println(apply(21, { it * 2 }), apply("hi", { it + "!" }))   // the lambda is checked last
println(identity<Float>(2))                                  // explicit: 2 is widened to a Float

type Box<T>(value: T) {
    fn get() = value
    fn map<R>(f: (T) -> R) = Box(f(value))                   // a method with its own parameter
}
b := Box(41)
println(b.get(), b.map { it + 1 }.get(), b.map { "v$it" }.get())
x: Box<String> = Box("typed")                                // spelled out in a type
println(x.get())
```

Generics are **monomorphized**: `Box<Int>` and `Box<String>` are separate types
and every instantiation of a function is compiled on its own, exactly as if you
had written it by hand.  So the body of a generic function can do anything the
concrete types allow — `a > b` works for `biggest(1, 2)` and `biggest("a", "b")`,
and is an error (naming the instance) for a type that has no `>`.  Members of a
generic type are only checked when something uses them, so a `List` of functions
works as long as nobody asks it to `join`.

There are no constraints or interfaces yet; the compiler simply tries the code.
`fn Box<T>.twice()` adds a method to a generic type from outside it.

## Methods on built-in types

Any module can add methods to `Int`, `Float`, `Bool` and `String`; the receiver
is `this`.

```aion
fn String.shout() = "$this!"
fn Int.isEven() = this % 2 == 0
fn Int.times(action: (Int) -> Unit) {
    for i in 0..this { action(i) }
}

println("hi".shout(), 4.isEven(), "héllo".length(), "banana".contains("nan"))
3.times { print(it, "") }
println()
```

`String` already has `length()` (bytes), `contains(s)`, `startsWith(s)`,
`indexOf(s)` (-1 if absent), `substring(start, end)`, `trim()`, `replace(old, new)`,
`split(separator)` (a `List<String>`), `toIntOr(fallback)`, `upper()` and
`lower()`; offsets are in bytes.  Methods are visible everywhere once the module that
declares them is part of the program (extension methods are global, so name
them with care).

## Collections

`Array<T>` is the built-in building block: a fixed-size block of slots, bounds
checked.  `List<T>`, `Map<K, V>` and `Set<T>` are ordinary Aion code in the
[prelude](stdlib.md#collections) built on top of it.

```aion
a := Array(3, 0)                 // size, fill value (or Array<Int>(3) for zeros)
a[1] = 5                         // a.set(1, 5)
println(a[1], a.size())          // a.get(1)

xs := [3, 1, 2]                  // a List<Int>
xs.add(10)
xs[0] += 1
println(xs, xs.size(), xs[3], 2 in xs)
println(xs.map { it * 2 }.filter { it > 4 })
println(xs.fold(0) { acc, x -> acc + x })
for x in xs { print(x, "") }
println()

ages := {"ann": 31, "bob": 25}   // a Map<String, Int>, keeps insertion order
ages["cy"] = 40
println(ages, ages["bob"], "ann" in ages, ages.getOr("zed", -1))
for e in ages { println(e.key, "is", e.value) }
ages.forEach { name, age -> println(name, age + 1) }

words: List<String> = []         // an empty literal needs a type to know what it holds
words.add("x")
println(words, Map<String, Int>().size(), setOf([1, 1, 2]))
```

The syntax is plain method calls under the hood, so your own types can take part:

| Written          | Means                                                           |
|------------------|-----------------------------------------------------------------|
| `x[i]`           | `x.get(i)`                                                      |
| `x[i] = v`       | `x.set(i, v)` (and `x[i] += v` reads with `get`, writes with `set`) |
| `a in x`         | `x.contains(a)` (`a in lo..hi` stays a range test)              |
| `for v in x`     | loops `i` over `0 until x.size()` calling `x.get(i)`; if `x` has an `iter()` method, loops over what that returns instead (a Map iterates its entries) |
| `[a, b, c]`      | a `List` built with `add`                                       |
| `{k: v, …}`      | a `Map` built with `set`                                        |

```aion
type Squares(count: Int) {
    fn size() = count
    fn get(i: Int) = i * i
}
for sq in Squares(4) { print(sq, "") }
println(Squares(10)[7])
```

Equality (`==`, `contains`, map keys) is the language's `==`: values for Int,
Float, Bool and String, identity for objects.  Map keys are hashed with the
built-in `hashOf(value)`.  Indexing past the end stops the program with
`index 5 is out of bounds for size 3`; a missing map key with `no such key: …`.

## Nullability

`T?` is a `T` or `null`.  `null` converts to any `T?`, and a `T` converts to
`T?`; the other way round you have to say what happens when it is null.

```aion
a: Int? = 5
b: Int? = null
c: String? = "hi"
println(a, b, c)                       // 5 null hi
println(a == null, b == null, a == 5)  // false true true

println(a ?: 0, b ?: 0)                // 5 0     `?:` supplies a default
println(a!!)                           // 5       `!!` unwraps, or stops the program if null

type Node(v: Int) {
    next: Node?
    fn double() = v * 2
}
n := Node(1)
n.next = Node(2)
println(n.next?.v, n.next?.next?.v)            // 2 null   `?.` gives null instead of failing
println(n.next?.double() ?: -1, n.next?.next?.double() ?: -1)   // 4 -1

fn describe(s: String?) -> String {
    if s == null { return "nothing" }          // after this, `s` is a plain String
    return "got " + s.upper()
}
println(describe("x"), describe(null))
```

| Written              | Meaning                                                              |
|----------------------|----------------------------------------------------------------------|
| `x == null`, `x != null` | test for null; `x == 5` also works (equal if both are null, or both present and equal) |
| `x!!`                | the value; `unexpected null value` panic if it is null                |
| `x ?: y`             | `x` unless it is null, then `y`; a `y` that doesn't return, such as `panic(...)`, is fine |
| `x?.f`, `x?.f(…)`    | `null` if `x` is null, otherwise the (optional) result; a chain stops at the first null |

**Smart casts.**  Inside `if x != null { … }`, in the `else` of `if x == null`, in
`a != null && a.ok`, and after an `if x == null { return … }` guard, an optional
variable is a plain value:

```aion
fn longest(words: List<String?>) -> Int {
    best := 0
    for w in words {
        if w == null { continue }              // skip them
        if w.length() > best { best = w.length() }
    }
    return best
}
println(longest(["a", null, "abc"]))

type Node(v: Int) { next: Node? }

fn sum(first: Node?) -> Int {
    total := 0
    node := first
    while node != null {                       // `node` is reassigned below, and that's fine:
        total += node.v                        // it is known non-null until the assignment
        node = node.next
    }
    return total
}
n := Node(1)
n.next = Node(2)
println(sum(n))
```

The compiler is deliberately cautious about when it believes a check still
holds.  A variable is **not** narrowed if a lambda assigns it, if it is a module
variable that some function assigns, or after a statement that assigns it; and
**fields** are never narrowed (copy the field into a variable first, or use
`?.` / `!!`).  When in doubt, you get an ordinary "`Int?` is not `Int`" error
that says how to fix it.

`Int?`, `Float?` and `Bool?` are stored as a small heap box, so they cost an
allocation; an optional `String`, object or function is the plain pointer.
Collections of optionals work (`List<Int?>`, `Map<String, Int?>`), and
the standard library uses them: `list.firstOrNull()`, `list.find { … }`,
`map.getOrNull(key)`, `"42".toIntOrNull()`.

## Line breaks

A line break ends a statement, with these exceptions:

* inside parentheses, newlines are ignored;
* a line may continue after an operator (`total = a +` ⏎ `b`);
* a line may **start** with `.`, `?.`, `?:`, `&&` or `||` and continue the previous one;
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

Interfaces and constraints on type parameters, `defer`, a way to recover from
errors, concurrency, inheritance, `char`, reading input, slices, and
user-defined hashing for map keys.  (Plain object types are not checked against
being unset; use `T?` where a value may be missing.)
