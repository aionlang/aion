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
| `Char`        | one Unicode code point, `'a'`                 | `'\0'`  |
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

## Characters

A `Char` is one Unicode code point, written between single quotes: `'a'`, `'é'`,
`'\n'`, `'\''`, `'\u{1F600}'`.  It is a type of its own (no implicit conversion
from or to `Int`) that compares, counts and prints like a letter:

```aion
c := 'a'
println(c, c < 'b', c + 2, 'e' - 'a', 'e' - 1)   // a true c 4 d
println(c.code(), 100.toChar(), 'Z'.lower(), 'é'.upper())   // 97 d z É
println('7'.isDigit(), 'x'.isLetter(), ' '.isWhitespace(), '7'.digitValue())

for ch in 'a'..='e' { print(ch, "") }              // ranges of characters
println()
println('m' in 'a'..'z')

text := "héllo"
println(text.length(), text.chars().size())        // 6 bytes, 5 characters
for ch in text { print(ch.upper(), "") }           // looping gives characters
println()
println(text[0], text.charAt(1))                   // h é   (offsets are bytes)
```

`c + n` and `c - n` are Chars, `c - d` is the distance (an `Int`), and `"$c"` or
`c.toString()` makes text.  Letter and case rules are exact for ASCII, Latin-1,
Greek and Cyrillic and a rule of thumb for other scripts.  `s[i]` and `charAt(i)`
take a **byte** offset (the character that starts there), so use `chars()` or a
`for` loop to walk text with non-ASCII letters.

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

**Slices.**  `xs[a..b]` is the part from `a` up to (not including) `b`; either end
may be left out, and `..=` includes the end.  Lists and strings have them:

```aion
xs := [10, 20, 30, 40, 50]
println(xs[1..3], xs[..2], xs[3..], xs[1..=3], xs[..])   // [20, 30] [10, 20] [40, 50] [20, 30, 40] [10, 20, 30, 40, 50]
text := "hello world"
println(text[0..5], text[6..])                           // hello world       (byte offsets)
```

A slice is a new list (a copy).  Out-of-range bounds stop the program with
`slice bounds [2..9] are out of range for size 5`.

The syntax is plain method calls under the hood, so your own types can take part:

| Written          | Means                                                           |
|------------------|-----------------------------------------------------------------|
| `x[i]`           | `x.get(i)`                                                      |
| `x[i] = v`       | `x.set(i, v)` (and `x[i] += v` reads with `get`, writes with `set`) |
| `x[a..b]`        | `x.slice(a, b)`; `x[a..]` ends at `x.size()`, `x[..b]` starts at 0, `x[a..=b]` is `x.slice(a, b + 1)` |
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
Float, Bool, Char and String, identity for objects.  Map keys are hashed with the
built-in `hashOf(value)`.  To compare your own objects by value, give the type an
`equals` method, and a `hashCode` so that it can be a map key or set member:

```aion
type Point(x: Int, y: Int) {
    fn equals(other: Point) = x == other.x && y == other.y
    fn hashCode() = x * 31 + y            // equal points must have equal hash codes
}
println(Point(1, 2) == Point(1, 2), Point(1, 2) != Point(2, 1))

visited := Set<Point>()
println(visited.add(Point(0, 0)), visited.add(Point(0, 0)))   // true false
distance := {Point(0, 0): 0, Point(1, 0): 1}
println(distance[Point(1, 0)], Point(5, 5) in distance)       // 1 false
```

`==` uses `equals` when both sides are that type (null is only equal to null);
`hashOf(p)` uses `hashCode` (combine fields with `hashOf(field)` and arithmetic).  Indexing past the end stops the program with
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

## Interfaces

An `interface` lists methods; any type that has them can be used as the interface.
There is no `implements`: satisfaction is checked by the shape of the type (like Go).

```aion
interface Shape {
    fn area() -> Float
    fn name() -> String
}

type Circle(r: Float) {
    fn area() = 3.0 * r * r
    fn name() = "circle"
}
type Square(side: Float) {
    fn area() = side * side
    fn name() = "square"
    fn toString() = "Square($side)"
}

fn describe(s: Shape) = "${s.name()} with area ${s.area()}"
println(describe(Circle(2)), describe(Square(3)))

shapes: List<Shape> = [Circle(1), Square(2), Circle(3)]   // mixed kinds in one list
total := 0.0
for s in shapes { total += s.area() }
println(total)
println(shapes)                                           // each element prints as its own type
```

Interfaces can take type parameters, `interface Container<T> { fn get(i: Int) -> T }`,
and are used as `Container<Int>`; the standard library has
`interface Comparable<T> { fn compareTo(other: T) -> Int }`, which `Int`, `Float`,
`String`, `Char` and `Bool` satisfy.  A function that takes `Container<T>` cannot
work out `T` from an argument that merely *has* the right methods; write it,
`firstOf<String>(box)`.

A `Circle` converts to a `Shape` automatically wherever a `Shape` is expected —
arguments, variables, fields, list elements, return values, `Shape?`.  The
methods must match in name, parameter types and result type; the error says what
is missing: ``Blob does not implement `Shape`: it needs `fn area() -> Float` ``.
Calls through the interface look up the real type at run time (a table indexed
by a type number kept in every object).  Interface methods have no default bodies
and cannot be generic or take named arguments, and interfaces cannot be
instantiated or inherit from each other.  Only object types (declared with `type`)
become interface values; use a wrapper type for an `Int` or `String`.

## Constraints on type parameters

`<T: Shape>` says what a type parameter must be able to do.  The check happens
where `T` becomes known, so a mistake is reported at the call, not inside the
function:

```aion
interface Shape { fn area() -> Float }
type Circle(r: Float) { fn area() = 3.0 * r * r }
type Square(side: Float) { fn area() = side * side }
shapes: List<Shape> = [Circle(1), Square(2)]

fn totalArea<T: Shape>(items: List<T>) -> Float {
    total := 0.0
    for item in items { total += item.area() }
    return total
}
println(totalArea([Circle(1), Circle(2)]))     // 15: T is Circle
println(totalArea(shapes))                      // 7: T can be the interface itself

type Holder<T: Shape>(item: T) {
    fn describe() = "holds area ${item.area()}"
}
println(Holder(Square(2)).describe())

interface Loud { fn shout() -> String }
fn String.shout() = this.upper() + "!"          // methods added to built-in types count
fn twice<T: Loud>(x: T) = x.shout() + x.shout()
println(twice("hey"))                           // HEY!HEY!
```

A bound can mention the parameter itself, which is how "can be ordered" is written:

```aion
type Version(major: Int, minor: Int) {
    fn compareTo(other: Version) = if major != other.major { major - other.major } else { minor - other.minor }
    fn toString() = "v$major.$minor"
}

fn newest<T: Comparable<T>>(a: T, b: T) = if a.compareTo(b) >= 0 { a } else { b }
println(newest(Version(1, 2), Version(1, 10)), newest(3, 9), newest("pear", "apple"))

versions := [Version(1, 10), Version(1, 2), Version(0, 9)]
println(versions.sorted(), versions.maxOrNull())          // [v0.9, v1.2, v1.10]  v1.10
println(["kiwi", "fig", "banana"].sortedBy { it.length() })   // [fig, kiwi, banana]
```

Without a bound, a generic function still works for any type whose operations
the body happens to use (each instance is checked as ordinary code); a bound
just makes the requirement visible and the error early.

## Cleanup and errors

`defer` schedules code to run when the **enclosing block** is left: by reaching
its end, `return` (after the result has been computed), `break`, `continue`, or a
panic.  Several defers in one block run newest first.

```aion
fn work(n: Int) -> Int {
    println("start", n)
    defer println("cleanup 1")
    defer {
        println("cleanup 2")
    }
    if n > 5 {
        defer println("leaving the if")
        return n * 2
    }
    return n
}
println(work(9))        // start 9, leaving the if, cleanup 2, cleanup 1, then 18

for i in 0..3 {
    defer println("  end of iteration", i)     // once per iteration
    if i == 1 { continue }
    println("body", i)
}

fn counter() {
    count := 0
    defer println("final count:", count)       // runs later, so it sees 6, not 0
    count += 6
}
counter()
```

A deferred statement is a closure: it sees the variables' values *when it runs*
(unlike Go, which evaluates the arguments immediately).  A `defer` at the top of a
file runs when the file's own code has finished.

**Errors** are panics.  `panic("message")` and the runtime errors — index out of
range, division by zero, `!!` on null, a missing map key — stop the program, unless
a `try` is watching:

```aion
fn parse(text: String) -> Int {
    n := text.toIntOrNull()
    if n == null { panic("not a number: $text") }
    return n
}

a := try { parse("42") } catch e { -1 }
b := try { parse("x") } catch e { println("caught:", e); -1 }
println(a, b)                                  // 42, then "caught: not a number: x", then -1

try {
    println(10 / (a - 42))
} catch {
    println("failed with", it)                 // without a name the message is `it`
}

fn cleanup() {
    defer println("  (cleanup still runs)")
    panic("something broke")
}
try { cleanup() } catch e { println("handled:", e) }
```

`try` is an expression: its value is the body's last expression, or the handler's
if something panicked, and the two must have the same type (a declared type
decides when there is one: `n: Int? = try { … } catch e { null }`).  The message
is a `String`.  Pending `defer`s run before the handler does.  Because the two
blocks run as closures, `return`, `break` and `continue` cannot leave them; assign
to a variable, or use the value of the `try`.  An uncaught panic runs the
remaining `defer`s, prints `[aion] panic: message` and exits with status 1.

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

## Input

`readLine()` returns the next line of standard input as a `String?`, `null` once
the input has ended; `readText()` returns whatever is left.  `args()` is the list
of command-line arguments after the script.

```aion no_run
print("your name? ")
name := readLine() ?: "stranger"
println("hello,", name)

total := 0
line := readLine()
while line != null {                  // until the input ends
    total += line.toIntOr(0)
    line = readLine()
}
println("sum:", total, "from", args().size(), "arguments")
```

## Calling C

`extern fn` declares a function implemented in C — see
[stdlib.md](stdlib.md#extern-fn).

```aion
extern fn llabs(n: Int) -> Int      // bind a libc function straight from your program
println(llabs(-5))
```

## Runtime errors

The program stops with exit code 1 and a message on stderr for: division by
zero, using a null object or function, indexing outside an array or list, out of
memory, and `panic(...)` — unless a [`try`](#cleanup-and-errors) catches it.

## Not (yet) in the language

Concurrency and inheritance.  (Plain object types are not checked against
being unset; use `T?` where a value may be missing.)
