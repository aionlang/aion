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
| `Int32`       | 32-bit signed integer (wraps), see [Sized integers](#sized-integers) | `0` |
| `Byte`        | 8-bit unsigned integer, 0–255 (wraps)         | `0`     |
| `Float`       | 64-bit floating point                         | `0.0`   |
| `Bool`        | `true` / `false`                              | `false` |
| `Char`        | one Unicode code point, `'a'`                 | `'\0'`  |
| `String`      | immutable UTF-8 text                          | `""`    |
| `Bytes`       | raw bytes, 0–255 each (see [Bytes](#bytes))   | empty   |
| `Point`       | an instance of a user-defined `type Point`    | null    |
| `(Int) -> Int`| a function value; `Unit` means "no value"    | null    |

An `Int` (and an `Int32` or a `Byte`) is accepted wherever a `Float` is expected and widens automatically; a
`Byte` widens to an `Int32` and an `Int32` to an `Int`.  There are no other implicit conversions (see
[Sized integers](#sized-integers) for the casts).

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

### Read-only variables: `val`

`val` declares a variable that cannot be assigned again, which is how to write a constant.  It works
for locals and top-level variables, the type may be written, and closures see the same variable.

```aion
val limit = 10
val greeting: String = "hello"
total := 0
for i in 0..limit { total += i }
println(total, greeting)

// limit = 11       error: cannot assign to `limit`: it was declared with `val`
// limit += 1       the same, and so is assigning to it from a closure
```

Only the variable is fixed, not what it holds: a `val` list can still grow.

```aion
val names = ["a"]
names.add("b")
println(names)      // [a, b]
```

A `val` needs a value, and `:=` stays the way to declare a variable that changes.

Blocks open a new scope; an inner declaration shadows an outer one, and declaring
the same name twice in one scope is an error.

## Operators

From loosest to tightest binding:

| Operators          | Operands                                              |
|--------------------|-------------------------------------------------------|
| `c ? a : b`        | Bool condition; the value of `a` or of `b`            |
| `a..b`  `a..=b`    | Int ranges (see [Ranges](#ranges)); `step` and `downTo` follow them |
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

`c ? a : b` is the conditional operator, sugar for `if c { a } else { b }`.  It nests to the
right (`a ? b : c ? d : e`) and both branches must have the same type, or one may be `null`.
After `is` or `as`, a `?` that has an expression after it is the conditional, not a nullable
type: `x is Dog ? 1 : 2` works, and `x is Dog? && y` is still a test for `Dog?`.

```aion
age := 20
label := age >= 18 ? "adult" : "youth"
println(label)                                    // adult
fn kind(n: Int) = n < 0 ? "negative" : n == 0 ? "zero" : "positive"
println(kind(-3), kind(0), kind(8))               // negative zero positive
```

Integers can be written in decimal, `0xFF` hex, `0b1010` binary or `0o17` octal, with
`_` between digits (`1_000_000`), and floats take an exponent (`1.5e-3`).  Bit
operations are methods on `Int`: `a.and(b)`, `a.or(b)`, `a.xor(b)`, `a.inv()`,
`a.shl(n)`, `a.shr(n)` (keeps the sign) and `a.ushr(n)` (fills with zeros); the shift
amount counts modulo 64.

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

### Ranges

A range is a value too: `1..5` (without 5), `1..=5`, `0..20 step 5`, `10 downTo 1`.  It can be
tested with `in`, looped over, and used like a list of its numbers.  `for i in a..b` itself is a
plain counting loop and builds nothing.

```aion
r := 1..=5
println(r, r.size(), 3 in r, 9 in r)                 // [1, 2, 3, 4, 5] 5 true false
println((1..=10).map { it * it })                    // [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
println((1..=100).sum(), (1..=5).reversed())         // 5050 [5, 4, 3, 2, 1]

for i in 0..=20 step 5 { print(i, "") }              // 0 5 10 15 20
println()
for i in 10 downTo 0 step 4 { print(i, "") }         // 10 6 2
println()

names := ["ann", "bob", "cy", "di", "ed"]
println((0..names.size() step 2).map { names[it] })  // [ann, cy, ed]
fn describe(r: Range) = "${r.size()} numbers from ${r.first()} to ${r.last()}"
println(describe(5..10))                             // 5 numbers from 5 to 9
```

A `Range` has `size() isEmpty() get first() last() contains step reversed toList sum` and
`forEach map filter fold any all count join`.  Only `Int` ranges are values; a `Char` range such as
`'a'..='e'` can still be looped over.  `step` and `downTo` are words between two expressions, not
keywords: `step` and `downTo` stay usable as names.

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

* Parameters need types.  A default is any expression (see [below](#defaults-varargs-and-nested-functions)),
  including `null` for an optional parameter such as `title: String? = null`.
* After a named argument, all following arguments must be named.  Arguments are
  evaluated in the order they are written.
* `= expr` infers the return type; write `-> Type` to state it.  With a block
  body, no `->` means "returns nothing", and a function that declares `-> T`
  must return on every path.
* A function that calls itself (or two that call each other) needs `-> Type` on
  at least one of them.

`Unit` names "no value" and is only valid as a return type, mainly in function
types: `(Int) -> Unit`.

### Defaults, varargs and nested functions

```aion
// defaults can be any expression, and can use the parameters before them
fn box(w: Int, h: Int = w, area: Int = w * h) = "$w x $h = $area"
println(box(3), box(3, 4), box(2, area: 7))     // 3 x 3 = 9 3 x 4 = 12 2 x 2 = 7

fn collect(item: Int, into: List<Int> = []) -> List<Int> {
    into.add(item)
    return into
}
println(collect(1), collect(2))                 // [1] [2]: a new list for every call

// vararg: any number of arguments, a List inside

fn sum(vararg xs: Int) = xs.fold(0) { acc, x -> acc + x }
fn log(level: String, vararg parts: String) = println("[$level] " + parts.join())
nums := [10, 20, 30]
println(sum(), sum(1, 2, 3), sum(*nums), sum(1, *nums))   // 0 6 60 61
log("info", "started", "ok")                              // [info] started, ok
println([0, *nums, 40])                                   // [0, 10, 20, 30, 40]

// a function inside a function sees its variables, and can call itself with `->`
fn describe(n: Int) -> String {
    fn label(x: Int) -> String = x % 2 == 0 ? "even" : "odd"
    fn fact(k: Int) -> Int {
        if k <= 1 { return 1 }
        return k * fact(k - 1)
    }
    return "${label(n)}, ${fact(n)}"
}
println(describe(5))                                      // odd, 120

fn counter() -> () -> Int {
    n := 0
    fn next() -> Int {
        n += 1
        return n
    }
    return next
}
c := counter()
c()
println(c())                                              // 2
```

* A **default** is worked out at each call, from the parameters before it (and, in a method, the
  fields).  A list default is therefore fresh every time.
* A **`vararg`** parameter must be last and has no default; leaving it out gives an empty list.
  `*xs` passes the items of a list one by one, in a call or in a list literal.  Generic
  functions, methods and `constructor`s can take one (a primary constructor list cannot).
  Pass a list without `*` and the compiler reminds you to spread it.
* A **nested function** is a variable holding a closure, so it sees (and can change) the
  variables around it and can be returned.  To call itself it needs `-> Type` written out; two
  nested functions cannot call each other, and they take no defaults, `vararg` or type
  parameters.  A variable with a function type can call itself too:
  `fib: (Int) -> Int = { n -> n < 2 ? n : fib(n - 1) + fib(n - 2) }`.

### Overloading

Functions, methods and methods added to built-in types can share a name when their parameters differ:

```aion
fn describe(x: Int) = "int $x"
fn describe(x: Float) = "float $x"
fn describe(x: Int, y: Int) = "two ints ${x + y}"

type Money(cents: Int) {
    fn plus(o: Money) = Money(cents + o.cents)
    fn plus(extra: Int) = Money(cents + extra)       // so `m + 7` works as well as `m + n`
    fn toString() = "$cents"
}

fn String.wrap(left: String) = left + this
fn String.wrap(left: String, right: String) = left + this + right

println(describe(1), describe(2.5), describe(3, 4))   // int 1 float 2.5 two ints 7
println(Money(100) + Money(5), Money(100) + 7)        // 105 107
println("x".wrap("<"), "x".wrap("<", ">"))            // <x <x>
```

A call picks the version that takes that many arguments (with defaults and names counted), then the one
whose parameter types accept the arguments, an exact type before a conversion (`describe(1)` is the `Int`
version, though an `Int` would also convert to `Float`).  A call that fits none, or two equally well,
is an error that lists the versions.  A lambda is not looked at until a version is chosen, so versions
that differ only in the function they take are ambiguous.

Limits: two versions cannot have the same parameter types, `extern fn`s and generic functions cannot be
overloaded, the methods of an `open` type or a subtype cannot (a subtype could replace one version but
not another), and an overloaded function
cannot be used as a value (`f := describe`); wrap it in a lambda.

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

A string in triple quotes spans lines and may hold quotes (`$name` and `${…}` still work, and a line
break right after the opening `"""` is dropped); a **raw** string, `r"C:\dir"` or `r"""…"""`, keeps backslashes
and dollar signs as they are, which suits regular expressions and JSON:

```aion
name := "Ann"
println("""He said "hi" to $name""")
println(r"""{"path": "C:\temp\n", "cost": "$5"}""")
```

`\u{1F600}` puts any Unicode code point in a string (`"caf\u{e9}"` is `café`).  A `Float`
prints as the shortest text that reads back as the same number: `0.1`, `0.30000000000000004`,
`12` for a whole number, `1e+20` for a huge one.  For fixed decimals use
`x.format(2)` (`3.14159.format(2)` is `3.14`).

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
* A type can extend an `open` type; see [Inheritance](#inheritance).

### Static functions

A `static fn` belongs to the type itself rather than to an instance: it has no `this`, so it cannot
read fields, and it is called on the type's name.  Use it for factories and helpers that go with a type.

```aion
type Point(x: Int, y: Int) {
    static fn origin() = Point(0, 0)
    static fn parse(s: String) -> Point {
        parts := s.split(",")
        return Point(parts.get(0).toIntOr(0), parts.get(1).toIntOr(0))
    }
    static fn diagonal(n: Int) = Point(n, n)

    fn doubled() = diagonal(x * 2)           // a method calls a static of its own type bare
}

static fn Point.unit() = Point(1, 1)         // written outside the type, like a method

println(Point.origin(), Point.parse("3,4"), Point.unit())
```

* Inside the type, other statics are called bare (`diagonal(n)`), from methods and from statics alike;
  from anywhere else write `Point.diagonal(n)`, or `shapes.Point.diagonal(n)` through a module.
* A subtype finds its parent's statics: `Circle.kinds()` works when `Shape` declares `kinds`.  A
  static is not virtual: it cannot be `open` or `override`n.
* A static and a method of one type cannot share a name.
* Not supported on generic types or on built-in types (`fn Int.parse()`); write a top-level function.

### Data types

`data type` writes the comparing and copying for you.  Its instances are equal when all their
parameters are, hash the same (so they work as `Map` keys and `Set` members), can be copied with
changes, and can be taken apart like a [tuple](#tuples-and-destructuring):

```aion
data type Point(x: Int, y: Int)

a := Point(2, 3)
println(a == Point(2, 3), a != Point(3, 2))        // true true: by value, not by identity
moved := a.copy(x: 10)                              // a copy with some parts changed
println(moved, a)                                   // Point(x=10, y=3) Point(x=2, y=3)
x, y := moved
println(x, y)                                       // 10 3

visited := Set<Point>()
for p in [Point(0, 0), Point(1, 0), Point(0, 0)] { visited.add(p) }
println(visited.size())                             // 2
```

`equals(other)`, `hashCode()`, `copy(…)` (every parameter defaults to the current value) and
`component1()`, `component2()`, … come from the primary parameters; a method of the same name that
you write yourself is kept.  A data type cannot be `open` or extend another type.  Fields hold
what they hold: a `List` field is compared by identity, a nested data type by value.

## Operators on your own types

An operator on an object calls a method of the type, so any type can join in:

| Written | Calls |
|---------|-------|
| `a + b` `a - b` `a * b` `a / b` `a % b` | `a.plus(b)` `minus` `times` `div` `rem` |
| `-a` | `a.unaryMinus()` |
| `a < b` `a <= b` `a > b` `a >= b` | `a.compareTo(b)` compared with 0 |
| `a == b` | `a.equals(b)` (see below), `a += b` is `a = a + b` |

```aion
type Money(cents: Int) {
    fn plus(o: Money) = Money(cents + o.cents)
    fn minus(o: Money) = Money(cents - o.cents)
    fn times(k: Int) = Money(cents * k)
    fn unaryMinus() = Money(-cents)
    fn compareTo(o: Money) = cents - o.cents
    fn equals(o: Money) = cents == o.cents
    fn toString() = "$${cents / 100}.${cents % 100}"
}

a := Money(250)
b := Money(175)
println(a + b, a - b, a * 3, (-a).cents)   // $4.25 $0.75 $7.50 -250
println(a > b, a <= b, a == Money(250))    // true false true
total := Money(0)
for m in [a, b, a] { total += m }          // += uses plus
println(total)                             // $6.75
```

`compareTo` returns a negative, zero or positive `Int`, as in [`Comparable`](#constraints-on-type-parameters).
The method on the left operand is used, and an `open` method is looked up on the real type at run
time.  An operator without a matching method says which one to write.

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
`c.toString()` makes text.  `c * n` and `s * n` repeat a Char or a String `n` times:
`'-' * 3` is `"---"`.  Letter and case rules are exact for ASCII, Latin-1,
Greek and Cyrillic and a rule of thumb for other scripts.  `s[i]` and `charAt(i)`
take a **byte** offset (the character that starts there), so use `chars()` or a
`for` loop to walk text with non-ASCII letters.

## Sized integers

`Int` is 64 bits.  `Int32` (signed, -2147483648 to 2147483647) and `Byte` (unsigned, 0 to 255) are there for binary
formats and for C functions that take `int32_t` or `uint8_t`.  They behave like `Int` but **wrap around** when a result
leaves their range, and a whole-number literal that fits becomes one where one is expected:

```aion
b: Byte = 200
println(b + 100, b * 2, 3 - b)               // 44 144 59  (wrapped into 0..255)
c: Int32 = 2147483647
println(c + 1, c * 2)                        // -2147483648 -2

n := 300
println(n as Byte, n as? Byte, 200 as? Byte) // 44 null 200  (`as` wraps, `as?` gives null when it does not fit)
println(b.toInt() + 100, b + n)              // 300 500      (mixed with an Int, the computation is an Int)
println(b.shl(1), b.inv(), c.ushr(28))       // 144 55 7     (and, or, xor, inv, shl, shr and ushr work as on Int)
```

A narrower type widens by itself (`Byte` to `Int32` to `Int`, and any of them to `Float`); going the other way needs
a word: `x as Byte` / `x.toByte()` / `x as Int32` / `x.toInt32()` wrap around, `x as? Byte` is `null` if `x` does not fit.
`b.toInt()`, `b.toHex()`, `"42".toByteOrNull()` and `"7".toInt32OrNull()` do what they say.  They print, compare, hash
and work as `Map` keys, `List` elements and in an `Any` like the other basic types.  Arrays of them still use one 64-bit
slot per element (use [`Bytes`](#bytes) for a compact run of bytes).  When you [call C](#calling-c), an `Int32`
is passed and returned as `int32_t` and a `Byte` as `uint8_t`:

```aion
extern fn abs(n: Int32) -> Int32            // libc's int abs(int)
println(abs(-7), abs(-2147483647))          // 7 2147483647
```

## Bytes

`Bytes` holds raw binary data: what a file, a socket or a hash function produces.
Unlike a `String` it may contain any byte, zero included.  Elements are the numbers
0 to 255.

```aion
b := Bytes(4)                    // four zero bytes; Bytes(4, 255) fills them with 255
b[0] = 255
b[3] = 0x10
println(b, b.size())             // Bytes(ff000010) 4
println(b[1..3], b + b)          // slices and joins make copies
println(b == bytesOf([255, 0, 0, 16]))   // equal when the bytes are equal

data := "héllo".toBytes()        // the UTF-8 bytes of a String
println(data.size(), data.toText(), data.toHex())   // 6 héllo 68c3a96c6c6f
println("6869".fromHex()?.toText(), "xyz".fromHex())   // hi null
for x in bytesOf([1, 2, 3]) { print(x, "") }
println()
println(data.indexOf(0x6c), 3 in bytesOf([1, 2, 3]))
```

`b.toText()` reads up to the first zero byte.  Values `0..255` only: writing anything
else stops the program, as does an index or slice outside the data.  A `Bytes` works as
a `Map` key and with `hashOf`; the zero value of an unset `Bytes` variable is empty.
Methods: `size() get(i) set(i, v) slice(from, to) concat(other) indexOf(v, from = 0)
contains(v) toHex() toText() toList()`, plus `String.toBytes()`, `String.fromHex()` and
`bytesOf(list)`.

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

A method your program declares hides the standard library's method of the same name, for your
program: the library may add `Int.isEven()` in a later version without breaking a program that
already has its own.  The library's own code keeps calling its own method.  The same goes for the
methods you add to `List`, `Map` and `Set`.  If the parameters differ, both are available (see
[overloading](#overloading)); if they are the same, yours is the one called.  Two methods of your
own with the same parameters are still an error.

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

## Tuples and destructuring

A few values travel together as a **tuple**: `(1, "one")` has the type `(Int, String)`.  Tuples have
two to five elements (`first`, `second`, `third`, `fourth`, `fifth`), print as `(1, one)`, and compare
and hash by value, so they work as `Map` keys and `Set` members.  For more, declare a `type`.

`a, b := e` takes a value apart through its `component1()`, `component2()`, … methods, which
tuples, map `Entry`s and the first elements of a `List` have, and your own types can add:

```aion
fn minmax(xs: List<Int>) -> (Int, Int) {
    lo := xs[0]
    hi := xs[0]
    for x in xs {
        if x < lo { lo = x }
        if x > hi { hi = x }
    }
    return (lo, hi)
}

lo, hi := minmax([3, 1, 4, 1, 5])          // takes the pair apart
println(lo, hi)                            // 1 5
pair := (1, "one")
println(pair, pair.first, pair.second)     // (1, one) 1 one

x, y := 1, 2
x, y = y, x                                // swap
println(x, y)                              // 2 1

(a, (b, c)) := (1, (2, 3))                 // nested; `_` skips a part
_, second := (10, 20)
println(a, b, c, second)                   // 1 2 3 20

ages := {"ann": 31, "bob": 25}
for (name, age) in ages.entries() { println("$name is $age") }

names := ["ann", "bob"]
for i, name in enumerate(names) { println("$i: $name") }
for n, age in names.zip([31, 25]) { println(n, age) }

seen := Set<(Int, Int)>()                  // tuples compare by value
seen.add((1, 2))
println((1, 2) in seen, (2, 1) in seen)    // true false
```

* `a, b := …`, `(a, b) := …` and nested `(a, (b, c)) := …` declare names; `_` skips a part.
* `a, b = b, a` assigns; with several values on the right, all are worked out first, so this swaps.
  Targets can be fields and elements: `xs[i], xs[j] = xs[j], xs[i]`.
* `for (k, v) in …` and `for k, v in …` take each element apart.
* In types, a parenthesised type is just grouping, so an optional function is
  `((Int) -> Int)?` and an optional pair is `(Int, Int)?`.
* The compiler writes tuples as the library types `Tuple2` … `Tuple5`.

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
| `x ?: y`             | `x` unless it is null, then `y`; a `y` that leaves, such as `panic(...)`, `return v`, `break` or `continue`, is fine |
| `x?.f`, `x?.f(…)`    | `null` if `x` is null, otherwise the (optional) result; a chain stops at the first null |

**Leaving early.**  `return`, `break` and `continue` are expressions, so they can follow `?:`
to leave when a value is missing, and they can be a branch of an `if` or `? :`:

```aion
fn priceWithTax(prices: Map<String, Int>, name: String) -> Int {
    price := prices.getOrNull(name) ?: return -1       // no such item: leave right here
    return price + price / 5
}
fn total(prices: Map<String, Int>, names: List<String>) -> Int {
    sum := 0
    for name in names {
        sum += prices.getOrNull(name) ?: break          // stop at the first unknown name
    }
    return sum
}
fn describe(n: Int) -> String {
    sign := n > 0 ? "positive" : n < 0 ? "negative" : return "zero"
    return "$sign number"
}
prices := {"pen": 150, "book": 1200}
println(priceWithTax(prices, "book"), priceWithTax(prices, "sofa"))   // 1440 -1
println(total(prices, ["pen", "book", "sofa", "pen"]))                // 1350
println(describe(4), describe(-4), describe(0))                       // positive number negative number zero
```

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

## Inheritance

A type that is declared `open` can be extended.  The subtype lists the parent after a
colon, with the arguments for the parent's constructor; methods the parent marks
`open` can be replaced with `override`.

```aion
open type Animal(name: String) {
    open fn sound() = "..."
    open fn speak() = "$name says ${sound()}"      // sound() is looked up on the real object
    fn loud() = speak().upper()                     // not open: the same for every kind
}
open type Dog(name: String) : Animal(name) {
    breed: String = "mutt"
    override fn sound() = "Woof"
    fn fetch() = "$name fetches"
}
type Puppy(name: String) : Dog(name) {
    override fn sound() = "Yip (" + super.sound() + ")"    // the parent's version
}
type Cat(name: String) : Animal(name) {
    override fn speak() = super.speak() + "!"
    override fn sound() = "Meow"
}

animals: List<Animal> = [Dog("Rex"), Cat("Tom"), Puppy("Bit"), Animal("generic")]
for a in animals { println(a.speak()) }     // Rex says Woof / Tom says Meow! / Bit says Yip (Woof) / generic says ...
println(animals)                            // each prints as its own kind: [Dog(name=Rex, breed=mutt), …]
```

* A subtype has its parent's fields first, then its own, and inherits the parent's
  methods.  A constructor parameter with the same name as an inherited field just
  hands its value to the parent (`Dog(name: String) : Animal(name)`).
* Construction runs the parent's initialiser first (its fields, `init` blocks and
  constructor body), then the subtype's.
* A `Dog` is accepted wherever an `Animal` is expected, and `if`/`else` branches of
  sibling types have their closest common parent as type.
* `open fn` makes a method replaceable, `override fn` replaces it and must take and
  return exactly what the original does; replacing a method that is not `open`, or
  forgetting `override`, is an error.  Calls to an open method, including bare calls
  inside the type, look the real type up at run time.
* Types can also be tested and converted: `x is Dog`, `x !is Dog`, `x as Dog` (stops
  with `cannot cast to Dog` if it is not one) and `x as? Dog` (a `Dog?`).  After a
  test the variable has the tested type, under the usual [smart-cast rules](#nullability):

```aion
open type Animal(name: String)
type Dog(name: String) : Animal(name) { fn fetch() = "$name fetches" }
type Cat(name: String) : Animal(name) { fn purr() = "$name purrs" }

fn act(a: Animal) = if a is Dog { a.fetch() } else if a is Cat { a.purr() } else { "${a.name} sits" }
println(act(Dog("Rex")), act(Cat("Tom")), act(Animal("Rock")))
pets: List<Animal> = [Dog("a"), Cat("b")]
println((pets[0] as Dog).fetch(), (pets[1] as? Dog)?.name)    // a fetches null
```

Types with type parameters cannot take part in inheritance, and a type has one
parent; use [interfaces](#interfaces) for what several unrelated types have in common.

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

## Tasks and channels

A **task** is a block of code that runs alongside the rest of the program.  Tasks are
cooperative: only one runs at a time, and the switch happens when a task waits for
something (`join`, `await`, a channel, `sleep`) or calls `yield()`.  So tasks can share
variables without locks, and nothing is interrupted halfway through a statement.

```aion
a := launch { for i in 0..3 { println("a", i); yield() } }     // a Job
b := launch { for i in 0..3 { println("b", i); yield() } }
a.join()
b.join()                                                         // a0 b0 a1 b1 a2 b2

squares := List<Task<Int>>()
for i in 1..=4 { squares.add(async { i * i }) }                  // a Task<Int>: a result to come
total := 0
for t in squares { total += t.await() }
println(total)                                                   // 30

log := List<String>()
launch { sleep(60); log.add("slow") }
launch { sleep(10); log.add("fast") }
waitAll()                                                        // until every task has finished
println(log)                                                     // [fast, slow]
```

`launch { … }` runs a block and gives a `Job` (`join()`, `isDone()`); `async { value }`
runs a block and gives a `Task<T>` (`await()`, `isDone()`); `sleep(ms)` pauses only the
current task; `waitAll()` waits for all of them.  When the main code finishes the program
ends, even if tasks are still running.  A panic in a task that does not `try` it ends
the whole program.

A **channel** passes values between tasks.  `Channel<T>()` hands a value over when a
receiver takes it; `Channel<T>(n)` buffers up to `n` values.  `receive()` waits for the
next value and gives `null` once the channel has been closed and emptied.

```aion
jobs := Channel<Int>(10)
results := Channel<Int>(10)
workers := List<Job>()
for w in 1..=3 {
    workers.add(launch {
        jobs.forEach { n -> results.send(n * n) }                // until `jobs` is closed
    })
}
for n in 1..=6 { jobs.send(n) }
jobs.close()
for w in workers { w.join() }
results.close()
println(results.drain().sorted())                                // [1, 4, 9, 16, 25, 36]

ch := Channel<String>()
launch { ch.send("ping"); ch.close() }
word := ch.receive()
while word != null {                                             // null: closed and empty
    println(word)
    word = ch.receive()
}
```

Also `send(v)`, `tryReceive()` (never waits), `close()`, `forEach(action)` and `drain()`.
Sending on a closed channel stops the program.  If every task is waiting for something
that can no longer happen, the program stops with `deadlock: every task is waiting and
none can continue` instead of hanging.  Each task has its own 256 KiB stack, so very
deep recursion belongs in the main code.  On Windows tasks run on fibers and behave the same, except that only sockets
(not files or the terminal) can make a task wait.

### Waiting for several channels: `select`

`select(…)` waits until one of several channel operations can happen, runs that case's block and gives its
position in the call (0 for the first).  A case is `ch.onReceive { v -> … }`, `ch.onSend(value) { … }`,
`ch.onClose { … }` (the channel is closed and empty), `onTimeout(ms) { … }` or `onDefault { … }` (runs at once
when nothing else can, so the select never waits).  When several can happen the first one listed wins, so put the
urgent ones first.

```aion
jobs := Channel<Int>()
quit := Channel<Int>()
launch {
    for n in 1..=3 { jobs.send(n) }
    quit.close()
}
sum := 0
running := true
while running {
    select(
        jobs.onReceive { sum += it },
        quit.onClose { running = false },
        onTimeout(1000) { println("too slow"); running = false })
}
println(sum)                                                     // 6

idle := Channel<Int>()
which := select(idle.onReceive { println("never") }, onDefault { println("nothing yet") })
println(which)                                                   // 1
```

A case whose channel is closed and empty is dropped for good (`onReceive` never fires again), and a `select`
with nothing left that can happen stops the program.  A `select` that sends on a channel without a buffer waits
for a receiver, and a `select` that receives counts as one.

## Threads

Tasks take turns on one processor.  **Threads** run side by side on several, for work that is mostly computing.
`thread { … }` starts one and gives a `Thread` (`join()`, `isDone()`); `compute { value }` runs a block on a thread and
gives a `Task<T>` whose `await()` is the result; `cpuCount()` says how many processors there are and `threadId()`
which thread you are on (0 is the main one).

```aion
parts := List<Task<Int>>()
for k in 0..4 {
    parts.add(compute {
        sum := 0
        for i in k * 250000..(k + 1) * 250000 { sum += i % 7 }
        sum
    })
}
total := 0
for p in parts { total += p.await() }
println(total)                                                   // 2999997
```

Threads share the heap: objects, lists and variables are visible to all of them, so data that is changed by more than
one thread needs protecting.  Use a `Mutex` (`lock()`, `unlock()`, `tryLock()` and `withLock { … }`), an `AtomicInt`
(`get`, `set`, `add`, `compareAndSet`), or pass the data through a `Channel`, which (like `Job`, `Task` and `select`)
is safe between threads.  Two threads changing a `List` or a `Map` at the same time can corrupt it.

```aion
hits := AtomicInt()
log := List<String>()
lock := Mutex()
workers := List<Thread>()
for w in 1..=4 {
    workers.add(thread {
        for i in 0..1000 { hits.add(1) }
        lock.withLock { log.add("worker $w done") }
    })
}
for w in workers { w.join() }
println(hits.get(), log.size())                                  // 4000 4
```

Each thread has its own tasks (`launch` and `async` inside a thread run on that thread), its own stack and its own
`try` and `defer`.  A panic in a thread that does not `try` it ends the program, and so does the end of the main
code, even if threads are still running.  If every thread is waiting for something that can no longer happen, the program stops
with the deadlock message.  The garbage collector stops all threads while it runs; a thread that computes for a long
time without creating any objects holds a collection up until it does.  Printing a single value is not interleaved
with another thread's, but `println` with several arguments is several prints.  The standard library modules (`http`, `sqlite`, `regex`, ...) are
not made for several threads at once: use one from one thread, or guard it with a `Mutex`.

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

* `import m` makes `m.name(...)` and `m.variable` available, and a type of the module can be
  written `m.Type` wherever a type is expected (`fn show(d: time.DateTime)`,
  `x: List<json.Json>`).
* **Folders are packages.**  `import shapes` loads `shapes/lib.aion` (or `shapes/shapes.aion`) and
  `import shapes.circle` loads `shapes/circle.aion`; there is no `package` keyword.  Two prefixes
  anchor an import: `import @util` starts at the package's root folder, and `import .circle` /
  `import ..shared.util` start at the importing file's folder / its parent (a relative import cannot
  leave the package).  They work with `from … import` and `as` too.
* A top-level name starting with `_` (`fn _helper()`, `type _Node`, `_cache := …`) is **private to its
  file**: other modules cannot use it (`_` alone and the prelude's `__x` helpers are exempt).
* A bare import is looked up as a file beside the importing file (or in its `lib/`
  folder, or the project root), then among the project's
  [dependencies](projects.md), then in the standard library.  A local file
  therefore shadows a standard-library module of the same name (with a warning).
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
| `typeOf(value)`            | the name of the value's type, as text                   |

Arguments may be Int, Float, Bool, String or objects.  Floats print with C's
`%g` (`3.0` prints as `3`).

`typeOf` gives the type the compiler knows (`Int`, `List<Int>`, `Point`, `(Int) -> Int`).  For a value
whose type can vary at run time (an `Any`, an interface, an `open type`) it gives the type the object
really is, and a null optional gives `"null"`.  (`type` is a keyword, so it cannot be the name.)

```aion
type Point(x: Int, y: Int)
println(typeOf(3), typeOf([1, 5, 1]), typeOf(Point(1, 2)))        // Int List<Int> Point

things := List<Any>()
things.add(1)
things.add("two")
things.add(Point(4, 5))
for t in things { print(typeOf(t), "") }                           // Int String Point
println()
```

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

A library that is not part of libc is linked by naming it: a `link = ["sqlite3"]` line
in `aion.project` (see [projects.md](projects.md)) or a `// link: sqlite3` comment at the
top of a standard-library module adds `-lsqlite3` to the build.

## Runtime errors

The program stops with exit code 1 and a message on stderr for: division by
zero, using a null object or function, indexing outside an array or list, out of
memory, and `panic(...)` — unless a [`try`](#cleanup-and-errors) catches it.

The message is followed by where it happened: each function of your own code that was running,
innermost first, with the line it was on.

```text
[aion] panic: insufficient funds
    at Account.withdraw (bank.aion:7)
    at process (bank.aion:15)
    at <top level> (bank.aion:21)
```

A lambda is `<lambda>`, top-level statements are `<top level>`, and the standard library's own
functions are left out (a bad index in `xs.get(i)` is reported at your line).  A very deep call chain
shows its first twelve and last three calls.  Keeping the trace costs a little speed on programs
that make very many small calls; `aion --no-trace file.aion` (or `aion build --no-trace`) leaves
it out.

A file with no `fn main()` and no top-level statements has nothing to run: `aion` says
`warning: no procedures to execute` and exits with status 0.  `--silent-empty-main` hides the warning.

## Not (yet) in the language

Inheritance from generic types.  (Plain object types are not checked against being
unset; use `T?` where a value may be missing.)
