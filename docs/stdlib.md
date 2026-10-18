# Standard library & C bindings

The standard library is a folder of modules:

```
stdlib/
├── prelude.aion   always in scope (panic, String methods, List, Map, Set, Entry, setOf)
├── prelude.c      …the C half of the prelude (String methods)
├── math.aion      import math       pi, sqrt, pow, sin, max, …
├── io.aion        import io         hello, banner, separator
├── strings.aion   import strings    length, contains, upper, …
├── sockets.aion   import sockets    TcpListener, TcpStream, TcpClient, UdpSocket
├── sockets.c      …the C half of sockets
├── gc.aion        import gc         collect, heap_size, object_count
└── name.aion
```

`build.rs` embeds the folder in the compiler.  **Adding a module needs no
registration:** drop `stdlib/<name>.aion` in (and optionally `stdlib/<name>.c`)
and `import <name>` works after the next `cargo build`.  (`import std.<name>` is the same module, spelled explicitly; a local file of the same name takes precedence over the bare form.)  The C file is
compiled and linked only into programs that import the module.

`print`, `println`, `panic`, `hashOf`, `Array`, and everything in `prelude.aion`
(the collections below and the `String` methods) are in scope without an import.

## `extern fn`

An `extern fn` declares a function that is implemented in C.  The declaration
*is* the binding — there is no second place to register it — and it is also the
documentation that editors and readers see.

```text
extern fn sqrt(x: Float) -> Float                  // C symbol: sqrt
extern fn abs(x: Float) -> Float = "fabs"          // Aion name differs from the C symbol
extern fn length(s: String) -> Int = "aion_strings_length"
```

* Without `= "symbol"` the C symbol is the function's own name.
* Parameter and return types are limited to `Int`, `Float`, `Bool`, `Char`, `String`, `Bytes`,
  `String?` and `Bytes?` (a C `NULL` is `null`); function values may be passed in, as an opaque
  closure pointer.
* The function is called like any other: `math.sqrt(2.0)`.  An `Int` argument is
  widened to `Float` where a `Float` is expected.
* `extern fn` works in any `.aion` file, so a program can bind libc/libm
  directly.  To bring your own C code, put it in a stdlib module.

### Type mapping

| Aion     | C                | Notes                                                    |
|----------|------------------|----------------------------------------------------------|
| `Int`    | `int64_t`        | exactly 64 bits — **not** `int` or `long`                |
| `Char`   | `int64_t`        | a Unicode code point                                     |
| `Float`  | `double`         |                                                          |
| `Bool`   | `bool`           | `<stdbool.h>`                                            |
| `String` | `const char *`   | NUL-terminated; see the ownership rules below            |
| `Bytes`  | `void *`         | raw data of any content; `aion_bytes_size()` / `aion_bytes_data()` |
| *(none)* | `void`           | omit `-> T`                                              |

### Strings across the boundary

* A `String` **passed to C** is an ordinary C string.  Read it, but don't free
  it and don't keep the pointer after returning.
* A `String` **returned from C** must be created with `aion_string(s)` or
  `aion_string_n(s, len)` (both copy), so the garbage collector owns it.  Never
  return a `malloc`ed or static buffer.

### Example: a complete C-backed module

`stdlib/strings.aion` (the prelude declares the same functions as `String`
methods with `extern fn String.length() -> Int = "aion_strings_length"`; the
receiver is the first C argument)

```text
extern fn length(s: String) -> Int = "aion_strings_length"
extern fn contains(s: String, needle: String) -> Bool = "aion_strings_contains"
extern fn upper(s: String) -> String = "aion_strings_upper"
```

`stdlib/prelude.c`

```c
#include "aion.h"
#include <ctype.h>
#include <string.h>

int64_t aion_strings_length(const char *s) { return (int64_t)strlen(s); }

bool aion_strings_contains(const char *s, const char *needle) {
    return strstr(s, needle) != NULL;
}

const char *aion_strings_upper(const char *s) {
    size_t n = strlen(s);
    char *copy = (char *)aion_string_n(s, n);      // GC-owned copy
    for (size_t i = 0; i < n; i++) copy[i] = (char)toupper((unsigned char)copy[i]);
    return copy;
}
```

That is the whole job: no registry entries, no codegen changes.

### The C API (`runtime/aion.h`)

```c
const char *aion_string(const char *s);              // copy into a new Aion String
const char *aion_string_n(const char *s, size_t n);  // …first n bytes
void aion_panic(const char *msg);                    // "[aion] panic: msg", exit 1

void *aion_bytes_new(int64_t len);                   // a zero-filled Bytes (never NULL; len may be 0)
int64_t aion_bytes_size(const void *bytes);          // its length
uint8_t *aion_bytes_data(void *bytes);               // its contents

int aion_wait_fd(int fd, int events, int64_t ms);    // let other tasks run until fd is ready (poll's POLLIN / POLLOUT)
```

Return a `Bytes` from C by creating it with `aion_bytes_new` and filling `aion_bytes_data`; return a
`Bytes?` by returning `NULL`.  A C function that waits for a socket or a pipe should call `aion_wait_fd`
instead of blocking, so that other [tasks](language.md#tasks-and-channels) keep running.  A `// link: name ...` comment at the
top of a module's `.aion` file adds `-lname` to the link step of every program that imports the module.

Don't call `aion_string*` while holding another Aion `String` obtained *from the
GC heap* in a C local — a collection may run inside the call.  (Arguments passed
to your function are safe: the caller keeps them alive for the duration of the
call.)

## Writing a pure-Aion module

Just write functions, types and (if you like) module variables such as `math.pi`; everything is public.  Prefix helpers with `_`
or `raw_` by convention if they are not meant for users.  `sockets.aion` is a
good example of a module that has a thin `extern fn` layer at the top and
ordinary Aion classes built on it.

## Module reference

### The prelude

Always in scope.

* `panic(message)`.
* **Input and arguments** — `readLine() -> String?` (the next line without its line
  break, `null` when the input has ended; pending output is flushed first, so a
  `print("name? ")` prompt shows up), `readText() -> String` (everything left),
  and `args() -> List<String>` (what follows the script: `aion script.aion a b`
  gives `["a", "b"]`).
* `eprint(text)` and `eprintln(text)` write to standard error.
* **Building text** — `StringBuilder()` with `append(text)`, `appendChar(c)`, `appendLine(text = "")`,
  `length()`, `truncate(n)`, `clear()` and `toString()`; `BytesBuilder()` does the same for bytes
  (`add`, `addText`, `addByte`, `find`, `take`, `toBytes`).  Use them instead of `text += piece` in a loop.
* **Char methods** — `isDigit() isLetter() isUpper() isLower() isWhitespace()`,
  `upper() lower()`, `code() -> Int`, `digitValue() -> Int?`, `byteLength()` (its
  UTF-8 size), `toString()`; and `Int.toChar()`.
* **String and characters** — `charAt(i)` / `s[i]` (the character that starts at
  byte `i`), `chars() -> List<Char>`; `for c in text` loops over the characters.
* **String methods** — `length()` (bytes), `charCount()`, `isEmpty()`, `isBlank()`,
  `contains(s)`, `startsWith(s)`, `endsWith(s)`, `indexOf(s)` and `lastIndexOf(s)`
  (-1 if absent), `substring(start, end)` (stops the program if the range is
  invalid), `trim()`, `trimStart()`, `trimEnd()`, `replace(old, new)`, `split(sep)`
  and `lines()` (a `List<String>`), `repeat(n)`, `padLeft(width, fill = " ")` and
  `padRight(width, fill = " ")` (widths count characters), `upper()`, `lower()`
  (ASCII).  Offsets are bytes.
* **Parsing numbers** — `isInt()`, `toIntOr(fallback)` and `toIntOrNull()` (the
  whole text must be a number), `parseInt(radix) -> Int?` (`"ff".parseInt(16)`),
  `isFloat()`, `toFloatOr(fallback)` and `toFloatOrNull()`.
* **Int and Float** — `n.toFloat()`, `x.toInt()` (towards zero), `x.round()`,
  `x.floor()`, `x.ceil()` (all give an `Int`), `x.format(decimals)` (text),
  `x.roundTo(decimals)` (a `Float`), `x.isNaN()`, `x.isInfinite()`, `abs() sign()
  min(o) max(o) clamp(low, high)` on both, `n.pow(k)`, `n.gcd(m)`,
  `n.toRadix(base)`, `n.toHex()`, `n.toBinary()`, and `toString()` on `Int`, `Float`
  and `Bool`.
* **Bytes** — raw binary data: `Bytes(n, fill = 0)`, `bytesOf(list)`, `b[i]`,
  `b[i] = v`, `b[a..b]`, `b + other`, `==`, `size() get set slice concat indexOf
  contains toHex() toText() toList()`, `String.toBytes()` and `String.fromHex()`.
  See the [language guide](language.md#bytes).
* `hashOf(value) -> Int` — a non-negative hash of an Int, Float, Bool, String
  (by content) or object (by identity); what `Map` and `Set` use.

#### Tasks

`launch(body) -> Job` (`join()`, `isDone()`), `async(body) -> Task<T>` (`await()`,
`isDone()`), `sleep(ms)`, `yield()`, `waitAll()`, and `Channel<T>(capacity = 0)` with
`send`, `receive() -> T?`, `tryReceive() -> T?`, `close()`, `isClosed()`, `size()`,
`forEach(action)` and `drain() -> List<T>`.  See the
[language guide](language.md#tasks-and-channels).

#### Ordering

`interface Comparable<T> { fn compareTo(other: T) -> Int }` (negative, zero or
positive); `Int`, `Float`, `String`, `Char` and `Bool` have `compareTo`, and any
type with a matching method qualifies.  `List` has `sorted()`, `sortedDescending()`,
`sortedBy(key)`, `sortedWith(compare)` (a stable O(n log n) merge sort that uses the runs already in the data, as Timsort does),
`maxOrNull()` and `minOrNull()`.

#### Collections

`Array<T>` is built into the compiler (see the [language guide](language.md#collections));
the rest is Aion source in `stdlib/prelude.aion`, so it doubles as a worked
example of generic types.

| Type | Creation | Members |
|------|----------|---------|
| `Array<T>` | `Array(n, fill)`, `Array<T>(n)` | `size()`, `get(i)`, `set(i, v)`, `clear(i)` (reset a slot to the zero value); `a[i]` |
| `List<T>` | `List<T>()`, `[a, b]` | `size() isEmpty() get set add addAll removeAt pop clear first last contains indexOf copy reversed take drop`; `getOrNull firstOrNull lastOrNull find` (return `T?`); `forEach map filter fold reduce any all count sortedWith join toString`; `plus` (`xs + ys`), `zip(other)` (a list of pairs), `component1()` … `component5()` |
| `Map<K, V>` | `Map<K, V>()`, `{k: v}` | `size() isEmpty() get set containsKey contains getOr getOrNull remove clear keys values entries forEach toString`; `m[k]`, `m[k] = v`; loops over its `Entry`s |
| `Entry<K, V>` | | `key`, `value`, `component1()`, `component2()` |
| `Tuple2<A, B>` … `Tuple5` | `(a, b)`, type `(A, B)` | `first second third fourth fifth`, `componentN()`, value `equals` / `hashCode` ([tuples](language.md#tuples-and-destructuring)) |
| `Range` | `1..5`, `1..=5`, `0..10 step 2`, `10 downTo 0` | `size() isEmpty() get first last contains step reversed toList sum forEach map filter fold any all count join`; loops and `in` work on it ([ranges](language.md#ranges)) |
| `Set<T>` | `Set<T>()`, `setOf(list)` | `size() isEmpty() contains add remove clear toList toString` |

`enumerate(xs)` is a list of `(index, element)` pairs: `for i, x in enumerate(xs)`.

A `Map` and a `Set` remember insertion order.  `List.sortedWith(compare)` takes a
function returning a negative, zero or positive `Int` and is a stable insertion
sort (fine for script-sized data).  A list prints as `[1, 2]`, a map as
`{a: 1}`, a set as `{1, 2}`.

#### Assertions

No import needed.  Each stops the program with a message (`aion test` counts that as a failed test) unless it holds:
`assert(condition, message?)`, `assertEq(actual, expected, message?)`, `assertNe(actual, unexpected, message?)`,
`assertFalse(condition)`, `assertNull(value)`, `assertNotNull(value)`, `assertClose(actual, expected, tolerance = 1e-9)`,
`assertContains(list, item)`, `assertContainsText(text, part)`, and `assertFails { … }`, which runs a block that must
panic and returns the panic's message.

### `std.test`

A small runner for many tests in one program.  `run(name) { … }` runs a block and prints `ok` or `FAIL` (a block
fails by panicking, which is what a failed assertion does) and goes on; `skip(name)`; `failed() -> Int`; and
`summary()`, which prints the totals and stops with status 1 if anything failed.

```aion
import test
defer test.summary()            // the totals are printed when the file ends

test.run("addition") { assertEq(1 + 1, 2) }
test.run("division by zero") { assertFails { 1 / 0 } }
test.skip("later")
```

### `std.log`

The default logger: `debug(message)`, `info`, `warn` and `error`, each taking an optional map of fields
(`log.warn("low", {"free": "3%"})` ends the line with `free=3%`).  A line goes to standard error as
`2026-10-18 12:00:00 INFO  message`.  The level starts at `info`, or at the environment variable `AION_LOG`
(`debug`, `info`, `warn`, `error`, `off`); `setLevel(name)` and `getLevel()` change and read it.
`setOutput(path)` appends to a file instead (`""` goes back to standard error), and `setFormat("{level} {name}{message}")`
chooses the layout from `{time}`, `{level}`, `{name}` and `{message}`.  `Logger("db")` is a logger with a name that
shows as `[db]`, with the same four methods.

```aion
import log

log.info("starting")
log.debug("not shown at the default level")
db := log.Logger("db")
db.warn("slow query", {"ms": "950"})
```

### `std.math`

Constants `pi tau e inf nan`; `sqrt cbrt pow exp log log2 log10 hypot` ·
`sin cos tan asin acos atan atan2 sinh cosh tanh` · `floor ceil round trunc fmod abs
fmax fmin` (Float → Float, from libm) · `toRadians toDegrees` · `max min` (Int, Int →
Int).  `Float.isNaN()` tells a `nan`; `nan == nan` is false, as in C.

### `std.fs` — files

```aion
import fs
import path

dir := fs.makeTempDir("demo-")!!
defer fs.removeAll(dir)
file := path.join(dir, "notes.txt")
fs.writeText(file, "one\ntwo\n")
println(fs.readLines(file), fs.size(file), fs.exists(file))
println(fs.readText(path.join(dir, "missing")), fs.lastError())   // null No such file or directory
```

`readText(path) -> String?`, `readLines`, `readBytes -> Bytes?`; `writeText`,
`writeLines`, `writeBytes`, `appendText`, `appendBytes` (→ `Bool`); `exists isDir
isFile size modified`; `listDir -> List<String>?` (sorted names), `mkdir`, `mkdirs`,
`remove` (a file or empty directory), `removeAll` (a tree), `rename`, `copy`; `cwd`,
`chdir`, `absolute -> String?`, `tempDir`, `makeTempDir(prefix) -> String?`.
Failure is `null`, `false` or `-1`, and `fs.lastError()` says why.  POSIX only.

### `std.path`

Paths as text: `join(a, b)`, `basename`, `dirname`, `extension`, `stem`,
`withExtension(p, ext)`, `isAbsolute`, `normalize` (resolves `.` and `..` without
touching the disk).  The separator is `/`.

### `std.os`

`env(name) -> String?`, `setEnv`, `unsetEnv`, `environment() -> Map<String, String>`;
`pid() cpuCount() hostname() platform() arch() homeDir()`; `exit(code)`.
Running programs: `exec(program, args, input = "") -> CommandResult` (no shell; collects
output; `status`, `stdout`, `stderr`, `ok()`), `shell(command, input = "")` (through
`sh -c`) and `run(program, args) -> Int` (shares the terminal).  `status` is -1 if the
program could not be started, 128 + N if signal N ended it.  The calling task waits for the program.

```aion no_run
import os
r := os.shell("ls | wc -l")
println(r.status, r.stdout.trim())
println(os.run("echo", ["hello"]))
```

### `std.time`

Clocks: `millis()` (wall clock, ms since 1970), `nanos()` and `monotonic()` (for measuring),
`measure { … } -> Int` (ms taken).  `DateTime(millis, utc = true)` with `year() month() day()
hour() minute() second() weekday() dayOfYear() weekdayName() monthName()`, `format(pattern)`
(strftime: `%Y-%m-%d %H:%M:%S %A %b %j %z`), `iso()`, `plus(ms) plusSeconds plusMinutes
plusHours plusDays plusMonths plusYears`, `since(other)`, `toUtc() toLocal()`; it compares
and works as a `Map` key.  `moment + ms` and `moment - ms` are a `DateTime` (`plus`), and `later - earlier` is the
milliseconds between them (`since`).  `now()`, `nowLocal()`, `of(year, month, day, hour = 0, …)`
(out-of-range fields carry over), `parse(text, pattern = "%Y-%m-%d %H:%M:%S") -> DateTime?`,
and the unit constants `second minute hour day` (ms).

### `std.random`

`int(bound)` (0 up to bound), `range(low, high)` (both included), `float()`,
`between(low, high)`, `bool()`, `chance(p)`, `choice(list)`, `shuffle(list)`,
`sample(list, n)`, `any()`, `seed(n)` for repeatable runs.  A fast generator that is
**not** for secrets: `secureBytes(n) -> Bytes` and `token(bytes = 16) -> String` use the
operating system's source.

### `std.unicode`

Unicode text handling with the [utf8proc](https://juliastrings.github.io/utf8proc/) library, found when first used
(install `libutf8proc`, for example the `libutf8proc3` package; a program that does not import `unicode` needs
nothing).  The built-in `lower()` and `upper()` know ASCII, Latin-1, Greek and Cyrillic; these know every script.

* `normalize(text, form = "NFC") -> String?` (`"NFC"`, `"NFD"`, `"NFKC"`, `"NFKD"`; null for an unknown form or text that
  is not valid UTF-8), so that "é" typed as one character and as `e` plus a combining accent compare equal.
* `lower(text)`, `upper(text)`, `title(text)` (one character at a time), `casefold(text)`, `sameText(a, b)` (equal apart from
  case and spelling of accents), `stripAccents(text)`.
* `category(c)` (`"Lu"`, `"Ll"`, `"Nd"`, `"Zs"`, `"Po"`, ...), `isLetter`, `isDigit`, `isUpper`, `isLower`, `isSpace`,
  `isPunctuation`.
* `width(text)`: the terminal columns it takes (wide East Asian characters 2, combining marks 0), `graphemes(text) ->
  List<String>?`: the characters as a reader sees them (a letter and its accent, a flag made of two code points).
* `version()`, `available()` and `lastError()`.

```aion
import unicode

println(unicode.sameText("Caf\u{e9}", "CAFE\u{301}"))      // true
println(unicode.stripAccents("Cr\u{e8}me br\u{fb}l\u{e9}e")!!)    // Creme brulee
println(unicode.width("\u{65e5}\u{672c}\u{8a9e}"))              // 6
```

### `std.bigint`

Integers of any size and exact fractions.  The arithmetic is [GMP](https://gmplib.org/)'s, found when first used (install
`libgmp`, for example the `libgmp10` package; a program that does not import `bigint` needs nothing).  Values never change.

```aion
import bigint

a := bigint.parse("123456789012345678901234567893")!!
b := bigint.of(2).pow(100)
println(a * b + 1, a / 7, a % 7, -a)         // operators take a BigInt or an Int, on either side
println(bigint.factorial(30), bigint.fibonacci(100), (a * a).sqrt())
println(bigint.of(7).modPow(bigint.of(128), bigint.of(13)), bigint.of(97).isProbablePrime())

half := bigint.rational(1, 2)
println(half + bigint.rational(1, 3))        // 5/6
```

* **Making one:** `of(n)`, `parse(text, base = 10) -> BigInt?` (an optional sign and digits; null if it is not a number),
  `ofFloat(x)`, `factorial(n)`, `fibonacci(n)`, `zero`, `one`.
* **Arithmetic:** `+ - * / %` and unary `-` (each also with an `Int`), `<`, `==` and the rest of the comparisons.  `/` and
  `%` round toward zero like `Int`'s; `floorDiv` and `mod` round down.  Dividing by zero stops the program.
  `abs`, `pow(n)`, `sqrt()`, `root(n)`, `gcd`, `lcm`, `choose(k)`, `modPow(exponent, modulus)`, `modInverse(modulus) -> BigInt?`.
* **Bits:** `and`, `or`, `xor`, `not`, `shl(n)`, `shr(n)` (as on two's complement integers of unlimited length), `bitLength()`.
* **Primes:** `isProbablePrime(rounds = 25)`, `nextPrime()`.
* **Reading one:** `toString()`, `toRadix(base)`, `toInt() -> Int?` (null if it does not fit), `toFloat()`, `signum()`, `isZero()`,
  `isNegative()`, `isEven()`.
* **Fractions:** `rational(numerator, denominator)` (BigInts or Ints) is always in lowest terms with a positive denominator, and
  has `+ - * /`, comparisons, `inverse()`, `pow(n)`, `floor()`, `ceil()`, `toFloat()`, `isInteger()`.

### `std.reflect`

Looking at a program's own types while it runs: the fields of an object and their types, reading and writing a field by
name, the methods and calling one by name, and making an object from the name of its type.

```aion
import reflect

type Point(x: Int, y: Int) {
    fn plus(other: Point) = Point(x + other.x, y + other.y)
}

p := Point(3, 4)
println(reflect.fields(p))                          // [x: Int, y: Int]
println(reflect.get(p, "x"), reflect.set(p, "y", 10), p.y)   // 3 true 10
println(reflect.methods(p))                         // [plus(Point) -> ?]
println(reflect.call(p, "plus", [Point(1, 1)]))     // Point(x=4, y=11)
q := reflect.create("Point", [7, 8])                // a Point made from its name
println(typeOf(q!!), reflect.isInstance(q!!, "Point"), reflect.typeNames())
```

* **Types:** `typeNames()`, `isKnown(name)`, `parentOf(name)`, `isInstance(x, name)` (also true for a type that extends `name`).
* **Fields:** `fields(x) -> List<FieldInfo>` (`name`, `typeName`; ancestors' first), `hasField(x, name)`, `get(x, name) -> Any?`,
  `set(x, name, value) -> Bool` (false if there is no such field or the value is of the wrong kind; an Int is accepted for a Float
  field).
* **Methods:** `methods(x) -> List<MethodSig>` (`name`, `paramTypes`, `returnType`, `owner`; the result type is `"?"` for a method
  whose type is inferred from its body), `hasMethod`, `call(x, name, args = []) -> Any?` (null for a method that gives nothing; the
  arguments are matched by number and kind, defaults are honoured, a wrong kind of argument stops the program like a failed `as`).
* **Making objects:** `create(typeName, args) -> Any?` (null for an unknown type or arguments no constructor takes) and
  `constructors(typeName)`.

How it works: when a program imports `reflect`, the compiler adds to each of the program's modules a short piece of ordinary Aion
code (visible as `<reflect>/module` in an error message) that registers the module's types, so a program that does not import
`reflect` pays nothing.  It covers the types the program declares itself, including `open` and extended ones and methods added
with `fn Type.name()` in the same module; it leaves out generic types, enums and interfaces, fields and methods whose types
include a function type, `static` methods, methods with their own type parameters or a `vararg`, and names that start with `_`.
Two types of the same name in different modules count as one.

### `std.strings`

Function forms of the `String` methods: `length(s) -> Int` (bytes),
`contains(s, needle)`, `starts_with(s, prefix)` (Bool), `upper(s)`, `lower(s)`
(ASCII).  Prefer the methods: `s.length()`.

### `std.hash`

Digests and checksums of text or `Bytes`.

```aion
import hash
println(hash.sha256("abc"))        // ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
println(hash.md5("abc"), hash.crc32("hello"))
key := "secret".toBytes()
println(hash.hmacSha256(key, "message".toBytes()).toHex().substring(0, 16))
```

`md5 sha1 sha256 sha512` take text and return lower-case hex; `md5Bytes sha1Bytes sha256Bytes
sha512Bytes` take and return `Bytes`; `hmacSha1 hmacSha256 hmacSha512(key, message)` return `Bytes`;
`crc32(text)` and `crc32Bytes(data, previous)` return an `Int`.  MD5 and SHA-1 are for checksums only.

### `std.encoding`

```aion
import encoding
println(encoding.base64("hello"), encoding.base64Decode("aGVsbG8=")?.toText())    // aGVsbG8= hello
println(encoding.urlEncode("a b&c"), encoding.urlDecode("a%20b"))                  // a%20b%26c a b
println(encoding.uuid().length())                                                  // 36
```

`base64`, `base64Bytes`, `base64Url`, `base64UrlBytes` (no padding, `-` and `_`),
`base64Decode(text) -> Bytes?` (either alphabet, padding optional);
`urlEncode`, `urlEncodePath` (keeps `/`), `urlDecode(text, plusIsSpace = false) -> String?`;
`uuid()` (random, version 4).  Hex is `bytes.toHex()` and `text.fromHex()`.

### `std.crypto`

Authenticated encryption, password hashing and signatures, using OpenSSL's `libcrypto` (found when first used;
a program that does not import `crypto` needs nothing installed).  Functions that can fail return null and
`lastError()` says why.

* `newKey(size = 32)` gives random key bytes.  `encrypt(key, data, context = Bytes(0))` is AES-GCM (a 16, 24 or
  32-byte key picks AES-128, 192 or 256) and gives a fresh nonce, the ciphertext and a tag in one `Bytes`;
  `decrypt(key, sealed, context)` is null when the key, the `context` or the data is wrong.  `encryptChaCha` and
  `decryptChaCha` are the same with ChaCha20-Poly1305 (32-byte key).  `encryptText(key, text) -> String?` (base64)
  and `decryptText(key, text)`.
* `pbkdf2(password, salt, iterations = 210000, length = 32, digest = "sha256")`; `hashPassword(password) -> String?`
  makes a line to store (`pbkdf2-sha256$210000$salt$hash`) and `verifyPassword(password, stored) -> Bool` checks it.
* `equal(a, b)` compares `Bytes` in time that does not depend on where they differ (use it for tokens and MACs).
* Ed25519: `newKeyPair() -> KeyPair?` (`privateKey`, `publicKey`), `publicKey(privateKey)`, `sign(privateKey, message)`
  (64 bytes) and `verify(publicKey, message, signature) -> Bool`.

```aion
import crypto

key := crypto.newKey()
sealed := crypto.encrypt(key, "secret".toBytes())!!
println(crypto.decrypt(key, sealed)!!.toText())      // secret

stored := crypto.hashPassword("hunter2")!!
println(crypto.verifyPassword("hunter2", stored))    // true
```

### `std.json`

```aion
import json

doc := json.parse(r"""{"name": "Ann", "tags": ["a", "b"], "address": {"city": "Oslo"}}""")
if doc == null {
    println(json.lastError())
} else {
    println(doc.getString("name"), doc.get("tags")?.at(1)?.asString(), doc.get("address")?.getString("city"))
    println(doc["tags"], doc.size(), doc.keys())
}

out := json.object()
out["ok"] = json.bool(true)
out["items"] = json.array().add(json.int(1)).add(json.string("two")).add(json.number(2.5))
println(out.compact())          // {"ok":true,"items":[1,"two",2.5]}
println(out.pretty(2).lines().size())
```

A parsed value is a `Json`: one of the types `JsonNull`, `JsonBool`, `JsonNumber`, `JsonString`,
`JsonArray` and `JsonObject` that extend it (an [inheritance](language.md#inheritance) example in the
library itself).  Every `Json` has `kind()`, `asString() asInt() asFloat() asBool()` (null for the
wrong kind), `get(key)`, `at(index)`, `size()`, `keys()`, `items()`, the `getString(key, fallback)`
family, `isNull()`, `compact()` and `pretty(indent = 2)`.  `parse(text)` returns null for invalid
text and `lastError()` gives the position and the problem.  Build documents with `object()`,
`array()`, `string(s)`, `int(n)`, `number(x)`, `bool(b)` and `nil()`.  Whole numbers stay exact
`Int`s; nesting deeper than 512 levels is refused.

### `std.regex`

```aion
import regex

r := regex.compile(r"(?<user>\w+)@(?<host>[\w.]+)")!!
m := r.find("write to ann@example.com today")!!
println(m.value(), m.named("user"), m.group(2), m.start())    // ann@example.com ann example.com 9
println(regex.compile(r"\s+")!!.replaceAll("a   b \t c", " "))                // a b c
println(regex.compile(r"(\d+)-(\d+)")!!.replaceAll("10-20", "$2-$1"))          // 20-10
println(regex.compile(r"[,;]\s*")!!.split("a, b;c"))                         // [a, b, c]
println(regex.matches(r"[a-z]+\d?", "abc1"), regex.test(r"^\d+$", "12a"))     // true false
```

Write patterns as [raw strings](language.md#strings).  Syntax: literals, `.`, classes
(`[a-z\d_]`, `[^…]`), `\d \D \w \W \s \S`, `\b \B \A \z`, `^ $`, groups `( )`, `(?: )`,
`(?<name> )`, alternation, `* + ? {n} {n,} {n,m}` and their lazy forms (`*?`), flags `"i"` (ignore case),
`"m"` (`^`/`$` at every line) and `"s"` (`.` matches line breaks).  Not supported: backreferences and
lookaround.  The matcher simulates the pattern instead of backtracking, so even `(a+)+b` on a long
text of `a`s answers at once.  Positions are byte offsets.

`compile(pattern, flags = "") -> Regex?` (`lastError()` says why not), `regex(pattern, flags)` (stops
the program on a bad pattern), `Regex`: `test find(text, from = 0) findAll count matches`
(the whole text) `replace replaceAll replaceWith(text) { match -> … } split`; `Match`: `value() start() end()
group(i) named(name) groups() groupStart(i) groupEnd(i)`.  A replacement may use `$1`, `${name}`
and `$$`.  `escape(text)` quotes a literal for use in a pattern.

### `std.net`

TCP clients and servers.  While one task waits for the network, the others keep running.

```aion
import net

server := net.listen(0, "127.0.0.1")!!        // port 0: any free port
launch {
    client := server.accept()
    if client != null {
        client.sendText("hello " + client.receive()!!.toText())
        client.close()
    }
}
conn := net.connect("127.0.0.1", server.port())!!
conn.sendText("net")
println(conn.receive()!!.toText())               // hello net
conn.close()
server.close()
```

`connect(host, port, timeoutMs = 30000) -> Conn?`, `listen(port, host = "", backlog = 128) -> Listener?`;
`Listener`: `accept(timeoutMs = -1) -> Conn?`, `port()`, `close()`; `Conn`: `send(bytes)`,
`sendText(text)`, `receive(max = 65536) -> Bytes?` (empty at the end of the stream, null on an error or a
timeout), `setTimeout(ms)`, `peer()`, `port()`, `startTls(host, verify = true, cafile = "", protocols = [])`, `protocol()`, `close()`.
`lastError()` explains a failure.  TLS uses OpenSSL's `libssl`, found at run time, so only programs that
use it need it; with `verify` the certificate must be valid for `host` and signed by a trusted authority
(the system's, or the file named by `cafile` or the `SSL_CERT_FILE` variable).  `protocols` (such as
`["h2", "http/1.1"]`) are offered to the server with ALPN, and `protocol()` says which one it chose.

Name lookup: `lookup(host) -> List<String>` gives the IP addresses of a name (IPv4 first; empty if it cannot be
resolved, and `lastError()` says why), and `reverse(ip) -> String?` the name of an address.  A slow lookup
holds up the other tasks.

```aion
import net

println(net.lookup("localhost").contains("127.0.0.1"))    // true
println(net.lookup("127.0.0.1"))                          // [127.0.0.1]
```

### `std.http2`

An HTTP/2 client (RFC 9113) over [`std.net`](#stdnet), next to the HTTP/1.1 client of [`std.http`](#stdhttp).
`https://` addresses negotiate `h2` with TLS (ALPN; the server must speak HTTP/2), `http://` ones speak HTTP/2 without
TLS ("prior knowledge"), which only some servers accept.  Answers are `http.Response` values with lower-case header names.

```aion no_run
import http2

r := http2.get("https://example.com/")
if r != null { println(r.status, r.header("content-type"), r.text().length()) }

conn := http2.connect("https://example.com")!!      // one connection, many requests at the same time
a := conn.request("GET", "/a")                      // call request from several tasks to overlap them
b := conn.request("POST", "/b", "data".toBytes(), {"content-type": "text/plain"})
conn.close()
```

`get`, `head`, `post(url, text, headers)`, `postBytes` and `request(method, url, body, headers)` each use a connection of their own;
all take `verify` and `timeoutMs` and give null on failure with `lastError()` saying why.  `connect(url) -> Connection?` keeps
the connection: `Connection.request(method, path, body = Bytes(0), headers = {})` and `close()`.  Flow control, settings,
pings and header compression are handled; server push is refused; compressed bodies are not requested.  `readFrame`,
`frameBytes` and the frame-type constants are the low level, for writing servers or tests.  The tests run it against a server
written in Aion and, when `nghttpd` is installed, against that.

### `std.hpack`

HPACK (RFC 7541), the header compression of HTTP/2: `Decoder(maxTableSize = 4096).decode(block) -> List<Header>?` (it keeps the
dynamic table between the blocks of one connection), `Encoder(huffman = true).encode(headers) -> Bytes`, `Header(name, value)`,
`huffmanEncode(bytes)` and `huffmanDecode(bytes) -> Bytes?`.  Checked against the examples of RFC 7541, appendix C.

### `std.websocket`

WebSockets (RFC 6455) over [`std.net`](#stdnet): a client for `ws://` and `wss://` (TLS needs OpenSSL's `libssl`)
and a server.

```aion
import websocket

server := websocket.listen(0, "127.0.0.1") { ws ->          // a task per client
    while true {
        msg := ws.receive()
        if msg == null { break }                              // null: the connection closed
        ws.send("you said: " + msg.text())
    }
}!!

ws := websocket.connect("ws://127.0.0.1:${server.port()}/chat")!!
ws.send("hello")
println(ws.receive()!!.text())                                // you said: hello
ws.close()
```

`connect(url, headers = {}, verify = true, timeoutMs = 30000) -> Socket?` and `lastError()`; `listen(port, host = "", handler)
-> Server?` (`Server.port()`, `close()`, and `accept() -> Socket?` to take clients one at a time).  A `Socket` has
`send(text)`, `sendBinary(bytes)`, `ping()`, `receive() -> Message?` (`binary`, `data`, `text()`), `close(code = 1000, reason = "")`,
`path` and `headers` of the handshake, `closed`, `closeCode`, `closeReason`, and `maxSize` (default 16 MiB; a larger message
closes the connection with 1009).  Fragments are joined and pings answered for you; `sendFrame(opcode, payload, fin)` sends a
single frame.  Compression and subprotocols are not supported; the TLS path is not covered by the tests.

### `std.smtp`

Send e-mail through an SMTP server.  `Server(host, port = 587, security = "starttls", username = "", password = "",
hello = "localhost", verify = true, timeoutMs = 30000)` says where and how: `security` is `"starttls"` (port 587),
`"tls"` (TLS from the start, port 465) or `"none"` (port 25, or a local test server); the login uses AUTH PLAIN or
LOGIN.  `Message(from, to, subject, body, cc = [], bcc = [], html = false, headers = {})` is sent as UTF-8 (base64),
with a non-ASCII subject encoded for you.  `send(server, message) -> Bool` and `lastError()`; `render(message)` gives
the text that would be sent, and `address("Ann <ann@example.org>")` the plain address (null if it is not one).
Values with line breaks, and addresses with spaces or angle brackets, are refused, so a value cannot add headers.
TLS needs OpenSSL's `libssl`, like [`std.net`](#stdnet); the TLS paths are not covered by the tests, which talk to a
small server written in Aion.

```aion no_run
import smtp

server := smtp.Server("smtp.example.com", 587, username: "me@example.com", password: "secret")
mail := smtp.Message(from: "me@example.com", to: ["ann@example.org"], subject: "Hello", body: "Hi Ann!\n")
if !smtp.send(server, mail) { println("not sent:", smtp.lastError()) }
```

### `std.http`

```aion no_run
import http
import json

r := http.get("https://example.com/api/items")
if r == null {
    println("failed: ${http.lastError()}")
} else if r.ok() {
    println(r.status, r.header("content-type"), r.json())
}

http.post("http://localhost:8080/notes", "hello", headers: {"X-Token": "abc"})
http.postJson("https://example.com/api", json.object().put("name", json.string("Ann")))
http.download("https://example.com/file.zip", "file.zip")
```

```aion
import http

server := http.listen(0, "127.0.0.1") { req ->
    if req.path == "/hello" { http.text("Hello, ${req.param("name") ?: "world"}!") } else { http.notFound() }
}!!
url := "http://127.0.0.1:${server.port()}"
println(http.get(url + "/hello?name=Ann")!!.text(), http.get(url + "/other")!!.status)   // Hello, Ann! 404
server.close()
```

Client: `get head delete(url, headers, options)`, `post put(url, text, headers, options)`, `postBytes`,
`postJson`, `putJson`, `request(method, url, headers, body, options)` and `download(url, path)`; each
returns a `Response?` — null when no answer arrived, with `http.lastError()` saying why (an answer with
status 404 is still a `Response`).  A `Response` has `status`, `reason`, `headers` (lower-case names),
`body` (`Bytes`), `ok()`, `text()`, `json()` and `header(name)`.  `Options(timeoutMs = 30000, follow = true,
maxRedirects = 10, verify = true, caFile = "", useProxy = true, maxBody = 268435456)`.  Redirects are
followed (a POST becomes a GET after 301/302/303); `http_proxy`, `https_proxy` and `no_proxy` are honoured
(never for `localhost` and `127.x`).  Responses are read as `Connection: close`, whole, with
`Content-Length`, chunked or until the stream ends; compressed answers are not requested.

Server: `listen(port, host = "") { req -> response } -> Server?` starts serving in the background and
`serve(...)` never returns.  A `Request` has `method`, `path`, `query` and `param(name)`, `headers`,
`body`, `peer`, `text()`, `json()` and `header(name)`; answer with `text`, `html`, `jsonResponse`,
`bytes`, `redirect`, `notFound` or `respond(status, body, contentType)` (add headers with
`.with(name, value)`).  A handler that panics answers 500.  Each connection is answered once and then closed.

### `std.sqlite`

```aion
import sqlite

db := sqlite.open(":memory:")!!
db.script("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, score REAL)")
db.exec("INSERT INTO users (name, score) VALUES (?, ?)", [sqlite.text("Ann"), sqlite.float(4.5)])
db.exec("INSERT INTO users (name, score) VALUES (?, ?)", [sqlite.text("Bob"), sqlite.nil()])
for row in db.query("SELECT id, name, score FROM users ORDER BY id")!! {
    println(row.int("id"), row.text("name"), row.float("score", -1.0), row.isNull("score"))
}
println(db.queryOne("SELECT count(*) AS n FROM users")!!.int("n"), db.lastInsertId())
db.close()
```

`open(path)` (or `":memory:"`) gives a `Database` with `script(sql)` (several statements, no rows), `exec(sql, params)`,
`query(sql, params) -> List<Row>?`, `queryOne`, `changes()`, `lastInsertId()`, `transaction { … }` (rolls back and
re-raises if the body panics) and `close()`.  Parameters (`?`) are `Value`s: `sqlite.int(5)`, `float`, `text`,
`blob(bytes)`, `bool`, `nil()`.  A `Row` reads by column name: `int text float blob isNull value(name)` (with
optional fallbacks); a `Value` has `kind`, `asInt() asFloat() asText() asBlob() isNull()`.  Failures return `false`/`null`
and `sqlite.lastError()` says why.  Building a program that imports `sqlite` needs the SQLite development files
(`libsqlite3-dev`); the module declares `// link: sqlite3` for you.

### `std.zlib`

```aion
import zlib

data := "hello hello hello hello hello hello".toBytes()
packed := zlib.gzip(data)!!
println(packed.size() < data.size(), zlib.gunzip(packed)?.toText())
println(zlib.decompress(zlib.compress(data, 9)!!)?.size(), zlib.gunzip("junk".toBytes()))   // 35 null
```

`gzip compress deflate(data, level = -1)` (gzip file format, zlib format, raw deflate; level 0–9) and
`gunzip decompress inflate decompressAny(data, maxSize = 1 GiB)`, all returning `Bytes?` — null for damaged or
truncated data or a result over `maxSize` (so a small file cannot expand into all of memory); `zlib.lastError()`
says which.  Needs `zlib1g-dev` to build.

### `std.csv`

```aion
import csv

rows := csv.parse("name,age\n\"Smith, Bob\",42\n")!!
println(rows.size(), rows[1][0], rows[1][1])                       // 2 Smith, Bob 42
for person in csv.parseRecords("name,age\nAnn,31\nBob,42")!! { println(person["name"], person["age"]) }
print(csv.format([["a", "b,c"], ["say \"hi\"", "1"]]))
```

`parse(text, delimiter = ",") -> List<List<String>>?` (RFC 4180: quoted fields, `""`, line breaks inside quotes,
`\r\n`; empty lines are skipped; null with `lastError()` for a bad quote), `parseRecords` (rows as maps keyed by the
first line), `format(rows, delimiter)`, `quote(field)`, `readFile(path)`, `writeFile(path, rows)`.

### `std.archive`

Reading and writing archives with the [libarchive](https://www.libarchive.org/) library, found when first used (install
`libarchive`, for example the `libarchive13t64` package; a program that does not import `archive` needs nothing).  It reads
zip, tar, 7z, rar, cpio, iso and more, with or without gzip, bzip2, xz or zstd compression, and finds the format itself.

```aion
import archive

archive.create("out.zip", {"hello.txt": "hi".toBytes(), "data/": Bytes(0)})      // a name ending in / is a folder
for e in archive.list("out.zip")!! { println(e.kind, e.name, e.size) }
println(archive.readText("out.zip", "hello.txt"))                                 // hi, without unpacking the rest
println(archive.extract("out.zip", "unpacked"))                                   // 1: the number of files written
archive.pack("site.tar.gz", "unpacked")                                           // a folder's contents, read from disk
```

* `list(path) -> List<Entry>?` (`Entry`: `name`, `size`, `modified` in seconds since 1970, `mode`, `kind` of `"file"`,
  `"folder"`, `"link"` or `"other"`, `isFile()`, `isFolder()`), `read(path, name, maxBytes) -> Bytes?`, `readText`.
* `extract(path, folder, maxBytes = 4 GiB) -> Int?` unpacks into a folder, over files of the same names, and gives the number
  of files written.  **It is careful**: a name that is absolute or climbs out with `..` stops it with an error, links and devices
  are never created, and more than `maxBytes` of output stops it, so a hostile archive cannot write elsewhere or fill the disk.
* `create(path, files, modified = now, allowUnsafeNames = false) -> Bool` makes an archive from a map of names to contents;
  `pack(path, folder) -> Bool` from a folder.  The format comes from the end of the name: `.zip`, `.7z`, `.tar`, `.tar.gz` or
  `.tgz`, `.tar.bz2`, `.tar.xz`, `.tar.zst`.  `create` refuses names that are absolute or contain `..`.
* Failures give null or false and `lastError()` says why.

### `std.xml`

XML and HTML as a tree of `Node`s, with CSS selectors to pick parts out.  `parse` reads XML with the
[expat](https://libexpat.github.io/) library, found when first used (install `libexpat`, for example the `libexpat1`
package; a program that does not import `xml` needs nothing).  It gives null for a document that is not well formed
(`lastError()` says where), loads no external entities, and refuses a document whose entities expand far beyond its size
or whose elements nest more than 500 deep.  `parseHtml` is written in Aion and never fails.

```aion
import xml

doc := xml.parse("<books><book id='1'><title>Dune</title></book><book id='2'><title>Emma</title></book></books>")!!
for b in doc.childrenNamed("book") { println(b.attr("id"), b.child("title")!!.text()) }      // 1 Dune / 2 Emma
println(doc.select("book[id=2] > title")[0].text())                                          // Emma
println(doc)                                                                                // back to text

page := xml.parseHtml("<ul class=menu><li><a href=/a>One<li><a href=/b>Two</ul>")           // untidy HTML is fine
for a in page.select("ul.menu li a") { println(a.attr("href"), a.text()) }                  // /a One / /b Two
```

* **Reading:** `parse(text, keepWhitespace = false) -> Node?` gives the root element (text made only of white space is
  dropped unless asked for), `parseHtml(text) -> Node` a `"#document"` node.  HTML names are lower case; an attribute
  without a value is `""`; `<li>`, `<p>`, `<td>`, `<tr>`, `<option>` and `<dt>`/`<dd>` close themselves, void tags (`<br>`,
  `<img>`, ...) have no children, `<script>` and `<style>` hold raw text, and character references are decoded.
* **A Node:** `name` (`"#text"` and `"#comment"` for those, the characters in `content`), `attributes`, `children`, `parent`;
  `attr(name)`, `hasAttr`, `elements()`, `child(name)`, `childrenNamed(name)`, `find(name)`, `findAll(name)` (`"*"` for all),
  `text()` (all the text below), `position()`, `siblings()`.
* **Selectors:** `select(selector) -> List<Node>`, `selectFirst`, `matches`, with tag, `*`, `#id`, `.class`, `[attr]`,
  `[attr=v]` and `^= $= *= ~= |=`, descendant, `>`, `+` and `~`, commas, and `:first-child`, `:last-child`, `:only-child`,
  `:nth-child(odd | even | n | an+b)`, `:nth-last-child`, `:empty`, `:root`, `:not(...)` and `:contains(text)`.  A selector that
  cannot be read stops the program; `validSelector(text)` checks one that comes from outside.
* **Writing:** `toXml()`, `pretty(indent = 2)`, `toHtml()` (void tags, raw script and style) and `toString()`; build trees with
  `element(name, attributes, children)`, `text(content)`, `comment(content)` and `Node.append`, `Node.detach`; `escape(text)`.

### `std.cli`

```aion
import cli

p := cli.parser("greet", "say hello")
p.flag("loud", "l", "shout")
p.option("times", "n", "how often", "1")
p.positional("name", "who to greet")
a := p.parseOrExit(["-l", "--times=2", "Ann"])
println(a.get("name"), a.flag("loud"), a.getInt("times"))        // Ann true 2
```

`parser(program, about)`, `flag(name, short, help)`, `option(name, short, help, default)`, `multi` (repeatable),
`positional(name, help)`, `rest(name, help)`; `parse(list) -> Args?` (null after `--help` or a mistake: `wantsHelp`,
`message`) and `parseOrExit(list)` (prints the usage and exits).  Accepts `--long`, `--long=v`, `--long v`, `-s`, `-sv`,
`-abc`, `--` and negative numbers as arguments.  `Args`: `flag get getInt getFloat has all rest`.  `usage()` is the
help text.  Pass `args()` from the program.

### `std.term`

`bold dim italic underline inverse`, `black red green yellow blue magenta cyan white gray`, `color(text, 0..255)`,
`rgb(text, r, g, b)` wrap text in ANSI codes — only when output is a terminal and `NO_COLOR` is unset
(`term.enabled` overrides); `strip(text)` removes codes.  `isTerminal(fd = 1)`, `size() -> Size(columns, rows)`,
`clearScreen clearLine moveTo(column, row) hideCursor showCursor`.  For interactive programs, `rawMode(true)` makes
`readKey(timeoutMs = -1)` return single key presses (`"a"`, `"enter"`, `"up"`, `"ctrl-c"`, …; `""` on timeout,
`null` at the end of the input) while other tasks keep running.

### `std.sockets`

`TcpListener(port)` → `.listen(backlog)`, `.accept() -> TcpStream`, `.close()`;
`TcpStream` → `.send(text) -> Int`, `.receive(max_bytes) -> String`, `.close()`,
`.isValid() -> Int`; `TcpClient(host, port)`; `UdpSocket(port)`.  Methods report
errors as negative `Int`s.  `send` writes the whole string.

### `std.gc`

`collect()`, `heap_size() -> Int`, `object_count() -> Int`.  Set the environment
variable `AION_GC_STRESS=1` when running a program to collect on every
allocation (slow; useful for chasing memory bugs).

### `std.io`, `std.name`

Tiny helpers (`io.hello()`, `io.banner()`, `io.separator()`, `io.newline()`;
`name.name()`).
