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
* Parameter and return types are limited to `Int`, `Float`, `Bool`, `String` and
  `String?` (a C `NULL` is `null`); function values may be passed in, as an opaque
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
```

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
`sortedBy(key)`, `sortedWith(compare)` (a stable O(n log n) merge sort),
`maxOrNull()` and `minOrNull()`.

#### Collections

`Array<T>` is built into the compiler (see the [language guide](language.md#collections));
the rest is Aion source in `stdlib/prelude.aion`, so it doubles as a worked
example of generic types.

| Type | Creation | Members |
|------|----------|---------|
| `Array<T>` | `Array(n, fill)`, `Array<T>(n)` | `size()`, `get(i)`, `set(i, v)`, `clear(i)` (reset a slot to the zero value); `a[i]` |
| `List<T>` | `List<T>()`, `[a, b]` | `size() isEmpty() get set add addAll removeAt pop clear first last contains indexOf copy reversed take drop`; `getOrNull firstOrNull lastOrNull find` (return `T?`); `forEach map filter fold reduce any all count sortedWith join toString` |
| `Map<K, V>` | `Map<K, V>()`, `{k: v}` | `size() isEmpty() get set containsKey contains getOr getOrNull remove clear keys values entries forEach toString`; `m[k]`, `m[k] = v`; loops over its `Entry`s |
| `Entry<K, V>` | | `key`, `value` |
| `Set<T>` | `Set<T>()`, `setOf(list)` | `size() isEmpty() contains add remove clear toList toString` |

A `Map` and a `Set` remember insertion order.  `List.sortedWith(compare)` takes a
function returning a negative, zero or positive `Int` and is a stable insertion
sort (fine for script-sized data).  A list prints as `[1, 2]`, a map as
`{a: 1}`, a set as `{1, 2}`.

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
and works as a `Map` key.  `now()`, `nowLocal()`, `of(year, month, day, hour = 0, …)`
(out-of-range fields carry over), `parse(text, pattern = "%Y-%m-%d %H:%M:%S") -> DateTime?`,
and the unit constants `second minute hour day` (ms).

### `std.random`

`int(bound)` (0 up to bound), `range(low, high)` (both included), `float()`,
`between(low, high)`, `bool()`, `chance(p)`, `choice(list)`, `shuffle(list)`,
`sample(list, n)`, `any()`, `seed(n)` for repeatable runs.  A fast generator that is
**not** for secrets: `secureBytes(n) -> Bytes` and `token(bytes = 16) -> String` use the
operating system's source.

### `std.strings`

Function forms of the `String` methods: `length(s) -> Int` (bytes),
`contains(s, needle)`, `starts_with(s, prefix)` (Bool), `upper(s)`, `lower(s)`
(ASCII).  Prefer the methods: `s.length()`.

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
