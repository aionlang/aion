# The REPL

`aion repl` is an interactive prompt: type a statement, a declaration or an expression, and see the result.

```text
$ aion repl
Aion 0.3.0 — :help for commands, Ctrl-D to leave
>>> 1 + 2
3
>>> nums := [3, 1, 2]
>>> nums.add(0)
>>> nums.sorted()
[0, 1, 2, 3]
>>> fn double(n: Int) = n * 2
>>> nums.map { double(it) }
[6, 2, 4, 0]
>>> if nums.size() > 3 {
...     println("long")
... }
long
```

* An **expression** prints its value (`typeOf(nums)` prints `List<Int>`); one that has no value, like
  `nums.add(0)`, just runs.
* A **declaration** (`fn`, `type`, `enum`, `interface`, `synonym`, `import`) is remembered.  Entering a
  function or type again with the same head replaces it; a function of the same name with other
  parameters becomes another version (see [overloading](language.md#overloading)).
* The prompt becomes `...` while a bracket or string is open.  A blank line ends an entry that is still open.
* A line that fails to compile, or panics, is reported and forgotten: the session stays as it was.
* Commands: `:help`, `:show` (the session as a program), `:reset`, `:quit` (or Ctrl-D).

## How it works

There is no incremental compiler, so each entry is compiled together with the session so far: the imports
and declarations, then the statements you have run, then the new entry.  The program prints a marker before the
new entry and the REPL shows only what comes after it.  A build of a small program takes a fraction of a
second, and the REPL builds without optimisation (`-O1` or `-O3` on the command line changes that).

The earlier statements therefore **run again** with every entry, with their output hidden.  This is
invisible for variables, lists and printing, but a statement with an outside effect (writing a file,
sending a request, a random number or the time) repeats, and a different random number is drawn each time.
`:reset` starts over.  Programs cannot read standard input in the REPL; it is the prompt's.
