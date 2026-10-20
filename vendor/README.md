# Vendored C libraries

Libraries that standard-library modules need, as source, so that a program using them builds on a machine with
nothing installed.  A module asks for one with a `// vendor: name` line at the top of its `.aion` file (like
`// link: name` for a system library).  The compiler embeds these folders; when a program needs one, it compiles the
`.c` files once with the C compiler it uses (the build flags are the lines of the folder's `FLAGS` file), keeps the
object files in the cache directory, and links them in.  A `SOURCES` file, where there is one, names the `.c` files to
compile (the others are included by those).  The module's own C file includes the library's header.

Files are copied unchanged from the upstream release.  To update a library, replace the files and the version here,
and run the test suite.

| Folder     | Library    | Version | Source | License |
|------------|------------|---------|--------|---------|
| `sqlite3`  | SQLite     | 3.53.2 (the amalgamation, `sqlite3.c` and `sqlite3.h`, as packaged by the `libsqlite3-sys` 0.38.2 crate) | https://sqlite.org/ | public domain |
| `zlib`     | zlib       | 1.3.1 (`v1.3.1`, commit `51b7f2abdade71cd9bb0e7a373ef2610ec6f9daf`; the library sources only) | https://github.com/madler/zlib | zlib license (`zlib/LICENSE`) |
| `utf8proc` | utf8proc   | 2.12.0 (`v2.12.0`, commit `79cdd5ab40c79afa559f689ffc13b76812dee1ac`; `utf8proc.c`, `utf8proc.h`, `utf8proc_data.c`) | https://github.com/JuliaStrings/utf8proc | MIT (`utf8proc/LICENSE.md`) |
