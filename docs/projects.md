# Projects and dependencies

A single `.aion` file runs on its own.  A **project** adds a name, an entry point
and dependencies, described by an `aion.project` file.

```text
myapp/
├── aion.project
├── main.aion
└── lib/            (optional: modules imported as `import name`)
```

```text
name = "myapp"
version = "0.1.0"
entry = "main.aion"           # optional; this is the default
lib = "src/lib.aion"          # optional: what `import myapp` loads in a dependent project
link = ["sqlite3"]            # optional: system libraries the package's C code needs (-lsqlite3)

[dependencies]
utils = { path = "../utils" }
http  = { git = "https://github.com/someone/http", tag = "v1.2" }
```

`aion new myapp` creates this layout.

`link` lists system libraries (`-l<name>`) that are added to the link step of any program
that uses the package, so a package with its own `extern fn` bindings can call into
`libsqlite3`, `libz` and the like.  The library and its development files must be installed.

## Commands

| Command                    | What it does                                                    |
|----------------------------|-----------------------------------------------------------------|
| `aion`                     | run the project's entry (from the project folder or any subfolder) |
| `aion file.aion`           | run one file; if it lies inside a project, that project's dependencies are available |
| `aion build [-o out]`      | build an executable (named after the project by default)        |
| `aion fetch`               | download dependencies now (other commands do it on demand)      |
| `aion new <name>`          | create a project                                                |
| `aion clean`               | empty the build cache                                           |

## Dependencies

Each entry under `[dependencies]` is a name and a source:

| Source                                       | Meaning                                    |
|----------------------------------------------|--------------------------------------------|
| `{ path = "../utils" }`                      | a folder, relative to the project file     |
| `{ git = "URL" }`                            | the repository's default branch            |
| `{ git = "URL", tag = "v1.0" }`              | a tag                                      |
| `{ git = "URL", branch = "dev" }`            | a branch                                   |
| `{ git = "URL", rev = "9fceb02" }`           | a commit                                   |

The dependency's own `aion.project` (if it has one) declares *its* dependencies,
which are fetched too.  Each package sees only the dependencies it declares
itself.

### Importing from a dependency

```text
import utils               # the dependency's lib file (below)
import utils.strings.pad   # utils/strings/pad.aion inside the dependency
import utils.shout         # `shout` from the lib file, if no shout.aion exists
from utils import shout, pad
```

`import utils` loads the dependency's **lib file**: the `lib = "…"` named in its
`aion.project`, else `lib.aion`, else `<dependency-name>.aion` in its root.  The
name you import by is the name in *your* `[dependencies]`, not the one the
library calls itself.

Name lookup for `import x` tries, in order: a file beside the importing file (or in
`lib/`, or the project root), then a declared dependency, then the standard
library.

### Git dependencies and `aion.lock`

The first build clones the repository into the cache, checks out the tag, branch
or commit you asked for, and records the exact commit in **`aion.lock`** next to
`aion.project`.  Later builds use that commit, so they are reproducible and work
offline — even if a tag is moved or the repository disappears — as long as the
cache still has the clone.  Commit `aion.lock` to your repository.  To move to a
newer version, delete its line from `aion.lock` (or the file) and build again.
Git dependencies need `git` on `PATH`.

## The build cache

`aion file.aion` reuses a previously built binary when the generated program is
identical, so re-running an unchanged script takes milliseconds instead of a
clang compile.  The cache lives in `$AION_CACHE`, else `~/.cache/aion`
(`%LOCALAPPDATA%\aion` on Windows), and keeps the 128 most recent programs.
`AION_NO_CACHE=1` bypasses it and `aion clean` empties it.
