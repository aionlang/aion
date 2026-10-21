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
packages = "aion-dependencies"  # optional: download git dependencies into this folder (default: the shared cache)
link = ["sqlite3"]            # optional: system libraries the package's C code needs (-lsqlite3)
c = ["native/glue.c"]         # optional: C files compiled into programs that use the package

[dependencies]
utils = { path = "../utils" }
http  = { git = "https://github.com/someone/http", tag = "v1.2" }
```

`aion new myapp` creates this layout, and a `.gitignore` that keeps `aion-dependencies/` out of git.

`c` lists C source files of the package (paths inside the project); they are compiled into every program that
uses it and may `#include "aion.h"`; see [packages.md](packages.md).  `link` lists system libraries (`-l<name>`) that are added to the link step of any program
that uses the package, so a package with its own `extern fn` bindings can call into
`libsqlite3`, `libz` and the like.  The library and its development files must be installed.

## Commands

| Command                    | What it does                                                    |
|----------------------------|-----------------------------------------------------------------|
| `aion`                     | run the project's entry (from the project folder or any subfolder) |
| `aion file.aion`           | run one file; if it lies inside a project, that project's dependencies are available |
| `aion build [-o out]`      | build an executable (named after the project by default)        |
| `aion add <source>`        | add a dependency to the project (or a script) — see below       |
| `aion install [<source>]`  | `add`, and download it; a new project asks where to keep packages. With no source: download what the project needs |
| `aion remove <name>`       | remove one                                                      |
| `aion update [name]`       | fetch newer commits for git dependencies (all, or one)          |
| `aion tree [file]`         | show the dependency tree with the pinned commits                |
| `aion fetch`               | download dependencies now (other commands do it on demand)      |
| `aion new <name>`          | create a project                                                |
| `aion clean`               | empty the build cache                                           |

## The ladder: script → script with dependencies → project

You only need as much structure as the program has.

```text
hello.aion                      one file: aion hello.aion
tool.aion  (// dep: lines)      one file that uses other people's code
myapp/aion.project + main.aion  several files, a name, dependencies, native code
```

**Scripts with dependencies.**  Put `// dep:` lines in the first comment lines of the script (after an
optional `#!` line); no project or folder is needed.  Git repositories are fetched into the cache and pinned
in `tool.aion.lock` next to the script.

```text
#!/usr/bin/env aion
// dep: http  = github.com/someone/http@v1.2
// dep: utils = ../utils
import http
import utils
```

A source is a path (`./x`, `../x`, `/abs`, `~/x`) or a git repository: `github.com/user/repo`
(also `gitlab.com`, `codeberg.org`, `bitbucket.org`), a full `https://…`, `ssh://…` or `git@host:…` URL,
optionally followed by `@REF`: `@v1.2` (a tag, branch or commit), or `@tag:v1.2`, `@branch:dev`,
`@rev:9fceb02`.  Only the entry script's header is read, and inside a project the lines are ignored
(with a warning) — use `aion.project` there.

**Managing dependencies.**

```text
aion add github.com/someone/http@v1.2      # into the project found here or above
aion add ../utils --as tools               # a path; --as picks the import name
aion add github.com/me/x --branch dev      # --tag / --branch / --rev
aion add github.com/me/x --script tool.aion   # into the script's header instead
aion remove http
aion update          # or: aion update http
aion tree
```

`aion add` writes to the nearest `aion.project` — in the current folder without asking; if it is further
up it asks `Use it? [Y/n]`; if there is none it asks whether to create one here (or you pass `--script`).
Without a terminal, or with `--yes`, it never asks and takes those defaults, and it always says what it did.
The name defaults to the repository or folder name (without `.git` and a leading `aion-`).  After editing it
fetches the dependency; if that fails the file is put back as it was.

**Which project?**  The nearest `aion.project` in the script's folder or above — but the search stops
before your home folder (`$HOME`), so a stray `~/aion.project` never captures unrelated scripts.
`--project <file-or-folder>` names one explicitly.

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

A dependency can be any folder: `import utils.strings.pad` also finds `utils/strings/pad/lib.aion`.

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

### Keeping the packages in the project

By default the clones live in the shared cache, which `aion clean` empties.  To keep a project's packages
beside it, run `aion install` instead of `aion add`:

```text
$ aion install github.com/someone/http@v1.2
No aion.project in this folder or above. Create one here? [Y/n]
Where should packages be downloaded? A folder in the project, or `cache` to share one copy between projects. [aion-dependencies]
added http (github.com/someone/http@tag:v1.2) to aion.project (created)
packages are downloaded into aion-dependencies
added it to .gitignore
```

This writes `packages = "aion-dependencies"` into `aion.project` and adds the folder to `.gitignore`
(creating the file if there is none).  Every git dependency, including the dependencies of dependencies, is then
downloaded to `aion-dependencies/<name>-<commit>/` as plain files, without the `.git` folder.  Because
`aion.lock` pins the commits, `aion install` with no source downloads exactly the same files again, for
example after a fresh `git clone` of your project.  Answer `cache` (or pass `--packages cache`) to keep using
the shared cache, `--yes` to take the defaults without questions, and `--packages DIR` to choose another folder, also
for an existing project.  Delete the line from `.gitignore` if you prefer to commit the packages.

`aion fmt`, `aion doc` and `aion test` skip a folder of downloaded packages.

## Documentation

`aion doc` writes an API reference, in Markdown, from the declarations of the project and the
[`/** … */` comments](language.md#documentation-comments) in front of them:

```text
aion doc                      # the whole project (not tests/ or examples/), to the standard output
aion doc src/lib.aion         # just this file, or every file in this folder
aion doc -o API.md            # to a file
```

Each file is a section, in the order of the source: every public function, type (with its fields, methods and
computed properties), enum, interface and synonym gets a heading, its signature and its comment.  Names that start
with `_` and the members the compiler makes for a `data type` are left out.  A file that does not parse is
skipped with a message, and the exit status is 1.

## The build cache

`aion file.aion` reuses a previously built binary when the generated program is
identical, so re-running an unchanged script takes milliseconds instead of a
clang compile.  The cache lives in `$AION_CACHE`, else `~/.cache/aion`
(`%LOCALAPPDATA%\aion` on Windows), and keeps the 128 most recent programs.
`AION_NO_CACHE=1` bypasses it and `aion clean` empties it.
