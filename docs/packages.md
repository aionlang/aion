# Writing and sharing packages

A package is a project other projects can depend on.  There is no registry to
upload to: **a package is a git repository**, and a dependency names the
repository and a version (a tag).  Publishing is `git tag` and `git push`.

## 1. Lay it out

```text
slugify/
├── aion.project
├── lib.aion            # what `import slugify` loads
├── internal/
│   └── tables.aion     # other files: `import slugify.internal.tables` from outside
├── tests/
│   └── check.aion
└── README.md
```

```text
name = "slugify"
version = "1.0.0"
lib = "lib.aion"
```

* `lib` is the file `import slugify` loads (default: `lib.aion`, else `<name>.aion`).
  Everything a module defines is public, so decide what belongs in `lib.aion`:
  re-export a few names rather than making users reach into your folders.
* Other modules of the package are reached as `import slugify.internal.tables`
  from outside and as `import internal.tables` from inside.
* The name your users write in `import` is *their* name for the dependency, so
  keep your own `name` short and unsurprising.

```text
// lib.aion
from internal.tables import foldAccents

fn slugify(text: String) -> String {
    out := StringBuilder()
    for c in foldAccents(text.lower()) {
        if c.isLetter() || c.isDigit() { out.appendChar(c) } else if out.length() > 0 && !out.toString().endsWith("-") { out.append("-") }
    }
    return out.toString().trimEnd()
}
```

## 2. Test it

Tests are plain programs that stop with a message (or a non-zero exit code) when something
is wrong.  Keep them in `tests/` and run them with `aion tests/check.aion`; a test sees the
project's own `lib` like any other module:

```text
// tests/check.aion
import slugify

fn expect(actual: String, wanted: String) {
    if actual != wanted { panic("slugify gave '$actual', wanted '$wanted'") }
}

expect(slugify.slugify("Hello, World!"), "hello-world")
println("ok")
```

## 3. Depend on other packages

```text
[dependencies]
utils = { path = "../utils" }                                      # during development
http  = { git = "https://github.com/someone/aion-http", tag = "v1.2.0" }
```

Each package sees only the dependencies it declares itself, and a dependency of a
dependency is fetched automatically.  The first build records exact commits in
`aion.lock`; commit that file in *applications*.  In a *library*, leave it out of
version control (or keep it for your own tests): your users' lock file decides.

## 4. Bring C code along

A package can wrap C code of its own — a codec, a binding, a fast inner loop:

```text
name = "fastsum"
lib = "lib.aion"
c = ["native/sum.c"]        # compiled into every program that uses the package
link = ["m"]                # system libraries it needs (-lm); they must be installed
```

```text
// native/sum.c
#include "aion.h"

int64_t fastsum_total(int64_t n) {
    int64_t t = 0;
    for (int64_t i = 1; i <= n; i++) t += i;
    return t;
}
```

```text
// lib.aion
extern fn total(n: Int) -> Int = "fastsum_total"
```

* C files must be inside the package (no absolute paths or `..`).
* `aion.h` gives you `aion_string(...)` to return text, `aion_bytes_new(...)` for binary data,
  `aion_panic(...)` for errors and `aion_wait_fd(...)` to let other tasks run while you wait; see
  [stdlib.md](stdlib.md#the-c-api-runtimeaionh).
* Prefix your C symbols with the package name (`fastsum_total`): all C code of a program
  shares one symbol table.
* A program that uses your package needs a C compiler (it needs one anyway) and the libraries you
  `link`.

## 5. Publish

```text
git add -A && git commit -m "slugify 1.0.0"
git tag v1.0.0
git push origin main --tags
```

That is all.  Users add

```text
slugify = { git = "https://github.com/you/slugify", tag = "v1.0.0" }
```

to their `aion.project`.  Some conventions:

* **Version with tags** named `vMAJOR.MINOR.PATCH`.  Users who want a fixed version name a tag or a
  `rev`; `branch` follows moving code.
* **Never move a published tag.**  `aion.lock` pins the commit, but other clones do not know.
  Fix mistakes with a new patch release.
* Raise the major number when you remove or change the meaning of a public name.
* Put a `README.md` with a short example, the functions you export and the C libraries
  (if any) that must be installed.
* Mention in the README what the package needs from the machine (`libsqlite3-dev`, a TLS
  library, ...): Aion links only what `link` lists.

## 6. Use a package from a private repository

`git` does the fetching, so whatever works for `git clone` works here: SSH URLs
(`git@github.com:you/private.git`), a credential helper, or a deploy token in the URL.  Keep
secrets out of `aion.project` by using SSH keys or a helper.
