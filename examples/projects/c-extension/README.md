# c-extension

An Aion project that carries its own C code.

* `native/bits.c` — the C functions. They `#include "aion.h"` for strings, bytes and panics.
* `aion.project` — `c = ["native/bits.c"]` compiles it into the program; `link = ["m"]` links libm.
* `lib/native.aion` — one `extern fn` line per C function, plus an ordinary Aion wrapper.
* `main.aion` — uses them.

```text
cd examples/projects/c-extension
aion
```

The same `c = [...]` setting works in a *package*, so a library can ship with its C code; see
[docs/packages.md](../../../docs/packages.md).
