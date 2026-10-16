# geometry

A small library package: points, polygons, convex hulls and shapes. It is meant to be *used by other projects*;
[`../geometry-demo`](../geometry-demo) does so.

* `lib.aion` — what `import geometry` loads.
* `shapes.aion` — `import geometry.shapes`.
* `tests/check.aion` — the package's own tests: `aion tests/check.aion` from this folder.

To publish it, put it in a git repository, tag a version, and let others depend on it:

```text
[dependencies]
geometry = { git = "https://github.com/you/geometry", tag = "v1.0.0" }
```

See [docs/packages.md](../../../docs/packages.md).
