# folder-packages

A folder is a package: `shapes/` has a `lib.aion` (what `import shapes` loads) and a `circle.aion`
(`import shapes.circle`). `@util` starts at the program's root, `.circle` at the importing file's folder,
and `_secret` is private to its file. Run it with `aion main.aion`.
