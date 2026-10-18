# Trying Aion with Docker

The image contains the compiler, `clang` and the libraries the standard library needs, so you can
run Aion programs without installing Rust, cargo or clang.

## Build the image

```bash
docker build -t aion .
```

## Run your own file

Mount the folder with your source into `/work`; the image's entry point is `aion`, so everything
after the image name is a normal `aion` command line.

```bash
# Linux / macOS / Git Bash
docker run --rm -v "$PWD":/work aion hello.aion

# PowerShell
docker run --rm -v "${PWD}:/work" aion hello.aion

# arguments for the program go after the file
docker run --rm -v "$PWD":/work aion cli_tool.aion --help
```

A program that reads stdin needs `-i` (`-it` for a terminal):

```bash
docker run --rm -it -v "$PWD":/work aion examples/guess_number.aion
```

A program that listens on a port (`http`, `net`) needs it published:

```bash
docker run --rm -p 8080:8080 -v "$PWD":/work aion examples/echo_server.aion
```

## Bundled examples

The repository's `examples/` are baked into the image at `/opt/aion/examples`:

```bash
docker run --rm aion /opt/aion/examples/tour.aion
```

## Other commands

```bash
docker run --rm -v "$PWD":/work aion build hello.aion      # writes ./hello (a Linux binary)
docker run --rm -v "$PWD":/work aion emit-ir hello.aion    # print the LLVM IR
docker run --rm -v "$PWD":/work aion new myapp             # create a project
```

## A shell inside the container

```bash
docker run --rm -it -v "$PWD":/work --entrypoint bash aion
```

## A Windows executable

`Dockerfile.windows` cross-compiles the compiler with MinGW, so a Windows `aion.exe` needs nothing installed but Docker:

```powershell
docker build -f Dockerfile.windows --output dist .
.\distion.exe --version
```

The executable runs the compiler's own commands (`check`, `emit-ir`, `fmt`, `new`, `help`).  Compiling and running programs on
Windows also needs clang on the `PATH`, and the C parts of the standard library are still POSIX only, so run programs with the
image above (or WSL) for now.

## Keeping the build cache

Compiled programs and cloned git dependencies go to `/cache`. Mount a named volume to make re-runs
instant across containers:

```bash
docker run --rm -v "$PWD":/work -v aion-cache:/cache aion hello.aion
```
