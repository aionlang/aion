# Building Aion from Source

Complete instructions for setting up the Aion compiler on a fresh machine.

---

## Prerequisites

| Tool | Version | Purpose |
|------|---------|---------|
| **Rust** | stable (edition 2024) | Compiler host language |
| **LLVM SDK** | 21.x | Code generation backend |
| **GCC or Clang** | any recent | Linking the final binary + compiling the runtime |
| **MSVC Build Tools** | 2022+ (Windows only) | Rust's default linker on Windows |

---

## Windows setup

### 1. Install Rust

Download and run the installer from <https://rustup.rs>.  
Accept the defaults (stable toolchain, `x86_64-pc-windows-msvc`).

Verify:
```powershell
rustc --version    # e.g. rustc 1.84.0
cargo --version
```

### 2. Install Visual Studio Build Tools

Rust on Windows needs the MSVC linker.  If you already have Visual Studio 2022
(Community or higher) with the **"Desktop development with C++"** workload,
you're set.

Otherwise download **Build Tools for Visual Studio 2022** from  
<https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022>  
and install the **"Desktop development with C++"** workload.

### 3. Install GCC

GCC is used to compile `runtime/aion_runtime.c` and to link the final
executable.  Any of these work:

- **Scoop (recommended):** `scoop install tdm-gcc`
- **MSYS2:** `pacman -S mingw-w64-ucrt-x86_64-gcc`
- **Strawberry Perl** includes GCC at `C:\Strawberry\c\bin\gcc.exe`

Verify:
```powershell
gcc --version
ar --version
```

### 4. Download the LLVM 21 SDK

> **Important:** You need the *full SDK* (the `clang+llvm-*` archive), not the
> plain LLVM installer from llvm.org — that one only ships tools, not the
> development libraries.

1. Go to <https://github.com/llvm/llvm-project/releases/tag/llvmorg-21.1.8>  
   (or the latest 21.x release)
2. Download **`clang+llvm-21.1.8-x86_64-pc-windows-msvc.tar.xz`** (~900 MB)
3. Extract it somewhere, for example:
   ```
   C:\llvm-dev\clang+llvm-21.1.8-x86_64-pc-windows-msvc
   ```

Verify the SDK has these files:
```
bin\llvm-config.exe
bin\LLVM-C.dll
lib\LLVM-C.lib
lib\cmake\llvm\LLVMConfig.cmake
```

### 5. Set the LLVM environment variable

The `llvm-sys` crate (used by Inkwell) finds LLVM via an environment variable
whose name includes the major version × 100 + minor:

```powershell
# Permanent (persists across restarts)
[System.Environment]::SetEnvironmentVariable(
    "LLVM_SYS_211_PREFIX",
    "C:\llvm-dev\clang+llvm-21.1.8-x86_64-pc-windows-msvc",
    "User"
)
```

> If LLVM is version 21.1.x the variable is `LLVM_SYS_211_PREFIX`.  
> For a hypothetical 22.0.x it would be `LLVM_SYS_220_PREFIX`.

### 6. Add LLVM-C.dll to PATH

The compiled Aion compiler (`aion.exe`) loads `LLVM-C.dll` at runtime.  
Add the SDK's `bin\` to your PATH:

```powershell
# Permanent
[System.Environment]::SetEnvironmentVariable(
    "Path",
    "C:\llvm-dev\clang+llvm-21.1.8-x86_64-pc-windows-msvc\bin;" + [System.Environment]::GetEnvironmentVariable("Path", "User"),
    "User"
)
```

Or copy `LLVM-C.dll` next to the built `aion.exe`.

### 7. Build and install

```powershell
git clone <repo-url> aion
cd aion

# Option A: install to ~/.cargo/bin (puts `aion` on your PATH)
cargo install --path .

# Option B: just build locally
cargo build --release
```

### 8. Test

```powershell
# If installed via cargo install:
aion main.aion -o hello.exe
.\hello.exe
# → Hello from Aion!

# If built locally:
cargo run -- main.aion -o hello.exe
.\hello.exe
```

---

## Linux setup

### 1. Install system packages

**Ubuntu / Debian:**
```bash
sudo apt update
sudo apt install -y build-essential curl
```

**Fedora:**
```bash
sudo dnf groupinstall "Development Tools"
```

### 2. Install Rust

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
```

### 3. Install LLVM 21

**Option A — system packages (if available):**
```bash
# Ubuntu (may need the LLVM apt repo: https://apt.llvm.org)
sudo apt install -y llvm-21-dev libpolly-21-dev
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21
```

**Option B — pre-built SDK:**
```bash
wget https://github.com/llvm/llvm-project/releases/download/llvmorg-21.1.8/clang+llvm-21.1.8-x86_64-linux-gnu-ubuntu-22.04.tar.xz
tar xf clang+llvm-21.1.8-x86_64-linux-gnu-ubuntu-22.04.tar.xz -C /opt
export LLVM_SYS_211_PREFIX=/opt/clang+llvm-21.1.8-x86_64-linux-gnu-ubuntu-22.04
```

Add the export to your `~/.bashrc` or `~/.zshrc` to make it permanent.

### 4. Build and test

```bash
git clone <repo-url> aion
cd aion

# Install to ~/.cargo/bin
cargo install --path .

# Use it
aion main.aion -o hello
./hello
# → Hello from Aion!
```

> **Note on Linux:** Static LLVM linking works without CRT issues, so the
> `.cargo/config.toml` override is Windows-specific and harmless on Linux
> (the `[target.x86_64-pc-windows-msvc.llvm-21]` section is ignored).

---

## macOS setup

### 1. Install Xcode Command Line Tools

```bash
xcode-select --install
```

### 2. Install Rust

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
```

### 3. Install LLVM 21

```bash
brew install llvm@21
export LLVM_SYS_211_PREFIX=$(brew --prefix llvm@21)
```

Add the export to `~/.zshrc`.

### 4. Build and test

```bash
cargo install --path .
aion main.aion -o hello
./hello
```

---

## Updating `.cargo/config.toml` for a different LLVM path

If you installed the LLVM SDK to a different path than
`C:\llvm-dev\clang+llvm-21.1.8-x86_64-pc-windows-msvc`, update the
`rustc-link-search` in `.cargo/config.toml`:

```toml
[target.x86_64-pc-windows-msvc.llvm-21]
rustc-link-lib = ["dylib=LLVM-C"]
rustc-link-search = ["YOUR\\LLVM\\SDK\\lib"]
```

---

## Troubleshooting

### `LLVM_SYS_211_PREFIX` not found

```
error: No suitable version of LLVM was found
```

Make sure the environment variable is set and points to the *root* of the
SDK (the folder containing `bin/`, `lib/`, `include/`).

### `LLVM-C.dll` not found at runtime

```
error: The code execution cannot proceed because LLVM-C.dll was not found
```

Add the SDK's `bin/` directory to your PATH, or copy `LLVM-C.dll` next to
`target/debug/aion.exe`.

### `gcc` not found

```
failed to run gcc for runtime
```

Install GCC (see step 3 above) and make sure it's on your PATH.

### `LNK1120: unresolved externals` for LLVM targets

If you see errors about `LLVMInitializeMipsTarget` or similar, the stubs
files are not being compiled.  Make sure `build.rs` and
`stubs/llvm_target_stubs.c` exist and that `cc` is in `[build-dependencies]`.

### `LNK4098: defaultlib 'libcmt.lib' conflicts`

This means static LLVM libs are being linked with the wrong CRT.  Ensure
`.cargo/config.toml` has the `[target.x86_64-pc-windows-msvc.llvm-21]`
override to force dynamic linking via `LLVM-C.dll`.

---

## What the compiled binary needs at runtime

The final binary (e.g. `hello.exe`) is **fully standalone**.  It does NOT
require LLVM, Rust, or GCC to run.  It only depends on the system's C runtime
(`msvcrt.dll` on Windows, `libc.so` on Linux).  You can copy it to any
machine with the same OS/architecture and run it directly.
