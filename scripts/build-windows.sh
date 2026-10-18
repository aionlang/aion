#!/bin/bash
# Build aion.exe for 64-bit Windows with the MinGW cross compiler, from Linux or from a Linux container.  From the
# repository root:
#
#   docker run --rm -v "$PWD":/src:ro -v aion-win-target:/work/target -v "$PWD/dist":/out rust:1-slim-trixie bash /src/scripts/build-windows.sh
#
# The result is dist/aion.exe.  Running Aion programs on Windows also needs clang on the PATH (LLVM for Windows).
set -e
apt-get update -qq
apt-get install -y -qq --no-install-recommends gcc-mingw-w64-x86-64 rsync > /dev/null 2>&1
rustup target add x86_64-pc-windows-gnu
mkdir -p /work /out
rsync -a --delete --exclude target --exclude .git --exclude dist /src/ /work/
cd /work
cargo build --release --locked --target x86_64-pc-windows-gnu
cp target/x86_64-pc-windows-gnu/release/aion.exe /out/aion.exe
ls -l /out/aion.exe
