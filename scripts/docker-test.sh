#!/bin/bash
# Run `cargo test` in Docker, for a machine without Rust or clang.  From the repository root:
#
#   docker run --rm -v "$PWD":/src:ro -v aion-target:/work/target rust:1-slim-trixie bash /src/scripts/docker-test.sh
#
# Arguments are passed to `cargo test`, e.g. `... bash /src/scripts/docker-test.sh --test run programs_produce`.
# The tree is copied to /work with LF line endings first: a Windows checkout has CRLF, which the tests
# (they compare output byte for byte) would report as differences.  The `aion-target` volume keeps the build.
set -e
apt-get update -qq
apt-get install -y -qq --no-install-recommends clang libc6-dev libsqlite3-dev zlib1g-dev libssl-dev ca-certificates git openssl rsync nghttp2 > /dev/null 2>&1
mkdir -p /work
rsync -a --delete --exclude target --exclude .git /src/ /work/
cd /work
find . -path ./target -prune -o -type f \( -name '*.aion' -o -name '*.out' -o -name '*.md' -o -name '*.MD' -o -name '*.rs' -o -name '*.c' -o -name '*.h' -o -name '*.toml' -o -name '*.project' \) -print0 | xargs -0 sed -i 's/\r$//'
exec cargo test "$@"
