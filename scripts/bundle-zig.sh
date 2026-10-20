#!/usr/bin/env bash
# Fetch the pinned Zig for one platform and pack it as toolchain/bundle/zig.tar.zst, which
# `cargo build --release --features bundled-zig` embeds into the aion executable, so that
# aion can compile programs on a machine with no C compiler installed.
#
#   scripts/bundle-zig.sh                  # the platform you are on
#   scripts/bundle-zig.sh aarch64-macos    # another platform from toolchain/zig.lock
#   scripts/bundle-zig.sh --check          # compare toolchain/zig.lock with ziglang.org (no download)
#
# Needs: bash, curl, tar, zstd, sha256sum or shasum; unzip or bsdtar for the Windows (.zip) archives;
# python3 for --check.  Works on Linux, macOS and Git Bash on Windows.
#
# The archive is only used after its SHA-256 matches the one in toolchain/zig.lock.  The downloads
# are kept in toolchain/cache/ (delete the folder to start over).  ZIG_DOWNLOAD_BASE=<url> downloads
# from a mirror (<url>/<version>/<file>) instead of ziglang.org.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOCK="$ROOT/toolchain/zig.lock"
CACHE="$ROOT/toolchain/cache"
OUT="$ROOT/toolchain/bundle"
# Where the archives come from; ZIG_DOWNLOAD_BASE points at a mirror of the same layout (<base>/<version>/<file>).
BASE_URL="${ZIG_DOWNLOAD_BASE:-https://ziglang.org/download}"
# https only, except for a mirror on this machine (used to test this script).
case "$BASE_URL" in http://127.0.0.1[:/]* | http://localhost[:/]*) PROTO='=http,https' ;; *) PROTO='=https' ;; esac

die() { echo "bundle-zig: $*" >&2; exit 1; }
need() { command -v "$1" >/dev/null 2>&1 || die "$1 is required but was not found"; }

# lock_field <platform> <column>: columns are 2 = file, 3 = sha256, 4 = size
lock_field() { awk -v p="$1" -v c="$2" '$1 == p { print $c; found = 1 } END { exit !found }' "$LOCK"; }
lock_version() { awk '$1 == "version" { print $2 }' "$LOCK"; }

host_platform() {
    local os arch
    case "$(uname -s)" in
        Linux*) os=linux ;;
        Darwin*) os=macos ;;
        MINGW* | MSYS* | CYGWIN*) os=windows ;;
        *) die "unknown operating system $(uname -s); name the platform as an argument" ;;
    esac
    case "$(uname -m)" in
        x86_64 | amd64) arch=x86_64 ;;
        aarch64 | arm64) arch=aarch64 ;;
        *) die "unknown CPU $(uname -m); name the platform as an argument" ;;
    esac
    echo "$arch-$os"
}

sha256_of() {
    if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | awk '{ print $1 }'
    else shasum -a 256 "$1" | awk '{ print $1 }'; fi
}

# Compare toolchain/zig.lock with the official index.
check_lock() {
    need curl
    need python3
    local index
    index="$(mktemp)"
    trap 'rm -f "$index"' RETURN
    curl --proto '=https' --tlsv1.2 -fsSL "$BASE_URL/index.json" -o "$index"
    python3 - "$LOCK" "$index" <<'PY'
import json, sys
lock, index = sys.argv[1], json.load(open(sys.argv[2]))
version, rows = None, []
for line in open(lock):
    line = line.split('#', 1)[0].split()
    if not line:
        continue
    if line[0] == 'version':
        version = line[1]
    else:
        rows.append(line)
release = index.get(version)
if release is None:
    sys.exit(f"zig.lock: version {version} is not in the official index")
bad = 0
for platform, name, sha, size in rows:
    entry = release.get(platform)
    want = (entry['tarball'].rsplit('/', 1)[1], entry['shasum'], entry['size']) if entry else None
    if want != (name, sha, size):
        bad += 1
        print(f"zig.lock: {platform}: have {(name, sha, size)}, the index says {want}", file=sys.stderr)
if bad:
    sys.exit(1)
print(f"zig.lock matches ziglang.org for Zig {version} ({len(rows)} platforms)")
PY
}

# Extract an archive (.tar.xz or .zip) into a directory.
extract() {
    local archive="$1" dest="$2"
    mkdir -p "$dest"
    case "$archive" in
        *.zip)
            if command -v unzip >/dev/null 2>&1; then unzip -q "$archive" -d "$dest"
            elif tar --version 2>/dev/null | grep -qi bsdtar; then tar -xf "$archive" -C "$dest"
            elif command -v bsdtar >/dev/null 2>&1; then bsdtar -xf "$archive" -C "$dest"
            elif [ -x "${SYSTEMROOT:-}/System32/tar.exe" ]; then "$SYSTEMROOT/System32/tar.exe" -xf "$archive" -C "$dest"
            else die "cannot unpack a .zip: install unzip or bsdtar"; fi ;;
        *) tar -xJf "$archive" -C "$dest" ;;
    esac
}

main() {
    [ -f "$LOCK" ] || die "$LOCK not found"
    if [ "${1:-}" = "--check" ]; then check_lock; exit 0; fi

    local platform="${1:-$(host_platform)}" version file sha size
    version="$(lock_version)"
    file="$(lock_field "$platform" 2)" || die "platform '$platform' is not in toolchain/zig.lock"
    sha="$(lock_field "$platform" 3)"
    size="$(lock_field "$platform" 4)"
    need curl
    need tar
    need zstd

    mkdir -p "$CACHE" "$OUT"
    local archive="$CACHE/$file"
    if [ -f "$archive" ] && [ "$(sha256_of "$archive")" = "$sha" ]; then
        echo "bundle-zig: using the downloaded $file"
    else
        echo "bundle-zig: downloading Zig $version for $platform ($((size / 1024 / 1024)) MB)"
        curl --proto "$PROTO" --tlsv1.2 -fL --retry 3 "$BASE_URL/$version/$file" -o "$archive.part"
        local got
        got="$(sha256_of "$archive.part")"
        if [ "$got" != "$sha" ]; then
            rm -f "$archive.part"
            die "SHA-256 of $file is $got, but toolchain/zig.lock says $sha; refusing to use it"
        fi
        mv "$archive.part" "$archive"
    fi

    local work
    work="$(mktemp -d)"
    trap "rm -rf '$work'" EXIT
    echo "bundle-zig: unpacking"
    extract "$archive" "$work/unpacked"
    # The archive holds one folder (zig-<platform>-<version>/); the bundle holds its contents.
    local top
    top="$(find "$work/unpacked" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
    [ -n "$top" ] || die "unexpected archive layout: no top-level folder"
    [ -f "$top/zig" ] || [ -f "$top/zig.exe" ] || die "unexpected archive layout: no zig executable in $top"

    echo "bundle-zig: compressing"
    tar -C "$top" -cf - . | zstd -q -19 -T0 -f -o "$OUT/zig.tar.zst.part"
    mv "$OUT/zig.tar.zst.part" "$OUT/zig.tar.zst"
    printf '%s\n%s\n' "$version" "$platform" > "$OUT/zig.version"
    echo "bundle-zig: wrote toolchain/bundle/zig.tar.zst ($(( $(wc -c < "$OUT/zig.tar.zst") / 1024 / 1024 )) MB)"
    echo "bundle-zig: now run: cargo build --release --features bundled-zig"
}

main "$@"
