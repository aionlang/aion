#!/bin/bash
# Build every program of tests/programs and tests/windows for Windows (clang + MinGW) and run it under Wine,
# comparing the output and the exit code with the expected ones.  Use the image of Dockerfile.wintest:
#
#   docker build -f Dockerfile.wintest -t aion-windows-test .
#   docker run --rm -v "$PWD":/src:ro -v aion-target-win:/work/target aion-windows-test
#
# To run only some programs, name them after the script: `... aion-windows-test bash /src/scripts/docker-test-windows.sh time fs`.
# WIN_SKIP lists more programs to leave out.  Skipped always: os_info (runs POSIX tools) and archive_formats (needs libarchive).
set -e
mkdir -p /work
rsync -a --delete --exclude target --exclude .git /src/ /work/
cd /work
find . -path ./target -prune -o -type f \( -name '*.aion' -o -name '*.out' -o -name '*.md' -o -name '*.MD' -o -name '*.rs' -o -name '*.c' -o -name '*.h' -o -name '*.toml' -o -name '*.project' \) -print0 | xargs -0 sed -i 's/\r$//'
cargo build 2>&1 | grep -E "^(warning|error)" -A8 | head -60 || true
export AION_BIN=/work/target/debug/aion AION_OPT=0 AION_TARGET=x86_64-w64-mingw32 AION_CLANG=clang-win AION_RUNNER=wine AION_CACHE=/work/target/cache-win
OUT=/tmp/win
mkdir -p $OUT
rm -f $OUT/*
SKIP=" os_info archive_formats ${WIN_SKIP} "
run_one() {
  f="$1"
  dir=$(dirname "$f")
  name=$(basename "$f" .aion)
  case "$SKIP" in *" $name "*) echo "skip  $name"; return;; esac
  code_expected=$(grep -m1 '^// exit:' "$f" | sed 's/.*exit: *//')
  [ -z "$code_expected" ] && code_expected=0
  set +e
  timeout 180 $AION_BIN "$f" > "$OUT/$name.stdout" 2> "$OUT/$name.stderr" < /dev/null
  code=$?
  set -e
  # programs on Windows write what they print as is; the tests/windows ones are checked with LF endings
  tr -d '\r' < "$OUT/$name.stdout" > "$OUT/$name.lf"
  if ! cmp -s "$OUT/$name.lf" "$dir/$name.out" && ! cmp -s "$OUT/$name.stdout" "$dir/$name.out"; then
    echo "DIFF  $name (exit $code)"
  elif [ "$code" != "$code_expected" ]; then
    echo "EXIT  $name ($code, expected $code_expected)"
  else
    echo "ok    $name"
  fi
}
export -f run_one
export SKIP OUT
if [ $# -gt 0 ]; then
  files=$(for n in "$@"; do ls tests/programs/$n.aion tests/windows/$n.aion 2>/dev/null; done)
else
  files=$(ls tests/programs/*.aion tests/windows/*.aion)
fi
echo "$files" | xargs -P ${JOBS:-4} -I{} bash -c 'run_one {}' | sort > $OUT/summary.txt
echo "passed: $(grep -c '^ok' $OUT/summary.txt), skipped: $(grep -c '^skip' $OUT/summary.txt)"
if grep -qv '^ok\|^skip' $OUT/summary.txt; then
  grep -v '^ok\|^skip' $OUT/summary.txt
  exit 1
fi
