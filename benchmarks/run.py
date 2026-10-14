#!/usr/bin/env python3
"""Count to 100,000,000 in Aion, C, Node.js and Python and compare wall time.

    python3 benchmarks/run.py

Needs `cargo build --release` first (or have `aion` on PATH).
"""
import os
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)


def timed(label, cmd):
    start = time.perf_counter()
    result = subprocess.run(cmd, capture_output=True, text=True)
    elapsed = time.perf_counter() - start
    last = (result.stdout.strip().splitlines() or [""])[-1]
    print(f"{label:<10} {elapsed:8.4f}s   output: {last}")
    if result.returncode != 0:
        print(f"  (exit code {result.returncode})")
    return elapsed


def aion_binary():
    release = os.path.join(ROOT, "target", "release", "aion")
    return release if os.path.exists(release) else shutil.which("aion")


def main():
    results = {}
    with tempfile.TemporaryDirectory() as tmp:
        aion = aion_binary()
        if aion:
            exe = os.path.join(tmp, "count_aion")
            subprocess.run([aion, "build", os.path.join(HERE, "count.aion"), "-o", exe], check=True)
            results["Aion"] = timed("Aion", [exe])
        else:
            print("Aion: skipped (run `cargo build --release`)")

        cc = shutil.which("clang") or shutil.which("gcc") or shutil.which("cc")
        if cc:
            exe = os.path.join(tmp, "count_c")
            subprocess.run([cc, "-O3", os.path.join(HERE, "count.c"), "-o", exe], check=True)
            results["C"] = timed("C -O3", [exe])

        node = shutil.which("node")
        if node:
            results["Node.js"] = timed("Node.js", [node, os.path.join(HERE, "count.js")])

        results["Python"] = timed("Python", [sys.executable, os.path.join(HERE, "count.py")])

    if "Aion" in results:
        print()
        for name, t in results.items():
            if name != "Aion" and results["Aion"] > 0:
                print(f"Aion vs {name}: {t / results['Aion']:.1f}x")


if __name__ == "__main__":
    main()
