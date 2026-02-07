import subprocess
import time
import sys

def bench(label, cmd):
    print(f"--- {label} ---")
    start = time.perf_counter()
    result = subprocess.run(cmd, capture_output=True, text=True)
    elapsed = time.perf_counter() - start
    output = (result.stdout.strip().splitlines() or [""])[-1]
    print(f"  output : {output}")
    print(f"  time   : {elapsed:.4f}s")
    if result.returncode != 0:
        print(f"  ⚠ exit code {result.returncode}")
    return elapsed

# 1) Compile main.aion (cargo run produces main.exe)
print("Compiling main.aion ...")
comp = subprocess.run(
    ["cargo", "run", "--release", "--", "main.aion"],
    capture_output=True, text=True
)
if comp.returncode != 0:
    print("Compilation failed:")
    print(comp.stdout)
    print(comp.stderr)
    sys.exit(1)
print("Compilation done.\n")

# 2) Benchmark Aion (compiled native binary)
t_aion = bench("Aion (compiled)", ["./main.exe"])

# 3) Benchmark Python
t_python = bench("Python", [sys.executable, "main.py"])

# 4) Summary
print()
print("=== Summary: count to 100,000,000 ===")
print(f"  Aion   : {t_aion:.4f}s")
print(f"  Python : {t_python:.4f}s")
if t_python > 0 and t_aion > 0:
    ratio = t_python / t_aion
    print(f"  Aion is {ratio:.1f}x {'faster' if ratio > 1 else 'slower'} than Python")
