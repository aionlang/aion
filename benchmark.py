import subprocess
import time
import sys
import os
import shutil

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

# 1) Compile main.aion (cargo run --release produces main.exe)
print("Compiling main.aion ...")
comp = subprocess.run(
    ["cargo", "run", "--release", "--", "main.aion"],
    capture_output=True, text=True
)
if comp.returncode != 0:
    print("Aion compilation failed:")
    print(comp.stdout)
    print(comp.stderr)
    sys.exit(1)
print("Compilation done.\n")

# 2) Benchmark Aion (compiled native binary)
aion_exe = "main.exe" if os.name == 'nt' else "./main"
if not os.path.exists(aion_exe):
    print(f"Warning: {aion_exe} not found; skipping Aion benchmark")
    t_aion = None
else:
    t_aion = bench("Aion (compiled)", [aion_exe])

# 3) Benchmark C (compile with gcc -O3, then run)
cc_bin = shutil.which("gcc") or shutil.which("cc")
if cc_bin:
    c_exe = "main_c.exe" if os.name == 'nt' else "./main_c"
    compile_cmd = [cc_bin, "-O3", "-march=native", "main.c", "-o", c_exe]
    print("Compiling C benchmark...")
    ccomp = subprocess.run(compile_cmd, capture_output=True, text=True)
    if ccomp.returncode != 0:
        print("C compilation failed:")
        print(ccomp.stderr)
        t_c = None
    else:
        print("C compilation done.")
        t_c = bench("C (gcc -O3)", [c_exe])
else:
    print("--- gcc/cc not found; skipping C benchmark ---")
    t_c = None

# 4) Benchmark Python
t_python = bench("Python", [sys.executable, "main.py"])

# 5) Benchmark Node.js (only if installed)
node_bin = shutil.which("node")
if node_bin:
    t_node = bench("Node.js", [node_bin, "main.js"])
else:
    print("--- Node.js not found; skipping ---")
    t_node = None

# 6) Summary
print()
print("=== Summary: count to 100,000,000 ===")
if t_aion is not None:
    print(f"  Aion   : {t_aion:.4f}s")
else:
    print("  Aion   : (skipped)")
if t_c is not None:
    print(f"  C      : {t_c:.4f}s")
else:
    print("  C      : (skipped)")
print(f"  Python : {t_python:.4f}s")
if t_node is not None:
    print(f"  Node   : {t_node:.4f}s")
print()
if t_aion and t_python > 0:
    print(f"  Aion is {t_python / t_aion:.1f}x {'faster' if t_python > t_aion else 'slower'} than Python")
if t_aion and t_node is not None:
    print(f"  Aion is {t_node / t_aion:.1f}x {'faster' if t_node > t_aion else 'slower'} than Node")
if t_aion and t_c is not None:
    print(f"  Aion is {t_c / t_aion:.1f}x {'faster' if t_c > t_aion else 'slower'} than C")
