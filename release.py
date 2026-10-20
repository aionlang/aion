#!/usr/bin/env python3
"""Make a release: bump the version, commit `release: vX.Y.Z`, tag `vX.Y.Z` and push both.

    python release.py patch|minor|major [--dry-run] [--no-push]

Pushing the tag starts .github/workflows/release.yml, which builds the binaries and the Windows installer and publishes
the GitHub release.  Run it from the `main` branch with no uncommitted changes to tracked files.
"""
import argparse
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent


def git(*args, check=True):
    done = subprocess.run(["git", *args], cwd=ROOT, capture_output=True, text=True, encoding="utf-8")
    if check and done.returncode:
        sys.exit(f"git {' '.join(args)} failed:\n{done.stderr.strip()}")
    return done.stdout.strip()


def bump(version, kind):
    major, minor, patch = map(int, version.split("."))
    if kind == "major":
        return f"{major + 1}.0.0"
    if kind == "minor":
        return f"{major}.{minor + 1}.0"
    return f"{major}.{minor}.{patch + 1}"


# Where the version is written: (file, pattern with the version as group 2 between groups 1 and 3)
SPOTS = [
    ("Cargo.toml", r'(?m)^(version = ")([^"]+)(")'),
    ("Cargo.lock", r'(name = "aion"\r?\nversion = ")([^"]+)(")'),
    ("README.md", r'(badge/version-)([\d.]+)(-informational)'),
    ("README.md", r'(alt="version )([\d.]+)(")'),
    ("README.md", r"(Aion is at version \*\*)([\d.]+)(\*\*)"),
    ("docs/repl.md", r"(Aion )([\d.]+)( — :help)"),
]


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("kind", choices=["patch", "minor", "major"], help="which part of the version to raise")
    parser.add_argument("--dry-run", action="store_true", help="show what would change and stop")
    parser.add_argument("--no-push", action="store_true", help="commit and tag here, but do not push")
    args = parser.parse_args()

    if git("branch", "--show-current") != "main":
        sys.exit("release from the `main` branch")
    if git("status", "--porcelain", "--untracked-files=no"):
        sys.exit("there are uncommitted changes to tracked files; commit or stash them first")
    git("fetch", "origin", "main", "--tags")
    if git("rev-list", "--count", "HEAD..origin/main") != "0":
        sys.exit("origin/main has commits you do not have; `git pull --rebase` first")

    old = re.search(r'(?m)^version = "([^"]+)"', (ROOT / "Cargo.toml").read_text(encoding="utf-8")).group(1)
    new = bump(old, args.kind)
    tag = f"v{new}"
    if git("tag", "--list", tag):
        sys.exit(f"the tag {tag} already exists")

    edits = {}
    for name, pattern in SPOTS:
        # CRLF checkouts: read and write the bytes as they are
        text = edits.get(name) or (ROOT / name).read_bytes().decode("utf-8")
        found = re.search(pattern, text)
        if not found or found.group(2) != old:
            sys.exit(f"{name}: expected version {old} where `{pattern}` matches, found {found.group(2) if found else 'nothing'}")
        edits[name] = text[: found.start(2)] + new + text[found.end(2):]

    print(f"{old} -> {new}  ({tag})")
    for name in edits:
        print(f"  updates {name}")
    if args.dry_run:
        print("dry run: nothing changed")
        return

    for name, text in edits.items():
        (ROOT / name).write_bytes(text.encode("utf-8"))
    git("add", *edits)
    git("commit", "-m", f"release: {tag}")
    git("tag", "-a", tag, "-m", f"release: {tag}")
    print(f"committed and tagged {tag}")

    if args.no_push:
        print(f"not pushed; push with: git push --atomic origin main {tag}")
        return
    git("push", "--atomic", "origin", "main", tag)
    print(f"pushed main and {tag}")


if __name__ == "__main__":
    main()
