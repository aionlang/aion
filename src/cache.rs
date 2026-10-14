//! On-disk cache: built programs and cloned git dependencies.
//!
//! Lives in `$AION_CACHE`, else the platform cache directory
//! (`$XDG_CACHE_HOME/aion`, `~/.cache/aion`, `%LOCALAPPDATA%\aion`), else the
//! system temp directory.

use std::path::PathBuf;

pub fn dir() -> PathBuf {
    let env = |k: &str| std::env::var_os(k).filter(|v| !v.is_empty()).map(PathBuf::from);
    if let Some(d) = env("AION_CACHE") {
        return d;
    }
    if cfg!(windows) {
        if let Some(d) = env("LOCALAPPDATA") {
            return d.join("aion");
        }
    } else if let Some(d) = env("XDG_CACHE_HOME") {
        return d.join("aion");
    } else if let Some(h) = env("HOME") {
        return h.join(".cache").join("aion");
    }
    std::env::temp_dir().join("aion-cache")
}

/// A stable 64-bit hash (FNV-1a) — cache keys must not change between runs.
pub fn hash(parts: &[&[u8]]) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for part in parts {
        for &b in *part {
            h ^= u64::from(b);
            h = h.wrapping_mul(0x100000001b3);
        }
        // Separator, so ("ab", "c") and ("a", "bc") differ.
        h ^= 0xff;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

/// Remove the oldest files in `dir` until at most `keep` remain.
pub fn trim(dir: &std::path::Path, keep: usize) {
    let Ok(rd) = std::fs::read_dir(dir) else { return };
    let mut files: Vec<_> = rd
        .filter_map(|e| e.ok())
        .filter_map(|e| Some((e.metadata().ok()?.modified().ok()?, e.path())))
        .collect();
    if files.len() <= keep {
        return;
    }
    files.sort();
    let excess = files.len() - keep;
    for (_, path) in files.into_iter().take(excess) {
        let _ = std::fs::remove_file(path);
    }
}
