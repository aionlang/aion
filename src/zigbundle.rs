//! The Zig compiler that `cargo build --features bundled-zig` embeds in the `aion` executable.
//!
//! `scripts/bundle-zig.sh` packs the pinned Zig (toolchain/zig.lock) as `toolchain/bundle/zig.tar.zst`, and
//! `build.rs` includes that archive.  The first time a program is compiled on a machine with no C compiler, the
//! archive is unpacked into the cache directory (`AION_CACHE`, else the platform cache directory) and used from
//! there; later runs find it unpacked.

use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

// `ARCHIVE` (the bytes of the .tar.zst), `VERSION` (the Zig version) and `ID` (a hash of the archive).
include!(concat!(env!("OUT_DIR"), "/zigbundle.rs"));

fn exe_name() -> &'static str {
    if cfg!(windows) { "zig.exe" } else { "zig" }
}

/// The embedded `zig` executable, unpacking the archive first if this version is not in the cache yet.
pub fn compiler_path() -> Result<PathBuf, String> {
    let root = crate::cache::dir().join(format!("zig-{VERSION}-{ID:016x}"));
    let exe = root.join(exe_name());
    if !exe.is_file() {
        unpack(&root)?;
    }
    Ok(exe)
}

/// Unpack into a temporary directory next to `root` and rename it into place, so that `root` is never seen half
/// done, and two `aion` processes unpacking at the same time cannot get in each other's way.
fn unpack(root: &Path) -> Result<(), String> {
    let parent = root.parent().ok_or("the cache directory has no parent")?;
    std::fs::create_dir_all(parent).map_err(|e| format!("cannot create {}: {e}", parent.display()))?;
    let nanos = SystemTime::now().duration_since(UNIX_EPOCH).map_or(0, |d| d.subsec_nanos());
    let temp = parent.join(format!(".zig-unpack-{}-{nanos}", std::process::id()));

    eprintln!("aion: unpacking the C compiler that comes with aion to {} (first run only)", root.display());
    let result = extract(&temp).and_then(|()| match std::fs::rename(&temp, root) {
        Ok(()) => Ok(()),
        // Another process finished first: its copy is as good as ours.
        Err(_) if root.join(exe_name()).is_file() => Ok(()),
        Err(e) => Err(format!("cannot move {} to {}: {e}", temp.display(), root.display())),
    });
    let _ = std::fs::remove_dir_all(&temp);
    result
}

fn extract(dest: &Path) -> Result<(), String> {
    let decoder = ruzstd::decoding::StreamingDecoder::new(ARCHIVE).map_err(|e| format!("bad archive: {e}"))?;
    let mut archive = tar::Archive::new(decoder);
    archive.set_preserve_permissions(true);
    archive.unpack(dest).map_err(|e| format!("cannot unpack into {}: {e}", dest.display()))
}
