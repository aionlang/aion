//! Embeds the standard library and the C runtime into the compiler binary.
//!
//! Every `stdlib/<name>.aion` becomes the module `<name>`; if a sibling
//! `stdlib/<name>.c` exists it is the module's C half and is compiled in
//! whenever the module is imported.  Adding a module is just adding files.

use std::fmt::Write as _;
use std::path::Path;
use std::{env, fs};

fn main() {
    let root = env::var("CARGO_MANIFEST_DIR").unwrap();
    let out = env::var("OUT_DIR").unwrap();
    let abs = |rel: &str| Path::new(&root).join(rel).display().to_string();

    let mut code = String::new();

    code.push_str("/// (module name, Aion source, optional C source)\n");
    code.push_str("pub static STDLIB: &[(&str, &str, Option<&str>)] = &[\n");
    for name in sorted_stems("stdlib", "aion", &root) {
        let c = format!("stdlib/{name}.c");
        let c_src = if Path::new(&abs(&c)).exists() {
            format!("Some(include_str!({:?}))", abs(&c))
        } else {
            "None".to_string()
        };
        writeln!(
            code,
            "    ({name:?}, include_str!({:?}), {c_src}),",
            abs(&format!("stdlib/{name}.aion"))
        )
        .unwrap();
    }
    code.push_str("];\n\n");

    code.push_str("/// (file name, contents) of the always-linked C runtime.\n");
    code.push_str("pub static RUNTIME: &[(&str, &str)] = &[\n");
    let mut files: Vec<String> = fs::read_dir(abs("runtime"))
        .expect("runtime/ directory")
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|n| n.ends_with(".c") || n.ends_with(".h"))
        .collect();
    files.sort();
    for f in files {
        writeln!(code, "    ({f:?}, include_str!({:?})),", abs(&format!("runtime/{f}"))).unwrap();
    }
    code.push_str("];\n");

    fs::write(Path::new(&out).join("embedded.rs"), code).unwrap();
    embed_zig(&root, &out);
    println!("cargo:rerun-if-changed=stdlib");
    println!("cargo:rerun-if-changed=runtime");
    println!("cargo:rerun-if-changed=build.rs");
}

fn sorted_stems(dir: &str, ext: &str, root: &str) -> Vec<String> {
    let mut v: Vec<String> = fs::read_dir(Path::new(root).join(dir))
        .unwrap_or_else(|e| panic!("cannot read {dir}/: {e}"))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == ext))
        .map(|p| p.file_stem().unwrap().to_string_lossy().into_owned())
        .collect();
    v.sort();
    v
}

/// With `--features bundled-zig`: include the archive that `scripts/bundle-zig.sh` made, as `zigbundle.rs` in OUT_DIR.
fn embed_zig(root: &str, out: &str) {
    println!("cargo:rerun-if-changed=toolchain");
    if env::var_os("CARGO_FEATURE_BUNDLED_ZIG").is_none() {
        return;
    }
    let bundle = Path::new(root).join("toolchain/bundle");
    let archive = bundle.join("zig.tar.zst");
    let info = fs::read_to_string(bundle.join("zig.version")).unwrap_or_default();
    let mut info = info.lines();
    let (version, platform) = (info.next().unwrap_or(""), info.next().unwrap_or(""));
    if !archive.is_file() || version.is_empty() {
        panic!("--features bundled-zig needs toolchain/bundle/zig.tar.zst: run scripts/bundle-zig.sh first");
    }

    // Embedding the Zig of another platform would give an executable that cannot compile anything.
    let arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let wanted = format!("{arch}-{os}");
    if platform != wanted {
        panic!(
            "toolchain/bundle holds the Zig for {platform}, but this build is for {wanted}: run scripts/bundle-zig.sh {wanted}"
        );
    }

    let bytes = fs::read(&archive).unwrap();
    let mut id: u64 = 0xcbf29ce484222325;
    for &b in &bytes {
        id ^= u64::from(b);
        id = id.wrapping_mul(0x100000001b3);
    }
    let code = format!(
        "pub static ARCHIVE: &[u8] = include_bytes!({:?});\npub const VERSION: &str = {version:?};\npub const ID: u64 = {id};\n",
        archive.display().to_string()
    );
    fs::write(Path::new(out).join("zigbundle.rs"), code).unwrap();
    println!("cargo:rerun-if-changed=toolchain/bundle/zig.tar.zst");
    println!("cargo:rerun-if-changed=toolchain/bundle/zig.version");
}
