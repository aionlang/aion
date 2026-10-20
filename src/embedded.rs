//! The standard library and C runtime, baked into the compiler by `build.rs`.

include!(concat!(env!("OUT_DIR"), "/embedded.rs"));
include!(concat!(env!("OUT_DIR"), "/vendor.rs"));

/// `(Aion source, optional C source)` of a standard-library module.
pub fn stdlib_module(name: &str) -> Option<(&'static str, Option<&'static str>)> {
    STDLIB.iter().find(|(n, ..)| *n == name).map(|&(_, aion, c)| (aion, c))
}

pub fn stdlib_names() -> Vec<&'static str> {
    STDLIB.iter().map(|(n, ..)| *n).filter(|n| *n != "prelude").collect()
}

/// The files of a vendored C library (`FLAGS`, `.c` and `.h`), from vendor/<name>/.
pub fn vendor_files(name: &str) -> Option<&'static [(&'static str, &'static str)]> {
    VENDOR.iter().find(|(n, _)| *n == name).map(|&(_, files)| files)
}
