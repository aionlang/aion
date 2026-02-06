/// Build script for the Aion compiler.
///
/// Compiles stub implementations of LLVM target initializers that are
/// referenced by inkwell but not present in the pre-built LLVM SDK.
fn main() {
    cc::Build::new()
        .file("stubs/llvm_target_stubs.c")
        .compile("llvm_target_stubs");

    // Tell cargo to re-run only if the stubs change.
    println!("cargo:rerun-if-changed=stubs/llvm_target_stubs.c");
}
