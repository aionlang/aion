/// LLVM-backed compiler — lowers the AST to native code via Inkwell.
pub mod compiler;
pub mod codegen;
pub mod linker;
pub mod runtime;
pub mod stdlib_registry;

pub use compiler::Compiler;
