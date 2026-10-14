//! Source text → syntax tree.

pub mod ast;
mod lexer;
mod parser;

pub use parser::parse_module;
