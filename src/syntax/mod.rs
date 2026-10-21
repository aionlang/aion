//! Source text → syntax tree.

pub mod ast;
mod lexer;
mod parser;

pub use lexer::{Token, doc_comments, lex};
pub use parser::parse_module;
