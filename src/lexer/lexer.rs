use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // ── keywords ────────────────────────────────────────────────
    #[token("fn")]
    Fn,

    #[token("import")]
    Import,

    #[token("print")]
    Print,

    // ── punctuation ─────────────────────────────────────────────
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(".")]
    Dot,

    #[token(";")]
    Semi,

    #[token(",")]
    Comma,

    // ── literals ────────────────────────────────────────────────

    /// Float literal: 3.14, 144.0, .5
    #[regex(r"[0-9]+\.[0-9]*|[0-9]*\.[0-9]+")]
    FloatLit,

    /// Integer literal: 42, 0, 100
    #[regex(r"[0-9]+")]
    IntLit,

    /// String literal: "hello world"
    #[regex(r#""([^"\\]|\\.)*""#)]
    Str,

    /// Identifier: foo, main, math, sqrt, my_var
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    // ── skip whitespace ─────────────────────────────────────────
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    Error,
}