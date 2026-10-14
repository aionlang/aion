//! Tokeniser (Logos-based).

use logos::Logos;

use crate::diag::{Diagnostic, Result, Span};

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token {
    // ── keywords ────────────────────────────────────────────────
    #[token("fn")]
    Fn,
    #[token("extern")]
    Extern,
    #[token("import")]
    Import,
    #[token("type")]
    Type,
    #[token("constructor")]
    Constructor,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("when")]
    When,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,

    // ── literals / names ────────────────────────────────────────
    #[regex(r"[0-9]+\.[0-9]+")]
    Float,
    #[regex(r"[0-9]+")]
    Int,
    /// A string literal, quotes included; see [`lex_string`].
    #[token("\"", lex_string)]
    Str,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    // ── punctuation ─────────────────────────────────────────────
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("..")]
    DotDot,
    #[token("..=")]
    DotDotEq,
    #[token(".")]
    Dot,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token("?")]
    Question,
    /// Safe member access: `a?.b`.
    #[token("?.")]
    QuestionDot,
    /// The elvis operator: `a ?: b`.
    #[token("?:")]
    Elvis,
    #[token(":")]
    Colon,
    /// `:=` and `::=` both declare-and-infer.
    #[token(":=")]
    #[token("::=")]
    ColonEq,
    #[token("->")]
    Arrow,
    /// Not valid syntax; lexed only to explain that `=` replaced it.
    #[token("=>")]
    FatArrow,

    // ── operators ───────────────────────────────────────────────
    #[token("=")]
    Eq,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("!")]
    Bang,

    // ── trivia ──────────────────────────────────────────────────
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip, allow_greedy = true)]
    Trivia,
}

impl Token {
    /// How the token is shown in "expected X, found Y" messages.
    pub fn describe(self) -> &'static str {
        match self {
            Token::Fn => "`fn`",
            Token::Extern => "`extern`",
            Token::Import => "`import`",
            Token::Type => "`type`",
            Token::Constructor => "`constructor`",
            Token::If => "`if`",
            Token::Else => "`else`",
            Token::While => "`while`",
            Token::For => "`for`",
            Token::In => "`in`",
            Token::When => "`when`",
            Token::Break => "`break`",
            Token::Continue => "`continue`",
            Token::Return => "`return`",
            Token::True | Token::False => "a boolean",
            Token::Null => "`null`",
            Token::Float | Token::Int => "a number",
            Token::Str => "a string",
            Token::Ident => "an identifier",
            Token::LParen => "`(`",
            Token::RParen => "`)`",
            Token::LBracket => "`[`",
            Token::RBracket => "`]`",
            Token::LBrace => "`{`",
            Token::RBrace => "`}`",
            Token::DotDot => "`..`",
            Token::DotDotEq => "`..=`",
            Token::Dot => "`.`",
            Token::Semi => "`;`",
            Token::Comma => "`,`",
            Token::Question => "`?`",
            Token::QuestionDot => "`?.`",
            Token::Elvis => "`?:`",
            Token::Colon => "`:`",
            Token::ColonEq => "`:=`",
            Token::Arrow => "`->`",
            Token::FatArrow => "`=>`",
            Token::Eq => "`=`",
            Token::PlusEq => "`+=`",
            Token::MinusEq => "`-=`",
            Token::StarEq => "`*=`",
            Token::SlashEq => "`/=`",
            Token::Plus => "`+`",
            Token::Minus => "`-`",
            Token::Star => "`*`",
            Token::Slash => "`/`",
            Token::Percent => "`%`",
            Token::EqEq => "`==`",
            Token::NotEq => "`!=`",
            Token::Lt => "`<`",
            Token::Le => "`<=`",
            Token::Gt => "`>`",
            Token::Ge => "`>=`",
            Token::AndAnd => "`&&`",
            Token::OrOr => "`||`",
            Token::Bang => "`!`",
            Token::Trivia => "whitespace",
        }
    }
}

/// Consume the rest of a string literal after its opening quote.
///
/// `${ ... }` interpolations may contain quotes and braces of their own, so
/// the scan tracks nesting instead of stopping at the first `"`.
fn lex_string(lex: &mut logos::Lexer<Token>) -> bool {
    let rest = lex.remainder().as_bytes();
    match string_len(rest) {
        Some(n) => {
            lex.bump(n);
            true
        }
        None => false,
    }
}

/// Length of the string body including the closing quote, if terminated.
fn string_len(b: &[u8]) -> Option<usize> {
    let mut i = 0;
    while i < b.len() {
        match b[i] {
            b'"' => return Some(i + 1),
            b'\\' => i += 2,
            b'$' if b.get(i + 1) == Some(&b'{') => i = interpolation_end(b, i + 2)?,
            _ => i += 1,
        }
    }
    None
}

/// Index just past the `}` that closes an interpolation starting at `i`.
pub(super) fn interpolation_end(b: &[u8], mut i: usize) -> Option<usize> {
    let mut depth = 1;
    while i < b.len() {
        match b[i] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i + 1);
                }
            }
            b'"' => i += string_len(&b[i + 1..])?,
            _ => {}
        }
        i += 1;
    }
    None
}

#[derive(Clone, Copy, Debug)]
pub struct Tok {
    pub kind: Token,
    pub span: Span,
    /// A line break separates this token from the previous one.
    pub nl: bool,
}

/// Tokenise `src`.  `base` is the offset of `src` inside its file, so that
/// code embedded in string interpolations still reports accurate spans.
///
/// A leading `#!` line (for `#!/usr/bin/env aion` scripts) is ignored.
pub fn lex(file: u32, src: &str, base: usize) -> Result<Vec<Tok>> {
    let skip = if base == 0 && src.starts_with("#!") { src.find('\n').unwrap_or(src.len()) } else { 0 };
    let mut out = Vec::new();
    let mut lexer = Token::lexer(&src[skip..]);
    let mut prev_end = 0;
    while let Some(result) = lexer.next() {
        let r = lexer.span();
        let span = Span::new(file, base + skip + r.start, base + skip + r.end);
        let nl = (prev_end == 0 && out.is_empty()) || lexer.source()[prev_end..r.start].contains('\n');
        prev_end = r.end;
        match result {
            Ok(kind) => out.push(Tok { kind, span, nl }),
            Err(()) if lexer.source()[r.start..].starts_with('"') => {
                return Err(Diagnostic::new("unterminated string literal", span));
            }
            Err(()) => {
                return Err(Diagnostic::new(
                    format!("unexpected character `{}`", &lexer.source()[r.clone()]),
                    span,
                ));
            }
        }
    }
    Ok(out)
}
