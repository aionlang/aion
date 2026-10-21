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
    #[token("interface")]
    Interface,
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
    #[token("defer")]
    Defer,
    #[token("try")]
    Try,
    #[token("catch")]
    Catch,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,

    // ── literals / names ────────────────────────────────────────
    /// `1.5`, `2.5e-3`, `1e9`
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?")]
    #[regex(r"[0-9][0-9_]*[eE][+-]?[0-9]+")]
    Float,
    /// `42`, `1_000_000`, `0xFF`, `0b1010`, `0o17`
    #[regex(r"[0-9][0-9_]*")]
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*")]
    #[regex(r"0[bB][01][01_]*")]
    #[regex(r"0[oO][0-7][0-7_]*")]
    Int,
    /// A character literal such as `'a'`, `'\n'` or `'\u{1F600}'`, quotes included.
    #[regex(r"'([^'\\\n]|\\[^\n]|\\u\{[0-9a-fA-F]{1,6}\})'")]
    Char,
    /// A string literal, quotes included; see [`lex_string`].  `"""…"""` may span lines and hold quotes.
    #[token("\"", lex_string)]
    Str,
    /// A raw string, `r"C:\dir"` or `r"""…"""`: no escapes and no interpolation.
    #[token("r\"", lex_raw_string)]
    RawStr,
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
    #[token("@")]
    At,
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
    #[token("/*", lex_block_comment)]
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
            Token::Interface => "`interface`",
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
            Token::Defer => "`defer`",
            Token::Try => "`try`",
            Token::Catch => "`catch`",
            Token::True | Token::False => "a boolean",
            Token::Null => "`null`",
            Token::Float | Token::Int => "a number",
            Token::Str | Token::RawStr => "a string",
            Token::Char => "a character",
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
            Token::At => "`@`",
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

/// The `/** … */` comments of `src` as byte ranges, in order.  (A comment is trivia to the lexer, so
/// they are found in the stretches between tokens, which hold nothing but white space and comments.)
pub fn doc_comments(src: &str) -> Vec<(usize, usize)> {
    let mut found = Vec::new();
    let mut gap_start = 0;
    let mut lexer = Token::lexer(src);
    loop {
        let next = lexer.next();
        let gap_end = if next.is_some() { lexer.span().start } else { src.len() };
        let (bytes, mut i) = (src.as_bytes(), gap_start);
        while i < gap_end {
            if bytes[i..gap_end].starts_with(b"//") {
                i += bytes[i..gap_end].iter().position(|&b| b == b'\n').unwrap_or(gap_end - i);
            } else if bytes[i..gap_end].starts_with(b"/*") {
                let (mut depth, mut j) = (1, i + 2);
                while j < gap_end && depth > 0 {
                    if bytes[j..gap_end].starts_with(b"/*") {
                        depth += 1;
                        j += 2;
                    } else if bytes[j..gap_end].starts_with(b"*/") {
                        depth -= 1;
                        j += 2;
                    } else {
                        j += 1;
                    }
                }
                if depth == 0 && bytes[i..j].starts_with(b"/**") && j - i > 4 {
                    found.push((i, j));
                }
                i = j;
            } else {
                i += 1;
            }
        }
        if next.is_none() {
            return found;
        }
        gap_start = lexer.span().end;
    }
}

/// A block comment (slash-star ... star-slash) may span lines and nest, so a stretch of code that
/// already holds comments can be commented out.  The doc form, with two stars, is a comment like
/// any other to the compiler.  The callback runs after the opening slash-star.
fn lex_block_comment(lex: &mut logos::Lexer<Token>) -> logos::FilterResult<(), ()> {
    let rest = lex.remainder().as_bytes();
    let (mut depth, mut i) = (1, 0);
    while i < rest.len() {
        if rest[i..].starts_with(b"/*") {
            depth += 1;
            i += 2;
        } else if rest[i..].starts_with(b"*/") {
            depth -= 1;
            i += 2;
            if depth == 0 {
                lex.bump(i);
                return logos::FilterResult::Skip;
            }
        } else {
            i += 1;
        }
    }
    lex.bump(rest.len());
    logos::FilterResult::Error(())
}

/// Consume the rest of a string literal after its opening quote.
///
/// `${ ... }` interpolations may contain quotes and braces of their own, so
/// the scan tracks nesting instead of stopping at the first `"`.
fn lex_string(lex: &mut logos::Lexer<Token>) -> bool {
    let rest = lex.remainder().as_bytes();
    let found = if rest.starts_with(b"\"\"") { triple_len(&rest[2..], true).map(|n| n + 2) } else { string_len(rest) };
    match found {
        Some(n) => {
            lex.bump(n);
            true
        }
        None => false,
    }
}

/// After the opening `r"`: a raw string ends at the next quote, or for `r"""` at the last of the
/// first run of three or more quotes.
fn lex_raw_string(lex: &mut logos::Lexer<Token>) -> bool {
    let rest = lex.remainder().as_bytes();
    let found = if rest.starts_with(b"\"\"") { triple_len(&rest[2..], false).map(|n| n + 2) } else { rest.iter().position(|&b| b == b'"').map(|i| i + 1) };
    match found {
        Some(n) => {
            lex.bump(n);
            true
        }
        None => false,
    }
}

/// Length of the body of a `"""` string including its closing quotes.  A run of more than three
/// quotes at the end closes the string with the last three (the others belong to the text).
/// With `cooked`, escapes and `${}` interpolations are skipped over.
fn triple_len(b: &[u8], cooked: bool) -> Option<usize> {
    let mut i = 0;
    while i < b.len() {
        match b[i] {
            b'"' if b[i..].starts_with(b"\"\"\"") => {
                let run = b[i..].iter().take_while(|&&c| c == b'"').count();
                return Some(i + run);
            }
            b'\\' if cooked => i += 2,
            b'$' if cooked && b.get(i + 1) == Some(&b'{') => i = interpolation_end(b, i + 2)?,
            _ => i += 1,
        }
    }
    None
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
    /// Where the token's text is in the source being parsed; differs from `span` only for generated code.
    pub raw: Span,
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
            Ok(kind) => out.push(Tok { kind, span, raw: span, nl }),
            Err(()) if lexer.source()[r.start..].starts_with('"') || lexer.source()[r.start..].starts_with("r\"") => {
                return Err(Diagnostic::new("unterminated string literal", span));
            }
            Err(()) if lexer.source()[r.start..].starts_with("/*") => {
                return Err(Diagnostic::new("unterminated block comment", span));
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
