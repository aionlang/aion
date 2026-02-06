//! Recursive-descent parser for the Aion language.
//!
//! Grammar (current subset):
//! ```text
//! program    = import* function* EOF
//! import     = "import" IDENT ("." IDENT)* ";"
//! function   = "fn" IDENT "(" ")" "{" statement* "}"
//! statement  = print_stmt | expr_stmt
//! print_stmt = "print" "(" expr ")"
//! expr_stmt  = expr
//! expr       = module_call | float_lit | int_lit | string_lit
//! module_call= IDENT "." IDENT "(" args ")"
//! args       = expr ("," expr)*
//! ```

use crate::ast::{Expr, Function, Import, Program};
use crate::lexer::lexer::Token;
use logos::Logos;

/// A single token together with the source text it matched.
#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    lexeme: String,
}

/// Recursive-descent parser.
///
/// Tokens are pre-lexed into a flat vector so we have random access to
/// both the token kind *and* the original source text (lexeme).
pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl Parser {
    /// Create a new parser by lexing the full source up-front.
    pub fn new(source: &str) -> Self {
        let mut lexer = Token::lexer(source);
        let mut tokens = Vec::new();

        while let Some(result) = lexer.next() {
            if let Ok(tok) = result {
                tokens.push(SpannedToken {
                    token: tok,
                    lexeme: lexer.slice().to_string(),
                });
            }
            // Lex errors (whitespace / unknown chars) are silently skipped.
        }

        Self { tokens, pos: 0 }
    }

    // ── helpers ──────────────────────────────────────────────────────

    /// Look at the current token without consuming it.
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|s| &s.token)
    }

    /// Look at the current spanned token without consuming it.
    fn peek_spanned(&self) -> Option<&SpannedToken> {
        self.tokens.get(self.pos)
    }

    /// Consume and return the current token + lexeme.  Panics if at EOF.
    fn advance(&mut self) -> SpannedToken {
        let item = self.tokens[self.pos].clone();
        self.pos += 1;
        item
    }

    /// Consume the next token and assert it matches `expected`.
    fn expect(&mut self, expected: Token, msg: &str) {
        let st = self.advance();
        assert!(
            st.token == expected,
            "Expected {expected:?}, got {:?} — {msg}",
            st.token
        );
    }

    /// Return `true` if there are more tokens to consume.
    fn has_more(&self) -> bool {
        self.pos < self.tokens.len()
    }

    // ── grammar rules ───────────────────────────────────────────────

    /// Entry point — parse the entire source into a [`Program`].
    pub fn parse_program(&mut self) -> Program {
        let mut imports = Vec::new();
        let mut functions = Vec::new();

        // Parse leading imports.
        while self.has_more() {
            match self.peek() {
                Some(Token::Import) => {
                    self.advance(); // consume 'import'
                    imports.push(self.parse_import());
                }
                _ => break,
            }
        }

        // Parse function definitions.
        while self.has_more() {
            match self.peek() {
                Some(Token::Fn) => {
                    self.advance(); // consume 'fn'
                    functions.push(self.parse_function());
                }
                _ => {
                    // Skip any unrecognised top-level token.
                    self.advance();
                }
            }
        }

        Program { imports, functions, user_modules: Vec::new() }
    }

    /// Parse an import path: `aion.math;`
    /// The `import` keyword has already been consumed.
    fn parse_import(&mut self) -> Import {
        let mut path = Vec::new();

        // First segment.
        path.push(self.parse_ident_string());

        // Additional dot-separated segments.
        while self.peek() == Some(&Token::Dot) {
            self.advance(); // consume '.'
            path.push(self.parse_ident_string());
        }

        self.expect(Token::Semi, "expected ';' after import path");

        Import { path }
    }

    /// Parse a function definition: `fn <name>() { … }`
    fn parse_function(&mut self) -> Function {
        let name = self.parse_ident_string();

        self.expect(Token::LParen, "expected '(' after function name");
        self.expect(Token::RParen, "expected ')' after parameter list");
        self.expect(Token::LBrace, "expected '{' to open function body");

        let mut body = Vec::new();

        loop {
            match self.peek() {
                Some(Token::RBrace) => {
                    self.advance(); // consume '}'
                    break;
                }
                Some(_) => {
                    body.push(self.parse_statement());
                }
                None => panic!("Unexpected end of input inside function body"),
            }
        }

        Function { name, body }
    }

    /// Parse a single statement inside a function body.
    fn parse_statement(&mut self) -> Expr {
        match self.peek() {
            Some(Token::Print) => {
                self.advance(); // consume 'print'
                self.parse_print()
            }
            _ => self.parse_expr(),
        }
    }

    /// Parse `print(...)` — the `print` keyword has already been consumed.
    ///
    /// If the argument is a string literal we emit `PrintStr`, otherwise
    /// `PrintExpr` wrapping an arbitrary expression.
    fn parse_print(&mut self) -> Expr {
        self.expect(Token::LParen, "expected '(' after 'print'");

        // Check if argument is a plain string literal.
        let expr = if self.peek() == Some(&Token::Str) {
            let st = self.advance();
            let content = st.lexeme[1..st.lexeme.len() - 1].to_string();
            Expr::PrintStr(content)
        } else {
            Expr::PrintExpr(Box::new(self.parse_expr()))
        };

        self.expect(Token::RParen, "expected ')' after print argument");
        expr
    }

    /// Parse an expression.
    fn parse_expr(&mut self) -> Expr {
        let st = self.peek_spanned().expect("expected expression").clone();

        match st.token {
            Token::FloatLit => {
                self.advance();
                Expr::FloatLiteral(
                    st.lexeme.parse::<f64>().expect("bad float literal"),
                )
            }
            Token::IntLit => {
                self.advance();
                Expr::IntLiteral(
                    st.lexeme.parse::<i64>().expect("bad integer literal"),
                )
            }
            Token::Str => {
                // A bare string literal as an expression (future use).
                self.advance();
                let content = st.lexeme[1..st.lexeme.len() - 1].to_string();
                Expr::PrintStr(content) // placeholder — will revisit with StringLiteral expr
            }
            // Identifier — may be the start of `module.func(…)`.
            Token::Ident | Token::Print | Token::Fn | Token::Import => {
                self.advance();
                let name = st.lexeme.clone();

                // Is it a module call?  module.func(args…)
                if self.peek() == Some(&Token::Dot) {
                    self.advance(); // consume '.'
                    let func = self.parse_ident_string();
                    self.expect(Token::LParen, "expected '(' after module function name");
                    let args = self.parse_arg_list();
                    self.expect(Token::RParen, "expected ')' after arguments");

                    Expr::ModuleCall {
                        module: name,
                        func,
                        args,
                    }
                } else {
                    panic!("Unexpected identifier '{name}' — not a known statement");
                }
            }
            other => panic!("Unexpected token in expression: {other:?}"),
        }
    }

    /// Parse a comma-separated argument list (without the surrounding parens).
    fn parse_arg_list(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();

        // Empty arg list?
        if self.peek() == Some(&Token::RParen) {
            return args;
        }

        args.push(self.parse_expr());

        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume ','
            args.push(self.parse_expr());
        }

        args
    }

    /// Consume the next token and interpret it as an identifier string.
    /// Handles both `Ident` and keyword tokens that might be used as names.
    fn parse_ident_string(&mut self) -> String {
        let st = self.advance();
        match st.token {
            Token::Ident | Token::Print | Token::Fn | Token::Import => st.lexeme,
            other => panic!("Expected identifier, got {other:?}"),
        }
    }
}
