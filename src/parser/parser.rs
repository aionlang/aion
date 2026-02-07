//! Recursive-descent parser for the Aion language.
//!
//! Grammar (current subset):
//! ```text
//! program    = import* function* EOF
//! import     = "import" IDENT ("." IDENT)* ";"
//! function   = "fn" IDENT "(" ")" "{" statement* "}"
//! statement  = var_def | expr_stmt
//! expr       = func_call | module_call | float_lit | int_lit | string_lit | var_ref
//! func_call  = IDENT "(" args ")"
//! module_call= IDENT "." IDENT "(" args ")"
//! args       = expr ("," expr)*
//! ```

use crate::ast::{Expr, Function, Import, Program, BinOperator, AssignBinOperator};
use crate::errors::{self, Phase};
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
        if st.token != expected {
            errors::fatal(
                Phase::Parser,
                format!("{msg} (expected {expected:?}, got {:?})", st.token),
            );
        }
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
                None => errors::fatal(Phase::Parser, "Unexpected end of input inside function body"),
            }
        }

        Function { name, body }
    }

    /// Parse a single statement inside a function body.
    fn parse_statement(&mut self) -> Expr {
        match self.peek() {
            Some(Token::Ident) => {
                // Look ahead to distinguish variable def from other expressions
                if self.peek_ahead_is_var_def() {
                    self.parse_var_def()
                } else if self.peek_ahead_is_assignment() {
                    self.parse_assignment()
                } else {
                    self.parse_expr()
                }
            }
            _ => self.parse_expr(),
        }
    }


    fn peek_ahead_is_var_def(&self) -> bool {
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        matches!(
            self.tokens[self.pos + 1].token,
            Token::Colon | Token::ColonEquals | Token::DoubleColonEquals
        )
    }

    fn peek_ahead_is_assignment(&self) -> bool {
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        matches!(
            self.tokens[self.pos + 1].token,
            Token::Equals | Token::PlusEquals | Token::MinusEquals | Token::DotEquals
        )
    }

    /// Parse an assignment: `a = 5`, `a += 1`, `a -= 3`, `a .= method()`
    fn parse_assignment(&mut self) -> Expr {
        let name = self.parse_ident_string();
        let op_token = self.advance(); // consume =, +=, -=, .=

        let op = match op_token.token {
            Token::Equals      => None,
            Token::PlusEquals  => Some(AssignBinOperator::AddAssign),
            Token::MinusEquals => Some(AssignBinOperator::SubAssign),
            Token::DotEquals   => Some(AssignBinOperator::DotAssign),
            _ => unreachable!(),
        };

        let value = self.parse_expr();

        Expr::VarAssign {
            name,
            value: Box::new(value),
            op,
        }
    }

    /// Parse an expression.
    fn parse_var_def(&mut self) -> Expr {
        let name = self.parse_ident_string();
        
        // Handle ::= (infer-and-assign shorthand)
        if self.peek() == Some(&Token::DoubleColonEquals) {
            self.advance(); // consume '::='
            let init = self.parse_expr();
            return Expr::VarDef {
                name,
                type_annotation: None,
                value: Some(Box::new(init)),
            };
        }

        let type_annotation = if self.peek() == Some(&Token::Colon) {
            self.advance(); // consume ':'
            
            // Parse type name — Int, Float, or any future custom type
            let type_name = match self.peek() {
                Some(Token::Ident) => self.parse_ident_string(),
                _ => errors::fatal_with_hint(
                    Phase::Parser,
                    format!("Expected type name after ':', got {:?}", self.peek()),
                    Some("Valid types: Int, Float".into()),
                ),
            };
            
            Some(type_name)
        } else {
            None
        };
        
        let value = if self.peek() == Some(&Token::ColonEquals)
            || self.peek() == Some(&Token::DoubleColonEquals)
        {
            self.advance(); // consume ':=' or '::='
            Some(Box::new(self.parse_expr()))
        } else {
            None
        };
        
        // Validate: must have either type or value (or both)
        if type_annotation.is_none() && value.is_none() {
            errors::fatal_with_hint(
                Phase::Parser,
                format!("Variable '{name}' must have a type annotation or initial value"),
                Some(format!("Try: {name} : Int := 0  or  {name} ::= 0")),
            );
        }
        
        Expr::VarDef {
            name,
            type_annotation,
            value,
        }
    }
    

    fn parse_expr(&mut self) -> Expr {
        let mut left = self.parse_additive();

        while let Some(op) = self.peek() {
            let bin_op = match op {
                Token::EqualsEquals  => BinOperator::Eq,
                Token::BangEquals    => BinOperator::Neq,
                Token::Less          => BinOperator::Lt,
                Token::Greater       => BinOperator::Gt,
                Token::LessEquals    => BinOperator::Lte,
                Token::GreaterEquals => BinOperator::Gte,
                _ => break,
            };
            self.advance(); // consume the comparison operator

            let right = self.parse_additive();
            left = Expr::BinaryOp {
                op: bin_op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    /// Parse additive expressions: `+` and `-`.
    fn parse_additive(&mut self) -> Expr {
        let mut left = self.parse_primary();

        while let Some(op) = self.peek() {
            let bin_op = match op {
                Token::Plus  => BinOperator::Add,
                Token::Minus => BinOperator::Sub,
                _ => break,
            };
            self.advance(); // consume the operator

            let right = self.parse_primary();
            left = Expr::BinaryOp {
                op: bin_op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    /// Parse an expression.
    fn parse_primary(&mut self) -> Expr {
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
                self.advance();
                let content = st.lexeme[1..st.lexeme.len() - 1].to_string();
                Expr::StringLiteral(content)
            }

            Token::While => {
                self.advance(); // consume 'while'
                self.expect(Token::LParen, "expected '(' after 'while'");
                let condition = self.parse_expr();
                self.expect(Token::RParen, "expected ')' after while condition");
                self.expect(Token::LBrace, "expected '{' to open 'while' body");

                let mut body = Vec::new();
                while self.peek() != Some(&Token::RBrace) {
                    body.push(self.parse_statement());
                }
                self.expect(Token::RBrace, "expected '}' to close 'while' body");

                Expr::WhileExpr { condition: Box::new(condition), body }
            }

            Token::If => {
                self.advance(); // consume 'if'
                self.expect(Token::LParen, "expected '(' after 'if'");
                let condition = self.parse_expr();
                self.expect(Token::RParen, "expected ')' after if condition");
                self.expect(Token::LBrace, "expected '{' to open 'if' body");

                let mut then_branch = Vec::new();
                while self.peek() != Some(&Token::RBrace) {
                    then_branch.push(self.parse_statement());
                }
                self.expect(Token::RBrace, "expected '}' to close 'if' body");

                let else_branch = if self.peek() == Some(&Token::Else) {
                    self.advance(); // consume 'else'
                    self.expect(Token::LBrace, "expected '{' after 'else'");
                    let mut stmts = Vec::new();
                    while self.peek() != Some(&Token::RBrace) {
                        stmts.push(self.parse_statement());
                    }
                    self.expect(Token::RBrace, "expected '}' to close 'else' body");
                    Some(stmts)
                } else {
                    None
                };

                Expr::IfExpr { condition: Box::new(condition), then_branch, else_branch }
            }
            
            // Identifier — could be variable reference, module call, or function call or conditional expression
            // Boolean literals
            Token::True => {
                self.advance();
                Expr::BooleanLiteral(true)
            }
            Token::False => {
                self.advance();
                Expr::BooleanLiteral(false)
            }

            Token::Ident | Token::Fn | Token::Import => {
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
                }
                // Is it a function call?  func(args…)
                else if self.peek() == Some(&Token::LParen) {
                    self.advance(); // consume '('
                    let args = self.parse_arg_list();
                    self.expect(Token::RParen, "expected ')' after arguments");

                    Expr::FuncCall { name, args }
                }

                // is it variable value modification? 


                else {
                    // Just a variable reference
                    Expr::VarRef(name)
                }
            }
            other => errors::fatal_with_hint(
                Phase::Parser,
                format!("Unexpected token in expression: {other:?}"),
                Some("Expected a literal, identifier, or function call".into()),
            ),
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
            Token::Ident | Token::Fn | Token::Import => st.lexeme,
            other => errors::fatal(
                Phase::Parser,
                format!("Expected identifier, got {other:?}"),
            ),
        }
    }
}
