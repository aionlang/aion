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

use crate::ast::{AssignBinOperator, BinOperator, ConstructorDef, Expr, FieldDef, Function, Import, Param, Program, StringInterpolationPart, TypeDef};
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
        let mut type_defs = Vec::new();
        let mut impl_methods: Vec<(String, crate::ast::Function)> = Vec::new();

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

        // Parse type definitions and function definitions.
        while self.has_more() {
            match self.peek() {
                Some(Token::Fn) => {
                    self.advance(); // consume 'fn'
                    // Check for impl method: fn Type::method(…)
                    if self.pos + 1 < self.tokens.len()
                        && self.tokens[self.pos + 1].token == Token::DoubleColon
                    {
                        let (type_name, func) = self.parse_impl_method();
                        impl_methods.push((type_name, func));
                    } else {
                        functions.push(self.parse_function());
                    }
                }
                Some(Token::Type) => {
                    self.advance(); // consume 'type'
                    type_defs.push(self.parse_type_def());
                }
                _ => {
                    // Skip any unrecognised top-level token.
                    self.advance();
                }
            }
        }

        Program { imports, functions, type_defs, impl_methods, user_modules: Vec::new() }
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

    /// Parse a type definition.
    ///
    /// ```text
    /// type Animal(it.name, it.age) {
    ///     name: String?
    ///     age: Int?
    ///     fn speak() -> String { ... }
    /// }
    /// ```
    ///
    /// The optional `(it.field, …)` declares constructor parameters.
    /// Inheritance-like syntax: `type Dog: Animal { … }`.
    fn parse_type_def(&mut self) -> TypeDef {
        let name = self.parse_ident_string();

        // ── optional parent type: `type Dog: Animal` ─────────────
        let parent = if self.peek() == Some(&Token::Colon) {
            self.advance(); // consume ':'
            Some(self.parse_ident_string())
        } else {
            None
        };

        // ── optional constructor params: `(it.name, it.age)` ─────
        let constructor_params = if self.peek() == Some(&Token::LParen) {
            self.advance(); // consume '('
            let mut params = Vec::new();
            while self.peek() != Some(&Token::RParen) {
                // Expect `it.fieldName`
                let prefix = self.parse_ident_string();
                if prefix != "it" {
                    errors::fatal_with_hint(
                        Phase::Parser,
                        format!("Expected 'it' in constructor parameter, got '{prefix}'"),
                        Some("Constructor params use `it.fieldName` syntax".into()),
                    );
                }
                self.expect(Token::Dot, "expected '.' after 'it' in constructor parameter");
                let field_name = self.parse_ident_string();
                params.push(field_name);

                if self.peek() == Some(&Token::Comma) {
                    self.advance(); // consume ','
                }
            }
            self.advance(); // consume ')'
            params
        } else {
            Vec::new()
        };

        // ── body: fields and methods ─────────────────────────────
        self.expect(Token::LBrace, "expected '{' to open type body");

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut constructor = None;

        while self.peek() != Some(&Token::RBrace) {
            match self.peek() {
                Some(Token::Fn) => {
                    self.advance(); // consume 'fn'
                    methods.push(self.parse_function());
                }
                Some(Token::Constructor) => {
                    self.advance(); // consume 'constructor'
                    self.expect(Token::LParen, "expected '(' after 'constructor'");
                    let params = self.parse_param_list();
                    self.expect(Token::RParen, "expected ')' after constructor parameters");
                    self.expect(Token::LBrace, "expected '{' to open constructor body");

                    let mut body = Vec::new();
                    while self.peek() != Some(&Token::RBrace) {
                        body.push(self.parse_statement());
                    }
                    self.advance(); // consume '}'
                    constructor = Some(ConstructorDef { params, body });
                }
                Some(Token::Ident) => {
                    // Field definition: `name: Type` or `name: Type?`
                    let field_name = self.parse_ident_string();
                    self.expect(Token::Colon, &format!("expected ':' after field name '{field_name}'"));
                    let type_name = self.parse_ident_string();
                    let nullable = if self.peek() == Some(&Token::Question) {
                        self.advance(); // consume '?'
                        true
                    } else {
                        false
                    };
                    fields.push(FieldDef { name: field_name, type_name, nullable });
                }
                None => errors::fatal(Phase::Parser, "Unexpected end of input inside type body"),
                _ => {
                    self.advance(); // skip unrecognised
                }
            }
        }
        self.advance(); // consume '}'

        TypeDef { name, parent, constructor_params, fields, methods, constructor }
    }

    /// Parse an impl method: `fn Type::method(…) { … }`
    ///
    /// The `fn` keyword has already been consumed.
    /// Returns `(type_name, Function)`.
    fn parse_impl_method(&mut self) -> (String, Function) {
        let type_name = self.parse_ident_string();
        self.expect(Token::DoubleColon, "expected '::' in impl method");
        let func = self.parse_function();
        (type_name, func)
    }

    /// Parse a function definition.
    ///
    /// Block form:  `fn <name>() { … }`
    /// Arrow form:  `fn <name>() => <expr>`
    fn parse_function(&mut self) -> Function {
        let name = self.parse_ident_string();

        self.expect(Token::LParen, "expected '(' after function name");
        let params = self.parse_param_list();
        self.expect(Token::RParen, "expected ')' after parameter list");

        // ── optional return type: fn name() -> Type ──────────────
        let return_type = if self.peek() == Some(&Token::ThinArrow) {
            self.advance(); // consume '->'
            Some(self.parse_ident_string())
        } else {
            None
        };

        // ── arrow function: fn name() => expr ────────────────────
        if self.peek() == Some(&Token::FatArrow) {
            self.advance(); // consume '=>'
            let expr = self.parse_expr();
            return Function { name, params, body: vec![expr], is_arrow: true, return_type };
        }

        // ── block function: fn name() { … } ─────────────────────
        self.expect(Token::LBrace, "expected '{' or '=>' after function signature");

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

        Function { name, params, body, is_arrow: false, return_type }
    }

    /// Parse a single statement inside a function body.
    fn parse_statement(&mut self) -> Expr {
        match self.peek() {
            Some(Token::Return) => {
                self.advance(); // consume 'return'
                // If the next token starts an expression, parse it.
                let value = match self.peek() {
                    Some(Token::RBrace) | None => None,
                    _ => Some(Box::new(self.parse_expr())),
                };
                Expr::ReturnExpr { value }
            }
            Some(Token::Ident) => {
                // Check for field assignment: ident.ident = expr
                if self.peek_ahead_is_field_assign() {
                    self.parse_field_assign()
                }
                // Look ahead to distinguish variable def from other expressions
                else if self.peek_ahead_is_var_def() {
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

    /// Check for `ident.ident =` pattern (field assignment).
    fn peek_ahead_is_field_assign(&self) -> bool {
        self.pos + 3 < self.tokens.len()
            && self.tokens[self.pos + 1].token == Token::Dot
            && self.tokens[self.pos + 2].token == Token::Ident
            && self.tokens[self.pos + 3].token == Token::Equals
    }

    /// Parse a field assignment: `it.name = value`
    fn parse_field_assign(&mut self) -> Expr {
        let object_name = self.parse_ident_string();
        self.expect(Token::Dot, "expected '.' in field assignment");
        let field = self.parse_ident_string();
        self.expect(Token::Equals, "expected '=' in field assignment");
        let value = self.parse_expr();
        Expr::FieldAssign {
            object: Box::new(Expr::VarRef(object_name)),
            field,
            value: Box::new(value),
        }
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

    fn parse_interpolated_string(&mut self, content: String) -> Expr {
        let mut parts = Vec::new();
        let mut current_literal = String::new();
        let mut chars = content.chars().peekable();
        
        while let Some(ch) = chars.next() {
            if ch == '$' && chars.peek() == Some(&'{') {
                // Found start of interpolation
                chars.next(); // consume '{'
                
                // Save any literal text we've accumulated
                if !current_literal.is_empty() {
                    parts.push(StringInterpolationPart::Literal(current_literal.clone()));
                    current_literal.clear();
                }
                
                // Extract the expression inside ${}
                let mut expr_str = String::new();
                let mut depth = 1;
                
                while depth > 0 {
                    match chars.next() {
                        Some('{') => {
                            depth += 1;
                            expr_str.push('{');
                        }
                        Some('}') => {
                            depth -= 1;
                            if depth > 0 {
                                expr_str.push('}');
                            }
                        }
                        Some(c) => expr_str.push(c),
                        None => {
                            errors::fatal(
                                Phase::Parser,
                                "Unclosed interpolation expression in string",
                            );
                        }
                    }
                }
                
                // Parse the expression
                let mut expr_parser = Parser::new(&expr_str);
                let expr = expr_parser.parse_expr();
                parts.push(StringInterpolationPart::Expr(Box::new(expr)));
            } else if ch == '\\' {
                // Handle escape sequences
                match chars.next() {
                    Some('n') => current_literal.push('\n'),
                    Some('t') => current_literal.push('\t'),
                    Some('r') => current_literal.push('\r'),
                    Some('\\') => current_literal.push('\\'),
                    Some('"') => current_literal.push('"'),
                    Some('$') => current_literal.push('$'),
                    Some(c) => {
                        current_literal.push('\\');
                        current_literal.push(c);
                    }
                    None => current_literal.push('\\'),
                }
            } else {
                current_literal.push(ch);
            }
        }
        
        // Save any remaining literal text
        if !current_literal.is_empty() {
            parts.push(StringInterpolationPart::Literal(current_literal));
        }
        
        Expr::InterpolatedString { parts }
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

                if content.contains("${") {
                    return self.parse_interpolated_string(content);
                } else {
                    Expr::StringLiteral(content)
                }
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

                // Is it a dot-access?  module.func(args…) or field access a.name
                if self.peek() == Some(&Token::Dot) {
                    self.advance(); // consume '.'
                    let member = self.parse_ident_string();

                    if self.peek() == Some(&Token::LParen) {
                        // module.func(args…) → ModuleCall
                        self.advance(); // consume '('
                        let args = self.parse_arg_list();
                        self.expect(Token::RParen, "expected ')' after arguments");

                        Expr::ModuleCall {
                            module: name,
                            func: member,
                            args,
                        }
                    } else {
                        // a.name → FieldAccess
                        Expr::FieldAccess {
                            object: Box::new(Expr::VarRef(name)),
                            field: member,
                        }
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
            Token::Ident | Token::Fn | Token::Import | Token::Type | Token::Return | Token::Constructor => st.lexeme,
            other => errors::fatal(
                Phase::Parser,
                format!("Expected identifier, got {other:?}"),
            ),
        }
    }

    /// Parse a comma-separated parameter list (without surrounding parens).
    ///
    /// Supports:
    ///   `name: Type`   — typed parameter
    ///   `name`         — untyped parameter (type inferred later)
    fn parse_param_list(&mut self) -> Vec<Param> {
        let mut params = Vec::new();

        // Empty param list?
        if self.peek() == Some(&Token::RParen) {
            return params;
        }

        params.push(self.parse_single_param());

        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume ','
            params.push(self.parse_single_param());
        }

        params
    }

    /// Parse a single parameter: `name: Type` (type is required).
    fn parse_single_param(&mut self) -> Param {
        let name = self.parse_ident_string();

        self.expect(Token::Colon, &format!("expected ':' and type after parameter '{name}'"));
        let type_annotation = self.parse_ident_string();

        Param { name, type_annotation }
    }
}
