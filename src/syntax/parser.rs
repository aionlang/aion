//! Recursive-descent parser.
//!
//! ```text
//! module   = (import | item)*
//! import   = "import" IDENT ("." IDENT)* ";"?
//! item     = fn | extern_fn | type | "fn" IDENT "::" fn
//! fn       = "fn" IDENT "(" params ")" ("->" TYPE)? (block | "=>" expr)
//! extern   = "extern" "fn" IDENT "(" params ")" ("->" TYPE)? ("=" STRING)?
//! type     = "type" IDENT ("(" "it" "." IDENT,* ")")? "{" member* "}"
//! member   = IDENT ":" TYPE "?"? | fn | "constructor" "(" params ")" block
//! stmt     = let | assign | if | while | return | expr
//! let      = IDENT (":" TYPE)? (":=" expr)?
//! if       = "if" expr block ("else" (if | block))?
//! ```

use super::ast::*;
use super::lexer::{Tok, Token, interpolation_end, lex};
use crate::diag::{Diagnostic, Result, Span};

pub fn parse_module(file: u32, text: &str) -> Result<Module> {
    Parser::new(file, text, 0)?.module()
}

struct Parser<'a> {
    file: u32,
    src: &'a str,
    base: usize,
    toks: Vec<Tok>,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(file: u32, src: &'a str, base: usize) -> Result<Self> {
        Ok(Parser { file, src, base, toks: lex(file, src, base)?, pos: 0 })
    }

    // ── token plumbing ──────────────────────────────────────────

    fn peek(&self) -> Option<Token> {
        self.toks.get(self.pos).map(|t| t.kind)
    }

    fn peek_at(&self, n: usize) -> Option<Token> {
        self.toks.get(self.pos + n).map(|t| t.kind)
    }

    fn text(&self, tok: Tok) -> &'a str {
        &self.src[tok.span.start as usize - self.base..tok.span.end as usize - self.base]
    }

    /// Span of the next token, or an empty span at the end of input.
    fn here(&self) -> Span {
        match self.toks.get(self.pos) {
            Some(t) => t.span,
            None => {
                let end = self.base + self.src.len();
                Span::new(self.file, end, end)
            }
        }
    }

    fn prev_span(&self) -> Span {
        self.toks.get(self.pos.wrapping_sub(1)).map_or(self.here(), |t| t.span)
    }

    fn bump(&mut self) -> Tok {
        let t = self.toks[self.pos];
        self.pos += 1;
        t
    }

    fn eat(&mut self, kind: Token) -> bool {
        if self.peek() == Some(kind) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn expected(&self, what: &str) -> Diagnostic {
        let found = self.peek().map_or("end of file", Token::describe);
        Diagnostic::new(format!("expected {what}, found {found}"), self.here())
    }

    fn expect(&mut self, kind: Token) -> Result<Tok> {
        if self.peek() == Some(kind) { Ok(self.bump()) } else { Err(self.expected(kind.describe())) }
    }

    fn ident(&mut self) -> Result<Ident> {
        let t = self.expect(Token::Ident)?;
        Ok(Ident { name: self.text(t).to_string(), span: t.span })
    }

    fn type_ref(&mut self) -> Result<TypeRef> {
        let id = self.ident()?;
        self.eat(Token::Question); // nullability is not tracked yet
        Ok(TypeRef { name: id.name, span: id.span })
    }

    // ── items ───────────────────────────────────────────────────

    fn module(&mut self) -> Result<Module> {
        let mut imports = Vec::new();
        let mut items = Vec::new();
        while let Some(tok) = self.peek() {
            match tok {
                Token::Semi => {
                    self.bump();
                }
                Token::Import => imports.push(self.import()?),
                Token::Type => items.push(Item::Type(self.type_decl()?)),
                Token::Extern => {
                    let start = self.bump().span;
                    self.expect(Token::Fn)?;
                    items.push(Item::Fn(self.fn_decl(start, true)?));
                }
                Token::Fn => {
                    let start = self.bump().span;
                    if self.peek() == Some(Token::Ident) && self.peek_at(1) == Some(Token::ColonColon) {
                        let owner = self.ident()?;
                        self.bump();
                        let decl = self.fn_decl(start, false)?;
                        items.push(Item::Method { owner, decl });
                    } else {
                        items.push(Item::Fn(self.fn_decl(start, false)?));
                    }
                }
                _ => return Err(self.expected("`fn`, `type` or `import`")),
            }
        }
        Ok(Module { imports, items })
    }

    fn import(&mut self) -> Result<Import> {
        let start = self.bump().span;
        let mut path = vec![self.ident()?];
        while self.eat(Token::Dot) {
            path.push(self.ident()?);
        }
        self.eat(Token::Semi);
        Ok(Import { span: start.to(self.prev_span()), path })
    }

    fn params(&mut self) -> Result<Vec<Param>> {
        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        while self.peek() != Some(Token::RParen) {
            let name = self.ident()?;
            self.expect(Token::Colon)?;
            let ty = self.type_ref()?;
            params.push(Param { name, ty });
            if !self.eat(Token::Comma) {
                break;
            }
        }
        self.expect(Token::RParen)?;
        Ok(params)
    }

    /// Parse after `fn` (and the optional `Type::`): name, signature, body.
    fn fn_decl(&mut self, start: Span, is_extern: bool) -> Result<FnDecl> {
        let name = self.ident()?;
        let params = self.params()?;
        let ret = if self.eat(Token::Arrow) { Some(self.type_ref()?) } else { None };
        let body = if is_extern {
            let symbol = if self.eat(Token::Eq) {
                let t = self.expect(Token::Str)?;
                let raw = self.text(t);
                Some(raw[1..raw.len() - 1].to_string())
            } else {
                None
            };
            FnBody::Extern { symbol }
        } else if self.eat(Token::FatArrow) {
            FnBody::Arrow(self.expr()?)
        } else if self.peek() == Some(Token::LBrace) {
            FnBody::Block(self.block()?)
        } else {
            return Err(self.expected("`{` or `=>`"));
        };
        Ok(FnDecl { name, params, ret, body, span: start.to(self.prev_span()) })
    }

    fn type_decl(&mut self) -> Result<TypeDecl> {
        self.bump(); // `type`
        let name = self.ident()?;
        if self.peek() == Some(Token::Colon) {
            return Err(Diagnostic::new("type inheritance is not supported", self.here()));
        }

        let mut ctor_fields = None;
        if self.eat(Token::LParen) {
            let mut fields = Vec::new();
            while self.peek() != Some(Token::RParen) {
                let it = self.ident()?;
                if it.name != "it" {
                    return Err(Diagnostic::new("constructor parameters are written `it.field`", it.span));
                }
                self.expect(Token::Dot)?;
                fields.push(self.ident()?);
                if !self.eat(Token::Comma) {
                    break;
                }
            }
            self.expect(Token::RParen)?;
            ctor_fields = Some(fields);
        }

        self.expect(Token::LBrace)?;
        let (mut fields, mut methods, mut ctor) = (Vec::new(), Vec::new(), None);
        while self.peek() != Some(Token::RBrace) {
            match self.peek() {
                Some(Token::Semi) => {
                    self.bump();
                }
                Some(Token::Fn) => {
                    let s = self.bump().span;
                    methods.push(self.fn_decl(s, false)?);
                }
                Some(Token::Constructor) => {
                    let s = self.bump().span;
                    let params = self.params()?;
                    let body = self.block()?;
                    if ctor.is_some() {
                        return Err(Diagnostic::new("a type can have only one constructor", s));
                    }
                    ctor = Some(CtorDecl { params, body });
                }
                Some(Token::Ident) => {
                    let name = self.ident()?;
                    self.expect(Token::Colon)?;
                    let ty = self.type_ref()?;
                    fields.push(FieldDecl { name, ty });
                }
                _ => return Err(self.expected("a field, `fn` or `constructor`")),
            }
        }
        self.expect(Token::RBrace)?;
        Ok(TypeDecl { name, ctor_fields, fields, ctor, methods })
    }

    // ── statements ──────────────────────────────────────────────

    fn block(&mut self) -> Result<Vec<Stmt>> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                Some(Token::RBrace) => {
                    self.bump();
                    return Ok(stmts);
                }
                Some(Token::Semi) => {
                    self.bump();
                }
                None => return Err(self.expected("`}`")),
                Some(_) => stmts.push(self.stmt()?),
            }
        }
    }

    fn stmt(&mut self) -> Result<Stmt> {
        let start = self.here();
        let kind = match self.peek() {
            Some(Token::Return) => {
                self.bump();
                let value = match self.peek() {
                    None | Some(Token::RBrace | Token::Semi) => None,
                    _ => Some(self.expr()?),
                };
                StmtKind::Return(value)
            }
            Some(Token::If) => return self.if_stmt(),
            Some(Token::While) => {
                self.bump();
                let cond = self.expr()?;
                StmtKind::While { cond, body: self.block()? }
            }
            Some(Token::Ident) if matches!(self.peek_at(1), Some(Token::Colon | Token::ColonEq)) => {
                let name = self.ident()?;
                let ty = if self.eat(Token::Colon) { Some(self.type_ref()?) } else { None };
                let init = if self.eat(Token::ColonEq) { Some(self.expr()?) } else { None };
                StmtKind::Let { name, ty, init }
            }
            _ => {
                let lhs = self.expr()?;
                let op = match self.peek() {
                    Some(Token::Eq) => Some(AssignOp::Set),
                    Some(Token::PlusEq) => Some(AssignOp::Add),
                    Some(Token::MinusEq) => Some(AssignOp::Sub),
                    Some(Token::StarEq) => Some(AssignOp::Mul),
                    Some(Token::SlashEq) => Some(AssignOp::Div),
                    _ => None,
                };
                match op {
                    None => StmtKind::Expr(lhs),
                    Some(op) => {
                        if !matches!(lhs.kind, ExprKind::Name(_) | ExprKind::Field(..)) {
                            return Err(Diagnostic::new("cannot assign to this expression", lhs.span));
                        }
                        self.bump();
                        StmtKind::Assign { target: lhs, op, value: self.expr()? }
                    }
                }
            }
        };
        Ok(Stmt { kind, span: start.to(self.prev_span()) })
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        let start = self.bump().span;
        let cond = self.expr()?;
        let then = self.block()?;
        let otherwise = if self.eat(Token::Else) {
            if self.peek() == Some(Token::If) { Some(vec![self.if_stmt()?]) } else { Some(self.block()?) }
        } else {
            None
        };
        Ok(Stmt { kind: StmtKind::If { cond, then, otherwise }, span: start.to(self.prev_span()) })
    }

    // ── expressions ─────────────────────────────────────────────

    fn expr(&mut self) -> Result<Expr> {
        self.binary(1)
    }

    fn binary(&mut self, min_prec: u8) -> Result<Expr> {
        let mut lhs = self.unary()?;
        while let Some((prec, op)) = self.peek().and_then(binop) {
            if prec < min_prec {
                break;
            }
            self.bump();
            let rhs = self.binary(prec + 1)?;
            let span = lhs.span.to(rhs.span);
            lhs = Expr { kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)), span };
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expr> {
        let op = match self.peek() {
            Some(Token::Minus) => UnOp::Neg,
            Some(Token::Bang) => UnOp::Not,
            _ => return self.postfix(),
        };
        let start = self.bump().span;
        let inner = self.unary()?;
        let span = start.to(inner.span);
        Ok(Expr { kind: ExprKind::Unary(op, Box::new(inner)), span })
    }

    fn postfix(&mut self) -> Result<Expr> {
        let mut e = self.primary()?;
        loop {
            match self.peek() {
                Some(Token::Dot) => {
                    self.bump();
                    let field = self.ident()?;
                    let span = e.span.to(field.span);
                    e = Expr { kind: ExprKind::Field(Box::new(e), field), span };
                }
                Some(Token::LParen) => {
                    self.bump();
                    let mut args = Vec::new();
                    while self.peek() != Some(Token::RParen) {
                        args.push(self.expr()?);
                        if !self.eat(Token::Comma) {
                            break;
                        }
                    }
                    let close = self.expect(Token::RParen)?;
                    let span = e.span.to(close.span);
                    e = Expr { kind: ExprKind::Call { callee: Box::new(e), args }, span };
                }
                _ => return Ok(e),
            }
        }
    }

    fn primary(&mut self) -> Result<Expr> {
        let Some(kind) = self.peek() else { return Err(self.expected("an expression")) };
        let tok = self.toks[self.pos];
        let span = tok.span;
        let kind = match kind {
            Token::Int => {
                self.bump();
                let v = self.text(tok).parse::<i64>().map_err(|_| {
                    Diagnostic::new("integer literal is too large for Int (64-bit)", span)
                })?;
                ExprKind::Int(v)
            }
            Token::Float => {
                self.bump();
                ExprKind::Float(self.text(tok).parse().expect("lexer guarantees a valid float"))
            }
            Token::True | Token::False => {
                self.bump();
                ExprKind::Bool(kind == Token::True)
            }
            Token::Ident => {
                self.bump();
                ExprKind::Name(self.text(tok).to_string())
            }
            Token::Str => {
                self.bump();
                return self.string_expr(tok);
            }
            Token::LParen => {
                self.bump();
                let inner = self.expr()?;
                self.expect(Token::RParen)?;
                return Ok(inner);
            }
            _ => return Err(self.expected("an expression")),
        };
        Ok(Expr { kind, span })
    }

    /// Decode escapes and split `${...}` interpolations out of a string literal.
    fn string_expr(&mut self, tok: Tok) -> Result<Expr> {
        let full = self.text(tok);
        let raw = &full[1..full.len() - 1];
        let raw_off = tok.span.start as usize + 1;
        let bytes = raw.as_bytes();

        let mut parts = Vec::new();
        let mut lit = String::new();
        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b'\\' => {
                    let esc = raw[i + 1..].chars().next().expect("lexer guarantees an escaped char");
                    lit.push(match esc {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '0' => '\0',
                        '\\' | '"' | '$' => esc,
                        other => {
                            let at = raw_off + i;
                            return Err(Diagnostic::new(
                                format!("unknown escape sequence `\\{other}`"),
                                Span::new(self.file, at, at + 1 + other.len_utf8()),
                            ));
                        }
                    });
                    i += 1 + esc.len_utf8();
                }
                b'$' if bytes.get(i + 1) == Some(&b'{') => {
                    let end = interpolation_end(bytes, i + 2).expect("lexer guarantees a closed ${}");
                    let inner = &raw[i + 2..end - 1];
                    let mut sub = Parser::new(self.file, inner, raw_off + i + 2)?;
                    let expr = sub.expr()?;
                    if sub.pos < sub.toks.len() {
                        return Err(sub.expected("`}`"));
                    }
                    if !lit.is_empty() {
                        parts.push(InterpPart::Lit(std::mem::take(&mut lit)));
                    }
                    parts.push(InterpPart::Expr(expr));
                    i = end;
                }
                _ => {
                    let c = raw[i..].chars().next().unwrap();
                    lit.push(c);
                    i += c.len_utf8();
                }
            }
        }

        let kind = if parts.is_empty() {
            ExprKind::Str(lit)
        } else {
            if !lit.is_empty() {
                parts.push(InterpPart::Lit(lit));
            }
            ExprKind::Interp(parts)
        };
        Ok(Expr { kind, span: tok.span })
    }
}

/// Binary operator precedence table (higher binds tighter).
fn binop(t: Token) -> Option<(u8, BinOp)> {
    Some(match t {
        Token::OrOr => (1, BinOp::Or),
        Token::AndAnd => (2, BinOp::And),
        Token::EqEq => (3, BinOp::Eq),
        Token::NotEq => (3, BinOp::Ne),
        Token::Lt => (4, BinOp::Lt),
        Token::Le => (4, BinOp::Le),
        Token::Gt => (4, BinOp::Gt),
        Token::Ge => (4, BinOp::Ge),
        Token::Plus => (5, BinOp::Add),
        Token::Minus => (5, BinOp::Sub),
        Token::Star => (6, BinOp::Mul),
        Token::Slash => (6, BinOp::Div),
        Token::Percent => (6, BinOp::Rem),
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr(src: &str) -> Expr {
        Parser::new(0, src, 0).unwrap().expr().unwrap()
    }

    /// Render an expression as a fully parenthesised string.
    fn show(e: &Expr) -> String {
        match &e.kind {
            ExprKind::Int(v) => v.to_string(),
            ExprKind::Name(n) => n.clone(),
            ExprKind::Binary(op, l, r) => format!("({} {} {})", show(l), op.symbol(), show(r)),
            ExprKind::Unary(UnOp::Neg, x) => format!("(-{})", show(x)),
            ExprKind::Unary(UnOp::Not, x) => format!("(!{})", show(x)),
            ExprKind::Field(o, f) => format!("{}.{}", show(o), f.name),
            ExprKind::Call { callee, args } => {
                format!("{}({})", show(callee), args.iter().map(show).collect::<Vec<_>>().join(", "))
            }
            other => format!("{other:?}"),
        }
    }

    #[test]
    fn operator_precedence_and_associativity() {
        assert_eq!(show(&parse_expr("1 + 2 * 3")), "(1 + (2 * 3))");
        assert_eq!(show(&parse_expr("10 - 4 - 3")), "((10 - 4) - 3)");
        assert_eq!(show(&parse_expr("a || b && c == d")), "(a || (b && (c == d)))");
        assert_eq!(show(&parse_expr("(1 + 2) * 3")), "((1 + 2) * 3)");
        assert_eq!(show(&parse_expr("-a * !b")), "((-a) * (!b))");
        assert_eq!(show(&parse_expr("a < b + 1")), "(a < (b + 1))");
    }

    #[test]
    fn postfix_chains() {
        assert_eq!(show(&parse_expr("a.b.c(1, x.y)")), "a.b.c(1, x.y)");
        assert_eq!(show(&parse_expr("m.f(1).g()")), "m.f(1).g()");
    }

    #[test]
    fn interpolation_splits_parts_and_keeps_offsets() {
        let src = r#"  "hi ${name}, ${1 + 2}!" "#;
        let e = parse_expr(src);
        let ExprKind::Interp(parts) = &e.kind else { panic!("expected interpolation") };
        assert_eq!(parts.len(), 5);
        let InterpPart::Expr(name) = &parts[1] else { panic!() };
        // The span of `name` points into the original text, not the substring.
        assert_eq!(&src[name.span.start as usize..name.span.end as usize], "name");
        let InterpPart::Expr(sum) = &parts[3] else { panic!() };
        assert_eq!(&src[sum.span.start as usize..sum.span.end as usize], "1 + 2");
    }

    #[test]
    fn nested_strings_inside_interpolation() {
        let e = parse_expr(r#""a ${"b ${c}"} d""#);
        assert!(matches!(e.kind, ExprKind::Interp(_)));
    }

    #[test]
    fn escapes_are_decoded() {
        let e = parse_expr(r#""a\tb\n\"q\" \\ \$""#);
        let ExprKind::Str(s) = e.kind else { panic!() };
        assert_eq!(s, "a\tb\n\"q\" \\ $");
    }

    #[test]
    fn bad_input_is_reported_not_swallowed() {
        let err = |src: &str| Parser::new(0, src, 0).and_then(|mut p| p.expr()).unwrap_err().message;
        assert!(err(r#""abc"#).contains("unterminated"));
        assert!(err(r#""a\q""#).contains("unknown escape"));
        assert!(err("1 +").contains("expected an expression"));
        assert!(err("#").contains("unexpected character"));
        assert!(err("99999999999999999999").contains("too large"));
    }

    #[test]
    fn module_structure() {
        let m = parse_module(
            0,
            "import std.math;\nimport utils\ntype T(it.x) { x: Int fn get() -> Int => it.x }\n\
             fn T::set(v: Int) { it.x = v }\nextern fn f(a: Int) -> Int = \"c_f\"\nfn main() {}",
        )
        .unwrap();
        assert_eq!(m.imports.len(), 2);
        assert_eq!(m.items.len(), 4);
        assert!(matches!(&m.items[1], Item::Method { owner, .. } if owner.name == "T"));
        assert!(matches!(&m.items[2], Item::Fn(f) if matches!(&f.body, FnBody::Extern { symbol: Some(s) } if s == "c_f")));
    }
}
