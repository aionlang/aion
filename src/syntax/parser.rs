//! Recursive-descent parser.
//!
//! ```text
//! module   = (import | item)*                 # items include plain statements
//! item     = fn | extern_fn | type | method | stmt
//! fn       = "fn" IDENT "(" params ")" ("->" TYPE)? (block | "=" expr)
//! method   = "fn" IDENT "." fn-rest
//! extern   = "extern" "fn" IDENT "(" params ")" ("->" TYPE)? ("=" STRING)?
//! type     = "type" IDENT ("(" params ")")? ("{" member* "}")?
//! member   = IDENT ":" TYPE ("=" expr)? | fn | "constructor" "(" params ")" block | "init" block
//! stmt     = let | assign | while | for | "break" | "continue" | "return" expr? | expr
//! expr     = binary (".." | "..=" binary)?
//! primary  = literal | name | "(" expr ")" | if | when
//! ```
//!
//! ## Newlines
//!
//! Statements need no terminator.  A line break ends the current expression
//! unless it sits inside `(...)`, or the next line starts with `.` or `&&`/`||`
//! (so method chains and long conditions can be split), or the line ended in an
//! operator.  A `(` that starts a new line is a new statement, not a call.

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
    /// Inside `( ... )` line breaks do not end expressions.
    ignore_nl: bool,
    /// While parsing an `if`/`while`/`for`/`when` header, a `{` starts the body.
    no_brace: bool,
}

impl<'a> Parser<'a> {
    fn new(file: u32, src: &'a str, base: usize) -> Result<Self> {
        Ok(Parser { file, src, base, toks: lex(file, src, base)?, pos: 0, ignore_nl: false, no_brace: false })
    }

    // ── token plumbing ──────────────────────────────────────────

    fn peek(&self) -> Option<Token> {
        self.toks.get(self.pos).map(|t| t.kind)
    }

    fn peek_at(&self, n: usize) -> Option<Token> {
        self.toks.get(self.pos + n).map(|t| t.kind)
    }

    /// Is the next token on a new line (and are line breaks significant here)?
    fn nl(&self) -> bool {
        !self.ignore_nl && self.toks.get(self.pos).is_some_and(|t| t.nl)
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
        // `(A, B) -> R`
        if self.peek() == Some(Token::LParen) {
            let start = self.bump().span;
            let mut params = Vec::new();
            while self.peek() != Some(Token::RParen) {
                params.push(self.type_ref()?);
                if !self.eat(Token::Comma) {
                    break;
                }
            }
            self.expect(Token::RParen)?;
            self.expect(Token::Arrow)?;
            let ret = self.type_ref()?;
            let span = start.to(ret.span);
            return Ok(TypeRef { kind: TypeKind::Fn { params, ret: Box::new(ret) }, span });
        }
        let id = self.ident()?;
        let mut span = id.span;
        let mut args = Vec::new();
        if self.peek() == Some(Token::Lt) && !self.nl() {
            self.bump();
            loop {
                args.push(self.type_ref()?);
                if !self.eat(Token::Comma) {
                    break;
                }
            }
            span = span.to(self.expect(Token::Gt)?.span);
        }
        let named = TypeRef { kind: TypeKind::Named { name: id.name, args }, span };
        if self.peek() == Some(Token::Question) {
            let q = self.bump().span;
            let span = named.span.to(q);
            return Ok(TypeRef { kind: TypeKind::Nullable(Box::new(named)), span });
        }
        Ok(named)
    }

    /// `<T, R>` after a declaration's name (empty if absent).
    fn type_params(&mut self) -> Result<Vec<Ident>> {
        let mut params = Vec::new();
        if self.peek() == Some(Token::Lt) {
            self.bump();
            loop {
                params.push(self.ident()?);
                if !self.eat(Token::Comma) {
                    break;
                }
            }
            self.expect(Token::Gt)?;
        }
        Ok(params)
    }

    /// `<Int, String>` in an expression, if that is what follows; otherwise
    /// nothing is consumed (it was a less-than sign).
    fn try_type_args(&mut self) -> Option<Vec<TypeRef>> {
        let save = self.pos;
        let attempt = (|p: &mut Self| -> Result<Vec<TypeRef>> {
            p.expect(Token::Lt)?;
            let mut args = Vec::new();
            loop {
                args.push(p.type_ref()?);
                if !p.eat(Token::Comma) {
                    break;
                }
            }
            p.expect(Token::Gt)?;
            Ok(args)
        })(self);
        if attempt.is_err() {
            self.pos = save;
        }
        attempt.ok()
    }

    /// Run `f` with the given newline / brace context, restoring it afterwards.
    fn scoped<T>(&mut self, ignore_nl: bool, no_brace: bool, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        let saved = (self.ignore_nl, self.no_brace);
        (self.ignore_nl, self.no_brace) = (ignore_nl, no_brace);
        let r = f(self);
        (self.ignore_nl, self.no_brace) = saved;
        r
    }

    /// An expression in an `if`/`while`/`for`/`when` header: a `{` ends it.
    fn header_expr(&mut self) -> Result<Expr> {
        let ignore = self.ignore_nl;
        self.scoped(ignore, true, |p| p.expr())
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
                Token::Ident if self.at_from_import() => imports.push(self.import_from()?),
                Token::Type => items.push(Item::Type(self.type_decl()?)),
                Token::Extern => {
                    let start = self.bump().span;
                    self.expect(Token::Fn)?;
                    items.push(self.fn_item(start, true)?);
                }
                Token::Fn => {
                    let start = self.bump().span;
                    items.push(self.fn_item(start, false)?);
                }
                _ => items.push(Item::Stmt(self.stmt()?)),
            }
        }
        Ok(Module { imports, items })
    }

    /// After `fn` / `extern fn`: a function, or a method if it is written
    /// `Owner.name(…)` / `Owner<T>.name(…)`.
    fn fn_item(&mut self, start: Span, is_extern: bool) -> Result<Item> {
        let save = self.pos;
        let owner = if self.peek() == Some(Token::Ident) {
            let owner = self.ident()?;
            match self.type_params() {
                Ok(owner_params) if self.peek() == Some(Token::Dot) => Some((owner, owner_params)),
                _ => None,
            }
        } else {
            None
        };
        match owner {
            Some((owner, owner_params)) => {
                self.bump(); // the `.`
                let decl = self.fn_decl(start, is_extern)?;
                Ok(Item::Method { owner, owner_params, decl })
            }
            None => {
                self.pos = save;
                Ok(Item::Fn(self.fn_decl(start, is_extern)?))
            }
        }
    }

    fn import(&mut self) -> Result<Import> {
        let start = self.bump().span;
        let path = self.import_path()?;
        let alias = if self.at_word("as") {
            self.bump();
            Some(self.ident()?)
        } else {
            None
        };
        self.eat(Token::Semi);
        Ok(Import { span: start.to(self.prev_span()), path, names: None, alias })
    }

    /// `from a.b import x, y`
    fn import_from(&mut self) -> Result<Import> {
        let start = self.bump().span;
        let path = self.import_path()?;
        self.expect(Token::Import)?;
        let mut names = vec![self.ident()?];
        while self.peek() == Some(Token::Comma) {
            self.bump();
            names.push(self.ident()?);
        }
        self.eat(Token::Semi);
        Ok(Import { span: start.to(self.prev_span()), path, names: Some(names), alias: None })
    }

    fn import_path(&mut self) -> Result<Vec<Ident>> {
        let mut path = vec![self.ident()?];
        while self.peek() == Some(Token::Dot) && !self.nl() {
            self.bump();
            path.push(self.ident()?);
        }
        Ok(path)
    }

    /// Is the next token the identifier `word` (a keyword only in context)?
    fn at_word(&self, word: &str) -> bool {
        self.peek() == Some(Token::Ident) && !self.nl() && self.text(self.toks[self.pos]) == word
    }

    /// `from` starts an import only as `from <path> import`.
    fn at_from_import(&self) -> bool {
        if self.text(self.toks[self.pos]) != "from" || self.peek_at(1) != Some(Token::Ident) {
            return false;
        }
        let mut i = 2;
        while self.peek_at(i) == Some(Token::Dot) && self.peek_at(i + 1) == Some(Token::Ident) {
            i += 2;
        }
        self.peek_at(i) == Some(Token::Import)
    }

    fn params(&mut self) -> Result<Vec<Param>> {
        self.expect(Token::LParen)?;
        let params = self.scoped(true, false, |p| {
            let mut params = Vec::new();
            while p.peek() != Some(Token::RParen) {
                let name = p.ident()?;
                p.expect(Token::Colon)?;
                let ty = p.type_ref()?;
                let default = if p.eat(Token::Eq) { Some(p.expr()?) } else { None };
                params.push(Param { name, ty, default });
                if !p.eat(Token::Comma) {
                    break;
                }
            }
            Ok(params)
        })?;
        self.expect(Token::RParen)?;
        Ok(params)
    }

    /// Parse after `fn` (and the optional `Type.`): name, signature, body.
    fn fn_decl(&mut self, start: Span, is_extern: bool) -> Result<FnDecl> {
        let name = self.ident()?;
        let type_params = self.type_params()?;
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
        } else if self.eat(Token::Eq) {
            FnBody::Expr(self.expr()?)
        } else if self.peek() == Some(Token::FatArrow) {
            return Err(Diagnostic::new("one-line functions are written with `=`, not `=>`", self.here())
                .hint("e.g. `fn double(x: Int) = x * 2`"));
        } else if self.peek() == Some(Token::LBrace) {
            FnBody::Block(self.block()?)
        } else {
            return Err(self.expected("`{` or `=`"));
        };
        Ok(FnDecl { name, type_params, params, ret, body, span: start.to(self.prev_span()) })
    }

    fn type_decl(&mut self) -> Result<TypeDecl> {
        self.bump(); // `type`
        let name = self.ident()?;
        let type_params = self.type_params()?;
        if self.peek() == Some(Token::Colon) {
            return Err(Diagnostic::new("type inheritance is not supported", self.here()));
        }
        let primary = if self.peek() == Some(Token::LParen) && !self.nl() { Some(self.params()?) } else { None };

        let (mut fields, mut methods, mut ctor, mut inits) = (Vec::new(), Vec::new(), None, Vec::new());
        if self.peek() == Some(Token::LBrace) && !self.nl() {
            self.bump();
            self.scoped(false, false, |p| {
                while p.peek() != Some(Token::RBrace) {
                    match p.peek() {
                        Some(Token::Semi) => {
                            p.bump();
                        }
                        Some(Token::Fn) => {
                            let s = p.bump().span;
                            methods.push(p.fn_decl(s, false)?);
                        }
                        Some(Token::Constructor) => {
                            let s = p.bump().span;
                            let params = p.params()?;
                            let body = p.block()?;
                            if ctor.is_some() {
                                return Err(Diagnostic::new("a type can have only one constructor", s));
                            }
                            ctor = Some(CtorDecl { params, body });
                        }
                        Some(Token::Ident)
                            if p.text(p.toks[p.pos]) == "init" && p.peek_at(1) == Some(Token::LBrace) =>
                        {
                            p.bump();
                            inits.push(p.block()?);
                        }
                        Some(Token::Ident) => {
                            let name = p.ident()?;
                            p.expect(Token::Colon)?;
                            let ty = p.type_ref()?;
                            let init = if p.eat(Token::Eq) { Some(p.expr()?) } else { None };
                            fields.push(FieldDecl { name, ty, init });
                        }
                        _ => return Err(p.expected("a field, `fn`, `constructor` or `init`")),
                    }
                }
                Ok(())
            })?;
            self.expect(Token::RBrace)?;
        }
        Ok(TypeDecl { name, type_params, primary, fields, ctor, inits, methods })
    }

    // ── statements ──────────────────────────────────────────────

    fn block(&mut self) -> Result<Vec<Stmt>> {
        self.expect(Token::LBrace)?;
        let stmts = self.scoped(false, false, |p| {
            let mut stmts = Vec::new();
            loop {
                match p.peek() {
                    Some(Token::RBrace) => return Ok(stmts),
                    Some(Token::Semi) => {
                        p.bump();
                    }
                    None => return Err(p.expected("`}`")),
                    Some(_) => stmts.push(p.stmt()?),
                }
            }
        })?;
        self.expect(Token::RBrace)?;
        Ok(stmts)
    }

    fn stmt(&mut self) -> Result<Stmt> {
        let start = self.here();
        let kind = match self.peek() {
            Some(Token::Return) => {
                self.bump();
                let value = match self.peek() {
                    None | Some(Token::RBrace | Token::Semi) => None,
                    _ if self.nl() => None,
                    _ => Some(self.expr()?),
                };
                StmtKind::Return(value)
            }
            Some(Token::Defer) => {
                self.bump();
                // `defer { … }` is a block of statements; otherwise one statement.
                let body = if self.peek() == Some(Token::LBrace) { self.block()? } else { vec![self.stmt()?] };
                StmtKind::Defer(body)
            }
            Some(Token::While) => {
                self.bump();
                let cond = self.header_expr()?;
                StmtKind::While { cond, body: self.block()? }
            }
            Some(Token::For) => {
                self.bump();
                let var = self.ident()?;
                self.expect(Token::In)?;
                let iter = self.header_expr()?;
                StmtKind::For { var, iter, body: self.block()? }
            }
            Some(Token::Break) => {
                self.bump();
                StmtKind::Break
            }
            Some(Token::Continue) => {
                self.bump();
                StmtKind::Continue
            }
            Some(Token::Ident) if matches!(self.peek_at(1), Some(Token::Colon | Token::ColonEq)) => {
                let name = self.ident()?;
                let ty = if self.eat(Token::Colon) { Some(self.type_ref()?) } else { None };
                // `x := v`, `x: T := v`, and `x: T = v` (the type already marks a declaration).
                let init = if self.eat(Token::ColonEq) || (ty.is_some() && self.eat(Token::Eq)) {
                    Some(self.expr()?)
                } else {
                    None
                };
                StmtKind::Let { name, ty, init }
            }
            _ => {
                let lhs = self.expr()?;
                let op = match self.peek() {
                    _ if self.nl() => None,
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
                        if !matches!(lhs.kind, ExprKind::Name(_) | ExprKind::Field(..) | ExprKind::Index { .. }) {
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

    fn expr_stmt(e: Expr) -> Stmt {
        Stmt { span: e.span, kind: StmtKind::Expr(e) }
    }

    // ── expressions ─────────────────────────────────────────────

    fn expr(&mut self) -> Result<Expr> {
        let lhs = self.binary(1)?;
        if matches!(self.peek(), Some(Token::DotDot | Token::DotDotEq)) && !self.nl() {
            let inclusive = self.bump().kind == Token::DotDotEq;
            let end = self.binary(1)?;
            let span = lhs.span.to(end.span);
            return Ok(Expr { kind: ExprKind::Range { start: Box::new(lhs), end: Box::new(end), inclusive }, span });
        }
        Ok(lhs)
    }

    fn binary(&mut self, min_prec: u8) -> Result<Expr> {
        let mut lhs = self.unary()?;
        while let Some(tok) = self.peek() {
            // A line break ends the expression, except before `&&` / `||` / `?:`.
            if self.nl() && !matches!(tok, Token::AndAnd | Token::OrOr | Token::Elvis) {
                break;
            }
            if tok == Token::Elvis {
                if 5 < min_prec {
                    break;
                }
                self.bump();
                let rhs = self.binary(5)?; // right-associative
                let span = lhs.span.to(rhs.span);
                lhs = Expr { kind: ExprKind::Elvis(Box::new(lhs), Box::new(rhs)), span };
                continue;
            }
            if tok == Token::In {
                if 4 < min_prec {
                    break;
                }
                self.bump();
                let lo = self.binary(5)?;
                if matches!(self.peek(), Some(Token::DotDot | Token::DotDotEq)) {
                    let inclusive = self.bump().kind == Token::DotDotEq;
                    let hi = self.binary(5)?;
                    let span = lo.span.to(hi.span);
                    let range = Expr { kind: ExprKind::Range { start: Box::new(lo), end: Box::new(hi), inclusive }, span };
                    lhs = Self::in_range(lhs, range);
                } else {
                    lhs = Self::contains_call(lo, lhs);
                }
                continue;
            }
            let Some((prec, op)) = binop(tok) else { break };
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

    /// `x in collection`  ⇒  `collection.contains(x)`
    fn contains_call(collection: Expr, item: Expr) -> Expr {
        let span = item.span.to(collection.span);
        let member = Ident { name: "contains".into(), span: collection.span };
        let callee = Expr { kind: ExprKind::Field(Box::new(collection), member), span };
        let args = vec![Arg { name: None, value: item }];
        Expr { kind: ExprKind::Call { callee: Box::new(callee), type_args: Vec::new(), args }, span }
    }

    /// `x in a..b`  ⇒  `{ $t := x;  $t >= a && $t < b }`
    fn in_range(subject: Expr, range: Expr) -> Expr {
        let span = subject.span.to(range.span);
        let tmp = format!("$in{}", span.start);
        let test = Self::range_test(&tmp, subject.span, range);
        let bind = Stmt {
            kind: StmtKind::Let { name: Ident { name: tmp, span }, ty: None, init: Some(subject) },
            span,
        };
        Expr { kind: ExprKind::Block(vec![bind, Self::expr_stmt(test)]), span }
    }

    /// `name >= start && name < end` (or `<=` when inclusive).
    fn range_test(name: &str, subject_span: Span, range: Expr) -> Expr {
        let span = subject_span.to(range.span);
        let ExprKind::Range { start, end, inclusive } = range.kind else { unreachable!("checked by callers") };
        let var = || Expr { kind: ExprKind::Name(name.to_string()), span: subject_span };
        let cmp = |op, l: Expr, r: Box<Expr>| {
            let span = l.span.to(r.span);
            Expr { kind: ExprKind::Binary(op, Box::new(l), r), span }
        };
        let lower = cmp(BinOp::Ge, var(), start);
        let upper = cmp(if inclusive { BinOp::Le } else { BinOp::Lt }, var(), end);
        Expr { kind: ExprKind::Binary(BinOp::And, Box::new(lower), Box::new(upper)), span }
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
                // A leading `.` continues the chain from the previous line.
                Some(Token::Dot) => {
                    self.bump();
                    let field = self.ident()?;
                    let span = e.span.to(field.span);
                    e = Expr { kind: ExprKind::Field(Box::new(e), field), span };
                }
                Some(Token::QuestionDot) => {
                    self.bump();
                    let field = self.ident()?;
                    let span = e.span.to(field.span);
                    e = Expr { kind: ExprKind::SafeField(Box::new(e), field), span };
                }
                // `x!!` — two adjacent `!`s, so `!!x` (double negation) still parses.
                Some(Token::Bang)
                    if !self.nl()
                        && self.peek_at(1) == Some(Token::Bang)
                        && self.toks[self.pos].span.end == self.toks[self.pos + 1].span.start =>
                {
                    self.bump();
                    let end = self.bump().span;
                    let span = e.span.to(end);
                    e = Expr { kind: ExprKind::NotNull(Box::new(e)), span };
                }
                Some(Token::LParen) if !self.nl() => {
                    self.bump();
                    let args = self.scoped(true, false, |p| p.args())?;
                    let close = self.expect(Token::RParen)?;
                    let span = e.span.to(close.span);
                    e = Expr { kind: ExprKind::Call { callee: Box::new(e), type_args: Vec::new(), args }, span };
                }
                Some(Token::LBracket) if !self.nl() => {
                    self.bump();
                    let index = self.scoped(true, false, |p| p.expr())?;
                    let close = self.expect(Token::RBracket)?;
                    let span = e.span.to(close.span);
                    e = Expr { kind: ExprKind::Index { obj: Box::new(e), index: Box::new(index) }, span };
                }
                // `List<Int>(…)`, `identity<Int>(3)`; otherwise `<` is a comparison.
                Some(Token::Lt) if !self.nl() && matches!(e.kind, ExprKind::Name(_) | ExprKind::Field(..)) => {
                    let save = self.pos;
                    match self.try_type_args() {
                        Some(type_args) if self.peek() == Some(Token::LParen) && !self.nl() => {
                            self.bump();
                            let args = self.scoped(true, false, |p| p.args())?;
                            let close = self.expect(Token::RParen)?;
                            let span = e.span.to(close.span);
                            e = Expr { kind: ExprKind::Call { callee: Box::new(e), type_args, args }, span };
                        }
                        _ => {
                            self.pos = save;
                            return Ok(e);
                        }
                    }
                }
                // A trailing lambda: `repeat(3) { … }`, `run { … }`, `xs.each { … }`.
                Some(Token::LBrace)
                    if !self.nl()
                        && !self.no_brace
                        && matches!(
                            e.kind,
                            ExprKind::Name(_)
                                | ExprKind::Field(..)
                                | ExprKind::SafeField(..)
                                | ExprKind::Call { .. }
                                | ExprKind::Index { .. }
                        ) =>
                {
                    let lambda = self.lambda()?;
                    let span = e.span.to(lambda.span);
                    let arg = Arg { name: None, value: lambda };
                    e = match e.kind {
                        ExprKind::Call { callee, type_args, mut args } => {
                            args.push(arg);
                            Expr { kind: ExprKind::Call { callee, type_args, args }, span }
                        }
                        kind => Expr {
                            kind: ExprKind::Call {
                                callee: Box::new(Expr { kind, span: e.span }),
                                type_args: Vec::new(),
                                args: vec![arg],
                            },
                            span,
                        },
                    };
                }
                _ => return Ok(e),
            }
        }
    }

    /// Call arguments up to (not including) the closing `)`.
    fn args(&mut self) -> Result<Vec<Arg>> {
        let mut args = Vec::new();
        while self.peek() != Some(Token::RParen) {
            let name = if self.peek() == Some(Token::Ident) && self.peek_at(1) == Some(Token::Colon) {
                let n = self.ident()?;
                self.bump();
                Some(n)
            } else {
                None
            };
            args.push(Arg { name, value: self.expr()? });
            if !self.eat(Token::Comma) {
                break;
            }
        }
        Ok(args)
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
            Token::Null => {
                self.bump();
                ExprKind::Null
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
                let inner = self.scoped(true, false, |p| p.expr())?;
                self.expect(Token::RParen)?;
                return Ok(inner);
            }
            Token::If => return self.if_expr(),
            Token::Try => return self.try_expr(),
            Token::When => return self.when_expr(),
            Token::LBrace => return self.brace_expr(),
            Token::LBracket => {
                let start = self.bump().span;
                let items = self.scoped(true, false, |p| {
                    let mut items = Vec::new();
                    while p.peek() != Some(Token::RBracket) {
                        items.push(p.expr()?);
                        if !p.eat(Token::Comma) {
                            break;
                        }
                    }
                    Ok(items)
                })?;
                let end = self.expect(Token::RBracket)?.span;
                return Ok(Expr { kind: ExprKind::List(items), span: start.to(end) });
            }
            _ => return Err(self.expected("an expression")),
        };
        Ok(Expr { kind, span })
    }

    /// `{ … }` in expression position: a lambda, or a map literal `{ "a": 1 }`.
    fn brace_expr(&mut self) -> Result<Expr> {
        let save = self.pos;
        self.bump();
        // `{ x -> … }` and `{ x: Int -> … }` are lambdas.
        let is_lambda = self.peek() == Some(Token::RBrace) || self.lambda_params().is_some();
        self.pos = save + 1;
        if !is_lambda && let Ok(entries) = self.try_map_entries() {
            let end = self.expect(Token::RBrace)?.span;
            return Ok(Expr { kind: ExprKind::Map(entries), span: self.toks[save].span.to(end) });
        }
        self.pos = save;
        self.lambda()
    }

    /// `key: value, …` up to (not including) the closing brace.
    fn try_map_entries(&mut self) -> Result<Vec<(Expr, Expr)>> {
        let start = self.pos;
        let result = self.scoped(true, false, |p| {
            let mut entries = Vec::new();
            while p.peek() != Some(Token::RBrace) {
                let key = p.expr()?;
                p.expect(Token::Colon)?;
                entries.push((key, p.expr()?));
                if !p.eat(Token::Comma) {
                    break;
                }
            }
            if entries.is_empty() || p.peek() != Some(Token::RBrace) {
                return Err(p.expected("`}`"));
            }
            Ok(entries)
        });
        if result.is_err() {
            self.pos = start;
        }
        result
    }

    /// `{ params -> body }` or `{ body }`.
    fn lambda(&mut self) -> Result<Expr> {
        let start = self.expect(Token::LBrace)?.span;
        let (params, explicit, body) = self.scoped(false, false, |p| {
            let save = p.pos;
            let (params, explicit) = match p.lambda_params() {
                Some(params) => (params, true),
                None => {
                    p.pos = save;
                    (Vec::new(), false)
                }
            };
            let mut body = Vec::new();
            loop {
                match p.peek() {
                    Some(Token::RBrace) => break,
                    Some(Token::Semi) => {
                        p.bump();
                    }
                    None => return Err(p.expected("`}`")),
                    Some(_) => body.push(p.stmt()?),
                }
            }
            Ok((params, explicit, body))
        })?;
        let end = self.expect(Token::RBrace)?.span;
        Ok(Expr { kind: ExprKind::Lambda { params, explicit, body }, span: start.to(end) })
    }

    /// `a, b: Int ->`, or just `->`.  `None` if this isn't a parameter list.
    fn lambda_params(&mut self) -> Option<Vec<LambdaParam>> {
        let mut params = Vec::new();
        if self.eat(Token::Arrow) {
            return Some(params);
        }
        loop {
            let name = self.ident().ok()?;
            let ty = if self.eat(Token::Colon) { Some(self.type_ref().ok()?) } else { None };
            params.push(LambdaParam { name, ty });
            if self.eat(Token::Comma) {
                continue;
            }
            return self.eat(Token::Arrow).then_some(params);
        }
    }

    /// `try { body } catch e { handler }`; the name is optional (then the message is `it`).
    fn try_expr(&mut self) -> Result<Expr> {
        let start = self.bump().span;
        let body = self.block()?;
        if self.peek() != Some(Token::Catch) {
            return Err(self.expected("`catch` after the `try` block"));
        }
        self.bump();
        let catch_name = if self.peek() == Some(Token::Ident) { Some(self.ident()?) } else { None };
        let handler = self.block()?;
        let span = start.to(self.prev_span());
        Ok(Expr { kind: ExprKind::Try { body, catch_name, handler }, span })
    }

    fn if_expr(&mut self) -> Result<Expr> {
        let start = self.bump().span;
        let cond = self.header_expr()?;
        let then = self.block()?;
        let otherwise = if self.eat(Token::Else) {
            if self.peek() == Some(Token::If) {
                Some(vec![Self::expr_stmt(self.if_expr()?)])
            } else {
                Some(self.block()?)
            }
        } else {
            None
        };
        let span = start.to(self.prev_span());
        Ok(Expr { kind: ExprKind::If { cond: Box::new(cond), then, otherwise }, span })
    }

    /// `when` is sugar for an if/else-if chain:
    ///
    /// ```text
    /// when x { 1, 2 -> a;  in 3..9 -> b;  else -> c }
    ///   ⇒  { $w := x;  if $w == 1 || $w == 2 { a } else if $w >= 3 && $w < 9 { b } else { c } }
    /// ```
    fn when_expr(&mut self) -> Result<Expr> {
        let start = self.bump().span;
        let subject = if self.peek() == Some(Token::LBrace) { None } else { Some(self.header_expr()?) };
        self.expect(Token::LBrace)?;

        let tmp = format!("$w{}", start.start);
        let subject_span = subject.as_ref().map(|s| s.span);
        let mut arms: Vec<(Expr, Vec<Stmt>)> = Vec::new();
        let mut fallback: Option<Vec<Stmt>> = None;

        self.scoped(false, false, |p| {
            loop {
                match p.peek() {
                    Some(Token::Semi) => {
                        p.bump();
                        continue;
                    }
                    Some(Token::RBrace) => return Ok(()),
                    None => return Err(p.expected("`}`")),
                    _ => {}
                }
                if p.peek() == Some(Token::Else) {
                    p.bump();
                    p.expect(Token::Arrow)?;
                    fallback = Some(p.arm_body()?);
                    continue;
                }
                let mut cond: Option<Expr> = None;
                loop {
                    let c = if p.peek() == Some(Token::In) {
                        let at = p.bump().span;
                        let Some(subject_span) = subject_span else {
                            return Err(Diagnostic::new("`in` arms need a `when <value>`", at));
                        };
                        let range = p.expr()?;
                        if matches!(range.kind, ExprKind::Range { .. }) {
                            Self::range_test(&tmp, subject_span, range)
                        } else {
                            let subj = Expr { kind: ExprKind::Name(tmp.clone()), span: subject_span };
                            Self::contains_call(range, subj)
                        }
                    } else {
                        let e = p.expr()?;
                        match subject_span {
                            Some(s) => {
                                let span = e.span;
                                let subj = Expr { kind: ExprKind::Name(tmp.clone()), span: s };
                                Expr { kind: ExprKind::Binary(BinOp::Eq, Box::new(subj), Box::new(e)), span }
                            }
                            None => e,
                        }
                    };
                    cond = Some(match cond {
                        None => c,
                        Some(prev) => {
                            let span = prev.span.to(c.span);
                            Expr { kind: ExprKind::Binary(BinOp::Or, Box::new(prev), Box::new(c)), span }
                        }
                    });
                    if !p.eat(Token::Comma) {
                        break;
                    }
                }
                p.expect(Token::Arrow)?;
                arms.push((cond.unwrap(), p.arm_body()?));
            }
        })?;
        let end = self.expect(Token::RBrace)?.span;
        let span = start.to(end);

        if arms.is_empty() && fallback.is_none() {
            return Err(Diagnostic::new("`when` needs at least one branch", span));
        }
        // Fold the arms into `if … else if … else …`.
        let mut top = match fallback {
            Some(body) => Expr { kind: ExprKind::Block(body), span },
            None => {
                // No `else`: the innermost arm is an `if` without one.
                let (cond, body) = arms.pop().expect("checked above");
                Expr { span: cond.span, kind: ExprKind::If { cond: Box::new(cond), then: body, otherwise: None } }
            }
        };
        for (cond, body) in arms.into_iter().rev() {
            let otherwise = Some(vec![Self::expr_stmt(top)]);
            top = Expr { span: cond.span, kind: ExprKind::If { cond: Box::new(cond), then: body, otherwise } };
        }
        top.span = span;

        match subject {
            None => Ok(top),
            Some(subject) => {
                let bind = Stmt {
                    kind: StmtKind::Let { name: Ident { name: tmp, span }, ty: None, init: Some(subject) },
                    span,
                };
                Ok(Expr { kind: ExprKind::Block(vec![bind, Self::expr_stmt(top)]), span })
            }
        }
    }

    /// The right-hand side of a `when` arm: a block or a single expression.
    fn arm_body(&mut self) -> Result<Vec<Stmt>> {
        if self.peek() == Some(Token::LBrace) { self.block() } else { Ok(vec![Self::expr_stmt(self.expr()?)]) }
    }

    /// Decode escapes and split `${...}` / `$name` interpolations out of a string literal.
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
                b'$' if bytes.get(i + 1).is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_') => {
                    let mut end = i + 1;
                    while bytes.get(end).is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_') {
                        end += 1;
                    }
                    if !lit.is_empty() {
                        parts.push(InterpPart::Lit(std::mem::take(&mut lit)));
                    }
                    let span = Span::new(self.file, raw_off + i + 1, raw_off + end);
                    parts.push(InterpPart::Expr(Expr { kind: ExprKind::Name(raw[i + 1..end].to_string()), span }));
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
        // 5 is the elvis operator, handled in `binary`
        Token::Plus => (6, BinOp::Add),
        Token::Minus => (6, BinOp::Sub),
        Token::Star => (7, BinOp::Mul),
        Token::Slash => (7, BinOp::Div),
        Token::Percent => (7, BinOp::Rem),
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr(src: &str) -> Expr {
        Parser::new(0, src, 0).unwrap().expr().unwrap()
    }

    fn module(src: &str) -> Module {
        parse_module(0, src).unwrap()
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
            ExprKind::Call { callee, args, .. } => {
                let args: Vec<String> = args
                    .iter()
                    .map(|a| match &a.name {
                        Some(n) => format!("{}: {}", n.name, show(&a.value)),
                        None => show(&a.value),
                    })
                    .collect();
                format!("{}({})", show(callee), args.join(", "))
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
    fn postfix_chains_and_named_arguments() {
        assert_eq!(show(&parse_expr("a.b.c(1, x.y)")), "a.b.c(1, x.y)");
        assert_eq!(show(&parse_expr("m.f(1).g()")), "m.f(1).g()");
        assert_eq!(show(&parse_expr("f(1, flag: true, n: 2 + 3)")), "f(1, flag: Bool(true), n: (2 + 3))");
    }

    #[test]
    fn a_line_break_ends_the_expression() {
        // `-3` and `(b)` on their own lines are new statements.
        let m = module("a := 1\n-3\nb := a\n(b)\n");
        assert_eq!(m.items.len(), 4);
        // …but `.`, `&&` and a trailing operator continue it.
        let m = module("r := x\n    .add(1)\n    .add(2)\nok := a\n    && b\nsum := 1 +\n    2\n");
        assert_eq!(m.items.len(), 3);
        // Newlines are ignored inside parentheses.
        let m = module("f(1,\n  2\n)\n");
        assert_eq!(m.items.len(), 1);
    }

    #[test]
    fn top_level_statements_are_items() {
        let m = module("import math\nx := 1\nprintln(x)\nfn f() {}\nif x > 0 { println(1) }\n");
        assert_eq!(m.imports.len(), 1);
        let kinds: Vec<&str> = m
            .items
            .iter()
            .map(|i| match i {
                Item::Stmt(_) => "stmt",
                Item::Fn(_) => "fn",
                _ => "other",
            })
            .collect();
        assert_eq!(kinds, ["stmt", "stmt", "fn", "stmt"]);
    }

    #[test]
    fn interpolation_splits_parts_and_keeps_offsets() {
        let src = r#"  "hi ${name}, ${1 + 2}! and $other." "#;
        let e = parse_expr(src);
        let ExprKind::Interp(parts) = &e.kind else { panic!("expected interpolation") };
        assert_eq!(parts.len(), 7);
        let slice = |e: &Expr| &src[e.span.start as usize..e.span.end as usize];
        // Spans point into the original text, not the substring.
        let InterpPart::Expr(name) = &parts[1] else { panic!() };
        assert_eq!(slice(name), "name");
        let InterpPart::Expr(sum) = &parts[3] else { panic!() };
        assert_eq!(slice(sum), "1 + 2");
        let InterpPart::Expr(other) = &parts[5] else { panic!() };
        assert_eq!(slice(other), "other");
        assert!(matches!(&parts[6], InterpPart::Lit(s) if s == "."));
    }

    #[test]
    fn dollar_without_a_name_is_literal() {
        let ExprKind::Str(s) = parse_expr(r#""cost: $5 or $ ""#).kind else { panic!() };
        assert_eq!(s, "cost: $5 or $ ");
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
    fn when_desugars_to_an_if_chain() {
        let e = parse_expr("when x {\n 1, 2 -> a\n in 3..9 -> b\n else -> c\n}");
        // { $w := x; if $w == 1 || $w == 2 { a } else if ($w >= 3 && $w < 9) { b } else { c } }
        let ExprKind::Block(stmts) = &e.kind else { panic!("expected a block, got {e:?}") };
        assert!(matches!(&stmts[0].kind, StmtKind::Let { .. }));
        let StmtKind::Expr(chain) = &stmts[1].kind else { panic!() };
        let ExprKind::If { cond, otherwise, .. } = &chain.kind else { panic!() };
        assert!(matches!(&cond.kind, ExprKind::Binary(BinOp::Or, ..)));
        let StmtKind::Expr(second) = &otherwise.as_ref().unwrap()[0].kind else { panic!() };
        assert!(matches!(&second.kind, ExprKind::If { .. }));
    }

    #[test]
    fn when_without_a_subject_uses_the_conditions_directly() {
        let e = parse_expr("when {\n a > 1 -> 1\n else -> 2\n}");
        assert!(matches!(&e.kind, ExprKind::If { .. }), "{e:?}");
    }

    #[test]
    fn x_in_range_is_sugar_for_two_comparisons() {
        let e = parse_expr("x in 1..=5");
        let ExprKind::Block(stmts) = &e.kind else { panic!() };
        let StmtKind::Expr(test) = &stmts[1].kind else { panic!() };
        assert!(matches!(&test.kind, ExprKind::Binary(BinOp::And, ..)));
    }

    #[test]
    fn type_declarations() {
        let m = module(
            "type User(name: String, age: Int = 0) {\n  tag: Int = 3\n  init { x = 1 }\n  fn hi() = name\n}\n\
             type Plain { a: Int\n constructor(v: Int) { a = v } }\ntype Bare(x: Int)\n\
             fn User.label() = name\n",
        );
        assert_eq!(m.items.len(), 4);
        let Item::Type(user) = &m.items[0] else { panic!() };
        let primary = user.primary.as_ref().unwrap();
        assert_eq!(primary.len(), 2);
        assert!(primary[1].default.is_some());
        assert_eq!((user.fields.len(), user.inits.len(), user.methods.len()), (1, 1, 1));
        let Item::Type(plain) = &m.items[1] else { panic!() };
        assert!(plain.primary.is_none() && plain.ctor.is_some());
        assert!(matches!(&m.items[3], Item::Method { owner, .. } if owner.name == "User"));
    }

    #[test]
    fn functions_and_externs() {
        let m = module("fn a(x: Int, y: Int = 2) -> Int = x + y\nextern fn f(a: Int) -> Int = \"c_f\"\nfn b() { }\n");
        assert_eq!(m.items.len(), 3);
        assert!(matches!(&m.items[0], Item::Fn(f) if matches!(f.body, FnBody::Expr(_)) && f.ret.is_some()));
        assert!(matches!(&m.items[1], Item::Fn(f) if matches!(&f.body, FnBody::Extern { symbol: Some(s) } if s == "c_f")));
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
    fn nullable_syntax() {
        // types
        let m = module("fn f(a: Int?, b: List<String?>?) -> Point? = null\nx: Line? = null\n");
        let Item::Fn(f) = &m.items[0] else { panic!("expected a function") };
        assert!(matches!(&f.params[0].ty.kind, TypeKind::Nullable(t) if matches!(&t.kind, TypeKind::Named { name, .. } if name == "Int")));
        let TypeKind::Nullable(list) = &f.params[1].ty.kind else { panic!("expected a nullable list") };
        let TypeKind::Named { args, .. } = &list.kind else { panic!("expected List<…>") };
        assert!(matches!(&args[0].kind, TypeKind::Nullable(_)));
        assert!(matches!(&f.ret.as_ref().unwrap().kind, TypeKind::Nullable(_)));

        // expressions
        assert!(matches!(parse_expr("null").kind, ExprKind::Null));
        assert!(matches!(parse_expr("a!!").kind, ExprKind::NotNull(_)));
        assert!(matches!(parse_expr("a?.b").kind, ExprKind::SafeField(..)));
        let call = parse_expr("a?.b(1)?.c");
        let ExprKind::SafeField(inner, c) = &call.kind else { panic!("expected ?.c, got {call:?}") };
        assert_eq!(c.name, "c");
        assert!(matches!(&inner.kind, ExprKind::Call { callee, .. } if matches!(callee.kind, ExprKind::SafeField(..))));
        // a trailing lambda after a safe call
        assert!(matches!(parse_expr("a?.each { it }").kind, ExprKind::Call { .. }));
    }

    #[test]
    fn elvis_precedence_and_associativity() {
        // tighter than comparison and `&&`, looser than arithmetic
        let e = parse_expr("a ?: b + 1 == c");
        let ExprKind::Binary(BinOp::Eq, l, _) = &e.kind else { panic!("expected ==, got {e:?}") };
        let ExprKind::Elvis(_, rhs) = &l.kind else { panic!("expected ?:") };
        assert!(matches!(rhs.kind, ExprKind::Binary(BinOp::Add, ..)));
        // right-associative: a ?: (b ?: c)
        let e = parse_expr("a ?: b ?: c");
        let ExprKind::Elvis(_, rhs) = &e.kind else { panic!("expected ?:") };
        assert!(matches!(rhs.kind, ExprKind::Elvis(..)));
        // may continue on the next line
        assert!(matches!(parse_expr("a\n    ?: b").kind, ExprKind::Elvis(..)));
        // `a !! b`-style misuse and double negation still parse as before
        let e = parse_expr("!!a");
        assert!(matches!(&e.kind, ExprKind::Unary(UnOp::Not, inner) if matches!(inner.kind, ExprKind::Unary(UnOp::Not, _))));
        assert!(matches!(parse_expr("a != b").kind, ExprKind::Binary(BinOp::Ne, ..)));
    }

    #[test]
    fn defer_and_try_syntax() {
        let m = module("fn f() {\n    defer cleanup()\n    defer { a()\n b() }\n}\n");
        let Item::Fn(f) = &m.items[0] else { panic!("expected a function") };
        let FnBody::Block(body) = &f.body else { panic!("expected a block body") };
        assert!(matches!(&body[0].kind, StmtKind::Defer(s) if s.len() == 1));
        assert!(matches!(&body[1].kind, StmtKind::Defer(s) if s.len() == 2));

        let e = parse_expr("try { a() } catch e { b(e) }");
        let ExprKind::Try { body, catch_name, handler } = &e.kind else { panic!("expected try, got {e:?}") };
        assert_eq!((body.len(), handler.len()), (1, 1));
        assert_eq!(catch_name.as_ref().unwrap().name, "e");
        // the name is optional, and `catch` may start a line
        let e = parse_expr("try { a() }\ncatch { b(it) }");
        assert!(matches!(&e.kind, ExprKind::Try { catch_name: None, .. }));
        // `try` is an ordinary expression
        assert!(matches!(parse_expr("1 + try { 2 } catch { 3 }").kind, ExprKind::Binary(..)));
    }

    #[test]
    fn in_collection_becomes_a_contains_call() {
        let e = parse_expr("x in items");
        let ExprKind::Call { callee, args, .. } = &e.kind else { panic!("expected a call, got {e:?}") };
        assert!(matches!(&callee.kind, ExprKind::Field(obj, m) if m.name == "contains" && matches!(&obj.kind, ExprKind::Name(n) if n == "items")));
        assert!(matches!(&args[0].value.kind, ExprKind::Name(n) if n == "x"));
        // …while a range keeps its comparison form.
        assert!(matches!(parse_expr("x in 1..5").kind, ExprKind::Block(_)));
    }

    fn lambda_of(e: &Expr) -> (usize, bool, usize) {
        let ExprKind::Lambda { params, explicit, body } = &e.kind else { panic!("expected a lambda, got {e:?}") };
        (params.len(), *explicit, body.len())
    }

    #[test]
    fn lambda_forms() {
        assert_eq!(lambda_of(&parse_expr("{ it + 1 }")), (0, false, 1));
        assert_eq!(lambda_of(&parse_expr("{ x -> x }")), (1, true, 1));
        assert_eq!(lambda_of(&parse_expr("{ a, b: Int -> a }")), (2, true, 1));
        assert_eq!(lambda_of(&parse_expr("{ -> 1 }")), (0, true, 1));
        assert_eq!(lambda_of(&parse_expr("{ n += 1; n }")), (0, false, 2));
        assert_eq!(lambda_of(&parse_expr("{ f: (Int) -> Int, x: Int -> f(x) }")), (2, true, 1));
    }

    #[test]
    fn trailing_lambdas_join_the_call() {
        let args = |src: &str| {
            let ExprKind::Call { args, .. } = parse_expr(src).kind else { panic!("not a call") };
            args.len()
        };
        assert_eq!(args("f(1) { it }"), 2);
        assert_eq!(args("run { 1 }"), 1);
        assert_eq!(args("xs.each { it }"), 1);
        assert_eq!(args("f(1, 2) { it }"), 3);
    }

    #[test]
    fn a_brace_after_a_header_expression_is_the_body_not_a_lambda() {
        // `if f(x) { … }` — the block belongs to the `if`.
        let m = module("if f(x) { g() }
while ok(1) { step() }
for i in 0..n(3) { p(i) }
");
        assert_eq!(m.items.len(), 3);
        let Item::Stmt(s) = &m.items[0] else { panic!() };
        let StmtKind::Expr(e) = &s.kind else { panic!() };
        let ExprKind::If { cond, then, .. } = &e.kind else { panic!("{e:?}") };
        assert!(matches!(&cond.kind, ExprKind::Call { args, .. } if args.len() == 1));
        assert_eq!(then.len(), 1);
    }

    #[test]
    fn a_brace_on_a_new_line_is_not_a_trailing_lambda() {
        let m = module("f(1)
{ it }
");
        assert_eq!(m.items.len(), 2);
    }

    #[test]
    fn function_types() {
        let m = module("fn f(g: (Int, String) -> Bool, h: () -> Unit) -> (Int) -> Int = x\n");
        let Item::Fn(f) = &m.items[0] else { panic!() };
        assert!(matches!(&f.params[0].ty.kind, TypeKind::Fn { params, .. } if params.len() == 2));
        assert!(matches!(&f.params[1].ty.kind, TypeKind::Fn { params, .. } if params.is_empty()));
        assert!(matches!(&f.ret.as_ref().unwrap().kind, TypeKind::Fn { .. }));
    }

    #[test]
    fn a_shebang_line_is_ignored() {
        let m = module("#!/usr/bin/env aion\nprintln(1)\n");
        assert_eq!(m.items.len(), 1);
    }
}
