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

/// An `enum` variant as written: `Red` or `Earth(5.97e24)`.
struct Variant {
    name: Ident,
    args: Vec<Arg>,
}

/// The words that begin a declaration, for suggesting a correction to a misspelled one.
const DECLARATION_WORDS: [&str; 15] = [
    "fn", "type", "data", "open", "static", "extern", "enum", "interface", "import", "from", "val", "synonym", "syn",
    "typealias", "constructor",
];

/// How many single-character insertions, deletions or replacements turn `a` into `b`.
fn edit_distance(a: &str, b: &str) -> usize {
    let b: Vec<char> = b.chars().collect();
    let mut row: Vec<usize> = (0..=b.len()).collect();
    for (i, ca) in a.chars().enumerate() {
        let mut diagonal = row[0];
        row[0] = i + 1;
        for (j, &cb) in b.iter().enumerate() {
            let above = row[j + 1];
            row[j + 1] = (diagonal + usize::from(ca != cb)).min(above + 1).min(row[j] + 1);
            diagonal = above;
        }
    }
    row[b.len()]
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
    /// How many temporaries the parser has introduced, to keep their names apart.
    temps: usize,
    /// After `is` / `as`: a `?` followed by an expression is the conditional operator, not `T?`.
    question_is_ternary: bool,
}

impl<'a> Parser<'a> {
    fn new(file: u32, src: &'a str, base: usize) -> Result<Self> {
        Ok(Parser { file, src, base, toks: lex(file, src, base)?, pos: 0, ignore_nl: false, no_brace: false, temps: 0, question_is_ternary: false })
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

    /// A type that may be followed by `?`.
    fn nullable(&mut self, ty: TypeRef) -> TypeRef {
        if self.peek() == Some(Token::Question) && !(self.question_is_ternary && self.expression_follows_question()) {
            let q = self.bump().span;
            let span = ty.span.to(q);
            return TypeRef { kind: TypeKind::Nullable(Box::new(ty)), span };
        }
        ty
    }

    /// Does the `?` ahead have an expression after it, on the same line, as in `x is Dog ? 1 : 2`?
    fn expression_follows_question(&self) -> bool {
        let Some(next) = self.toks.get(self.pos + 1) else { return false };
        !next.nl
            && matches!(
                next.kind,
                Token::Int
                    | Token::Float
                    | Token::Char
                    | Token::Str
                    | Token::RawStr
                    | Token::Ident
                    | Token::LParen
                    | Token::LBracket
                    | Token::Minus
                    | Token::Bang
                    | Token::True
                    | Token::False
                    | Token::Null
                    | Token::If
                    | Token::When
            )
    }

    /// The type after `is` or `as`.
    fn type_after_is(&mut self) -> Result<TypeRef> {
        let saved = std::mem::replace(&mut self.question_is_ternary, true);
        let ty = self.type_ref();
        self.question_is_ternary = saved;
        ty
    }

    fn type_ref(&mut self) -> Result<TypeRef> {
        // `(A, B) -> R`, a tuple `(A, B)`, or a type in parentheses `((A) -> B)?`
        if self.peek() == Some(Token::LParen) {
            let start = self.bump().span;
            let mut items = Vec::new();
            while self.peek() != Some(Token::RParen) {
                items.push(self.type_ref()?);
                if !self.eat(Token::Comma) {
                    break;
                }
            }
            let close = self.expect(Token::RParen)?.span;
            if self.eat(Token::Arrow) {
                let ret = self.type_ref()?;
                let span = start.to(ret.span);
                return Ok(TypeRef { kind: TypeKind::Fn { params: items, ret: Box::new(ret) }, span });
            }
            let span = start.to(close);
            let ty = match items.len() {
                0 => return Err(Diagnostic::new("expected `->` after the parameter types", close)),
                1 => items.pop().expect("one item"),
                2..=5 => TypeRef { kind: TypeKind::Named { name: format!("Tuple{}", items.len()), args: items }, span },
                _ => return Err(Diagnostic::new("a tuple has at most 5 elements", span).hint("use a type with named fields")),
            };
            return Ok(self.nullable(ty));
        }
        let id = self.ident()?;
        let mut span = id.span;
        // `module.Type`
        let mut name = id.name;
        while self.peek() == Some(Token::Dot) && self.peek_at(1) == Some(Token::Ident) && !self.nl() {
            self.bump();
            let part = self.ident()?;
            name.push('.');
            name.push_str(&part.name);
            span = span.to(part.span);
        }
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
        let named = TypeRef { kind: TypeKind::Named { name, args }, span };
        Ok(self.nullable(named))
    }

    /// `<T, R>` after a declaration's name (empty if absent).
    fn type_params(&mut self) -> Result<Vec<Ident>> {
        Ok(self.bounded_type_params()?.0)
    }

    /// `<T, R: Shape>`: the names, and the interface each must satisfy.
    fn bounded_type_params(&mut self) -> Result<(Vec<Ident>, Vec<Option<TypeRef>>)> {
        let (mut params, mut bounds) = (Vec::new(), Vec::new());
        if self.peek() == Some(Token::Lt) {
            self.bump();
            loop {
                params.push(self.ident()?);
                bounds.push(if self.eat(Token::Colon) { Some(self.type_ref()?) } else { None });
                if !self.eat(Token::Comma) {
                    break;
                }
            }
            self.expect(Token::Gt)?;
        }
        Ok((params, bounds))
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
        // The variables holding enum variants run before any other top-level code.
        let mut variables: Vec<Stmt> = Vec::new();
        while let Some(tok) = self.peek() {
            match tok {
                Token::Semi => {
                    self.bump();
                }
                Token::Import => imports.push(self.import()?),
                Token::Ident if self.at_from_import() => imports.push(self.import_from()?),
                Token::Type => items.push(Item::Type(self.type_decl()?)),
                Token::Ident if self.text(self.toks[self.pos]) == "open" && self.peek_at(1) == Some(Token::Type) => {
                    self.bump();
                    let mut decl = self.type_decl()?;
                    decl.open = true;
                    items.push(Item::Type(decl));
                }
                // `data type P(x: Int, y: Int)`: equals, hashCode, copy and componentN come with it
                Token::Ident if self.text(self.toks[self.pos]) == "data" && self.peek_at(1) == Some(Token::Type) => {
                    self.bump();
                    let mut decl = self.type_decl()?;
                    Self::data_members(&mut decl)?;
                    items.push(Item::Type(decl));
                }
                // `enum Color { Red, Green }`: see [`Parser::enum_decl`]
                Token::Ident if self.text(self.toks[self.pos]) == "enum" && self.peek_at(1) == Some(Token::Ident) => {
                    let (decl, lets) = self.enum_decl()?;
                    items.push(Item::Type(decl));
                    variables.extend(lets);
                }
                Token::Interface => items.push(Item::Interface(self.interface_decl()?)),
                // `synonym Scope<V> = Map<String, V>`; `syn` and `typealias` mean the same
                Token::Ident if self.at_alias() => items.push(Item::Alias(self.alias_decl()?)),
                Token::Extern => {
                    let start = self.bump().span;
                    self.expect(Token::Fn)?;
                    items.push(self.fn_item(start, true)?);
                }
                Token::Fn => {
                    let start = self.bump().span;
                    items.push(self.fn_item(start, false)?);
                }
                // `static fn Type.make() = …`: a function that belongs to a type
                Token::Ident if self.text(self.toks[self.pos]) == "static" && self.peek_at(1) == Some(Token::Fn) => {
                    self.bump();
                    let start = self.bump().span;
                    let mut item = self.fn_item(start, false)?;
                    match &mut item {
                        Item::Method { decl, .. } => decl.is_static = true,
                        Item::Fn(decl) => {
                            return Err(Diagnostic::new("a `static fn` written outside a type needs the type's name", decl.name.span)
                                .hint(format!("e.g. `static fn Type.{}(…)`", decl.name.name)));
                        }
                        _ => {}
                    }
                    items.push(item);
                }
                _ => {
                    let mut stmts = Vec::new();
                    self.stmt_into(&mut stmts)?;
                    items.extend(stmts.into_iter().map(Item::Stmt));
                }
            }
        }
        items.splice(0..0, variables.into_iter().map(Item::Stmt));
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
        let (base, path) = self.import_path()?;
        let alias = if self.at_word("as") {
            self.bump();
            Some(self.ident()?)
        } else {
            None
        };
        self.eat(Token::Semi);
        Ok(Import { span: start.to(self.prev_span()), base, path, names: None, alias })
    }

    /// `from a.b import x, y`
    fn import_from(&mut self) -> Result<Import> {
        let start = self.bump().span;
        let (base, path) = self.import_path()?;
        self.expect(Token::Import)?;
        let mut names = vec![self.ident()?];
        while self.peek() == Some(Token::Comma) {
            self.bump();
            names.push(self.ident()?);
        }
        self.eat(Token::Semi);
        Ok(Import { span: start.to(self.prev_span()), base, path, names: Some(names), alias: None })
    }

    /// `a.b`, `@a.b` (from the package root), `.a` / `..a` (relative to this file's folder).
    fn import_path(&mut self) -> Result<(ImportBase, Vec<Ident>)> {
        let base = if self.eat(Token::At) {
            ImportBase::Root
        } else if matches!(self.peek(), Some(Token::Dot | Token::DotDot)) {
            let mut dots = 0;
            loop {
                match self.peek() {
                    Some(Token::Dot) => dots += 1,
                    Some(Token::DotDot) => dots += 2,
                    _ => break,
                }
                self.bump();
            }
            ImportBase::Relative(dots - 1)
        } else {
            ImportBase::Bare
        };
        let mut path = vec![self.ident()?];
        while self.peek() == Some(Token::Dot) && !self.nl() {
            self.bump();
            path.push(self.ident()?);
        }
        Ok((base, path))
    }

    /// Is the next token the identifier `word` (a keyword only in context)?
    fn at_word(&self, word: &str) -> bool {
        self.peek() == Some(Token::Ident) && !self.nl() && self.text(self.toks[self.pos]) == word
    }

    /// `from` starts an import only as `from <path> import`.
    fn at_from_import(&self) -> bool {
        if self.text(self.toks[self.pos]) != "from" {
            return false;
        }
        // `from @a.b import`, `from ..a import`: skip the prefix
        let mut i = 1;
        if self.peek_at(i) == Some(Token::At) {
            i += 1;
        } else {
            while matches!(self.peek_at(i), Some(Token::Dot | Token::DotDot)) {
                i += 1;
            }
        }
        if self.peek_at(i) != Some(Token::Ident) {
            return false;
        }
        i += 1;
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
                // `vararg` is only a keyword in front of `name:`
                let vararg = p.peek() == Some(Token::Ident)
                    && p.text(p.toks[p.pos]) == "vararg"
                    && p.peek_at(1) == Some(Token::Ident)
                    && p.peek_at(2) == Some(Token::Colon);
                if vararg {
                    p.bump();
                }
                let name = p.ident()?;
                p.expect(Token::Colon)?;
                let ty = p.type_ref()?;
                let default = if p.eat(Token::Eq) { Some(p.expr()?) } else { None };
                if vararg && default.is_some() {
                    return Err(Diagnostic::new("a `vararg` parameter has no default value", name.span)
                        .hint("leaving it out gives an empty list"));
                }
                if params.last().is_some_and(|q: &Param| q.vararg) {
                    return Err(Diagnostic::new("`vararg` must be the last parameter", name.span));
                }
                params.push(Param { name, ty, default, vararg });
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
        let (type_params, bounds) = self.bounded_type_params()?;
        let params = self.params()?;
        if let Some(v) = params.iter().find(|p| p.vararg).filter(|_| is_extern) {
            return Err(Diagnostic::new("an `extern` function cannot take a `vararg`", v.name.span));
        }
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
        } else if self.peek() == Some(Token::Colon) {
            return Err(Diagnostic::new("a return type is written with `->`, not `:`", self.here())
                .hint("e.g. `fn double(x: Int) -> Int = x * 2`"));
        } else {
            return Err(self.expected("`{` or `=`"));
        };
        Ok(FnDecl { name, type_params, bounds, params, ret, body, span: start.to(self.prev_span()), modifier: Modifier::None, is_static: false })
    }

    /// `interface Name { fn method(params) -> Type … }`
    fn interface_decl(&mut self) -> Result<InterfaceDecl> {
        self.bump();
        let name = self.ident()?;
        let type_params = self.type_params()?;
        self.expect(Token::LBrace)?;
        let (methods, defaults) = self.scoped(false, false, |p| {
            let mut methods = Vec::new();
            let mut defaults = Vec::new();
            loop {
                match p.peek() {
                    Some(Token::RBrace) => return Ok((methods, defaults)),
                    Some(Token::Semi) => {
                        p.bump();
                    }
                    Some(Token::Fn) => {
                        let after_fn = p.pos + 1;
                        let start = p.bump().span;
                        let name = p.ident()?;
                        let params = p.params()?;
                        if let Some(v) = params.iter().find(|p| p.vararg) {
                            return Err(Diagnostic::new("an interface method cannot take a `vararg`", v.name.span));
                        }
                        let ret = if p.eat(Token::Arrow) { Some(p.type_ref()?) } else { None };
                        if matches!(p.peek(), Some(Token::LBrace | Token::Eq)) && !p.nl() {
                            // a method with a body: read the whole declaration again, as a function
                            p.pos = after_fn;
                            let decl = p.fn_decl(start, false)?;
                            if let Some(q) = decl.params.iter().find(|q| q.default.is_some()) {
                                return Err(Diagnostic::new("interface methods cannot have default arguments", q.name.span));
                            }
                            let params = decl
                                .params
                                .iter()
                                .map(|q| Param { name: q.name.clone(), ty: q.ty.clone(), default: None, vararg: false })
                                .collect();
                            methods.push(MethodSig { name: decl.name.clone(), params, ret: decl.ret.clone(), default: true });
                            defaults.push(decl);
                            continue;
                        }
                        methods.push(MethodSig { name, params, ret, default: false });
                    }
                    _ => return Err(p.expected("`fn` or `}`")),
                }
            }
        })?;
        self.expect(Token::RBrace)?;
        Ok(InterfaceDecl { name, type_params, methods, defaults })
    }

    fn type_decl(&mut self) -> Result<TypeDecl> {
        self.bump(); // `type`
        let name = self.ident()?;
        let (type_params, bounds) = self.bounded_type_params()?;
        let primary = if self.peek() == Some(Token::LParen) && !self.nl() { Some(self.params()?) } else { None };
        if let Some(p) = primary.as_ref().and_then(|ps| ps.iter().find(|p| p.vararg)) {
            return Err(Diagnostic::new("a primary constructor cannot take a `vararg`", p.name.span)
                .hint("declare a constructor: `constructor(vararg xs: Int) { … }`"));
        }
        // `: Parent(args)` — extend another type, handing it its constructor arguments.
        let parent = if self.peek() == Some(Token::Colon) && !self.nl() {
            let colon = self.bump().span;
            let name = self.ident()?;
            let mut type_args = Vec::new();
            if self.peek() == Some(Token::Lt) && !self.nl() {
                self.bump();
                loop {
                    type_args.push(self.type_ref()?);
                    if !self.eat(Token::Comma) {
                        break;
                    }
                }
                self.expect(Token::Gt)?;
            }
            let args = if self.peek() == Some(Token::LParen) && !self.nl() {
                self.bump();
                let args = self.scoped(true, false, |p| p.args())?;
                self.expect(Token::RParen)?;
                args
            } else {
                Vec::new()
            };
            Some(ParentRef { span: colon.to(self.prev_span()), name, type_args, args })
        } else {
            None
        };

        let (mut fields, mut methods, mut ctor, mut inits) = (Vec::new(), Vec::new(), None, Vec::new());
        self.type_body(None, &mut fields, &mut methods, &mut ctor, &mut inits)?;
        Ok(TypeDecl { name, type_params, bounds, primary, fields, ctor, inits, methods, open: false, variants: None, parent })
    }

    /// The `{ … }` of a type: fields, methods, a constructor and `init` blocks.  An `enum` passes
    /// `variants`, which are read first (see [`Parser::enum_decl`]).
    fn type_body(
        &mut self,
        mut variants: Option<&mut Vec<Variant>>,
        fields: &mut Vec<FieldDecl>,
        methods: &mut Vec<FnDecl>,
        ctor: &mut Option<CtorDecl>,
        inits: &mut Vec<Vec<Stmt>>,
    ) -> Result<()> {
        if self.peek() == Some(Token::LBrace) && !self.nl() {
            self.bump();
            self.scoped(false, false, |p| {
                if let Some(list) = variants.as_deref_mut() {
                    while p.peek() == Some(Token::Ident) && !p.member_starts() {
                        let name = p.ident()?;
                        let args = if p.peek() == Some(Token::LParen) && !p.nl() {
                            p.bump();
                            let args = p.scoped(true, false, |p| p.args())?;
                            p.expect(Token::RParen)?;
                            args
                        } else {
                            Vec::new()
                        };
                        list.push(Variant { name, args });
                        if !p.eat(Token::Comma) && p.peek() == Some(Token::Ident) && !p.nl() && !p.member_starts() {
                            return Err(p.expected("`,` between enum variants"));
                        }
                    }
                    p.eat(Token::Semi);
                }
                while p.peek() != Some(Token::RBrace) {
                    match p.peek() {
                        Some(Token::Semi) => {
                            p.bump();
                        }
                        Some(Token::Fn) => {
                            let s = p.bump().span;
                            methods.push(p.fn_decl(s, false)?);
                        }
                        // `open fn` and `override fn`
                        Some(Token::Ident)
                            if matches!(p.text(p.toks[p.pos]), "open" | "override") && p.peek_at(1) == Some(Token::Fn) =>
                        {
                            let word = p.bump();
                            let modifier = if p.text(word) == "open" { Modifier::Open } else { Modifier::Override };
                            let s = p.bump().span;
                            let mut decl = p.fn_decl(s, false)?;
                            decl.modifier = modifier;
                            methods.push(decl);
                        }
                        // `static fn`: belongs to the type itself
                        Some(Token::Ident)
                            if p.text(p.toks[p.pos]) == "static" && p.peek_at(1) == Some(Token::Fn) =>
                        {
                            p.bump();
                            let s = p.bump().span;
                            let mut decl = p.fn_decl(s, false)?;
                            decl.is_static = true;
                            methods.push(decl);
                        }
                        Some(Token::Constructor) => {
                            let s = p.bump().span;
                            let params = p.params()?;
                            let body = p.block()?;
                            if ctor.is_some() {
                                return Err(Diagnostic::new("a type can have only one constructor", s));
                            }
                            *ctor = Some(CtorDecl { params, body });
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
                        _ => return Err(p.expected("a field, `fn`, `static fn`, `constructor` or `init`")),
                    }
                }
                Ok(())
            })?;
            self.expect(Token::RBrace)?;
        }
        Ok(())
    }


    /// Two names in a row can't start a statement, so when the first is a near miss of a word that
    /// starts a declaration (`synonim Name = …`), say which word was probably meant.
    fn misspelled_keyword(&self) -> Option<Diagnostic> {
        let tok = *self.toks.get(self.pos)?;
        let next = self.toks.get(self.pos + 1)?;
        if tok.kind != Token::Ident || next.kind != Token::Ident || next.nl {
            return None;
        }
        let word = self.text(tok);
        if DECLARATION_WORDS.contains(&word) {
            return None;
        }
        // `n as Float` and `x is Dog` are expressions, not a name followed by a name
        if matches!(self.text(*next), "as" | "is") {
            return None;
        }
        if matches!(word, "var" | "let" | "const") {
            return Some(
                Diagnostic::new(format!("`{word}` is not an Aion keyword"), tok.span)
                    .hint("declare a variable with `name := value`, or `val name = value` if it never changes"),
            );
        }
        let reach = if word.chars().count() <= 3 { 1 } else { 2 };
        let (best, distance) = DECLARATION_WORDS.iter().map(|&k| (k, edit_distance(word, k))).min_by_key(|&(_, d)| d)?;
        (distance <= reach).then(|| {
            Diagnostic::new(format!("unknown word `{word}`"), tok.span).hint(format!("did you mean `{best}`?"))
        })
    }

    /// `val name =` / `val name:`: a read-only variable (`val` is a keyword only here).
    /// `synonym Name =` / `syn Name =` / `typealias Name<T> =`: two names in a row can't start anything else.
    fn at_alias(&self) -> bool {
        matches!(self.text(self.toks[self.pos]), "synonym" | "syn" | "typealias")
            && self.peek_at(1) == Some(Token::Ident)
            && matches!(self.peek_at(2), Some(Token::Eq | Token::Lt))
    }

    fn alias_decl(&mut self) -> Result<AliasDecl> {
        self.bump(); // synonym
        let name = self.ident()?;
        let type_params = self.type_params()?;
        self.expect(Token::Eq)?;
        let target = self.type_ref()?;
        Ok(AliasDecl { name, type_params, target })
    }

    fn at_val(&self) -> bool {
        self.text(self.toks[self.pos]) == "val"
            && self.peek_at(1) == Some(Token::Ident)
            && matches!(self.peek_at(2), Some(Token::Eq | Token::Colon | Token::ColonEq))
            && !self.toks[self.pos + 1].nl
    }

    /// A `when` arm that starts with `is` or `!is`: a test of the subject's type.
    fn at_is_arm(&self) -> bool {
        let is = |at: usize| self.peek_at(at) == Some(Token::Ident) && self.text(self.toks[self.pos + at]) == "is";
        let type_follows = |at: usize| self.peek_at(at + 1) == Some(Token::Ident);
        (is(0) && type_follows(0)) || (self.peek() == Some(Token::Bang) && is(1) && type_follows(1))
    }
    /// Does the next token start a member of a type body (rather than an enum variant)?
    fn member_starts(&self) -> bool {
        let word = self.text(self.toks[self.pos]);
        match self.peek_at(1) {
            Some(Token::Colon) => true,
            Some(Token::Fn) => matches!(word, "open" | "override" | "static"),
            Some(Token::LBrace) => word == "init",
            _ => false,
        }
    }

    /// `enum Color { Red, Green, Blue }` or `enum Planet(mass: Float) { Earth(5.97e24), … }`, optionally
    /// followed by `;` and the members of a type (`fn`, `static fn`, fields, …).
    ///
    /// An enum is an ordinary type whose only instances are its variants, so nothing downstream needs
    /// to know about it, except that the checker reads `variants` for `Color.Red` and exhaustive `when`:
    ///
    /// ```text
    /// type Color(name: String, ordinal: Int, …your parameters)
    /// Color$Red := Color("Red", 0, …)           // one module variable per variant, hoisted to the top
    /// fn toString() = name;  fn compareTo(other: Color) = ordinal - other.ordinal
    /// static fn values() -> List<Color> = [Color.Red, …]
    /// static fn valueOf(text: String) -> Color? { for e in values() { if e.name == text { return e } }; return null }
    /// ```
    fn enum_decl(&mut self) -> Result<(TypeDecl, Vec<Stmt>)> {
        self.bump(); // `enum`
        let name = self.ident()?;
        let span = name.span;
        let extra = if self.peek() == Some(Token::LParen) && !self.nl() { self.params()? } else { Vec::new() };
        if let Some(p) = extra.iter().find(|p| p.vararg) {
            return Err(Diagnostic::new("an enum cannot take a `vararg`", p.name.span));
        }
        if let Some(p) = extra.iter().find(|p| p.name.name == "name" || p.name.name == "ordinal") {
            return Err(Diagnostic::new(format!("`{}` is already a field of every enum", p.name.name), p.name.span));
        }
        if self.peek() != Some(Token::LBrace) || self.nl() {
            return Err(self.expected("`{` and the enum's variants"));
        }

        let (mut fields, mut methods, mut ctor, mut inits) = (Vec::new(), Vec::new(), None, Vec::new());
        let mut list: Vec<Variant> = Vec::new();
        self.type_body(Some(&mut list), &mut fields, &mut methods, &mut ctor, &mut inits)?;
        if list.is_empty() {
            return Err(Diagnostic::new("an enum needs at least one variant", span));
        }
        for (i, v) in list.iter().enumerate() {
            if list[..i].iter().any(|u| u.name.name == v.name.name) {
                return Err(Diagnostic::new(format!("`{}` has two variants called `{}`", name.name, v.name.name), v.name.span));
            }
        }
        if ctor.is_some() {
            return Err(Diagnostic::new("an enum cannot declare a constructor", span)
                .hint("its variants are built from the parameters after its name"));
        }

        let named = |n: &str, args: Vec<TypeRef>| TypeRef { kind: TypeKind::Named { name: n.to_string(), args }, span };
        let me = named(&name.name, Vec::new());
        let ident = |n: &str| Ident { name: n.to_string(), span };
        let expr = |kind: ExprKind| Expr { kind, span };
        let variable = |n: &str| expr(ExprKind::Name(n.to_string()));
        let param = |n: &str, ty: TypeRef| Param { name: ident(n), ty, default: None, vararg: false };
        let function = |n: &str, is_static: bool, params: Vec<Param>, ret: TypeRef, body: FnBody| FnDecl {
            name: ident(n),
            type_params: Vec::new(),
            bounds: Vec::new(),
            params,
            ret: Some(ret),
            body,
            span,
            modifier: Modifier::None,
            is_static,
        };
        let global = |variant: &str| format!("{}${variant}", name.name);
        let declared = |m: &str, methods: &[FnDecl]| methods.iter().any(|d| d.name.name == m);

        if !declared("toString", &methods) {
            methods.push(function("toString", false, Vec::new(), named("String", Vec::new()), FnBody::Expr(variable("name"))));
        }
        if !declared("compareTo", &methods) {
            let difference = expr(ExprKind::Binary(
                BinOp::Sub,
                Box::new(variable("ordinal")),
                Box::new(expr(ExprKind::Field(Box::new(variable("other")), ident("ordinal")))),
            ));
            let params = vec![param("other", me.clone())];
            methods.push(function("compareTo", false, params, named("Int", Vec::new()), FnBody::Expr(difference)));
        }
        if !declared("values", &methods) {
            let items = expr(ExprKind::List(list.iter().map(|v| variable(&global(&v.name.name))).collect()));
            methods.push(function("values", true, Vec::new(), named("List", vec![me.clone()]), FnBody::Expr(items)));
        }
        if !declared("valueOf", &methods) {
            let each = expr(ExprKind::Call { callee: Box::new(variable("values")), type_args: Vec::new(), args: Vec::new() });
            let same = expr(ExprKind::Binary(
                BinOp::Eq,
                Box::new(expr(ExprKind::Field(Box::new(variable("e")), ident("name")))),
                Box::new(variable("text")),
            ));
            let found = Stmt {
                kind: StmtKind::Expr(expr(ExprKind::If {
                    cond: Box::new(same),
                    then: vec![Stmt { kind: StmtKind::Return(Some(variable("e"))), span }],
                    otherwise: None,
                })),
                span,
            };
            let body = vec![
                Stmt { kind: StmtKind::For { var: ident("e"), iter: each, body: vec![found] }, span },
                Stmt { kind: StmtKind::Return(Some(expr(ExprKind::Null))), span },
            ];
            let optional = TypeRef { kind: TypeKind::Nullable(Box::new(me.clone())), span };
            let params = vec![param("text", named("String", Vec::new()))];
            methods.push(function("valueOf", true, params, optional, FnBody::Block(body)));
        }

        // One module variable per variant, `Color$Red := Color("Red", 0, …)` (no one can write that name).
        let variants: Vec<Ident> = list.iter().map(|v| v.name.clone()).collect();
        let mut lets = Vec::new();
        for (ordinal, v) in list.into_iter().enumerate() {
            let site = v.name.span;
            let mut args = vec![
                Arg { name: None, value: expr(ExprKind::Str(v.name.name.clone())) },
                Arg { name: None, value: expr(ExprKind::Int(ordinal as i64)) },
            ];
            args.extend(v.args);
            let call = Expr { kind: ExprKind::Call { callee: Box::new(variable(&name.name)), type_args: Vec::new(), args }, span: site };
            let var = Ident { name: global(&v.name.name), span: site };
            lets.push(Stmt { kind: StmtKind::Let { name: var, ty: Some(me.clone()), init: Some(call) }, span: site });
        }

        let mut primary = vec![param("name", named("String", Vec::new())), param("ordinal", named("Int", Vec::new()))];
        primary.extend(extra);
        let decl = TypeDecl {
            name,
            type_params: Vec::new(),
            bounds: Vec::new(),
            primary: Some(primary),
            fields,
            ctor: None,
            inits,
            methods,
            open: false,
            variants: Some(variants),
            parent: None,
        };
        Ok((decl, lets))
    }

    /// The members a `data type` gets from its primary parameters (unless it declares them itself):
    /// `equals` and `hashCode` by value, `copy(x = …)`, and `component1()`, `component2()`, … for `a, b := p`.
    fn data_members(decl: &mut TypeDecl) -> Result<()> {
        let span = decl.name.span;
        if decl.open || decl.parent.is_some() {
            return Err(Diagnostic::new("a data type cannot be open or extend another type", span));
        }
        let params: Vec<(Ident, TypeRef)> = match &decl.primary {
            Some(ps) if !ps.is_empty() => ps.iter().map(|p| (p.name.clone(), p.ty.clone())).collect(),
            _ => {
                return Err(Diagnostic::new("a data type needs parameters", span)
                    .hint("e.g. `data type Point(x: Int, y: Int)`"));
            }
        };
        let named = |name: &str, args: Vec<TypeRef>| TypeRef { kind: TypeKind::Named { name: name.to_string(), args }, span };
        let me = named(&decl.name.name, decl.type_params.iter().map(|t| named(&t.name, Vec::new())).collect());
        let name = |s: &str| Expr { kind: ExprKind::Name(s.to_string()), span };
        let field = |obj: Expr, f: &Ident| Expr { kind: ExprKind::Field(Box::new(obj), Ident { name: f.name.clone(), span }), span };
        let binary = |op: BinOp, l: Expr, r: Expr| Expr { kind: ExprKind::Binary(op, Box::new(l), Box::new(r)), span };
        let call = |callee: Expr, args: Vec<Expr>| Expr {
            kind: ExprKind::Call {
                callee: Box::new(callee),
                type_args: Vec::new(),
                args: args.into_iter().map(|value| Arg { name: None, value }).collect(),
            },
            span,
        };
        let mut members: Vec<FnDecl> = Vec::new();
        let mut add = |fn_name: &str, params: Vec<Param>, ret: Option<TypeRef>, body: Expr| {
            members.push(FnDecl {
                name: Ident { name: fn_name.to_string(), span },
                type_params: Vec::new(),
                bounds: Vec::new(),
                params,
                ret,
                body: FnBody::Expr(body),
                span,
                modifier: Modifier::None,
                is_static: false,
            });
        };
        let param = |n: &str, ty: TypeRef, default: Option<Expr>| Param { name: Ident { name: n.to_string(), span }, ty, default, vararg: false };

        // equals: every field equal
        let mut equal: Option<Expr> = None;
        for (p, _) in &params {
            let same = binary(BinOp::Eq, field(name("this"), p), field(name("other"), p));
            equal = Some(match equal {
                None => same,
                Some(before) => binary(BinOp::And, before, same),
            });
        }
        add("equals", vec![param("other", me.clone(), None)], Some(named("Bool", Vec::new())), equal.expect("a data type has parameters"));

        // hashCode: the field hashes, mixed
        let mut hash: Option<Expr> = None;
        for (p, _) in &params {
            let h = call(name("hashOf"), vec![field(name("this"), p)]);
            hash = Some(match hash {
                None => h,
                Some(before) => {
                    let mixed = binary(
                        BinOp::Rem,
                        binary(BinOp::Mul, before, Expr { kind: ExprKind::Int(31), span }),
                        Expr { kind: ExprKind::Int(1_000_003), span },
                    );
                    binary(BinOp::Add, mixed, h)
                }
            });
        }
        add("hashCode", Vec::new(), Some(named("Int", Vec::new())), hash.expect("a data type has parameters"));

        // copy: every parameter defaults to the current value
        let copy_params = params.iter().map(|(p, ty)| param(&p.name, ty.clone(), Some(field(name("this"), p)))).collect();
        let copy_args = params.iter().map(|(p, _)| name(&p.name)).collect();
        add("copy", copy_params, Some(me), call(name(&decl.name.name), copy_args));

        // component1(), component2(), …
        for (i, (p, ty)) in params.iter().enumerate() {
            add(&format!("component{}", i + 1), Vec::new(), Some(ty.clone()), field(name("this"), p));
        }

        for m in members {
            if !decl.methods.iter().any(|d| d.name.name == m.name.name) {
                decl.methods.push(m);
            }
        }
        Ok(())
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
                    Some(_) => p.stmt_into(&mut stmts)?,
                }
            }
        })?;
        self.expect(Token::RBrace)?;
        Ok(stmts)
    }

    /// One statement, appended to `out`.  Destructuring (`a, b := e`) expands to several.
    fn stmt_into(&mut self, out: &mut Vec<Stmt>) -> Result<()> {
        if let Some(unknown) = self.misspelled_keyword() {
            return Err(unknown);
        }
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
                let mut body = Vec::new();
                if self.peek() == Some(Token::LBrace) {
                    body = self.block()?;
                } else {
                    self.stmt_into(&mut body)?;
                }
                StmtKind::Defer(body)
            }
            Some(Token::Ident) if self.peek_at(1) == Some(Token::At) && matches!(self.peek_at(2), Some(Token::While | Token::For)) => {
                let label = self.ident()?;
                self.bump(); // `@`
                let mut inner = Vec::new();
                self.stmt_into(&mut inner)?;
                let stmt = inner.pop().expect("a loop is one statement");
                StmtKind::Labeled { label, stmt: Box::new(stmt) }
            }
            Some(Token::While) => {
                self.bump();
                let cond = self.header_expr()?;
                StmtKind::While { cond, body: self.block()? }
            }
            Some(Token::For) => {
                self.bump();
                if self.peek() == Some(Token::LParen) || self.peek_at(1) == Some(Token::Comma) {
                    // `for (k, v) in m` and `for k, v in m` take each element apart.
                    let pattern = self.for_pattern()?;
                    self.expect(Token::In)?;
                    let iter = self.header_expr()?;
                    let span = pattern.span;
                    let var = Ident { name: self.temp("e"), span };
                    let mut body = Vec::new();
                    let element = Expr { kind: ExprKind::Name(var.name.clone()), span };
                    self.bind(pattern, element, true, &mut body)?;
                    body.extend(self.block()?);
                    StmtKind::For { var, iter, body }
                } else {
                    let var = self.ident()?;
                    self.expect(Token::In)?;
                    let iter = self.header_expr()?;
                    StmtKind::For { var, iter, body: self.block()? }
                }
            }
            Some(Token::Fn) => {
                let fn_start = self.bump().span;
                let decl = self.fn_decl(fn_start, false)?;
                StmtKind::Let { name: decl.name.clone(), ty: Self::nested_fn_type(&decl)?, init: Some(Self::nested_fn(decl)?) }
            }
            Some(Token::Break | Token::Continue) => {
                let is_break = self.bump().kind == Token::Break;
                if self.peek() == Some(Token::At) && !self.nl() {
                    self.bump();
                    StmtKind::JumpTo { label: self.ident()?, is_break }
                } else if is_break {
                    StmtKind::Break
                } else {
                    StmtKind::Continue
                }
            }
            // `val name = value`: a variable that cannot be assigned again
            Some(Token::Ident) if self.at_val() => {
                self.bump(); // `val`
                let name = self.ident()?;
                let ty = if self.eat(Token::Colon) { Some(self.type_ref()?) } else { None };
                if !self.eat(Token::Eq) && !self.eat(Token::ColonEq) {
                    return Err(Diagnostic::new(format!("`val {}` needs a value", name.name), name.span)
                        .hint(format!("e.g. `val {} = 10`; a variable without a value can't be set later", name.name)));
                }
                let init = Some(self.expr()?);
                out.push(Stmt { kind: StmtKind::Let { name: name.clone(), ty, init }, span: start.to(self.prev_span()) });
                StmtKind::Val(name)
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
                let mut targets = vec![lhs];
                while self.peek() == Some(Token::Comma) && !self.nl() {
                    self.bump();
                    targets.push(self.expr()?);
                }
                let lone_tuple = targets.len() == 1 && Self::is_tuple(&targets[0]);
                if targets.len() > 1 || (self.peek() == Some(Token::ColonEq) && !self.nl()) || (lone_tuple && self.peek() == Some(Token::Eq)) {
                    return self.destructure(targets, start, out);
                }
                let lhs = targets.pop().expect("one target");
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
        out.push(Stmt { kind, span: start.to(self.prev_span()) });
        Ok(())
    }

    /// A function declared inside another: `fn helper(x: Int) -> Int { … }` is a variable holding a
    /// closure, so it sees the variables around it.  Its type is written out when the result type is
    /// known, which also lets it call itself.
    fn nested_fn(decl: FnDecl) -> Result<Expr> {
        let span = decl.span;
        if !decl.type_params.is_empty() {
            return Err(Diagnostic::new("a function inside another cannot have type parameters", decl.name.span)
                .hint("declare it at the top level instead"));
        }
        if let Some(p) = decl.params.iter().find(|p| p.default.is_some()) {
            return Err(Diagnostic::new("a function inside another cannot have default values", p.name.span)
                .hint("declare it at the top level instead"));
        }
        if let Some(p) = decl.params.iter().find(|p| p.vararg) {
            return Err(Diagnostic::new("a function inside another cannot take a `vararg`", p.name.span)
                .hint("declare it at the top level instead"));
        }
        let params = decl.params.into_iter().map(|p| LambdaParam { name: p.name, ty: Some(p.ty) }).collect();
        let body = match decl.body {
            FnBody::Block(body) => body,
            FnBody::Expr(e) => vec![Self::expr_stmt(e)],
            FnBody::Extern { .. } => return Err(Diagnostic::new("a function inside another cannot be `extern`", span)),
        };
        Ok(Expr { kind: ExprKind::Lambda { params, explicit: true, body }, span })
    }

    /// The type of a nested function, when its result type is written or implied (a block body
    /// without `->` returns nothing).
    fn nested_fn_type(decl: &FnDecl) -> Result<Option<TypeRef>> {
        let ret = match (&decl.ret, &decl.body) {
            (Some(ret), _) => ret.clone(),
            (None, FnBody::Block(_)) => TypeRef { kind: TypeKind::Named { name: "Unit".into(), args: Vec::new() }, span: decl.name.span },
            (None, _) => return Ok(None),
        };
        let params = decl.params.iter().map(|p| p.ty.clone()).collect();
        Ok(Some(TypeRef { kind: TypeKind::Fn { params, ret: Box::new(ret) }, span: decl.name.span }))
    }

    /// A fresh name for something the parser introduces; `$` cannot appear in a source identifier.
    fn temp(&mut self, what: &str) -> String {
        self.temps += 1;
        format!("${what}{}", self.temps)
    }

    fn is_tuple(e: &Expr) -> bool {
        matches!(&e.kind, ExprKind::Call { callee, .. } if matches!(&callee.kind, ExprKind::Name(n) if n.starts_with("Tuple")))
    }

    /// The names a `for` loop takes apart: `(k, v)`, or `k, v` without the parentheses.
    fn for_pattern(&mut self) -> Result<Expr> {
        let first = self.pattern()?;
        if self.peek() != Some(Token::Comma) {
            return Ok(first);
        }
        let mut items = vec![first];
        while self.eat(Token::Comma) {
            items.push(self.pattern()?);
        }
        let span = items[0].span.to(items[items.len() - 1].span);
        Self::tuple_expr(items, span)
    }

    /// A name, or names in parentheses (which may nest): `x`, `(a, b)`, `(a, (b, c))`.
    fn pattern(&mut self) -> Result<Expr> {
        if self.peek() != Some(Token::LParen) {
            let id = self.ident()?;
            return Ok(Expr { kind: ExprKind::Name(id.name), span: id.span });
        }
        let start = self.bump().span;
        let mut items = vec![self.pattern()?];
        while self.eat(Token::Comma) {
            items.push(self.pattern()?);
        }
        let end = self.expect(Token::RParen)?.span;
        if items.len() == 1 {
            return Ok(items.pop().expect("one item"));
        }
        Self::tuple_expr(items, start.to(end))
    }

    /// `a, b := e`, `(a, b) := e`, `a, b = x, y`, `a[i], a[j] = a[j], a[i]`: several targets are
    /// filled in one go.  A single value is taken apart with `component1()`, `component2()`, …;
    /// several are first held in temporaries, so that `a, b = b, a` swaps.
    fn destructure(&mut self, mut targets: Vec<Expr>, start: Span, out: &mut Vec<Stmt>) -> Result<()> {
        let declare = match self.peek() {
            Some(Token::ColonEq) => true,
            Some(Token::Eq) => false,
            _ => return Err(self.expected("`:=` or `=` after the names")),
        };
        self.bump();
        let mut values = vec![self.expr()?];
        while self.eat(Token::Comma) {
            values.push(self.expr()?);
        }
        let span = start.to(self.prev_span());
        if targets.len() == 1 {
            if values.len() > 1 {
                return Err(Diagnostic::new("a tuple on the left takes one value", span));
            }
            let target = targets.pop().expect("one target");
            return self.bind(target, values.pop().expect("one value"), declare, out);
        }
        if values.len() == 1 {
            let tmp = self.temp("d");
            out.push(Self::let_stmt(&tmp, values.pop().expect("one value"), span));
            for (i, target) in targets.into_iter().enumerate() {
                self.bind(target, Self::component(&tmp, i + 1, span), declare, out)?;
            }
            return Ok(());
        }
        if values.len() != targets.len() {
            return Err(Diagnostic::new(format!("{} names on the left but {} values on the right", targets.len(), values.len()), span));
        }
        let mut temps = Vec::new();
        for value in values {
            let tmp = self.temp("t");
            out.push(Self::let_stmt(&tmp, value, span));
            temps.push(tmp);
        }
        for (target, tmp) in targets.into_iter().zip(temps) {
            let value = Expr { kind: ExprKind::Name(tmp), span };
            self.bind(target, value, declare, out)?;
        }
        Ok(())
    }

    fn let_stmt(name: &str, init: Expr, span: Span) -> Stmt {
        Stmt { kind: StmtKind::Let { name: Ident { name: name.to_string(), span }, ty: None, init: Some(init) }, span }
    }

    /// Store `value` into `pattern`: a name (declared when `declare`, else assigned), `_` (dropped),
    /// a tuple of patterns (taken apart), or — for assignment — a field or element.
    fn bind(&mut self, pattern: Expr, value: Expr, declare: bool, out: &mut Vec<Stmt>) -> Result<()> {
        let span = pattern.span;
        match pattern.kind {
            ExprKind::Name(name) if name == "_" => Ok(()),
            ExprKind::Name(name) if declare => {
                out.push(Self::let_stmt(&name, value, span));
                Ok(())
            }
            ExprKind::Call { callee, args, .. } if matches!(&callee.kind, ExprKind::Name(n) if n.starts_with("Tuple")) => {
                let tmp = self.temp("d");
                out.push(Self::let_stmt(&tmp, value, span));
                for (i, arg) in args.into_iter().enumerate() {
                    self.bind(arg.value, Self::component(&tmp, i + 1, span), declare, out)?;
                }
                Ok(())
            }
            kind @ (ExprKind::Name(_) | ExprKind::Field(..) | ExprKind::Index { .. }) if !declare => {
                let target = Expr { kind, span };
                out.push(Stmt { kind: StmtKind::Assign { target, op: AssignOp::Set, value }, span });
                Ok(())
            }
            _ if declare => Err(Diagnostic::new("`:=` declares plain names", span).hint("use `=` to store into a field or an element")),
            _ => Err(Diagnostic::new("cannot assign to this expression", span)),
        }
    }

    fn expr_stmt(e: Expr) -> Stmt {
        Stmt { span: e.span, kind: StmtKind::Expr(e) }
    }

    // ── expressions ─────────────────────────────────────────────

    /// An expression, the loosest level being the conditional `cond ? a : b` (sugar for an `if`).
    fn expr(&mut self) -> Result<Expr> {
        let cond = self.range_expr()?;
        if self.peek() != Some(Token::Question) {
            return Ok(cond);
        }
        self.bump();
        let then = self.expr()?;
        self.expect(Token::Colon)?;
        let otherwise = self.expr()?; // right-associative: `a ? b : c ? d : e`
        let span = cond.span.to(otherwise.span);
        Ok(Expr {
            kind: ExprKind::If {
                cond: Box::new(cond),
                then: vec![Self::expr_stmt(then)],
                otherwise: Some(vec![Self::expr_stmt(otherwise)]),
            },
            span,
        })
    }

    fn range_expr(&mut self) -> Result<Expr> {
        let mut lhs = self.binary(1)?;
        if matches!(self.peek(), Some(Token::DotDot | Token::DotDotEq)) && !self.nl() {
            let inclusive = self.bump().kind == Token::DotDotEq;
            let end = self.binary(1)?;
            let span = lhs.span.to(end.span);
            lhs = Expr { kind: ExprKind::Range { start: Box::new(lhs), end: Box::new(end), inclusive }, span };
        }
        // `a downTo b`, `0..10 step 2`: the words are only operators between two expressions on a line
        while self.peek() == Some(Token::Ident)
            && !self.nl()
            && matches!(self.text(self.toks[self.pos]), "step" | "downTo")
            && !matches!(self.peek_at(1), None | Some(Token::RBrace | Token::RParen | Token::Comma | Token::Eq))
        {
            let word = self.bump();
            let method = Ident { name: self.text(word).to_string(), span: word.span };
            let arg = self.binary(1)?;
            let span = lhs.span.to(arg.span);
            let callee = Expr { kind: ExprKind::Field(Box::new(lhs), method), span };
            lhs = Expr {
                kind: ExprKind::Call { callee: Box::new(callee), type_args: Vec::new(), args: vec![Arg { name: None, value: arg }] },
                span,
            };
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
            // `x is T`, `x !is T` (comparison level) and `x as T`, `x as? T` (above `*`).
            let word = if tok == Token::Ident { self.text(self.toks[self.pos]) } else { "" };
            let negated_is = tok == Token::Bang
                && self.peek_at(1) == Some(Token::Ident)
                && self.text(self.toks[self.pos + 1]) == "is"
                && self.toks[self.pos].span.end == self.toks[self.pos + 1].span.start;
            if word == "is" || negated_is {
                if 4 < min_prec {
                    break;
                }
                self.bump();
                if negated_is {
                    self.bump();
                }
                let ty = self.type_after_is()?;
                let span = lhs.span.to(ty.span);
                lhs = Expr { kind: ExprKind::Is { value: Box::new(lhs), ty, negated: negated_is }, span };
                continue;
            }
            if word == "as" {
                if 8 < min_prec {
                    break;
                }
                self.bump();
                let safe = self.eat(Token::Question);
                let ty = self.type_after_is()?;
                let span = lhs.span.to(ty.span);
                lhs = Expr { kind: ExprKind::Cast { value: Box::new(lhs), ty, safe }, span };
                continue;
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
                // `r?`: unless an expression follows it, which would make it a conditional `c ? a : b`
                Some(Token::Question) if !self.nl() && !self.expression_follows_question() => {
                    let end = self.bump().span;
                    let span = e.span.to(end);
                    e = Expr { kind: ExprKind::Propagate(Box::new(e)), span };
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
                    // `[i]`, or a slice `[a..b]` / `[a..]` / `[..b]` / `[..]`
                    let (start, range) = self.scoped(true, false, |p| {
                        let start = if matches!(p.peek(), Some(Token::DotDot | Token::DotDotEq)) {
                            None
                        } else {
                            Some(Box::new(p.binary(1)?))
                        };
                        if !matches!(p.peek(), Some(Token::DotDot | Token::DotDotEq)) {
                            return Ok((start, None));
                        }
                        let inclusive = p.bump().kind == Token::DotDotEq;
                        let end = if p.peek() == Some(Token::RBracket) { None } else { Some(Box::new(p.binary(1)?)) };
                        Ok((start, Some((end, inclusive))))
                    })?;
                    let close = self.expect(Token::RBracket)?;
                    let span = e.span.to(close.span);
                    e = match range {
                        None => {
                            let index = start.expect("a plain index has an expression");
                            Expr { kind: ExprKind::Index { obj: Box::new(e), index }, span }
                        }
                        Some((end, inclusive)) => {
                            Expr { kind: ExprKind::Slice { obj: Box::new(e), start, end, inclusive }, span }
                        }
                    };
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
            let value = if name.is_none() { self.element()? } else { self.expr()? };
            args.push(Arg { name, value });
            if !self.eat(Token::Comma) {
                break;
            }
        }
        Ok(args)
    }

    /// An argument or list element: an expression, or `*xs` to spread a list into place.
    fn element(&mut self) -> Result<Expr> {
        if self.peek() != Some(Token::Star) {
            return self.expr();
        }
        let star = self.bump().span;
        let inner = self.expr()?;
        let span = star.to(inner.span);
        Ok(Expr { kind: ExprKind::Spread(Box::new(inner)), span })
    }

    fn primary(&mut self) -> Result<Expr> {
        let Some(kind) = self.peek() else { return Err(self.expected("an expression")) };
        let tok = self.toks[self.pos];
        let span = tok.span;
        let kind = match kind {
            Token::Int => {
                self.bump();
                ExprKind::Int(int_literal(self.text(tok), span)?)
            }
            Token::Float => {
                self.bump();
                let text = self.text(tok).replace('_', "");
                ExprKind::Float(text.parse().expect("lexer guarantees a valid float"))
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
            Token::RawStr => {
                self.bump();
                let full = self.text(tok);
                let body = if full.starts_with("r\"\"\"") && full.len() >= 8 { &full[4..full.len() - 3] } else { &full[2..full.len() - 1] };
                let body = body.strip_prefix("\r\n").or_else(|| body.strip_prefix('\n')).filter(|_| full.starts_with("r\"\"\"")).unwrap_or(body);
                ExprKind::Str(body.to_string())
            }
            Token::Char => {
                self.bump();
                ExprKind::Char(self.char_literal(tok)?)
            }
            Token::LParen => {
                let start = self.bump().span;
                let mut items = self.scoped(true, false, |p| {
                    let mut items = vec![p.expr()?];
                    while p.eat(Token::Comma) {
                        if p.peek() == Some(Token::RParen) {
                            break;
                        }
                        items.push(p.expr()?);
                    }
                    Ok(items)
                })?;
                let end = self.expect(Token::RParen)?.span;
                if items.len() == 1 {
                    return Ok(items.pop().expect("one item"));
                }
                return Self::tuple_expr(items, start.to(end));
            }
            Token::If => return self.if_expr(),
            Token::Return => {
                self.bump();
                let ends = match self.peek() {
                    None | Some(Token::RBrace | Token::RParen | Token::RBracket | Token::Semi | Token::Comma | Token::Colon) => true,
                    _ => self.nl(),
                };
                let value = if ends { None } else { Some(Box::new(self.expr()?)) };
                let end = value.as_ref().map_or(span, |v| v.span);
                return Ok(Expr { kind: ExprKind::Return(value), span: span.to(end) });
            }
            Token::Break => {
                self.bump();
                ExprKind::Break
            }
            Token::Continue => {
                self.bump();
                ExprKind::Continue
            }
            Token::Try => return self.try_expr(),
            Token::When => return self.when_expr(),
            Token::LBrace => return self.brace_expr(),
            Token::LBracket => {
                let start = self.bump().span;
                let items = self.scoped(true, false, |p| {
                    let mut items = Vec::new();
                    while p.peek() != Some(Token::RBracket) {
                        items.push(p.element()?);
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

    /// `(a, b)`: a tuple, which is the library type `Tuple2` (up to `Tuple5`).
    fn tuple_expr(items: Vec<Expr>, span: Span) -> Result<Expr> {
        if items.len() > 5 {
            return Err(Diagnostic::new("a tuple has at most 5 elements", span).hint("use a type with named fields"));
        }
        let callee = Expr { kind: ExprKind::Name(format!("Tuple{}", items.len())), span };
        let args = items.into_iter().map(|value| Arg { name: None, value }).collect();
        Ok(Expr { kind: ExprKind::Call { callee: Box::new(callee), type_args: Vec::new(), args }, span })
    }

    /// `$name.componentN()`: the N-th part of a value being taken apart.
    fn component(name: &str, n: usize, span: Span) -> Expr {
        let target = Expr { kind: ExprKind::Name(name.to_string()), span };
        let member = Ident { name: format!("component{n}"), span };
        let callee = Expr { kind: ExprKind::Field(Box::new(target), member), span };
        Expr { kind: ExprKind::Call { callee: Box::new(callee), type_args: Vec::new(), args: Vec::new() }, span }
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
                    Some(_) => p.stmt_into(&mut body)?,
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
        let subject_name = match &subject {
            Some(Expr { kind: ExprKind::Name(n), .. }) => Some(n.clone()),
            _ => None,
        };
        self.expect(Token::LBrace)?;

        let tmp = format!("$w{}", start.start);
        let subject_span = subject.as_ref().map(|s| s.span);
        let mut arms: Vec<(Expr, Vec<Stmt>)> = Vec::new();
        let mut fallback: Option<Vec<Stmt>> = None;
        let mut covered: Vec<Ident> = Vec::new();

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
                let covered_before = covered.len();
                let mut cond: Option<Expr> = None;
                loop {
                    let c = if p.at_is_arm() {
                        let negated = p.eat(Token::Bang);
                        let at = p.bump().span; // `is`
                        let Some(subject_span) = subject_span else {
                            return Err(Diagnostic::new("`is` arms need a `when <value>`", at));
                        };
                        let ty = p.type_ref()?;
                        // a plain variable is tested itself, so it is smart-cast inside the branch
                        let tested = subject_name.clone().unwrap_or_else(|| tmp.clone());
                        let value = Expr { kind: ExprKind::Name(tested), span: subject_span };
                        Expr { kind: ExprKind::Is { value: Box::new(value), ty, negated }, span: at.to(p.prev_span()) }
                    } else if p.peek() == Some(Token::In) {
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
                        if subject_span.is_some() && let ExprKind::Field(_, variant) = &e.kind {
                            covered.push(variant.clone());
                        }
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
                // `1, 2 if x > 0 ->`: the arm applies only when the guard holds too (and then covers nothing for a
                // `when` over an enum)
                if p.peek() == Some(Token::If) {
                    p.bump();
                    let guard = p.expr()?;
                    let span = cond.as_ref().expect("an arm has a pattern").span.to(guard.span);
                    cond = Some(Expr { kind: ExprKind::Binary(BinOp::And, Box::new(cond.take().unwrap()), Box::new(guard)), span });
                    covered.truncate(covered_before);
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
        // Without an `else`, a subject that is an enum must have all its variants covered (the checker looks).
        let check = (subject.is_some() && !covered.is_empty())
            .then(|| vec![Stmt { kind: StmtKind::WhenCheck { subject: tmp.clone(), covered }, span }]);
        // Fold the arms into `if … else if … else …`.
        let mut top = match fallback {
            Some(body) => Expr { kind: ExprKind::Block(body), span },
            None => {
                // No `else`: the innermost arm is an `if` without one.
                let (cond, body) = arms.pop().expect("checked above");
                Expr { span: cond.span, kind: ExprKind::If { cond: Box::new(cond), then: body, otherwise: check } }
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
    /// The code point a character literal stands for.
    fn char_literal(&self, tok: Tok) -> Result<u32> {
        let full = self.text(tok);
        let body = &full[1..full.len() - 1];
        let Some(rest) = body.strip_prefix('\\') else {
            return Ok(body.chars().next().expect("the lexer guarantees one character") as u32);
        };
        let bad = |what: &str| Diagnostic::new(what.to_string(), tok.span);
        let mut chars = rest.chars();
        Ok(match chars.next().expect("the lexer guarantees an escape") {
            'n' => '\n' as u32,
            't' => '\t' as u32,
            'r' => '\r' as u32,
            '0' => 0,
            c @ ('\\' | '\'' | '"') => c as u32,
            'u' => {
                let hex = rest.trim_start_matches("u{").trim_end_matches('}');
                let code = u32::from_str_radix(hex, 16).map_err(|_| bad("invalid unicode escape"))?;
                char::from_u32(code).ok_or_else(|| bad("not a valid unicode code point"))? as u32
            }
            other => return Err(bad(&format!("unknown escape sequence `\\{other}`"))),
        })
    }

    fn string_expr(&mut self, tok: Tok) -> Result<Expr> {
        let full = self.text(tok);
        // `"""…"""` holds quotes and line breaks; a line break right after the opening is not part of the text.
        let triple = full.len() >= 6 && full.starts_with("\"\"\"");
        let quote = if triple { 3 } else { 1 };
        let raw = &full[quote..full.len() - quote];
        let raw_off = tok.span.start as usize + quote;
        let bytes = raw.as_bytes();

        let mut parts = Vec::new();
        let mut lit = String::new();
        let mut i = 0;
        if triple {
            if bytes.starts_with(b"\r\n") {
                i = 2;
            } else if bytes.starts_with(b"\n") {
                i = 1;
            }
        }
        while i < bytes.len() {
            match bytes[i] {
                b'\\' => {
                    let esc = raw[i + 1..].chars().next().expect("lexer guarantees an escaped char");
                    let mut width = 1 + esc.len_utf8();
                    lit.push(match esc {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '0' => '\0',
                        '\\' | '"' | '$' => esc,
                        'u' => {
                            // `\u{1F600}`
                            let at = raw_off + i;
                            let bad = |what: &str, len: usize| {
                                Diagnostic::new(what.to_string(), Span::new(self.file, at, at + len))
                            };
                            let Some(close) = raw[i + 2..].strip_prefix('{').and_then(|r| r.find('}')) else {
                                return Err(bad("expected `\\u{...}` with the code point in hex", 2));
                            };
                            width = 2 + 1 + close + 1;
                            let code = u32::from_str_radix(&raw[i + 3..i + 3 + close], 16)
                                .map_err(|_| bad("invalid unicode escape", width))?;
                            char::from_u32(code).ok_or_else(|| bad("not a valid unicode code point", width))?
                        }
                        other => {
                            let at = raw_off + i;
                            return Err(Diagnostic::new(
                                format!("unknown escape sequence `\\{other}`"),
                                Span::new(self.file, at, at + 1 + other.len_utf8()),
                            ));
                        }
                    });
                    i += width;
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
/// The value of an integer literal: decimal, or `0x` / `0b` / `0o` (which may use all 64 bits).
fn int_literal(text: &str, span: Span) -> Result<i64> {
    let text = text.replace('_', "");
    let too_large = || Diagnostic::new("integer literal is too large for Int (64-bit)", span);
    let radix = |prefix: &str, base: u32| {
        let digits = &text[prefix.len()..];
        // a hex or binary literal may spell all 64 bits, e.g. 0xFFFF_FFFF_FFFF_FFFF is -1
        u64::from_str_radix(digits, base).map(|v| v as i64).map_err(|_| too_large())
    };
    match text.get(..2).map(str::to_ascii_lowercase).as_deref() {
        Some("0x") => radix(&text[..2], 16),
        Some("0b") => radix(&text[..2], 2),
        Some("0o") => radix(&text[..2], 8),
        _ => text.parse::<i64>().map_err(|_| too_large()),
    }
}

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
    fn unicode_escapes_in_strings() {
        let ExprKind::Str(s) = parse_expr(r#""a\u{1F}b\u{e9}\u{1F600}""#).kind else { panic!() };
        assert_eq!(s, "a\u{1f}b\u{e9}\u{1F600}");
        let err = |src: &str| Parser::new(0, src, 0).and_then(|mut p| p.expr()).unwrap_err().message;
        assert!(err(r#""\u41""#).contains("expected"));
        assert!(err(r#""\u{zz}""#).contains("invalid unicode escape"));
        assert!(err(r#""\u{110000}""#).contains("not a valid unicode code point"));
    }

    #[test]
    fn triple_quoted_and_raw_strings() {
        let text = |src: &str| match parse_expr(src).kind {
            ExprKind::Str(s) => s,
            other => panic!("expected a plain string, got {other:?}"),
        };
        assert_eq!(text("\"\"\"a \"b\" c\"\"\""), "a \"b\" c");
        assert_eq!(text("\"\"\"\nline1\nline2\"\"\""), "line1\nline2");
        assert_eq!(text("\"\"\"\"q\"\"\"\""), "\"q\"");
        assert_eq!(text("\"\"\"tab\\tend\"\"\""), "tab\tend");
        assert_eq!(text("r\"C:\\dir\\n$x\""), "C:\\dir\\n$x");
        assert_eq!(text("r\"\"\"a\\n \"b\" ${c}\"\"\""), "a\\n \"b\" ${c}");
        assert_eq!(text("r\"\""), "");
        assert!(matches!(parse_expr("\"\"\"v = $name\"\"\"").kind, ExprKind::Interp(_)));
        let err = |src: &str| Parser::new(0, src, 0).and_then(|mut p| p.expr()).unwrap_err().message;
        assert!(err("\"\"\"never closed").contains("unterminated"));
        assert!(err("r\"\"\"never closed").contains("unterminated"));
    }

    #[test]
    fn import_paths_can_start_at_the_root_or_be_relative() {
        let m = module("import a.b\nimport @a.b\nimport .a\nimport ..a.b as x\nfrom @pkg.m import f\nfrom ...m import g\n");
        let bases: Vec<_> = m.imports.iter().map(|i| (i.base, i.path.len())).collect();
        assert_eq!(
            bases,
            [
                (ImportBase::Bare, 2),
                (ImportBase::Root, 2),
                (ImportBase::Relative(0), 1),
                (ImportBase::Relative(1), 2),
                (ImportBase::Root, 2),
                (ImportBase::Relative(2), 1)
            ]
        );
        assert!(m.imports[4].names.is_some());
    }

    #[test]
    fn step_and_down_to_are_operators_between_expressions() {
        // `0..10 step 2` is `(0..10).step(2)`, `a downTo b step c` is `a.downTo(b).step(c)`
        let ExprKind::Call { callee, .. } = &parse_expr("0..10 step 2").kind else { panic!("expected a call") };
        let ExprKind::Field(recv, name) = &callee.kind else { panic!() };
        assert_eq!(name.name, "step");
        assert!(matches!(recv.kind, ExprKind::Range { .. }));
        let ExprKind::Call { callee, .. } = &parse_expr("a downTo b step 2").kind else { panic!() };
        let ExprKind::Field(recv, name) = &callee.kind else { panic!() };
        assert_eq!(name.name, "step");
        assert!(matches!(&recv.kind, ExprKind::Call { .. }));
        // as plain names they are still names
        assert!(matches!(parse_expr("step").kind, ExprKind::Name(_)));
        assert!(matches!(parse_expr("f(step, downTo)").kind, ExprKind::Call { .. }));
    }

    #[test]
    fn data_types_get_their_members() {
        let m = module("data type P(x: Int, y: Int) {\n    fn norm() = x\n}\ndata type Q(z: Int) {\n    fn equals(o: Q) = true\n}");
        let Item::Type(p) = &m.items[0] else { panic!() };
        let names: Vec<&str> = p.methods.iter().map(|d| d.name.name.as_str()).collect();
        assert_eq!(names, ["norm", "equals", "hashCode", "copy", "component1", "component2"]);
        // a member the type declares itself is kept
        let Item::Type(q) = &m.items[1] else { panic!() };
        assert_eq!(q.methods.iter().filter(|d| d.name.name == "equals").count(), 1);
        assert!(Parser::new(0, "data type E", 0).and_then(|mut p| p.module()).is_err());
    }

    #[test]
    fn early_exits_are_expressions() {
        let ExprKind::Elvis(_, rhs) = &parse_expr("a ?: return -1").kind else { panic!("expected ?:") };
        assert!(matches!(&rhs.kind, ExprKind::Return(Some(_))));
        let ExprKind::Elvis(_, rhs) = &parse_expr("a ?: break").kind else { panic!("expected ?:") };
        assert!(matches!(rhs.kind, ExprKind::Break));
        assert!(matches!(&parse_expr("c ? x : continue").kind, ExprKind::If { .. }));
        // a bare `return` ends at the end of the line
        let m = module("fn f(a: Int?) {\n    a ?: return\n    println(a)\n}");
        assert_eq!(m.items.len(), 1);
    }

    #[test]
    fn destructuring_expands_to_statements() {
        // one value taken apart: a temporary, then one `component` call per name
        let m = module("a, b := f()");
        assert_eq!(m.items.len(), 3);
        // several values go through temporaries, so `a, b = b, a` swaps
        let m = module("a, b = b, a");
        assert_eq!(m.items.len(), 4);
        // nested patterns and `_`
        let m = module("(a, (b, _)) := t");
        assert_eq!(m.items.len(), 1 + 1 + 1 + 1);
        // a tuple type and literal are the library's TupleN
        let m = module("t: (Int, String)? = (1, \"a\")");
        assert_eq!(m.items.len(), 1);
        assert!(Parser::new(0, "(1, 2, 3, 4, 5, 6)", 0).and_then(|mut p| p.expr()).is_err());
    }

    #[test]
    fn conditional_expression() {
        let e = parse_expr("a > 1 ? b : c ? d : e");
        let ExprKind::If { cond, then, otherwise } = &e.kind else { panic!("expected an if, got {e:?}") };
        assert!(matches!(cond.kind, ExprKind::Binary(BinOp::Gt, ..)));
        assert_eq!(then.len(), 1);
        // right-associative: the else branch is itself a conditional
        let StmtKind::Expr(inner) = &otherwise.as_ref().unwrap()[0].kind else { panic!() };
        assert!(matches!(inner.kind, ExprKind::If { .. }));
        // `?:` and `?.` are still their own operators
        assert!(matches!(parse_expr("a ?: b").kind, ExprKind::Elvis(..)));
        assert!(matches!(parse_expr("a ? b ?: c : d").kind, ExprKind::If { .. }));
        assert!(Parser::new(0, "a ? b", 0).and_then(|mut p| p.expr()).is_err());
        // `T?` after `is` is still a nullable type unless an expression follows the `?`
        assert!(matches!(parse_expr("a is Dog ? 1 : 2").kind, ExprKind::If { .. }));
        assert!(matches!(parse_expr("a is Dog? && b").kind, ExprKind::Binary(BinOp::And, ..)));
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
    fn a_misspelled_declaration_word_is_named() {
        let err = |src: &str| Parser::new(0, src, 0).and_then(|mut p| p.module().map(|_| ())).unwrap_err();
        let e = err("synonim Cmp = (Int) -> Bool
");
        assert!(e.message.contains("unknown word `synonim`"), "{}", e.message);
        assert_eq!(e.hint.as_deref(), Some("did you mean `synonym`?"));
        assert_eq!(err("fun add(a: Int) = a
").hint.as_deref(), Some("did you mean `fn`?"));
        assert!(err("var x = 1
").message.contains("not an Aion keyword"));
        // a near miss followed by something else is left to the usual error, and the real words parse
        assert!(!err("synonim = 1 +
").message.contains("unknown word"));
        assert_eq!(module("syn Cmp = (Int) -> Bool
").items.len(), 1);
        assert_eq!(module("typealias Cmp = (Int) -> Bool
").items.len(), 1);
    }

    #[test]
    fn a_cast_or_type_test_can_start_a_statement() {
        // `n` is one letter away from `fn`, but `n as Float` is a cast
        assert_eq!(module("x := if n > 1 { n as Float } else { 0.0 }
").items.len(), 1);
        assert_eq!(module("x := if n > 1 { n is Dog } else { false }
").items.len(), 1);
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
    fn interface_syntax() {
        let m = module("interface Shape {\n    fn area() -> Float\n    fn scale(by: Float, name: String) -> Shape\n    fn draw()\n}\n");
        let Item::Interface(i) = &m.items[0] else { panic!("expected an interface") };
        assert_eq!(i.name.name, "Shape");
        assert_eq!(i.methods.len(), 3);
        assert_eq!(i.methods[1].params.len(), 2);
        assert!(i.methods[0].ret.is_some() && i.methods[2].ret.is_none());
        let m = module("interface Comparable<T> { fn compareTo(other: T) -> Int }");
        let Item::Interface(g) = &m.items[0] else { panic!("expected an interface") };
        assert_eq!(g.type_params.len(), 1);
        // a method with a body is a default
        let m = module("interface Shape { fn area() -> Float\n fn describe() -> String = \"area ${area()}\"\n fn twice() { println(1) } }");
        let Item::Interface(d) = &m.items[0] else { panic!("expected an interface") };
        assert_eq!(d.methods.len(), 3);
        assert_eq!(d.defaults.len(), 2);
        assert!(!d.methods[0].default && d.methods[1].default && d.methods[2].default);
    }

    #[test]
    fn type_parameter_bounds() {
        let m = module("fn f<T: Shape, R>(a: T) -> R = a\ntype Box<T: Shape>(item: T)\n");
        let Item::Fn(f) = &m.items[0] else { panic!("expected a function") };
        assert_eq!(f.type_params.len(), 2);
        assert!(f.bounds[0].is_some() && f.bounds[1].is_none());
        let Item::Type(t) = &m.items[1] else { panic!("expected a type") };
        assert!(t.bounds[0].is_some());
    }

    #[test]
    fn character_literals() {
        let ch = |src: &str| match parse_expr(src).kind {
            ExprKind::Char(c) => c,
            other => panic!("expected a character, got {other:?}"),
        };
        assert_eq!(ch("'a'"), 'a' as u32);
        assert_eq!(ch("'\\n'"), 10);
        assert_eq!(ch("'\\''"), '\'' as u32);
        assert_eq!(ch("'\\\\'"), '\\' as u32);
        assert_eq!(ch("'é'"), 'é' as u32);
        assert_eq!(ch("'\\u{1F600}'"), 0x1F600);
        assert_eq!(ch("'\"'"), '"' as u32);
        let err = |src: &str| Parser::new(0, src, 0).and_then(|mut p| p.expr()).unwrap_err().message;
        assert!(err("'\\q'").contains("unknown escape"));
        assert!(err("'\\u{D800}'").contains("code point"));
        // two characters, or none, are not a character literal
        assert!(Parser::new(0, "'ab'", 0).is_err() || Parser::new(0, "'ab'", 0).and_then(|mut p| p.expr()).is_err());
        assert!(Parser::new(0, "''", 0).is_err() || Parser::new(0, "''", 0).and_then(|mut p| p.expr()).is_err());
    }

    #[test]
    fn slices_and_indexing() {
        assert!(matches!(parse_expr("xs[1]").kind, ExprKind::Index { .. }));
        assert!(matches!(parse_expr("xs[i + 1]").kind, ExprKind::Index { .. }));
        let slice = |src: &str| match parse_expr(src).kind {
            ExprKind::Slice { start, end, inclusive, .. } => (start.is_some(), end.is_some(), inclusive),
            other => panic!("expected a slice, got {other:?}"),
        };
        assert_eq!(slice("xs[1..3]"), (true, true, false));
        assert_eq!(slice("xs[1..=3]"), (true, true, true));
        assert_eq!(slice("xs[2..]"), (true, false, false));
        assert_eq!(slice("xs[..4]"), (false, true, false));
        assert_eq!(slice("xs[..=4]"), (false, true, true));
        assert_eq!(slice("xs[..]"), (false, false, false));
        assert_eq!(slice("xs[n - 2..n]"), (true, true, false));
        // slices chain with other postfix operators
        assert!(matches!(parse_expr("xs[1..3][0]").kind, ExprKind::Index { .. }));
        assert!(matches!(parse_expr("xs[1..3].size()").kind, ExprKind::Call { .. }));
    }

    #[test]
    fn inheritance_syntax() {
        let m = module(
            "open type Animal(name: String) {\n    open fn sound() = \"...\"\n    fn describe() = name\n}\n\
             type Dog(name: String, breed: String) : Animal(name) {\n    override fn sound() = \"Woof\"\n}\n\
             type Puppy : Dog(\"rex\", breed: \"lab\")\nopen = 1\n",
        );
        let Item::Type(animal) = &m.items[0] else { panic!("expected a type") };
        assert!(animal.open && animal.parent.is_none());
        assert_eq!(animal.methods[0].modifier, Modifier::Open);
        assert_eq!(animal.methods[1].modifier, Modifier::None);
        let Item::Type(dog) = &m.items[1] else { panic!("expected a type") };
        assert!(!dog.open);
        let parent = dog.parent.as_ref().unwrap();
        assert_eq!((parent.name.name.as_str(), parent.args.len()), ("Animal", 1));
        assert_eq!(dog.methods[0].modifier, Modifier::Override);
        let Item::Type(puppy) = &m.items[2] else { panic!("expected a type") };
        let parent = puppy.parent.as_ref().unwrap();
        assert!(puppy.primary.is_none() && parent.args.len() == 2 && parent.args[1].name.is_some());
        // `open` is not reserved: it is still a fine variable name
        assert!(matches!(&m.items[3], Item::Stmt(_)));
    }

    #[test]
    fn type_tests_and_casts() {
        let is = |src: &str| match parse_expr(src).kind {
            ExprKind::Is { negated, ty, .. } => (negated, matches!(&ty.kind, TypeKind::Named { name, .. } if name == "Dog")),
            other => panic!("expected an `is`, got {other:?}"),
        };
        assert_eq!(is("x is Dog"), (false, true));
        assert_eq!(is("x !is Dog"), (true, true));
        let cast = |src: &str| match parse_expr(src).kind {
            ExprKind::Cast { safe, .. } => safe,
            other => panic!("expected a cast, got {other:?}"),
        };
        assert!(!cast("x as Dog"));
        assert!(cast("x as? Dog"));
        // `as` binds tighter than `*` and `+`; `is` sits with the comparisons, below `&&`
        let e = parse_expr("a + b as Dog");
        assert!(matches!(&e.kind, ExprKind::Binary(BinOp::Add, _, r) if matches!(r.kind, ExprKind::Cast { .. })));
        let e = parse_expr("a is Dog && b");
        assert!(matches!(&e.kind, ExprKind::Binary(BinOp::And, l, _) if matches!(l.kind, ExprKind::Is { .. })));
        // `x !is T` does not disturb `!=` or `!!`
        assert!(matches!(parse_expr("a != b").kind, ExprKind::Binary(BinOp::Ne, ..)));
        assert!(matches!(parse_expr("a!!").kind, ExprKind::NotNull(_)));
        // a variable can still be called `is` or `as` where no operator fits
        assert!(matches!(parse_expr("as").kind, ExprKind::Name(_)));
    }

    #[test]
    fn number_literals() {
        let int = |src: &str| match parse_expr(src).kind {
            ExprKind::Int(v) => v,
            other => panic!("expected an integer, got {other:?}"),
        };
        assert_eq!(int("42"), 42);
        assert_eq!(int("1_000_000"), 1_000_000);
        assert_eq!(int("0xFF"), 255);
        assert_eq!(int("0Xff_ff"), 65535);
        assert_eq!(int("0b1010_1010"), 170);
        assert_eq!(int("0o755"), 493);
        assert_eq!(int("0xFFFFFFFFFFFFFFFF"), -1);
        assert_eq!(int("0x8000000000000000"), i64::MIN);
        let float = |src: &str| match parse_expr(src).kind {
            ExprKind::Float(v) => v,
            other => panic!("expected a float, got {other:?}"),
        };
        assert_eq!(float("1.5"), 1.5);
        assert_eq!(float("2.5e-3"), 0.0025);
        assert_eq!(float("1e3"), 1000.0);
        assert_eq!(float("1_0.5"), 10.5);
        let err = |src: &str| Parser::new(0, src, 0).and_then(|mut p| p.expr()).unwrap_err().message;
        assert!(err("9223372036854775808").contains("too large"));
        assert!(err("0x1FFFFFFFFFFFFFFFF").contains("too large"));
        // ranges and member access on integers are not floats
        assert!(matches!(parse_expr("1..3").kind, ExprKind::Range { .. }));
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
