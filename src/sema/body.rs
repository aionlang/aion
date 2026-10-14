//! Type-checking of function bodies.

use std::collections::HashMap;

use super::hir::{self, ExprKind as K, FnId, LocalId, Ty};
use super::{Sym, Tables, Work};
use crate::diag::{Diagnostic, Result, Span};
use crate::syntax::ast::{self, AssignOp, FnBody, InterpPart, StmtKind};

pub fn check_fn(t: &Tables, id: FnId, work: &Work) -> Result<hir::Func> {
    let info = &t.funcs[id];
    let mut cx = Cx::new(t, info.module, info.ret);
    for (name, ty) in &info.params {
        let local = cx.new_local(name, *ty);
        cx.scopes[0].insert(name.clone(), local);
    }

    let body = match work {
        Work::Fn(decl) | Work::Method(decl) => match &decl.body {
            FnBody::Extern { .. } => None,
            FnBody::Block(stmts) => {
                let body = cx.block(stmts)?;
                if info.ret != Ty::Void && !always_returns(&body) {
                    return Err(Diagnostic::new(
                        format!("`{}` may reach its end without returning a {}", info.name, t.ty_name(info.ret)),
                        decl.name.span,
                    ));
                }
                Some(body)
            }
            FnBody::Arrow(e) => {
                let e = cx.expr(e)?;
                Some(if info.ret == Ty::Void {
                    vec![hir::Stmt::Expr(e)]
                } else {
                    let e = cx.coerce(e, info.ret, decl.span)?;
                    vec![hir::Stmt::Return(Some(e))]
                })
            }
        },
        Work::Ctor { ty, decl, shorthand } => {
            let it = cx.new_local("it", Ty::Obj(*ty));
            cx.scopes[0].insert("it".into(), it);
            cx.ctor_it = Some(it);
            let mut body = vec![hir::Stmt::Let(it, Some(hir::Expr { kind: K::New(*ty), ty: Ty::Obj(*ty) }))];
            match decl {
                Some(c) => body.extend(cx.block(&c.body)?),
                None => {
                    for (param, &field) in shorthand.iter().enumerate() {
                        body.push(hir::Stmt::SetField {
                            obj: cx.local_expr(it),
                            index: field,
                            value: cx.local_expr(param),
                        });
                    }
                }
            }
            body.push(hir::Stmt::Return(Some(cx.local_expr(it))));
            Some(body)
        }
    };

    Ok(hir::Func {
        symbol: info.symbol.clone(),
        kind: info.kind,
        ret: info.ret,
        n_params: info.params.len(),
        locals: cx.locals,
        body,
    })
}

/// Does every path through `stmts` end in a `return`?
fn always_returns(stmts: &[hir::Stmt]) -> bool {
    match stmts.last() {
        Some(hir::Stmt::Return(_)) => true,
        Some(hir::Stmt::If { then, otherwise, .. }) => {
            !otherwise.is_empty() && always_returns(then) && always_returns(otherwise)
        }
        _ => false,
    }
}

struct Cx<'t> {
    t: &'t Tables,
    module: usize,
    locals: Vec<hir::Local>,
    scopes: Vec<HashMap<String, LocalId>>,
    ret: Ty,
    /// Inside a constructor: the object being built.
    ctor_it: Option<LocalId>,
}

fn mk(kind: K, ty: Ty) -> hir::Expr {
    hir::Expr { kind, ty }
}

impl<'t> Cx<'t> {
    fn new(t: &'t Tables, module: usize, ret: Ty) -> Self {
        Cx { t, module, locals: Vec::new(), scopes: vec![HashMap::new()], ret, ctor_it: None }
    }

    fn new_local(&mut self, name: &str, ty: Ty) -> LocalId {
        self.locals.push(hir::Local { name: name.to_string(), ty });
        self.locals.len() - 1
    }

    fn local_expr(&self, id: LocalId) -> hir::Expr {
        mk(K::Local(id), self.locals[id].ty)
    }

    fn find_local(&self, name: &str) -> Option<LocalId> {
        self.scopes.iter().rev().find_map(|s| s.get(name).copied())
    }

    fn name(&self, ty: Ty) -> String {
        self.t.ty_name(ty)
    }

    // ── statements ──────────────────────────────────────────────

    /// Check statements in a fresh lexical scope.
    fn block(&mut self, stmts: &[ast::Stmt]) -> Result<Vec<hir::Stmt>> {
        self.scopes.push(HashMap::new());
        let mut out = Vec::new();
        let result = stmts.iter().try_for_each(|s| self.stmt(s, &mut out));
        self.scopes.pop();
        result.map(|()| out)
    }

    fn stmt(&mut self, s: &ast::Stmt, out: &mut Vec<hir::Stmt>) -> Result<()> {
        match &s.kind {
            StmtKind::Let { name, ty, init } => {
                let declared = ty.as_ref().map(|t| self.t.resolve_type(self.module, t)).transpose()?;
                let init_span = init.as_ref().map(|e| e.span);
                let init = init.as_ref().map(|e| self.value(e)).transpose()?;
                let (local_ty, init) = match (declared, init) {
                    (Some(ty), Some(e)) => (ty, Some(self.coerce(e, ty, init_span.unwrap())?)),
                    (Some(ty), None) => (ty, None),
                    (None, Some(e)) => (e.ty, Some(e)),
                    (None, None) => unreachable!("the parser requires a type or an initialiser"),
                };
                if self.scopes.last().unwrap().contains_key(&name.name) {
                    return Err(Diagnostic::new(format!("`{}` is already defined in this scope", name.name), name.span));
                }
                let id = self.new_local(&name.name, local_ty);
                self.scopes.last_mut().unwrap().insert(name.name.clone(), id);
                out.push(hir::Stmt::Let(id, init));
            }

            StmtKind::Assign { target, op, value } => self.assign(target, *op, value, s.span, out)?,

            StmtKind::Expr(e) => out.push(hir::Stmt::Expr(self.expr(e)?)),

            StmtKind::If { cond, then, otherwise } => {
                let cond = self.condition(cond)?;
                let then = self.block(then)?;
                let otherwise = match otherwise {
                    Some(b) => self.block(b)?,
                    None => Vec::new(),
                };
                out.push(hir::Stmt::If { cond, then, otherwise });
            }

            StmtKind::While { cond, body } => {
                let cond = self.condition(cond)?;
                let body = self.block(body)?;
                out.push(hir::Stmt::While { cond, body });
            }

            StmtKind::Return(value) => {
                let value = match (value, self.ctor_it) {
                    (None, Some(it)) => Some(self.local_expr(it)),
                    (Some(_), Some(_)) => {
                        return Err(Diagnostic::new("a constructor cannot return a value", s.span));
                    }
                    (None, None) if self.ret == Ty::Void => None,
                    (None, None) => {
                        return Err(Diagnostic::new(format!("expected a {} to return", self.name(self.ret)), s.span));
                    }
                    (Some(e), None) if self.ret == Ty::Void => {
                        return Err(Diagnostic::new("this function does not return a value", e.span)
                            .hint("add `-> Type` to the signature"));
                    }
                    (Some(e), None) => {
                        let v = self.value(e)?;
                        Some(self.coerce(v, self.ret, e.span)?)
                    }
                };
                out.push(hir::Stmt::Return(value));
            }
        }
        Ok(())
    }

    fn condition(&mut self, e: &ast::Expr) -> Result<hir::Expr> {
        let v = self.value(e)?;
        self.coerce(v, Ty::Bool, e.span)
    }

    fn assign(
        &mut self,
        target: &ast::Expr,
        op: AssignOp,
        value: &ast::Expr,
        span: Span,
        out: &mut Vec<hir::Stmt>,
    ) -> Result<()> {
        let value_span = value.span;
        let value = self.value(value)?;
        let combine = |cx: &mut Self, cur: hir::Expr, value: hir::Expr| -> Result<hir::Expr> {
            let bin = match op {
                AssignOp::Set => unreachable!(),
                AssignOp::Add => ast::BinOp::Add,
                AssignOp::Sub => ast::BinOp::Sub,
                AssignOp::Mul => ast::BinOp::Mul,
                AssignOp::Div => ast::BinOp::Div,
            };
            let ty = cur.ty;
            let r = cx.binary(bin, cur, value, span)?;
            cx.coerce(r, ty, span)
        };

        match &target.kind {
            ast::ExprKind::Name(n) => {
                let Some(id) = self.find_local(n) else {
                    return Err(Diagnostic::new(format!("undefined variable `{n}`"), target.span));
                };
                let ty = self.locals[id].ty;
                let new = if op == AssignOp::Set {
                    self.coerce(value, ty, value_span)?
                } else {
                    let cur = self.local_expr(id);
                    combine(self, cur, value)?
                };
                out.push(hir::Stmt::Set(id, new));
            }
            ast::ExprKind::Field(obj, field) => {
                let obj = self.value(obj)?;
                let (index, fty) = self.field_of(&obj, field)?;
                if op == AssignOp::Set {
                    let value = self.coerce(value, fty, value_span)?;
                    out.push(hir::Stmt::SetField { obj, index, value });
                } else {
                    // Evaluate the receiver once.
                    let tmp = self.new_local("$recv", obj.ty);
                    out.push(hir::Stmt::Let(tmp, Some(obj)));
                    let cur = mk(K::Field { obj: Box::new(self.local_expr(tmp)), index }, fty);
                    let value = combine(self, cur, value)?;
                    out.push(hir::Stmt::SetField { obj: self.local_expr(tmp), index, value });
                }
            }
            _ => unreachable!("the parser only accepts names and fields as targets"),
        }
        Ok(())
    }

    // ── conversions ─────────────────────────────────────────────

    /// Reject expressions that produce no value.
    fn value(&mut self, e: &ast::Expr) -> Result<hir::Expr> {
        let v = self.expr(e)?;
        if v.ty == Ty::Void {
            return Err(Diagnostic::new("this expression does not produce a value", e.span));
        }
        Ok(v)
    }

    fn coerce(&self, e: hir::Expr, to: Ty, span: Span) -> Result<hir::Expr> {
        match (e.ty, to) {
            (a, b) if a == b => Ok(e),
            (Ty::Int, Ty::Float) => Ok(mk(K::IntToFloat(Box::new(e)), Ty::Float)),
            (found, _) => Err(Diagnostic::new(
                format!("expected {}, found {}", self.name(to), self.name(found)),
                span,
            )),
        }
    }

    fn to_string(&self, e: hir::Expr, span: Span) -> Result<hir::Expr> {
        match e.ty {
            Ty::Str => Ok(e),
            Ty::Int | Ty::Float | Ty::Bool => Ok(mk(K::ToStr(Box::new(e)), Ty::Str)),
            other => Err(Diagnostic::new(format!("cannot turn a {} into text", self.name(other)), span)),
        }
    }

    // ── expressions ─────────────────────────────────────────────

    fn expr(&mut self, e: &ast::Expr) -> Result<hir::Expr> {
        use ast::ExprKind as A;
        Ok(match &e.kind {
            A::Int(v) => mk(K::Int(*v), Ty::Int),
            A::Float(v) => mk(K::Float(*v), Ty::Float),
            A::Bool(v) => mk(K::Bool(*v), Ty::Bool),
            A::Str(s) => mk(K::Str(s.clone()), Ty::Str),

            A::Interp(parts) => {
                let mut acc: Option<hir::Expr> = None;
                for part in parts {
                    let piece = match part {
                        InterpPart::Lit(s) => mk(K::Str(s.clone()), Ty::Str),
                        InterpPart::Expr(x) => {
                            let v = self.value(x)?;
                            self.to_string(v, x.span)?
                        }
                    };
                    acc = Some(match acc {
                        None => piece,
                        Some(prev) => mk(K::Binary(hir::BinOp::Add, Box::new(prev), Box::new(piece)), Ty::Str),
                    });
                }
                acc.expect("the parser never produces an empty interpolation")
            }

            A::Name(n) => match self.find_local(n) {
                Some(id) => self.local_expr(id),
                None => return Err(self.unresolved_name(n, e.span)),
            },

            A::Field(obj, field) => {
                self.reject_module_value(obj)?;
                let obj = self.value(obj)?;
                let (index, ty) = self.field_of(&obj, field)?;
                mk(K::Field { obj: Box::new(obj), index }, ty)
            }

            A::Call { callee, args } => self.call(callee, args, e.span)?,

            A::Unary(op, inner) => {
                let v = self.value(inner)?;
                match (op, v.ty, &v.kind) {
                    (ast::UnOp::Neg, Ty::Int, K::Int(n)) => mk(K::Int(n.wrapping_neg()), Ty::Int),
                    (ast::UnOp::Neg, Ty::Float, K::Float(x)) => mk(K::Float(-x), Ty::Float),
                    (ast::UnOp::Neg, Ty::Int | Ty::Float, _) => {
                        let ty = v.ty;
                        mk(K::Neg(Box::new(v)), ty)
                    }
                    (ast::UnOp::Not, Ty::Bool, _) => mk(K::Not(Box::new(v)), Ty::Bool),
                    (ast::UnOp::Neg, other, _) => {
                        return Err(Diagnostic::new(format!("cannot negate a {}", self.name(other)), e.span));
                    }
                    (ast::UnOp::Not, other, _) => {
                        return Err(Diagnostic::new(format!("`!` needs a Bool, found {}", self.name(other)), e.span));
                    }
                }
            }

            A::Binary(op, l, r) => {
                let l = self.value(l)?;
                let r = self.value(r)?;
                self.binary(*op, l, r, e.span)?
            }
        })
    }

    fn unresolved_name(&self, n: &str, span: Span) -> Diagnostic {
        if self.t.scopes[self.module].modules.contains_key(n) {
            return Diagnostic::new(format!("`{n}` is a module, not a value"), span)
                .hint(format!("call one of its functions, e.g. `{n}.name(...)`"));
        }
        match self.t.lookup(self.module, n) {
            Some(Sym::Fn(_)) => Diagnostic::new(format!("`{n}` is a function; call it with `{n}(...)`"), span),
            Some(Sym::Type(_)) => Diagnostic::new(format!("`{n}` is a type; construct one with `{n}(...)`"), span),
            None => Diagnostic::new(format!("undefined variable `{n}`"), span),
        }
    }

    /// `math.pi` style access to a module member as a value is not supported.
    fn reject_module_value(&self, obj: &ast::Expr) -> Result<()> {
        if let ast::ExprKind::Name(n) = &obj.kind
            && self.find_local(n).is_none()
            && self.t.scopes[self.module].modules.contains_key(n)
        {
            return Err(Diagnostic::new(
                format!("`{n}` is a module; only functions and types can be used from it"),
                obj.span,
            ));
        }
        Ok(())
    }

    fn field_of(&self, obj: &hir::Expr, field: &ast::Ident) -> Result<(usize, Ty)> {
        let Ty::Obj(id) = obj.ty else {
            return Err(Diagnostic::new(
                format!("a {} has no field `{}`", self.name(obj.ty), field.name),
                field.span,
            ));
        };
        let info = &self.t.types[id];
        match info.fields.iter().position(|f| f.name == field.name) {
            Some(i) => Ok((i, info.fields[i].ty)),
            None => Err(Diagnostic::new(format!("`{}` has no field `{}`", info.name, field.name), field.span)),
        }
    }

    fn binary(&mut self, op: ast::BinOp, l: hir::Expr, r: hir::Expr, span: Span) -> Result<hir::Expr> {
        use ast::BinOp as B;
        if matches!(op, B::And | B::Or) {
            let l = self.coerce(l, Ty::Bool, span)?;
            let r = self.coerce(r, Ty::Bool, span)?;
            return Ok(mk(K::Logical { and: op == B::And, lhs: Box::new(l), rhs: Box::new(r) }, Ty::Bool));
        }

        // Mixed Int/Float arithmetic and comparison happens in Float.
        let (l, r) = match (l.ty, r.ty) {
            (Ty::Int, Ty::Float) => (mk(K::IntToFloat(Box::new(l)), Ty::Float), r),
            (Ty::Float, Ty::Int) => (l, mk(K::IntToFloat(Box::new(r)), Ty::Float)),
            _ => (l, r),
        };

        if l.ty != r.ty {
            let e = Diagnostic::new(
                format!("cannot apply `{}` to {} and {}", op.symbol(), self.name(l.ty), self.name(r.ty)),
                span,
            );
            return Err(if op == B::Add && (l.ty == Ty::Str || r.ty == Ty::Str) {
                e.hint("to join text and values, use interpolation: \"${a}${b}\"")
            } else {
                e
            });
        }

        let (hop, result) = match op {
            B::Add if l.ty == Ty::Str => (hir::BinOp::Add, Ty::Str),
            B::Add | B::Sub | B::Mul | B::Div | B::Rem if matches!(l.ty, Ty::Int | Ty::Float) => (
                match op {
                    B::Add => hir::BinOp::Add,
                    B::Sub => hir::BinOp::Sub,
                    B::Mul => hir::BinOp::Mul,
                    B::Div => hir::BinOp::Div,
                    _ => hir::BinOp::Rem,
                },
                l.ty,
            ),
            B::Lt | B::Le | B::Gt | B::Ge if matches!(l.ty, Ty::Int | Ty::Float) => (
                match op {
                    B::Lt => hir::BinOp::Lt,
                    B::Le => hir::BinOp::Le,
                    B::Gt => hir::BinOp::Gt,
                    _ => hir::BinOp::Ge,
                },
                Ty::Bool,
            ),
            B::Eq | B::Ne if l.ty != Ty::Void => {
                (if op == B::Eq { hir::BinOp::Eq } else { hir::BinOp::Ne }, Ty::Bool)
            }
            _ => {
                return Err(Diagnostic::new(
                    format!("cannot apply `{}` to {} values", op.symbol(), self.name(l.ty)),
                    span,
                ));
            }
        };
        Ok(mk(K::Binary(hop, Box::new(l), Box::new(r)), result))
    }

    // ── calls ───────────────────────────────────────────────────

    fn call(&mut self, callee: &ast::Expr, args: &[ast::Expr], span: Span) -> Result<hir::Expr> {
        match &callee.kind {
            ast::ExprKind::Name(n) => {
                if self.find_local(n).is_some() {
                    return Err(Diagnostic::new(format!("`{n}` is a variable, not a function"), callee.span));
                }
                match self.t.lookup(self.module, n) {
                    Some(Sym::Fn(id)) => self.call_fn(id, None, args, span),
                    Some(Sym::Type(ty)) => self.call_fn(self.t.types[ty].ctor, None, args, span),
                    None if n == "print" || n == "println" => self.print(n == "println", args, span),
                    None => Err(Diagnostic::new(format!("undefined function `{n}`"), callee.span)),
                }
            }

            ast::ExprKind::Field(obj, member) => {
                // `module.func(...)` / `module.Type(...)`
                if let ast::ExprKind::Name(a) = &obj.kind
                    && self.find_local(a).is_none()
                {
                    let Some(&target) = self.t.scopes[self.module].modules.get(a) else {
                        let import = if crate::embedded::stdlib_names().contains(&a.as_str()) {
                            format!("import std.{a};")
                        } else {
                            format!("import {a};")
                        };
                        return Err(Diagnostic::new(format!("undefined name `{a}`"), obj.span)
                            .hint(format!("did you forget `{import}`?")));
                    };
                    return match self.t.scopes[target].items.get(&member.name) {
                        Some(&Sym::Fn(id)) => self.call_fn(id, None, args, span),
                        Some(&Sym::Type(ty)) => self.call_fn(self.t.types[ty].ctor, None, args, span),
                        None => Err(Diagnostic::new(
                            format!("module `{a}` has no function or type `{}`", member.name),
                            member.span,
                        )),
                    };
                }

                // `value.method(...)`
                let recv = self.value(obj)?;
                let Ty::Obj(ty) = recv.ty else {
                    return Err(Diagnostic::new(
                        format!("a {} has no method `{}`", self.name(recv.ty), member.name),
                        member.span,
                    ));
                };
                match self.t.types[ty].methods.get(&member.name) {
                    Some(&id) => self.call_fn(id, Some(recv), args, span),
                    None => Err(Diagnostic::new(
                        format!("`{}` has no method `{}`", self.t.types[ty].name, member.name),
                        member.span,
                    )),
                }
            }

            _ => Err(Diagnostic::new("this expression cannot be called", callee.span)),
        }
    }

    fn call_fn(&mut self, id: FnId, recv: Option<hir::Expr>, args: &[ast::Expr], span: Span) -> Result<hir::Expr> {
        let info = &self.t.funcs[id];
        let skip = usize::from(recv.is_some());
        let expected = &info.params[skip..];
        if expected.len() != args.len() {
            return Err(Diagnostic::new(
                format!("`{}` takes {} argument(s) but {} were given", info.name, expected.len(), args.len()),
                span,
            ));
        }
        let mut out: Vec<hir::Expr> = recv.into_iter().collect();
        for ((_, ty), arg) in expected.iter().zip(args) {
            let v = self.value(arg)?;
            out.push(self.coerce(v, *ty, arg.span)?);
        }
        Ok(mk(K::Call { func: id, args: out }, info.ret))
    }

    /// The built-in, type-polymorphic `print` / `println`.
    fn print(&mut self, newline: bool, args: &[ast::Expr], span: Span) -> Result<hir::Expr> {
        let arg = match args {
            [] if newline => None,
            [a] => {
                let v = self.value(a)?;
                if matches!(v.ty, Ty::Obj(_)) {
                    return Err(Diagnostic::new(format!("cannot print a {}", self.name(v.ty)), a.span)
                        .hint("print its fields, or build text with \"${...}\""));
                }
                Some(Box::new(v))
            }
            _ => {
                let name = if newline { "println" } else { "print" };
                return Err(Diagnostic::new(format!("`{name}` takes one value to print"), span));
            }
        };
        Ok(mk(K::Print { newline, arg }, Ty::Void))
    }
}
