//! Smart casts: after a null check, an optional variable can be used as its plain type.
//!
//! ```text
//! if user != null { user.name }       // `user` is a `User` here
//! if user == null { return }          // …and for the rest of the block
//! while node != null { node = node.next }
//! ```
//!
//! A variable is narrowed while it is known to be non-null, which the checker
//! decides syntactically and conservatively:
//!
//! * facts come from `x != null` / `x == null` conditions, combined with `&&`,
//!   `||` and `!`, and from `if c { return }` guards;
//! * a variable assigned inside a lambda is never narrowed (the lambda could run
//!   at any time), nor is a module variable that any function assigns;
//! * otherwise a narrowing lasts until the first statement that assigns the
//!   variable (a plain `x = e` still sees it while `e` is evaluated).
//!
//! Narrowings are keyed by the *variable* (not the local slot), so a lambda that
//! captures a narrowed variable sees it narrowed too.

use std::collections::HashSet;

use super::{Cx, K, mk};
use crate::sema::hir::{self, GlobalId, Ty};
use crate::syntax::ast::{self, ExprKind as A, InterpPart, StmtKind};

/// What a narrowing is about.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum Key {
    Var(usize),
    Global(GlobalId),
}

#[derive(Clone, Debug)]
pub(super) struct Narrow {
    pub key: Key,
    pub name: String,
    /// The type without the `?`.
    pub plain: Ty,
}

/// One assignment found in the syntax tree.
pub(crate) struct Assign<'a> {
    pub name: &'a str,
    /// Inside a lambda?
    pub in_lambda: bool,
    /// `a.name = …` rather than `name = …`
    pub field: bool,
}

pub(crate) fn walk_stmts<'a>(stmts: &'a [ast::Stmt], lambda: bool, f: &mut dyn FnMut(Assign<'a>)) {
    for s in stmts {
        walk_stmt(s, lambda, f);
    }
}

pub(crate) fn walk_stmt<'a>(s: &'a ast::Stmt, lambda: bool, f: &mut dyn FnMut(Assign<'a>)) {
    match &s.kind {
        StmtKind::Let { init, .. } => {
            if let Some(e) = init {
                walk_expr(e, lambda, f);
            }
        }
        StmtKind::Assign { target, value, .. } => {
            match &target.kind {
                A::Name(n) => f(Assign { name: n, in_lambda: lambda, field: false }),
                A::Field(obj, field) => {
                    f(Assign { name: &field.name, in_lambda: lambda, field: true });
                    walk_expr(obj, lambda, f);
                }
                other => walk_expr_kind(other, lambda, f),
            }
            walk_expr(value, lambda, f);
        }
        StmtKind::Expr(e) => walk_expr(e, lambda, f),
        StmtKind::While { cond, body } => {
            walk_expr(cond, lambda, f);
            walk_stmts(body, lambda, f);
        }
        StmtKind::For { iter, body, .. } => {
            walk_expr(iter, lambda, f);
            walk_stmts(body, lambda, f);
        }
        StmtKind::Return(Some(e)) => walk_expr(e, lambda, f),
        // a deferred body runs later, like a lambda
        StmtKind::Defer(body) => walk_stmts(body, true, f),
        StmtKind::Return(None) | StmtKind::Break | StmtKind::Continue => {}
    }
}

pub(crate) fn walk_expr<'a>(e: &'a ast::Expr, lambda: bool, f: &mut dyn FnMut(Assign<'a>)) {
    walk_expr_kind(&e.kind, lambda, f);
}

fn walk_expr_kind<'a>(kind: &'a A, lambda: bool, f: &mut dyn FnMut(Assign<'a>)) {
    match kind {
        A::Int(_) | A::Float(_) | A::Bool(_) | A::Str(_) | A::Char(_) | A::Name(_) | A::Null => {}
        A::Interp(parts) => {
            for p in parts {
                if let InterpPart::Expr(e) = p {
                    walk_expr(e, lambda, f);
                }
            }
        }
        A::Field(e, _) | A::SafeField(e, _) | A::NotNull(e) | A::Unary(_, e) => walk_expr(e, lambda, f),
        A::Elvis(a, b) | A::Binary(_, a, b) => {
            walk_expr(a, lambda, f);
            walk_expr(b, lambda, f);
        }
        A::Index { obj, index } => {
            walk_expr(obj, lambda, f);
            walk_expr(index, lambda, f);
        }
        A::Range { start, end, .. } => {
            walk_expr(start, lambda, f);
            walk_expr(end, lambda, f);
        }
        A::Slice { obj, start, end, .. } => {
            walk_expr(obj, lambda, f);
            for e in [start, end].into_iter().flatten() {
                walk_expr(e, lambda, f);
            }
        }
        A::Call { callee, args, .. } => {
            walk_expr(callee, lambda, f);
            for a in args {
                walk_expr(&a.value, lambda, f);
            }
        }
        A::List(items) => items.iter().for_each(|e| walk_expr(e, lambda, f)),
        A::Map(entries) => {
            for (k, v) in entries {
                walk_expr(k, lambda, f);
                walk_expr(v, lambda, f);
            }
        }
        A::If { cond, then, otherwise } => {
            walk_expr(cond, lambda, f);
            walk_stmts(then, lambda, f);
            if let Some(b) = otherwise {
                walk_stmts(b, lambda, f);
            }
        }
        A::Block(stmts) => walk_stmts(stmts, lambda, f),
        // both parts run as closures
        A::Try { body, handler, .. } => {
            walk_stmts(body, true, f);
            walk_stmts(handler, true, f);
        }
        A::Lambda { body, .. } => walk_stmts(body, true, f),
    }
}

/// Names assigned anywhere inside lambdas within `stmts`.
pub(crate) fn lambda_assigned<'a>(visit: impl FnOnce(&mut dyn FnMut(Assign<'a>))) -> HashSet<String> {
    let mut names = HashSet::new();
    visit(&mut |a| {
        if a.in_lambda && !a.field {
            names.insert(a.name.to_string());
        }
    });
    names
}

/// Does a block of checked statements always leave (`return`, `break`, `continue`, `panic`)?
fn diverges(ck: &crate::sema::Checker, stmts: &[hir::Stmt]) -> bool {
    match stmts.last() {
        Some(hir::Stmt::Return(_) | hir::Stmt::Break | hir::Stmt::Continue) => true,
        Some(hir::Stmt::Expr(e)) => super::expr_returns(ck, e),
        _ => false,
    }
}

impl Cx<'_, '_> {
    /// The variable `name` and its non-null type, if it is optional and safe to narrow.
    fn narrow_target(&mut self, name: &str) -> Option<Narrow> {
        match self.resolve(name)? {
            super::Named::Local(id) => {
                let Ty::Opt(o) = self.locals[id].ty else { return None };
                if self.lambda_assigned.contains(name) {
                    return None;
                }
                Some(Narrow { key: Key::Var(self.locals[id].var), name: name.to_string(), plain: self.ck.t.opt_inner(o) })
            }
            super::Named::Global(g) => {
                let Some(Ty::Opt(o)) = self.ck.t.globals[g].ty.get() else { return None };
                if self.ck.unstable_globals.contains(name) {
                    return None;
                }
                Some(Narrow { key: Key::Global(g), name: name.to_string(), plain: self.ck.t.opt_inner(o) })
            }
            _ => None,
        }
    }

    /// What a condition tells us when it comes out `when_true`.
    pub(super) fn facts(&mut self, cond: &ast::Expr, when_true: bool) -> Vec<Narrow> {
        match &cond.kind {
            A::Binary(op @ (ast::BinOp::Eq | ast::BinOp::Ne), l, r) => {
                let name = match (&l.kind, &r.kind) {
                    (A::Name(n), A::Null) | (A::Null, A::Name(n)) => n,
                    _ => return Vec::new(),
                };
                // `x != null` is true / `x == null` is false  ⇒  x is not null
                if (*op == ast::BinOp::Ne) == when_true {
                    return self.narrow_target(name).into_iter().collect();
                }
                Vec::new()
            }
            A::Binary(ast::BinOp::And, a, b) if when_true => {
                let mut facts = self.facts(a, true);
                facts.extend(self.facts(b, true));
                facts
            }
            A::Binary(ast::BinOp::Or, a, b) if !when_true => {
                let mut facts = self.facts(a, false);
                facts.extend(self.facts(b, false));
                facts
            }
            A::Unary(ast::UnOp::Not, inner) => self.facts(inner, !when_true),
            _ => Vec::new(),
        }
    }

    /// Run `body` with `facts` in force.
    pub(super) fn with_facts<T>(&mut self, facts: Vec<Narrow>, body: impl FnOnce(&mut Self) -> T) -> T {
        let mark = self.narrowed.len();
        self.narrowed.extend(facts);
        let out = body(self);
        self.narrowed.truncate(mark);
        out
    }

    /// Is `key` narrowed right now?
    fn narrowing(&self, key: Key) -> Option<Ty> {
        self.narrowed.iter().rev().find(|n| n.key == key).map(|n| n.plain)
    }

    /// A variable's value, as its plain type if it is narrowed.
    pub(super) fn narrowed_var(&self, e: hir::Expr, var: usize) -> hir::Expr {
        match self.narrowing(Key::Var(var)) {
            Some(plain) => mk(K::Unwrap(Box::new(e)), plain),
            None => e,
        }
    }

    pub(super) fn narrowed_global(&self, e: hir::Expr, g: GlobalId) -> hir::Expr {
        match self.narrowing(Key::Global(g)) {
            Some(plain) => mk(K::Unwrap(Box::new(e)), plain),
            None => e,
        }
    }

    /// Forget narrowings of the names `s` might assign.  A plain `x = e` still sees
    /// `x` narrowed while `e` runs; the returned names are forgotten after the statement.
    pub(super) fn kill_before(&mut self, s: &ast::Stmt) -> Vec<String> {
        if self.narrowed.is_empty() {
            return Vec::new();
        }
        let mut names = HashSet::new();
        let mut after = Vec::new();
        match &s.kind {
            StmtKind::Assign { target, value, .. } if matches!(target.kind, A::Name(_)) => {
                walk_expr(value, false, &mut |a| {
                    names.insert(a.name.to_string());
                });
                if let A::Name(n) = &target.kind {
                    after.push(n.clone());
                }
            }
            _ => walk_stmt(s, false, &mut |a| {
                names.insert(a.name.to_string());
            }),
        }
        self.narrowed.retain(|n| !names.contains(&n.name));
        after
    }

    pub(super) fn kill_after(&mut self, names: &[String]) {
        self.narrowed.retain(|n| !names.contains(&n.name));
    }

    /// After `if cond { …leave… }` (no `else`), the rest of the block knows `cond` was false.
    pub(super) fn guard_facts(&mut self, e: &ast::Expr, checked: &hir::Stmt) {
        let A::If { cond, otherwise: None, .. } = &e.kind else { return };
        let hir::Stmt::Expr(hir::Expr { kind: K::Cond { then, .. }, .. }) = checked else { return };
        let hir::Expr { kind: K::Block(stmts, _), .. } = &**then else { return };
        if diverges(self.ck, stmts) {
            let facts = self.facts(cond, false);
            self.narrowed.extend(facts);
        }
    }
}
