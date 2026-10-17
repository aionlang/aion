//! Type-checking of function bodies.

use std::collections::{HashMap, HashSet};
use std::mem::{replace, take};
use std::rc::Rc;

use super::hir::{self, ExprKind as K, FnId, GlobalId, LocalId, Ty, TypeId};
use super::{Checker, Const, ParamDefault, Subst, Sym, Work};
use crate::diag::{Diagnostic, Result, Span};
use crate::syntax::ast::{self, AssignOp, ExprKind as A, FnBody, InterpPart, StmtKind};

mod generic_calls;
mod narrow;

pub(super) use narrow::{Assign, lambda_assigned, walk_expr, walk_stmt, walk_stmts};

const MAX_CAPTURES: usize = 63;

pub(super) fn check_fn(ck: &Checker, id: FnId, work: &Work) -> Result<hir::Func> {
    let info = ck.func(id);
    let mut cx = Cx::new(ck, info.module);
    if ck.trace
        && let Some(&file) = ck.module_files.get(info.module)
        && !ck.files[file as usize].std
    {
        cx.trace_file = Some(ck.files[file as usize].name.clone());
    }
    for p in &info.params {
        let local = cx.new_local(&p.name, p.ty);
        cx.scopes[0].insert(p.name.clone(), local);
    }
    cx.ret = info.ret.get();
    cx.lambda_assigned = match work {
        Work::Fn { decl, .. } | Work::Method { decl, .. } => match &decl.body {
            FnBody::Block(stmts) => lambda_assigned(|f| walk_stmts(stmts, false, f)),
            FnBody::Expr(e) => lambda_assigned(|f| walk_expr(e, false, f)),
            FnBody::Extern { .. } => HashSet::new(),
        },
        Work::Ctor { decl, .. } | Work::TypeInit { decl, .. } => lambda_assigned(|f| {
            decl.fields.iter().filter_map(|fl| fl.init.as_ref()).for_each(|e| walk_expr(e, false, f));
            decl.inits.iter().for_each(|blk| walk_stmts(blk, false, f));
            if let Some(c) = &decl.ctor {
                walk_stmts(&c.body, false, f);
            }
        }),
        Work::Init { stmts } => lambda_assigned(|f| stmts.iter().for_each(|s| walk_stmt(s, false, f))),
        Work::Default { expr, .. } => lambda_assigned(|f| walk_expr(expr, false, f)),
    };

    let body = match work {
        Work::Default { expr, subst } => {
            cx.subst = subst.clone();
            if info.params.first().is_some_and(|p| p.name == "this")
                && let Ty::Obj(ty) = info.params[0].ty
            {
                cx.this = Some(ty);
            }
            let ret = info.ret.get().expect("a default has its parameter's type");
            let v = cx.value_expected(expr, Some(ret))?;
            Some(vec![hir::Stmt::Return(Some(cx.coerce(v, ret, expr.span)?))])
        }
        Work::Fn { decl, subst, .. } | Work::Method { decl, subst } => {
            cx.subst = subst.clone();
            if let Work::Fn { owner, .. } = work {
                cx.static_owner = *owner;
            }
            if matches!(work, Work::Method { .. })
                && let Ty::Obj(ty) = info.params[0].ty
            {
                cx.this = Some(ty);
            }
            match &decl.body {
                FnBody::Extern { .. } => None,
                FnBody::Block(stmts) => {
                    let body = cx.block(stmts)?;
                    let ret = info.ret.get().expect("block bodies never infer");
                    if ret != Ty::Void && !always_returns(ck, &body) {
                        return Err(Diagnostic::new(
                            format!("`{}` may reach its end without returning a {}", info.name, ck.t.ty_name(ret)),
                            decl.name.span,
                        ));
                    }
                    Some(body)
                }
                FnBody::Expr(e) => Some(match info.ret.get() {
                    Some(ret) => {
                        let v = cx.value_expected(e, Some(ret))?;
                        vec![hir::Stmt::Return(Some(cx.coerce(v, ret, e.span)?))]
                    }
                    None => {
                        // Infer the return type from the expression.
                        let v = cx.expr(e)?;
                        info.ret.set(Some(v.ty));
                        if v.ty == Ty::Void { vec![hir::Stmt::Expr(v)] } else { vec![hir::Stmt::Return(Some(v))] }
                    }
                }),
            }
        }

        // allocate, run the type's initialiser on the new object, hand it back
        Work::Ctor { ty, .. } if ck.t.ty(*ty).init.is_some() => {
            let init = ck.t.ty(*ty).init.expect("checked by the guard");
            let this = cx.new_local("this", Ty::Obj(*ty));
            let mut args = vec![cx.local_expr(this)];
            args.extend((0..info.params.len()).map(|i| cx.local_expr(i)));
            Some(vec![
                hir::Stmt::Let(this, Some(mk(K::New(*ty), Ty::Obj(*ty)))),
                hir::Stmt::Expr(mk(K::Call { func: init, args }, Ty::Void)),
                hir::Stmt::Return(Some(cx.local_expr(this))),
            ])
        }

        // the part of construction that a subtype's constructor reuses on its own object
        Work::TypeInit { ty, decl } => {
            cx.this = Some(*ty);
            let mut body = Vec::new();
            let fields = ck.t.ty(*ty).fields.clone();
            let inherited = ck.t.ty(*ty).parent.map_or(0, |p| ck.t.ty(p).fields.len());

            // The parent first, with the arguments written after the colon.
            if let (Some(parent), Some(clause)) = (ck.t.ty(*ty).parent, &decl.parent) {
                let init = ck.t.ty(parent).init.expect("an open type has an initialiser");
                body.push(hir::Stmt::Expr(cx.call_fn(init, Some(cx.local_expr(0)), &clause.args, clause.span)?));
            }
            // Primary-constructor parameters that are fields of this type (the others went to the parent).
            for (i, p) in decl.primary.iter().flatten().enumerate() {
                let index = fields.iter().position(|f| f.name == p.name.name).expect("every parameter is a field");
                if index >= inherited {
                    body.push(hir::Stmt::SetField { obj: cx.local_expr(0), index, value: cx.local_expr(i + 1) });
                }
            }
            for f in &decl.fields {
                if let Some(init) = &f.init {
                    let index = fields.iter().position(|g| g.name == f.name.name).unwrap();
                    let v = cx.value_expected(init, Some(fields[index].ty))?;
                    let value = cx.coerce(v, fields[index].ty, init.span)?;
                    body.push(hir::Stmt::SetField { obj: cx.local_expr(0), index, value });
                }
            }
            for blk in &decl.inits {
                body.extend(cx.block(blk)?);
            }
            if let Some(c) = &decl.ctor {
                body.extend(cx.block(&c.body)?);
            }
            Some(body)
        }

        Work::Ctor { ty, decl, subst } => {
            cx.subst = subst.clone();
            let this = cx.new_local("this", Ty::Obj(*ty));
            cx.scopes[0].insert("this".into(), this);
            cx.this = Some(*ty);
            cx.in_ctor = true;
            let mut body = vec![hir::Stmt::Let(this, Some(mk(K::New(*ty), Ty::Obj(*ty))))];

            // Primary-constructor parameters initialise the leading fields.
            for (i, _) in decl.primary.iter().flatten().enumerate() {
                body.push(hir::Stmt::SetField { obj: cx.local_expr(this), index: i, value: cx.local_expr(i) });
            }
            // Field initialisers, then `init` blocks, then an explicit constructor body.
            for f in &decl.fields {
                if let Some(init) = &f.init {
                    let index = ck.t.ty(*ty).fields.iter().position(|g| g.name == f.name.name).unwrap();
                    let want = ck.t.ty(*ty).fields[index].ty;
                    let v = cx.value_expected(init, Some(want))?;
                    let value = cx.coerce(v, want, init.span)?;
                    body.push(hir::Stmt::SetField { obj: cx.local_expr(this), index, value });
                }
            }
            for blk in &decl.inits {
                body.extend(cx.block(blk)?);
            }
            if let Some(c) = &decl.ctor {
                body.extend(cx.block(&c.body)?);
            }
            body.push(hir::Stmt::Return(Some(cx.local_expr(this))));
            Some(body)
        }

        Work::Init { stmts } => {
            cx.init = Some(InitState { fn_id: id, declared: HashSet::new() });
            let mut out = Vec::new();
            cx.block_defers.push(None);
            for s in stmts {
                let after = cx.kill_before(s);
                cx.top_stmt(s, &mut out)?;
                cx.kill_after(&after);
            }
            // A top-level `defer` runs when the file's own code has finished.
            if let Some(Some(mark)) = cx.block_defers.pop() {
                out.push(cx.run_defers(mark)?);
            }
            Some(out)
        }
    };

    cx.flush_lambdas();
    let mut body = body;
    let trace = cx.trace_file.clone().map(|file| {
        let span = match work {
            Work::Fn { decl, .. } | Work::Method { decl, .. } => decl.name.span,
            Work::Ctor { decl, .. } | Work::TypeInit { decl, .. } => decl.name.span,
            Work::Default { expr, .. } => expr.span,
            Work::Init { stmts } => stmts.first().map_or_else(Span::default, |s| s.span),
        };
        let name = match (work, info.kind, info.params.first().map(|p| p.ty)) {
            (Work::Init { .. }, ..) => "<top level>".to_string(),
            (_, hir::FuncKind::Method, Some(Ty::Obj(t))) => format!("{}.{}", ck.t.ty(t).name, info.name),
            (_, hir::FuncKind::Constructor, _) => format!("{}.new", info.name),
            _ => info.name.clone(),
        };
        hir::Trace { name, file, line: ck.line_of(span) }
    });
    if let (Some(t), Some(b)) = (&trace, &mut body) {
        b.insert(0, hir::Stmt::Line(t.line));
    }
    Ok(hir::Func {
        trace,
        symbol: info.symbol.clone(),
        kind: info.kind,
        ret: info.ret.get().unwrap_or(Ty::Void),
        n_params: info.params.len(),
        locals: cx.hir_locals(),
        body,
        captures: Vec::new(),
    })
}

impl Checker<'_> {
    /// An adapter that lets declared function `id` be used as a function value:
    /// `thunk(env, args…) = id(args…)`.
    fn thunk_for(&self, id: FnId, ret: Ty) -> FnId {
        if let Some(&t) = self.thunks.borrow().get(&id) {
            return t;
        }
        let thunk = self.reserve_fn();
        self.thunks.borrow_mut().insert(id, thunk);

        let info = self.func(id);
        let locals: Vec<hir::Local> = info
            .params
            .iter()
            .map(|p| hir::Local { name: p.name.clone(), ty: p.ty, boxed: false })
            .collect();
        let args = locals
            .iter()
            .enumerate()
            .map(|(i, l)| mk(K::Local(i), l.ty))
            .collect();
        let call = mk(K::Call { func: id, args }, ret);
        let func = hir::Func {
            trace: None,
            symbol: format!("thunk__{}", info.symbol),
            kind: hir::FuncKind::Closure,
            ret,
            n_params: locals.len(),
            locals,
            body: Some(vec![if ret == Ty::Void { hir::Stmt::Expr(call) } else { hir::Stmt::Return(Some(call)) }]),
            captures: Vec::new(),
        };
        self.set_func(thunk, func);
        thunk
    }

    /// The `toString` used when an object is printed or interpolated: the
    /// type's own `toString()` if it has one, otherwise a generated
    /// `Name(field=value, …)`.
    pub(in crate::sema) fn tostring_fn(&self, ty: TypeId) -> Result<FnId> {
        if let Some(&f) = self.tostring.borrow().get(&ty) {
            return Ok(f);
        }
        if let Some(&m) = self.t.ty(ty).methods.get("toString")
            && self.func(m).params.len() == 1
            && self.ret_of(m, Span::default())? == Ty::Str
        {
            self.tostring.borrow_mut().insert(ty, m);
            return Ok(m);
        }

        // Reserve the id first so self-referential types find themselves.
        let id = self.reserve_fn();
        self.tostring.borrow_mut().insert(ty, id);

        let info = self.t.ty(ty);
        let mut cx = Cx::new(self, 0);
        let this = cx.new_local("this", Ty::Obj(ty));
        let text = |s: String| mk(K::Str(s), Ty::Str);
        let concat = |a: hir::Expr, b: hir::Expr| mk(K::Binary(hir::BinOp::Add, Box::new(a), Box::new(b)), Ty::Str);

        let mut acc = text(format!("{}(", info.name));
        for (i, f) in info.fields.iter().enumerate() {
            let label = if i == 0 { format!("{}=", f.name) } else { format!(", {}=", f.name) };
            acc = concat(acc, text(label));
            let field = mk(K::Field { obj: Box::new(cx.local_expr(this)), index: i }, f.ty);
            let is_fn = match f.ty {
                Ty::Fn(_) => true,
                Ty::Opt(o) => matches!(self.t.opt_inner(o), Ty::Fn(_)),
                _ => false,
            };
            let piece = if is_fn {
                text("<function>".into())
            } else {
                cx.stringify(field, Span::default())?
            };
            acc = concat(acc, piece);
        }
        acc = concat(acc, text(")".into()));

        let func = hir::Func {
            trace: None,
            symbol: format!("tostring__{}__{ty}", info.name),
            kind: hir::FuncKind::Method,
            ret: Ty::Str,
            n_params: 1,
            locals: cx.hir_locals(),
            body: Some(vec![hir::Stmt::Return(Some(acc))]),
            captures: Vec::new(),
        };
        self.set_func(id, func);
        Ok(id)
    }
}

/// Does every path through `stmts` end in a `return`?
fn always_returns(ck: &Checker, stmts: &[hir::Stmt]) -> bool {
    match stmts.last() {
        Some(hir::Stmt::Return(_)) => true,
        Some(hir::Stmt::Expr(e)) => expr_returns(ck, e),
        _ => false,
    }
}

/// Does the expression always leave — by `return`, `break`, `continue` or `panic` — instead of producing a value?
fn diverges(ck: &Checker, e: &hir::Expr) -> bool {
    match &e.kind {
        K::Cond { then, otherwise, .. } => diverges(ck, then) && diverges(ck, otherwise),
        K::Block(stmts, tail) => {
            let leaves = |s: &hir::Stmt| match s {
                hir::Stmt::Return(_) | hir::Stmt::Break | hir::Stmt::Continue | hir::Stmt::JumpOut { .. } => true,
                hir::Stmt::Expr(e) => diverges(ck, e),
                _ => false,
            };
            stmts.iter().any(leaves) || diverges(ck, tail)
        }
        _ => is_panic(ck, e),
    }
}

fn expr_returns(ck: &Checker, e: &hir::Expr) -> bool {
    match &e.kind {
        K::Cond { then, otherwise, .. } => expr_returns(ck, then) && expr_returns(ck, otherwise),
        K::Block(stmts, tail) => always_returns(ck, stmts) || expr_returns(ck, tail),
        _ => is_panic(ck, e),
    }
}

/// A call to `panic(…)`, which never comes back.
fn is_panic(ck: &Checker, e: &hir::Expr) -> bool {
    let K::Call { func, .. } = &e.kind else { return false };
    ck.fns.borrow()[*func].info.as_ref().is_some_and(|i| i.symbol == "aion_panic")
}

struct InitState {
    fn_id: FnId,
    /// Globals whose definition has been passed (for use-before-definition errors).
    declared: HashSet<GlobalId>,
}

/// What a lambda is known to have to look like.
struct LamSig {
    params: Vec<Ty>,
    /// `None` while still unknown (it is inferred from the body).
    ret: Option<Ty>,
}

/// A local variable while checking (becomes a [`hir::Local`]).
struct LocalInfo {
    name: String,
    ty: Ty,
    /// Which source variable this is; a closure's copy shares its origin's.
    var: usize,
}

/// Whether a variable is captured by a closure and reassigned, which decides
/// if it needs a shared heap cell.
#[derive(Default)]
struct VarInfo {
    captured: bool,
    mutated: bool,
    /// Declared with `val`: assigning to it is an error.
    immutable: bool,
}

/// The per-function state that is swapped out while checking a lambda body.
struct Frame {
    locals: Vec<LocalInfo>,
    scopes: Vec<HashMap<String, LocalId>>,
    ret: Option<Ty>,
    this: Option<TypeId>,
    in_ctor: bool,
    init: Option<InitState>,
    loops: u32,
    captures: Vec<hir::Capture>,
    block_defers: Vec<Option<LocalId>>,
    loop_bases: Vec<(usize, Option<String>)>,
    in_try: bool,
}

/// A checked lambda waiting for the enclosing function to finish, which
/// settles which of the variables it captures need boxing.
struct PendingLambda {
    trace: Option<hir::Trace>,
    id: FnId,
    ret: Ty,
    n_params: usize,
    locals: Vec<LocalInfo>,
    body: Vec<hir::Stmt>,
    captures: Vec<hir::Capture>,
}

struct Cx<'c, 'a> {
    ck: &'c Checker<'a>,
    module: usize,
    locals: Vec<LocalInfo>,
    scopes: Vec<HashMap<String, LocalId>>,
    /// `None` while the return type is still being inferred.
    ret: Option<Ty>,
    /// Inside a method or constructor: the type of `this`.
    this: Option<TypeId>,
    /// Inside a `static fn`: the type it belongs to, whose other statics can be called bare.
    static_owner: Option<TypeId>,
    in_ctor: bool,
    init: Option<InitState>,
    loops: u32,
    /// Variables this function copies out of its closure object.
    captures: Vec<hir::Capture>,
    /// Type parameters of the generic instance being checked, bound to concrete types.
    subst: Subst,
    /// The functions enclosing the one being checked (innermost last).
    outer: Vec<Frame>,
    vars: Vec<VarInfo>,
    lambdas: Vec<PendingLambda>,
    /// The source file's name when this function keeps a frame in the stack trace.
    trace_file: Option<String>,
    /// One entry per open block of the current function: the local holding the
    /// `defer` stack depth at its first `defer`, if it has one.
    block_defers: Vec<Option<LocalId>>,
    /// `block_defers.len()` at the start of each enclosing loop body.
    loop_bases: Vec<(usize, Option<String>)>,
    /// A `outer@` label seen, to be taken by the loop that follows it.
    pending_label: Option<String>,
    /// Checking the body or handler of a `try` (which run as closures)?
    in_try: bool,
    /// The next lambda frame is the body of a `try`.
    pending_try: bool,
    /// Optional variables currently known to be non-null (see `narrow.rs`).
    narrowed: Vec<narrow::Narrow>,
    /// Names assigned inside lambdas of the function being checked: never narrowed.
    lambda_assigned: HashSet<String>,
}

fn mk(kind: K, ty: Ty) -> hir::Expr {
    hir::Expr { kind, ty }
}

fn unit() -> hir::Expr {
    mk(K::Unit, Ty::Void)
}

/// What a bare name refers to.
enum Named {
    Local(LocalId),
    /// A field of `this`.
    Field(usize, Ty),
    Global(GlobalId),
    Fn(FnId),
    Type(TypeId),
    /// A generic function or type, which can't be used without arguments.
    Generic,
    Module(usize),
}

/// Somewhere a value can be stored.
enum Target {
    Local(LocalId),
    Global(GlobalId),
    Field { obj: hir::Expr, index: usize },
    /// `obj[index]`: an array element, or `obj.set(index, value)` for other types.
    Index { obj: hir::Expr, index: hir::Expr },
}

fn lookup_scopes(scopes: &[HashMap<String, LocalId>], name: &str) -> Option<LocalId> {
    scopes.iter().rev().find_map(|s| s.get(name).copied())
}

impl<'c, 'a> Cx<'c, 'a> {
    fn new(ck: &'c Checker<'a>, module: usize) -> Self {
        Cx {
            ck,
            module,
            locals: Vec::new(),
            scopes: vec![HashMap::new()],
            ret: Some(Ty::Void),
            this: None,
            static_owner: None,
            in_ctor: false,
            init: None,
            loops: 0,
            captures: Vec::new(),
            subst: ck.empty.clone(),
            outer: Vec::new(),
            vars: Vec::new(),
            lambdas: Vec::new(),
            trace_file: None,
            block_defers: Vec::new(),
            loop_bases: Vec::new(),
            pending_label: None,
            in_try: false,
            pending_try: false,
            narrowed: Vec::new(),
            lambda_assigned: HashSet::new(),
        }
    }

    /// In a traced function, note that the next statement starts on `span`'s line.
    fn mark_line(&self, span: Span, out: &mut Vec<hir::Stmt>) {
        if self.trace_file.is_some() {
            out.push(hir::Stmt::Line(self.ck.line_of(span)));
        }
    }

    fn new_local(&mut self, name: &str, ty: Ty) -> LocalId {
        self.vars.push(VarInfo::default());
        self.locals.push(LocalInfo { name: name.to_string(), ty, var: self.vars.len() - 1 });
        self.locals.len() - 1
    }

    fn local_expr(&self, id: LocalId) -> hir::Expr {
        mk(K::Local(id), self.locals[id].ty)
    }

    /// Find a variable by name.  If it belongs to an enclosing function, the
    /// lambda(s) in between capture it, and the local standing for it here is returned.
    fn find_local(&mut self, name: &str) -> Option<LocalId> {
        if let Some(id) = lookup_scopes(&self.scopes, name) {
            return Some(id);
        }
        let (k, mut id) = (0..self.outer.len())
            .rev()
            .find_map(|k| lookup_scopes(&self.outer[k].scopes, name).map(|id| (k, id)))?;
        let (ty, var) = {
            let l = &self.outer[k].locals[id];
            (l.ty, l.var)
        };
        self.vars[var].captured = true;
        for j in (k + 1)..=self.outer.len() {
            let (locals, scopes, captures) = if j < self.outer.len() {
                let f = &mut self.outer[j];
                (&mut f.locals, &mut f.scopes, &mut f.captures)
            } else {
                (&mut self.locals, &mut self.scopes, &mut self.captures)
            };
            locals.push(LocalInfo { name: name.to_string(), ty, var });
            let hidden = locals.len() - 1;
            scopes[0].insert(name.to_string(), hidden);
            captures.push(hir::Capture { outer: id, inner: hidden });
            id = hidden;
        }
        Some(id)
    }

    fn mark_mutated(&mut self, id: LocalId) {
        let var = self.locals[id].var;
        self.vars[var].mutated = true;
    }

    fn name(&self, ty: Ty) -> String {
        self.ck.t.ty_name(ty)
    }

    fn this_expr(&mut self) -> Option<hir::Expr> {
        self.this?;
        let id = self.find_local("this")?;
        Some(self.local_expr(id))
    }

    /// Resolve a bare name: locals, then fields of `this`, then the module.
    fn resolve(&mut self, n: &str) -> Option<Named> {
        if let Some(id) = self.find_local(n) {
            return Some(Named::Local(id));
        }
        if let Some(ty) = self.this
            && let Some(i) = self.ck.t.ty(ty).fields.iter().position(|f| f.name == n)
        {
            return Some(Named::Field(i, self.ck.t.ty(ty).fields[i].ty));
        }
        match self.ck.t.lookup(self.module, n) {
            Some(Sym::Fn(id)) => Some(Named::Fn(id)),
            Some(Sym::Type(id)) => Some(Named::Type(id)),
            Some(Sym::Global(id)) => Some(Named::Global(id)),
            Some(Sym::GenericFn(_) | Sym::GenericType(_) | Sym::GenericIface(_)) => Some(Named::Generic),
            Some(Sym::Alias(a)) => Some(match self.ck.resolve_alias(a, Vec::new(), Span::default()) {
                Ok(Ty::Obj(id)) => Named::Type(id),
                _ => Named::Generic,
            }),
            None => self.ck.t.scopes[self.module].modules.get(n).map(|&m| Named::Module(m)),
        }
    }

    // ── lambdas ─────────────────────────────────────────────────

    fn enter_frame(&mut self, ret: Option<Ty>) {
        let saved = Frame {
            locals: take(&mut self.locals),
            scopes: replace(&mut self.scopes, vec![HashMap::new()]),
            ret: self.ret,
            this: self.this,
            in_ctor: self.in_ctor,
            init: self.init.take(),
            loops: self.loops,
            captures: take(&mut self.captures),
            block_defers: take(&mut self.block_defers),
            loop_bases: take(&mut self.loop_bases),
            in_try: self.in_try,
        };
        self.outer.push(saved);
        (self.ret, self.in_ctor, self.loops) = (ret, false, 0);
        self.in_try = replace(&mut self.pending_try, false);
    }

    fn exit_frame(&mut self) -> (Vec<LocalInfo>, Vec<hir::Capture>) {
        let f = self.outer.pop().expect("exit_frame pairs with enter_frame");
        let locals = replace(&mut self.locals, f.locals);
        let captures = replace(&mut self.captures, f.captures);
        (self.scopes, self.ret, self.this) = (f.scopes, f.ret, f.this);
        (self.in_ctor, self.init, self.loops) = (f.in_ctor, f.init, f.loops);
        (self.block_defers, self.loop_bases, self.in_try) = (f.block_defers, f.loop_bases, f.in_try);
        (locals, captures)
    }

    fn convert_locals(&self, locals: Vec<LocalInfo>) -> Vec<hir::Local> {
        locals
            .into_iter()
            .map(|l| {
                let v = &self.vars[l.var];
                hir::Local { name: l.name, ty: l.ty, boxed: v.captured && v.mutated }
            })
            .collect()
    }

    fn hir_locals(&mut self) -> Vec<hir::Local> {
        let locals = take(&mut self.locals);
        self.convert_locals(locals)
    }

    /// Hand the finished lambdas to the checker.  Called once the outermost
    /// function is done, when every variable's capture/mutation status is known.
    fn flush_lambdas(&mut self) {
        for l in take(&mut self.lambdas) {
            let mut body = l.body;
            if let Some(t) = &l.trace {
                body.insert(0, hir::Stmt::Line(t.line));
            }
            let func = hir::Func {
                trace: l.trace,
                symbol: format!("lambda__{}", l.id),
                kind: hir::FuncKind::Closure,
                ret: l.ret,
                n_params: l.n_params,
                locals: self.convert_locals(l.locals),
                body: Some(body),
                captures: l.captures,
            };
            self.ck.set_func(l.id, func);
        }
    }

    /// Check `{ params -> body }` against the function type it is expected to have.
    fn lambda(
        &mut self,
        params: &[ast::LambdaParam],
        explicit: bool,
        body: &[ast::Stmt],
        expected: Option<Ty>,
        span: Span,
    ) -> Result<hir::Expr> {
        let sig = match expected {
            Some(Ty::Fn(id)) => {
                let s = self.ck.t.fn_sig(id);
                Some(LamSig { params: s.params, ret: Some(s.ret) })
            }
            _ => None,
        };
        self.lambda_sig(params, explicit, body, sig, span)
    }

    /// Like [`Self::lambda`], with what is known about the expected type: the
    /// parameter types, and the result type if it is known yet.
    fn lambda_sig(
        &mut self,
        params: &[ast::LambdaParam],
        explicit: bool,
        body: &[ast::Stmt],
        sig: Option<LamSig>,
        span: Span,
    ) -> Result<hir::Expr> {

        let mut ps: Vec<(String, Ty)> = Vec::new();
        if explicit {
            if let Some(sig) = &sig
                && sig.params.len() != params.len()
            {
                return Err(Diagnostic::new(
                    format!(
                        "a function taking {} parameter(s) is expected here, but this lambda takes {}",
                        sig.params.len(),
                        params.len()
                    ),
                    span,
                ));
            }
            for (i, p) in params.iter().enumerate() {
                let written = p.ty.as_ref().map(|t| self.ck.resolve_value_type(self.module, t, &self.subst)).transpose()?;
                let wanted = sig.as_ref().map(|s| s.params[i]);
                let ty = match (written, wanted) {
                    (Some(w), Some(e)) if w != e => {
                        return Err(Diagnostic::new(
                            format!("`{}` is {}, but {} is expected here", p.name.name, self.name(w), self.name(e)),
                            p.name.span,
                        ));
                    }
                    (Some(t), _) | (None, Some(t)) => t,
                    (None, None) => {
                        return Err(Diagnostic::new(format!("cannot infer the type of `{}`", p.name.name), p.name.span)
                            .hint(format!("write `{}: Type`", p.name.name)));
                    }
                };
                ps.push((p.name.name.clone(), ty));
            }
        } else if let Some(sig) = &sig {
            match sig.params.len() {
                0 => {}
                1 => ps.push(("it".into(), sig.params[0])),
                n => {
                    return Err(Diagnostic::new(format!("this lambda must take {n} parameters"), span)
                        .hint("name them: `{ a, b -> … }`"));
                }
            }
        }
        let want_ret = sig.as_ref().and_then(|s| s.ret);

        let id = self.ck.reserve_fn();
        self.enter_frame(want_ret);
        for (n, t) in &ps {
            let l = self.new_local(n, *t);
            self.scopes[0].insert(n.clone(), l);
        }
        let result = self.lambda_body(body, want_ret, span);
        let (locals, captures) = self.exit_frame();
        let (stmts, ret) = result?;

        if captures.len() > MAX_CAPTURES {
            return Err(Diagnostic::new(format!("a lambda can capture at most {MAX_CAPTURES} variables"), span));
        }
        let trace = self.trace_file.as_ref().map(|file| hir::Trace {
            name: "<lambda>".into(),
            file: file.clone(),
            line: self.ck.line_of(span),
        });
        self.lambdas.push(PendingLambda { trace, id, ret, n_params: ps.len(), locals, body: stmts, captures });
        let ty = self.ck.t.intern_fn(ps.iter().map(|p| p.1).collect(), ret);
        Ok(mk(K::Closure { func: id }, ty))
    }

    /// The statements of a lambda; the last expression is its result.
    fn lambda_body(&mut self, body: &[ast::Stmt], want_ret: Option<Ty>, span: Span) -> Result<(Vec<hir::Stmt>, Ty)> {
        self.scopes.push(HashMap::new());
        self.block_defers.push(None);
        let saved = self.narrowed.clone();
        let mut out = Vec::new();
        let mut result = (want_ret == Some(Ty::Void)).then_some(Ty::Void);
        for (i, s) in body.iter().enumerate() {
            match &s.kind {
                StmtKind::Expr(e) if i + 1 == body.len() && want_ret != Some(Ty::Void) => {
                    self.mark_line(s.span, &mut out);
                    let v = self.expr_expected(e, want_ret)?;
                    let v = match want_ret {
                        Some(want) => {
                            if v.ty == Ty::Void {
                                return Err(Diagnostic::new(
                                    format!("this lambda must produce a {}", self.name(want)),
                                    e.span,
                                ));
                            }
                            self.coerce(v, want, e.span)?
                        }
                        None => v,
                    };
                    result = Some(v.ty);
                    if v.ty == Ty::Void {
                        out.push(hir::Stmt::Expr(v));
                    } else {
                        self.emit_return(Some(v), &mut out)?;
                    }
                }
                _ => self.stmt_narrowing(s, &mut out)?,
            }
        }
        self.narrowed = saved;
        if let Some(Some(mark)) = self.block_defers.pop()
            && !matches!(out.last(), Some(hir::Stmt::Return(_) | hir::Stmt::Break | hir::Stmt::Continue | hir::Stmt::JumpOut { .. }))
        {
            out.push(self.run_defers(mark)?);
        }
        self.scopes.pop();
        // A body that always `return`s (like a nested function's) needs no final value.
        if let (Some(want), None) = (want_ret, result)
            && always_returns(self.ck, &out)
        {
            result = Some(want);
        }
        match (want_ret, result) {
            (Some(want), None) => Err(Diagnostic::new(
                format!("this lambda must end with a {} value", self.name(want)),
                span,
            )),
            (None, None) => Ok((out, Ty::Void)),
            (_, Some(ty)) => Ok((out, ty)),
        }
    }

    /// A reference to a global, checking that top-level code doesn't use one
    /// before the line that defines it.
    fn global_use(&self, g: GlobalId, span: Span) -> Result<Ty> {
        let info = &self.ck.t.globals[g];
        if let Some(init) = &self.init
            && init.fn_id == info.init_fn
            && !init.declared.contains(&g)
        {
            return Err(Diagnostic::new(format!("`{}` is used before it is defined", info.name), span));
        }
        self.ck.global_ty(g, span)
    }

    // ── statements ──────────────────────────────────────────────

    /// Check statements in a fresh lexical scope.
    fn block(&mut self, stmts: &[ast::Stmt]) -> Result<Vec<hir::Stmt>> {
        self.scopes.push(HashMap::new());
        self.block_defers.push(None);
        let saved = self.narrowed.clone();
        let mut out = Vec::new();
        let result = stmts.iter().try_for_each(|s| self.stmt_narrowing(s, &mut out));
        self.narrowed = saved;
        let mark = self.block_defers.pop().flatten();
        self.scopes.pop();
        result?;
        if let Some(mark) = mark
            && !matches!(out.last(), Some(hir::Stmt::Return(_) | hir::Stmt::Break | hir::Stmt::Continue | hir::Stmt::JumpOut { .. }))
        {
            out.push(self.run_defers(mark)?);
        }
        Ok(out)
    }

    /// Call a function declared in the prelude (the helpers behind `defer` and `try`).
    fn prelude_call(&mut self, name: &str, args: Vec<hir::Expr>, span: Span) -> Result<hir::Expr> {
        let Some(&Sym::Fn(id)) = self.ck.t.scopes[0].items.get(name) else {
            return Err(Diagnostic::new(format!("the prelude has no `{name}`"), span));
        };
        self.call_values(id, None, args.into_iter().map(|a| (a, span)).collect(), span)
    }

    /// `__defer_run(mark)`: run the deferred closures registered since `mark`.
    fn run_defers(&mut self, mark: LocalId) -> Result<hir::Stmt> {
        let depth = self.local_expr(mark);
        Ok(hir::Stmt::Expr(self.prelude_call("__defer_run", vec![depth], Span::default())?))
    }

    /// `return value`, first running every `defer` of the blocks being left.
    fn emit_return(&mut self, value: Option<hir::Expr>, out: &mut Vec<hir::Stmt>) -> Result<()> {
        let outermost = self.block_defers.iter().flatten().next().copied();
        let Some(mark) = outermost else {
            out.push(hir::Stmt::Return(value));
            return Ok(());
        };
        // The value is computed before the deferred code runs.
        let value = match value {
            Some(v) => {
                let tmp = self.new_local("$result", v.ty);
                out.push(hir::Stmt::Let(tmp, Some(v)));
                Some(self.local_expr(tmp))
            }
            None => None,
        };
        out.push(self.run_defers(mark)?);
        out.push(hir::Stmt::Return(value));
        Ok(())
    }

    /// Run the `defer`s of the blocks a `break` / `continue` leaves.
    fn leave_loop_body(&mut self, out: &mut Vec<hir::Stmt>) -> Result<()> {
        let base = self.loop_bases.last().map_or(0, |l| l.0);
        if let Some(&mark) = self.block_defers[base.min(self.block_defers.len())..].iter().flatten().next() {
            out.push(self.run_defers(mark)?);
        }
        Ok(())
    }

    /// [`Self::stmt`], keeping the null-check knowledge of `narrow.rs` up to date.
    fn stmt_narrowing(&mut self, s: &ast::Stmt, out: &mut Vec<hir::Stmt>) -> Result<()> {
        self.mark_line(s.span, out);
        let after = self.kill_before(s);
        self.stmt(s, out)?;
        self.kill_after(&after);
        if let (StmtKind::Expr(e), Some(done)) = (&s.kind, out.last()) {
            self.guard_facts(e, done);
        }
        Ok(())
    }

    /// A top-level statement of a module: `x := …` defines a global.
    fn top_stmt(&mut self, s: &ast::Stmt, out: &mut Vec<hir::Stmt>) -> Result<()> {
        self.mark_line(s.span, out);
        let StmtKind::Let { name, init, .. } = &s.kind else { return self.stmt(s, out) };
        let Some(Sym::Global(gid)) = self.ck.t.scopes[self.module].items.get(&name.name).copied() else {
            unreachable!("declare registered every top-level `:=` as a global")
        };
        let declared = self.ck.t.globals[gid].ty.get();
        let init_span = init.as_ref().map(|e| e.span);
        let init = init.as_ref().map(|e| self.value_expected(e, declared)).transpose()?;
        let value = match (declared, init) {
            (Some(ty), Some(v)) => self.coerce(v, ty, init_span.unwrap())?,
            (Some(ty), None) => default_expr(ty),
            (None, Some(v)) => v,
            (None, None) => unreachable!("the parser requires a type or an initialiser"),
        };
        self.ck.t.globals[gid].ty.set(Some(value.ty));
        self.init.as_mut().unwrap().declared.insert(gid);
        out.push(hir::Stmt::SetGlobal(gid, value));
        Ok(())
    }

    fn stmt(&mut self, s: &ast::Stmt, out: &mut Vec<hir::Stmt>) -> Result<()> {
        match &s.kind {
            StmtKind::Let { name, ty, init } => {
                let declared = ty.as_ref().map(|t| self.ck.resolve_value_type(self.module, t, &self.subst)).transpose()?;
                let init_span = init.as_ref().map(|e| e.span);
                if let (Some(fn_ty @ Ty::Fn(_)), Some(lambda)) = (declared, init.as_ref())
                    && matches!(lambda.kind, ast::ExprKind::Lambda { .. })
                {
                    // `f: (Int) -> Int = { … f(n - 1) … }`: a function variable can call itself, so
                    // it exists before its body is checked, and is boxed so that the body sees the value.
                    if self.scopes.last().unwrap().contains_key(&name.name) {
                        return Err(Diagnostic::new(format!("`{}` is already defined in this scope", name.name), name.span));
                    }
                    let id = self.new_local(&name.name, fn_ty);
                    self.scopes.last_mut().unwrap().insert(name.name.clone(), id);
                    let value = self.value_expected(lambda, declared)?;
                    let value = self.coerce(value, fn_ty, lambda.span)?;
                    self.mark_mutated(id);
                    out.push(hir::Stmt::Let(id, None));
                    out.push(hir::Stmt::Set(id, value));
                    return Ok(());
                }
                let init = init.as_ref().map(|e| self.value_expected(e, declared)).transpose()?;
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

            StmtKind::Expr(e) => out.push(self.stmt_expr(e)?),

            StmtKind::While { cond: test, body } => {
                let label = self.pending_label.take();
                let cond = self.condition(test)?;
                let facts = self.facts(test, true);
                self.loops += 1;
                self.loop_bases.push((self.block_defers.len(), label));
                let body = self.with_facts(facts, |cx| cx.block(body));
                self.loop_bases.pop();
                self.loops -= 1;
                out.push(hir::Stmt::While { cond, body: body?, step: Vec::new() });
            }

            StmtKind::For { var, iter, body } => {
                let label = self.pending_label.take();
                self.for_loop(var, iter, body, label, out)?
            }

            StmtKind::Labeled { label, stmt } => {
                self.pending_label = Some(label.name.clone());
                let result = self.stmt(stmt, out);
                self.pending_label = None;
                result?
            }

            StmtKind::JumpTo { label, is_break } => self.jump_to(label, *is_break, out)?,

            StmtKind::WhenCheck { subject, covered } => self.when_check(subject, covered, s.span, out)?,

            // top-level `val`s are marked when the module is declared
            StmtKind::Val(name) => {
                if let Some(id) = lookup_scopes(&self.scopes, &name.name) {
                    let var = self.locals[id].var;
                    self.vars[var].immutable = true;
                }
            }
            StmtKind::Break | StmtKind::Continue => self.jump_stmt(matches!(s.kind, StmtKind::Break), s.span, out)?,

            StmtKind::Defer(body) => {
                let unit_fn = self.ck.t.intern_fn(Vec::new(), Ty::Void);
                let closure = self.lambda(&[], true, body, Some(unit_fn), s.span)?;
                if self.block_defers.last().is_some_and(Option::is_none) {
                    let depth = self.prelude_call("__defer_mark", Vec::new(), s.span)?;
                    let mark = self.new_local("$mark", Ty::Int);
                    out.push(hir::Stmt::Let(mark, Some(depth)));
                    *self.block_defers.last_mut().unwrap() = Some(mark);
                }
                let push = self.prelude_call("__defer_push", vec![closure], s.span)?;
                out.push(hir::Stmt::Expr(push));
            }

            StmtKind::Return(value) => self.return_stmt(value.as_ref(), s.span, out)?,
        }
        Ok(())
    }

    /// `return`, as a statement or (`x ?: return -1`) inside an expression.
    fn return_stmt(&mut self, value: Option<&ast::Expr>, span: Span, out: &mut Vec<hir::Stmt>) -> Result<()> {
        if self.in_try {
            return Err(Diagnostic::new("`return` is not allowed inside `try` or `catch`", span)
                .hint("set a variable inside, and return after the `try`; or make the `try` the last expression"));
        }
        let value = match (value, self.in_ctor, self.ret) {
            (None, true, _) => self.this_expr(),
            (Some(e), true, _) => {
                return Err(Diagnostic::new("a constructor cannot return a value", e.span));
            }
            (_, _, None) => {
                return Err(Diagnostic::new("`return` needs the function to declare its return type", span)
                    .hint("write `-> Type` on the function, or drop `return` and use `= expr`"));
            }
            (None, _, Some(Ty::Void)) => None,
            (None, _, Some(ret)) => {
                return Err(Diagnostic::new(format!("expected a {} to return", self.name(ret)), span));
            }
            (Some(e), _, Some(Ty::Void)) => {
                return Err(Diagnostic::new("this function does not return a value", e.span)
                    .hint("add `-> Type` to the signature"));
            }
            (Some(e), _, Some(ret)) => {
                let v = self.value_expected(e, Some(ret))?;
                Some(self.coerce(v, ret, e.span)?)
            }
        };
        self.emit_return(value, out)
    }

    /// `break` or `continue`, as a statement or inside an expression.
    fn jump_stmt(&mut self, is_break: bool, span: Span, out: &mut Vec<hir::Stmt>) -> Result<()> {
        if self.loops == 0 {
            let word = if is_break { "break" } else { "continue" };
            return Err(Diagnostic::new(format!("`{word}` outside of a loop"), span));
        }
        self.leave_loop_body(out)?;
        out.push(if is_break { hir::Stmt::Break } else { hir::Stmt::Continue });
        Ok(())
    }

    /// `break@outer` / `continue@outer`: jump out of the loop labelled `outer`, running the `defer`s on the way.
    fn jump_to(&mut self, label: &ast::Ident, is_break: bool, out: &mut Vec<hir::Stmt>) -> Result<()> {
        let word = if is_break { "break" } else { "continue" };
        let Some(at) = self.loop_bases.iter().rposition(|l| l.1.as_deref() == Some(label.name.as_str())) else {
            return Err(Diagnostic::new(format!("`{word}@{}` is not inside a loop labelled `{}`", label.name, label.name), label.span)
                .hint(format!("label a loop by writing `{}@` before it: `{}@ for …`", label.name, label.name)));
        };
        let base = self.loop_bases[at].0;
        if let Some(&mark) = self.block_defers[base.min(self.block_defers.len())..].iter().flatten().next() {
            out.push(self.run_defers(mark)?);
        }
        out.push(hir::Stmt::JumpOut { levels: self.loop_bases.len() - 1 - at, is_break });
        Ok(())
    }

    /// An expression used as a statement: its value (if any) is discarded.
    fn stmt_expr(&mut self, e: &ast::Expr) -> Result<hir::Stmt> {
        Ok(hir::Stmt::Expr(match &e.kind {
            A::If { cond: test, then, otherwise } => {
                let cond = self.condition(test)?;
                let (when_true, when_false) = (self.facts(test, true), self.facts(test, false));
                let then = self.with_facts(when_true, |cx| cx.block(then))?;
                let then = mk(K::Block(then, Box::new(unit())), Ty::Void);
                let otherwise = match otherwise {
                    Some(b) => {
                        let b = self.with_facts(when_false, |cx| cx.block(b))?;
                        mk(K::Block(b, Box::new(unit())), Ty::Void)
                    }
                    None => unit(),
                };
                mk(K::Cond { cond: Box::new(cond), then: Box::new(then), otherwise: Box::new(otherwise) }, Ty::Void)
            }
            A::Block(stmts) => mk(K::Block(self.block(stmts)?, Box::new(unit())), Ty::Void),
            A::Try { body, catch_name, handler } => {
                self.try_expr(body, catch_name.as_ref(), handler, Some(Ty::Void), e.span)?
            }
            _ => self.expr(e)?,
        }))
    }

    fn for_loop(&mut self, var: &ast::Ident, iter: &ast::Expr, body: &[ast::Stmt], label: Option<String>, out: &mut Vec<hir::Stmt>) -> Result<()> {
        let A::Range { start, end, inclusive } = &iter.kind else {
            return self.for_each(var, iter, body, label, out);
        };
        // The bounds are Ints, or Chars (`'a'..='z'`), and the loop variable has their type.
        let first = self.value(start)?;
        let kind = if first.ty == Ty::Char { Ty::Char } else { Ty::Int };
        let start = self.coerce(first, kind, start.span)?;
        let end_value = self.value(end)?;
        let end = self.coerce(end_value, kind, end.span)?;

        // for i in a..b { body }  ⇒  $i := a; $end := b; while $i < $end { i := $i; body; $i += 1 }
        self.scopes.push(HashMap::new());
        let counter = self.new_local("$i", kind);
        let limit = self.new_local("$end", kind);
        out.push(hir::Stmt::Let(counter, Some(start)));
        out.push(hir::Stmt::Let(limit, Some(end)));
        let cmp = if *inclusive { hir::BinOp::Le } else { hir::BinOp::Lt };
        let cond = mk(
            K::Binary(cmp, Box::new(self.local_expr(counter)), Box::new(self.local_expr(limit))),
            Ty::Bool,
        );

        self.scopes.push(HashMap::new());
        let v = self.new_local(&var.name, kind);
        self.scopes.last_mut().unwrap().insert(var.name.clone(), v);
        self.loops += 1;
        self.loop_bases.push((self.block_defers.len(), label));
        let inner = self.block(body);
        self.loop_bases.pop();
        self.loops -= 1;
        self.scopes.pop();
        self.scopes.pop();

        let mut full = vec![hir::Stmt::Let(v, Some(self.local_expr(counter)))];
        full.extend(inner?);
        let bump = mk(
            K::Binary(hir::BinOp::Add, Box::new(self.local_expr(counter)), Box::new(mk(K::Int(1), Ty::Int))),
            kind,
        );
        out.push(hir::Stmt::While { cond, body: full, step: vec![hir::Stmt::Set(counter, bump)] });
        Ok(())
    }

    /// `for x in collection { body }`, for anything with `size()` and `get(index)`:
    ///
    /// ```text
    /// $coll := collection; $i := 0
    /// while $i < $coll.size() { x := $coll.get($i); body; $i += 1 }
    /// ```
    fn for_each(&mut self, var: &ast::Ident, iter: &ast::Expr, body: &[ast::Stmt], label: Option<String>, out: &mut Vec<hir::Stmt>) -> Result<()> {
        let mut coll = self.value(iter)?;
        // A type can say what to loop over with `iter()` (a Map loops over its entries,
        // a String over its characters).
        for _ in 0..4 {
            let iter_fn = match coll.ty {
                Ty::Obj(t) => self.ck.t.ty(t).methods.get("iter").copied(),
                other => self.ck.ext_method(other, "iter"),
            };
            let Some(iter_fn) = iter_fn else { break };
            let before = coll.ty;
            let call = self.call_values(iter_fn, Some(coll), Vec::new(), iter.span)?;
            coll = self.virtual_on(call, before, "iter");
        }
        let looping = match coll.ty {
            Ty::Obj(t) => {
                let info = self.ck.t.ty(t);
                info.array_elem.is_some() || (info.methods.contains_key("size") && info.methods.contains_key("get"))
            }
            _ => false,
        };
        if !looping {
            return Err(Diagnostic::new(format!("cannot loop over a {}", self.name(coll.ty)), iter.span)
                .hint("`for` takes a range such as `0..10`, or a collection with `size()` and `get(index)`"));
        }
        self.scopes.push(HashMap::new());
        let all = self.new_local("$coll", coll.ty);
        let counter = self.new_local("$i", Ty::Int);
        out.push(hir::Stmt::Let(all, Some(coll)));
        out.push(hir::Stmt::Let(counter, Some(mk(K::Int(0), Ty::Int))));
        let size = self.size_of(self.local_expr(all), iter.span)?;
        let cond = mk(K::Binary(hir::BinOp::Lt, Box::new(self.local_expr(counter)), Box::new(size)), Ty::Bool);
        let item = self.get_element(self.local_expr(all), self.local_expr(counter), iter.span)?;

        self.scopes.push(HashMap::new());
        let v = self.new_local(&var.name, item.ty);
        self.scopes.last_mut().unwrap().insert(var.name.clone(), v);
        self.loops += 1;
        self.loop_bases.push((self.block_defers.len(), label));
        let inner = self.block(body);
        self.loop_bases.pop();
        self.loops -= 1;
        self.scopes.pop();
        self.scopes.pop();

        let mut full = vec![hir::Stmt::Let(v, Some(item))];
        full.extend(inner?);
        let bump = mk(
            K::Binary(hir::BinOp::Add, Box::new(self.local_expr(counter)), Box::new(mk(K::Int(1), Ty::Int))),
            Ty::Int,
        );
        out.push(hir::Stmt::While { cond, body: full, step: vec![hir::Stmt::Set(counter, bump)] });
        Ok(())
    }

    /// `collection[a..b]`  ⇒  `{ $s := collection; $s.slice(a, b) }`
    ///
    /// A missing start is 0, a missing end is `$s.size()`, and `..=b` ends at `b + 1`.
    fn slice_expr(
        &mut self,
        obj: &ast::Expr,
        start: Option<&ast::Expr>,
        end: Option<&ast::Expr>,
        inclusive: bool,
        span: Span,
    ) -> Result<hir::Expr> {
        let whole = self.value(obj)?;
        let method = match whole.ty {
            Ty::Obj(t) => self.ck.t.ty(t).methods.get("slice").copied(),
            other => self.ck.ext_method(other, "slice"),
        };
        let Some(method) = method else {
            return Err(Diagnostic::new(format!("a {} cannot be sliced", self.name(whole.ty)), span)
                .hint("slicing needs a `slice(from, to)` method"));
        };
        let tmp = self.new_local("$slice", whole.ty);
        let int = |cx: &mut Self, e: &ast::Expr| -> Result<hir::Expr> {
            let v = cx.value(e)?;
            cx.coerce(v, Ty::Int, e.span)
        };
        let from = match start {
            Some(e) => int(self, e)?,
            None => mk(K::Int(0), Ty::Int),
        };
        let to = match end {
            Some(e) => {
                let v = int(self, e)?;
                if inclusive {
                    mk(K::Binary(hir::BinOp::Add, Box::new(v), Box::new(mk(K::Int(1), Ty::Int))), Ty::Int)
                } else {
                    v
                }
            }
            None => {
                let all = self.local_expr(tmp);
                self.size_of(all, span)?
            }
        };
        let recv = self.local_expr(tmp);
        let whole_ty = whole.ty;
        let call = self.call_values(method, Some(recv), vec![(from, span), (to, span)], span)?;
        let call = self.virtual_on(call, whole_ty, "slice");
        let ty = call.ty;
        Ok(mk(K::Block(vec![hir::Stmt::Let(tmp, Some(whole))], Box::new(call)), ty))
    }

    /// `collection.size()`
    fn size_of(&mut self, coll: hir::Expr, span: Span) -> Result<hir::Expr> {
        if !matches!(coll.ty, Ty::Obj(_))
            && let Some(size) = self.ck.ext_method(coll.ty, "size")
        {
            let call = self.call_values(size, Some(coll), Vec::new(), span)?;
            return self.coerce(call, Ty::Int, span);
        }
        if let Ty::Obj(t) = coll.ty {
            let info = self.ck.t.ty(t);
            if info.array_elem.is_some() {
                return Ok(mk(K::ArrayLen(Box::new(coll)), Ty::Int));
            }
            if let Some(&size) = info.methods.get("size") {
                let ty = coll.ty;
                let call = self.call_values(size, Some(coll), Vec::new(), span)?;
                let call = self.virtual_on(call, ty, "size");
                return self.coerce(call, Ty::Int, span);
            }
        }
        Err(Diagnostic::new(format!("a {} has no `size()`", self.name(coll.ty)), span))
    }

    fn condition(&mut self, e: &ast::Expr) -> Result<hir::Expr> {
        let v = self.value(e)?;
        self.coerce(v, Ty::Bool, e.span)
    }

    // ── assignment ──────────────────────────────────────────────

    fn target(&mut self, target: &ast::Expr) -> Result<(Target, Ty)> {
        match &target.kind {
            A::Name(n) => match self.resolve(n) {
                Some(Named::Local(id)) => {
                    if self.vars[self.locals[id].var].immutable {
                        return Err(read_only(n, target.span));
                    }
                    Ok((Target::Local(id), self.locals[id].ty))
                }
                Some(Named::Field(index, ty)) => {
                    let obj = self.this_expr().expect("fields imply `this`");
                    Ok((Target::Field { obj, index }, ty))
                }
                Some(Named::Global(g)) => {
                    if self.ck.t.globals[g].immutable {
                        return Err(read_only(n, target.span));
                    }
                    Ok((Target::Global(g), self.global_use(g, target.span)?))
                }
                Some(_) => Err(Diagnostic::new(format!("cannot assign to `{n}`"), target.span)),
                None => Err(Diagnostic::new(format!("undefined variable `{n}`"), target.span)),
            },
            A::Field(obj, field) => {
                if self.enum_variant(obj, field)?.is_some() {
                    return Err(Diagnostic::new(
                        format!("cannot assign to `{}`: it is an enum variant", field.name),
                        field.span,
                    ));
                }
                if let Some(g) = self.module_global(obj, field)? {
                    if self.ck.t.globals[g].immutable {
                        return Err(read_only(&field.name, field.span));
                    }
                    return Ok((Target::Global(g), self.global_use(g, field.span)?));
                }
                let obj = self.value(obj)?;
                let (index, ty) = self.field_of(&obj, field)?;
                Ok((Target::Field { obj, index }, ty))
            }
            A::Index { obj, index } => {
                let obj = self.value(obj)?;
                let index = self.value(index)?;
                let ty = self.element_type(&obj, target.span)?;
                Ok((Target::Index { obj, index }, ty))
            }
            _ => unreachable!("the parser only accepts names, fields and indexing as targets"),
        }
    }

    fn assign(
        &mut self,
        target: &ast::Expr,
        op: AssignOp,
        value: &ast::Expr,
        span: Span,
        out: &mut Vec<hir::Stmt>,
    ) -> Result<()> {
        let (target, ty) = self.target(target)?;
        let value_span = value.span;
        let value = self.value_expected(value, (op == AssignOp::Set).then_some(ty))?;

        // A field target is evaluated once even for `a.b += 1`.
        let target = match target {
            Target::Field { obj, index } if op != AssignOp::Set && !matches!(obj.kind, K::Local(_)) => {
                let tmp = self.new_local("$recv", obj.ty);
                out.push(hir::Stmt::Let(tmp, Some(obj)));
                Target::Field { obj: self.local_expr(tmp), index }
            }
            Target::Index { obj, index } if op != AssignOp::Set => {
                let mut spill = |cx: &mut Self, e: hir::Expr, name: &str| {
                    if matches!(e.kind, K::Local(_) | K::Int(_)) {
                        return e;
                    }
                    let tmp = cx.new_local(name, e.ty);
                    out.push(hir::Stmt::Let(tmp, Some(e)));
                    cx.local_expr(tmp)
                };
                let obj = spill(self, obj, "$recv");
                let index = spill(self, index, "$idx");
                Target::Index { obj, index }
            }
            other => other,
        };

        let new = if op == AssignOp::Set {
            self.coerce(value, ty, value_span)?
        } else {
            let bin = match op {
                AssignOp::Add => ast::BinOp::Add,
                AssignOp::Sub => ast::BinOp::Sub,
                AssignOp::Mul => ast::BinOp::Mul,
                _ => ast::BinOp::Div,
            };
            let current = self.read(&target, ty, span)?;
            let r = self.binary(bin, current, value, span)?;
            self.coerce(r, ty, span)?
        };
        if let Target::Local(id) = target {
            self.mark_mutated(id);
        }
        out.push(match target {
            Target::Local(id) => hir::Stmt::Set(id, new),
            Target::Global(g) => hir::Stmt::SetGlobal(g, new),
            Target::Field { obj, index } => hir::Stmt::SetField { obj, index, value: new },
            Target::Index { obj, index } => self.set_element(obj, index, new, span)?,
        });
        Ok(())
    }

    /// Read the current value of a target (for `+=` and friends).
    fn read(&mut self, target: &Target, ty: Ty, span: Span) -> Result<hir::Expr> {
        Ok(match target {
            Target::Local(id) => self.local_expr(*id),
            Target::Global(g) => mk(K::Global(*g), ty),
            Target::Field { obj, index } => {
                let K::Local(id) = obj.kind else { unreachable!("compound targets are spilled to locals") };
                mk(K::Field { obj: Box::new(self.local_expr(id)), index: *index }, ty)
            }
            Target::Index { obj, index } => {
                let (obj, index) = (self.copy_of(obj), self.copy_of(index));
                self.get_element(obj, index, span)?
            }
        })
    }

    /// A second use of an already-spilled expression (a local or a constant).
    fn copy_of(&self, e: &hir::Expr) -> hir::Expr {
        match e.kind {
            K::Local(id) => self.local_expr(id),
            K::Int(v) => mk(K::Int(v), e.ty),
            _ => unreachable!("compound targets are spilled to locals"),
        }
    }

    // ── indexing ────────────────────────────────────────────────

    /// The type of `obj[…]` when read (`get`) or written (`set`'s value).
    fn element_type(&self, obj: &hir::Expr, span: Span) -> Result<Ty> {
        if let Ty::Obj(t) = obj.ty {
            let info = self.ck.t.ty(t);
            if let Some(elem) = info.array_elem {
                return Ok(elem);
            }
            // `this`, the index and the value
            if let Some(&set) = info.methods.get("set")
                && let [_, _, value] = self.ck.func(set).params.as_slice()
            {
                return Ok(value.ty);
            }
        }
        // `bytes[i] = v`: a `set` method added to a built-in type
        if !matches!(obj.ty, Ty::Obj(_))
            && let Some(set) = self.ck.ext_method(obj.ty, "set")
            && let [_, _, value] = self.ck.func(set).params.as_slice()
        {
            return Ok(value.ty);
        }
        Err(Diagnostic::new(format!("cannot assign to an element of a {}", self.name(obj.ty)), span)
            .hint("indexing assignment needs a `set(index, value)` method"))
    }

    /// `obj[index]`
    fn get_element(&mut self, obj: hir::Expr, index: hir::Expr, span: Span) -> Result<hir::Expr> {
        // `"text"[i]`: a `get` method added to a built-in type
        if !matches!(obj.ty, Ty::Obj(_))
            && let Some(get) = self.ck.ext_method(obj.ty, "get")
        {
            return self.call_values(get, Some(obj), vec![(index, span)], span);
        }
        if let Ty::Obj(t) = obj.ty {
            let info = self.ck.t.ty(t);
            if let Some(elem) = info.array_elem {
                let index = self.coerce(index, Ty::Int, span)?;
                return Ok(mk(K::Elem { array: Box::new(obj), index: Box::new(index) }, elem));
            }
            if let Some(&get) = info.methods.get("get") {
                let ty = obj.ty;
                let call = self.call_values(get, Some(obj), vec![(index, span)], span)?;
                return Ok(self.virtual_on(call, ty, "get"));
            }
        }
        Err(Diagnostic::new(format!("a {} cannot be indexed", self.name(obj.ty)), span)
            .hint("indexing needs a `get(index)` method"))
    }

    /// `obj[index] = value`
    fn set_element(&mut self, obj: hir::Expr, index: hir::Expr, value: hir::Expr, span: Span) -> Result<hir::Stmt> {
        if !matches!(obj.ty, Ty::Obj(_))
            && let Some(set) = self.ck.ext_method(obj.ty, "set")
        {
            let call = self.call_values(set, Some(obj), vec![(index, span), (value, span)], span)?;
            return Ok(hir::Stmt::Expr(call));
        }
        if let Ty::Obj(t) = obj.ty {
            let info = self.ck.t.ty(t);
            if let Some(elem) = info.array_elem {
                let index = self.coerce(index, Ty::Int, span)?;
                let value = self.coerce(value, elem, span)?;
                return Ok(hir::Stmt::SetElem { array: obj, index, value });
            }
            if let Some(&set) = info.methods.get("set") {
                let ty = obj.ty;
                let call = self.call_values(set, Some(obj), vec![(index, span), (value, span)], span)?;
                return Ok(hir::Stmt::Expr(self.virtual_on(call, ty, "set")));
            }
        }
        Err(Diagnostic::new(format!("cannot assign to an element of a {}", self.name(obj.ty)), span)
            .hint("indexing assignment needs a `set(index, value)` method"))
    }

    /// Call `id` with arguments that are already checked: positional, no defaults.
    fn call_values(
        &mut self,
        id: FnId,
        recv: Option<hir::Expr>,
        values: Vec<(hir::Expr, Span)>,
        span: Span,
    ) -> Result<hir::Expr> {
        let ck = self.ck;
        let info = ck.func(id);
        let ret = ck.ret_of(id, span)?;
        let params = &info.params[usize::from(recv.is_some())..];
        if params.len() != values.len() {
            return Err(Diagnostic::new(
                format!("`{}` takes {} argument(s) but {} were given", info.name, params.len(), values.len()),
                span,
            ));
        }
        let mut args: Vec<hir::Expr> = recv.into_iter().collect();
        for (p, (v, at)) in params.iter().zip(values) {
            args.push(self.coerce(v, p.ty, at)?);
        }
        Ok(mk(K::Call { func: id, args }, ret))
    }

    // ── arrays ──────────────────────────────────────────────────

    /// `Array(size, fill)` / `Array<T>(size, fill)`
    fn array_new(
        &mut self,
        type_args: &[ast::TypeRef],
        args: &[ast::Arg],
        span: Span,
        expected: Option<Ty>,
    ) -> Result<hir::Expr> {
        let hint = match (type_args, expected) {
            ([t], _) => Some(self.ck.resolve_value_type(self.module, t, &self.subst)?),
            ([], Some(Ty::Obj(t))) => self.ck.t.ty(t).array_elem,
            ([], _) => None,
            (_, _) => return Err(Diagnostic::new("`Array` takes 1 type argument, e.g. `Array<Int>`", span)),
        };
        let (len, fill) = match args {
            [len] => (len, None),
            [len, fill] => (len, Some(fill)),
            _ => {
                return Err(Diagnostic::new(
                    "`Array` is created with a size and optionally a fill value: `Array(10, 0)`",
                    span,
                ));
            }
        };
        if let Some(n) = args.iter().find_map(|a| a.name.as_ref()) {
            return Err(Diagnostic::new("`Array` doesn't take named arguments", n.span));
        }
        let len_v = self.value_expected(&len.value, Some(Ty::Int))?;
        let len_v = self.coerce(len_v, Ty::Int, len.value.span)?;
        let (elem, fill_v) = match fill {
            Some(fill) => {
                let v = self.value_expected(&fill.value, hint)?;
                let elem = hint.unwrap_or(v.ty);
                (elem, self.coerce(v, elem, fill.value.span)?)
            }
            // Without a fill value: the element type's zero (0, 0.0, false, "" or null).
            None => {
                let Some(elem) = hint else {
                    return Err(Diagnostic::new("cannot infer the element type of this Array", span)
                        .hint("write `Array<Int>(size)`, or give a fill value: `Array(size, 0)`"));
                };
                (elem, default_expr(elem))
            }
        };
        Ok(mk(
            K::ArrayNew { elem, len: Box::new(len_v), fill: Box::new(fill_v) },
            self.ck.t.array_of(elem),
        ))
    }

    /// `try { body } catch e { handler }`
    ///
    /// Both parts become closures handed to the runtime's `aion_try`, which runs the
    /// body and, if anything panics, calls the handler with the message.  The result
    /// type is the expected type if there is one, else the body's.
    fn try_expr(
        &mut self,
        body: &[ast::Stmt],
        catch_name: Option<&ast::Ident>,
        handler: &[ast::Stmt],
        expected: Option<Ty>,
        span: Span,
    ) -> Result<hir::Expr> {
        let want = expected.filter(|t| *t != Ty::Null);
        self.pending_try = true;
        let body_fn = self.lambda_sig(&[], true, body, Some(LamSig { params: Vec::new(), ret: want }), span)?;
        let Ty::Fn(sig) = body_fn.ty else { unreachable!("a lambda is a function") };
        let result = self.ck.t.fn_sig(sig).ret;

        let named: Vec<ast::LambdaParam> =
            catch_name.map(|n| ast::LambdaParam { name: n.clone(), ty: None }).into_iter().collect();
        self.pending_try = true;
        let sig = Some(LamSig { params: vec![Ty::Str], ret: Some(result) });
        let handler_fn = self.lambda_sig(&named, catch_name.is_some(), handler, sig, span)?;

        let runner = match result {
            Ty::Void => "__try",
            Ty::Int => "__try_int",
            Ty::Float => "__try_float",
            Ty::Bool => "__try_bool",
            _ => "__try_ptr",
        };
        let Some(&Sym::Fn(func)) = self.ck.t.scopes[0].items.get(runner) else {
            return Err(Diagnostic::new(format!("the prelude has no `{runner}`"), span));
        };
        self.ck.ret_of(func, span)?;
        Ok(mk(K::Call { func, args: vec![body_fn, handler_fn] }, result))
    }

    /// `hashOf(value)`
    fn hash_of(&mut self, args: &[ast::Arg], span: Span) -> Result<hir::Expr> {
        let [arg] = args else {
            return Err(Diagnostic::new("`hashOf` takes 1 argument", span));
        };
        let v = self.value(&arg.value)?;
        if matches!(v.ty, Ty::Fn(_)) {
            return Err(Diagnostic::new("a function has no hash", arg.value.span));
        }
        if v.ty == Ty::Bytes {
            return self.prelude_call("__bytes_hash", vec![v], span);
        }
        if let Some(hash) = self.custom_hash(&v) {
            return Ok(self.object_hash(hash, v));
        }
        Ok(mk(K::Hash(Box::new(v)), Ty::Int))
    }

    /// The `hashCode() -> Int` method of an object (or optional object) type, if it has one.
    fn custom_hash(&self, v: &hir::Expr) -> Option<FnId> {
        let ty = match v.ty {
            Ty::Opt(o) => self.ck.t.opt_inner(o),
            other => other,
        };
        let Ty::Obj(t) = ty else { return None };
        let info = self.ck.t.ty(t);
        if info.iface.is_some() || info.array_elem.is_some() {
            return None;
        }
        let &f = info.methods.get("hashCode")?;
        let fits = self.ck.func(f).params.len() == 1;
        (fits && self.ck.ret_of(f, Span::default()).ok()? == Ty::Int).then_some(f)
    }

    /// `v.hashCode()` made non-negative (a map uses it as an index); null hashes to 0.
    fn object_hash(&mut self, hash: FnId, v: hir::Expr) -> hir::Expr {
        const MODULUS: i64 = 2_147_483_647;
        let ty = v.ty;
        let tmp = self.new_local("$obj", ty);
        let raw = self.new_local("$hash", Ty::Int);
        let int = |n: i64| mk(K::Int(n), Ty::Int);
        let object = match ty {
            Ty::Opt(o) => mk(K::Unwrap(Box::new(self.local_expr(tmp))), self.ck.t.opt_inner(o)),
            _ => self.local_expr(tmp),
        };
        let object_ty = object.ty;
        let call = mk(K::Call { func: hash, args: vec![object] }, Ty::Int);
        let call = self.virtual_on(call, object_ty, "hashCode");
        let reduced = |cx: &Self| {
            mk(K::Binary(hir::BinOp::Rem, Box::new(cx.local_expr(raw)), Box::new(int(MODULUS))), Ty::Int)
        };
        let shifted = mk(K::Binary(hir::BinOp::Add, Box::new(reduced(self)), Box::new(int(MODULUS))), Ty::Int);
        let negative = mk(K::Binary(hir::BinOp::Lt, Box::new(self.local_expr(raw)), Box::new(int(0))), Ty::Bool);
        let nonneg = mk(K::Cond { cond: Box::new(negative), then: Box::new(shifted), otherwise: Box::new(reduced(self)) }, Ty::Int);
        let is_null = mk(K::IsNull(Box::new(self.local_expr(tmp))), Ty::Bool);
        let body = mk(
            K::Block(vec![hir::Stmt::Let(raw, Some(call))], Box::new(nonneg)),
            Ty::Int,
        );
        let result = mk(K::Cond { cond: Box::new(is_null), then: Box::new(int(0)), otherwise: Box::new(body) }, Ty::Int);
        mk(K::Block(vec![hir::Stmt::Let(tmp, Some(v))], Box::new(result)), Ty::Int)
    }

    // ── collection literals ─────────────────────────────────────

    /// The built-in-ish generic `List` / `Map` from the prelude, instantiated.
    fn collection(&mut self, name: &str, args: Vec<Ty>, span: Span) -> Result<TypeId> {
        match self.ck.t.scopes[0].items.get(name) {
            Some(&Sym::GenericType(g)) => self.ck.instantiate_type(g, args, span),
            _ => Err(Diagnostic::new(format!("the prelude has no `{name}`"), span)),
        }
    }

    /// The type arguments of the expected type, if it is an instance of the generic type `name`.
    fn expected_args(&self, expected: Option<Ty>, name: &str) -> Option<Vec<Ty>> {
        let Some(Ty::Obj(t)) = expected else { return None };
        let (g, args) = self.ck.instance_of.borrow().get(&t)?.clone();
        (self.ck.generic_types.borrow()[g].decl.name.name == name).then_some(args)
    }

    /// `[a, b, c]`  ⇒  `{ $l := List<T>(); $l.add(a); $l.add(b); $l.add(c); $l }`
    fn list_literal(&mut self, items: &[ast::Expr], expected: Option<Ty>, span: Span) -> Result<hir::Expr> {
        self.list_from(items.iter().map(|e| (e, None)).collect(), expected, span)
    }

    /// A list of `items`, which may already be evaluated (for a `*spread`, the list inside).
    fn list_from(&mut self, items: Vec<(&ast::Expr, Option<hir::Expr>)>, expected: Option<Ty>, span: Span) -> Result<hir::Expr> {
        let hint = self.expected_args(expected, "List").map(|a| a[0]);
        let mut rows = Vec::new();
        let mut elem = hint;
        for (item, done) in items {
            if let A::Spread(inner) = &item.kind {
                let want = match elem {
                    Some(e) => Some(Ty::Obj(self.collection("List", vec![e], inner.span)?)),
                    None => None,
                };
                let v = match done {
                    Some(v) => v,
                    None => self.value_expected(inner, want)?,
                };
                let Some(found) = self.expected_args(Some(v.ty), "List") else {
                    return Err(Diagnostic::new(format!("`*` spreads a list, not a {}", self.name(v.ty)), inner.span));
                };
                let e = *elem.get_or_insert(found[0]);
                let list = Ty::Obj(self.collection("List", vec![e], inner.span)?);
                rows.push(("addAll", vec![(self.coerce(v, list, inner.span)?, inner.span)]));
            } else {
                let v = match done {
                    Some(v) => v,
                    None => self.value_expected(item, elem)?,
                };
                let want = *elem.get_or_insert(v.ty);
                if v.ty != want && self.expected_args(Some(v.ty), "List").is_some() && self.expected_args(Some(want), "List").is_none() {
                    return Err(Diagnostic::new(format!("expected {}, found {}", self.name(want), self.name(v.ty)), item.span)
                        .hint("to pass the items of a list one by one, write `*xs`"));
                }
                rows.push(("add", vec![(self.coerce(v, want, item.span)?, item.span)]));
            }
        }
        let Some(elem) = elem else {
            return Err(Diagnostic::new("cannot infer the element type of an empty list", span)
                .hint("write `List<Int>()`, or give the variable a type: `xs: List<Int> = []`"));
        };
        let ty = self.collection("List", vec![elem], span)?;
        self.fill_collection(ty, rows, span)
    }

    /// `{ k: v, … }`  ⇒  `{ $m := Map<K, V>(); $m.set(k, v); …; $m }`
    fn map_literal(&mut self, entries: &[(ast::Expr, ast::Expr)], expected: Option<Ty>, span: Span) -> Result<hir::Expr> {
        let hint = self.expected_args(expected, "Map");
        let (mut key_ty, mut val_ty) = (hint.as_ref().map(|a| a[0]), hint.as_ref().map(|a| a[1]));
        let mut rows = Vec::new();
        for (k, v) in entries {
            let kv = self.value_expected(k, key_ty)?;
            let kt = *key_ty.get_or_insert(kv.ty);
            let vv = self.value_expected(v, val_ty)?;
            let vt = *val_ty.get_or_insert(vv.ty);
            rows.push(("set", vec![(self.coerce(kv, kt, k.span)?, k.span), (self.coerce(vv, vt, v.span)?, v.span)]));
        }
        let (Some(k), Some(v)) = (key_ty, val_ty) else {
            return Err(Diagnostic::new("cannot infer the types of an empty map", span)
                .hint("write `Map<String, Int>()`"));
        };
        let ty = self.collection("Map", vec![k, v], span)?;
        self.fill_collection(ty, rows, span)
    }

    /// A new `ty` with a method called once per row: `(method, arguments)`.
    fn fill_collection(&mut self, ty: TypeId, rows: Vec<(&str, Vec<(hir::Expr, Span)>)>, span: Span) -> Result<hir::Expr> {
        let info = self.ck.t.ty(ty);
        let obj_ty = Ty::Obj(ty);
        let tmp = self.new_local("$lit", obj_ty);
        let make = self.call_values(info.ctor, None, Vec::new(), span)?;
        let mut stmts = vec![hir::Stmt::Let(tmp, Some(make))];
        for (method, row) in rows {
            let add = *info
                .methods
                .get(method)
                .ok_or_else(|| Diagnostic::new(format!("`{}` has no `{method}` method", info.name), span))?;
            let call = self.call_values(add, Some(self.local_expr(tmp)), row, span)?;
            stmts.push(hir::Stmt::Expr(call));
        }
        Ok(mk(K::Block(stmts, Box::new(self.local_expr(tmp))), obj_ty))
    }

    /// `array.size()`, `array.get(i)`, `array.set(i, v)`
    fn array_method(
        &mut self,
        recv: hir::Expr,
        member: &ast::Ident,
        args: &[ast::Arg],
        span: Span,
    ) -> Result<hir::Expr> {
        let elem = match recv.ty {
            Ty::Obj(t) => self.ck.t.ty(t).array_elem.expect("checked by the caller"),
            _ => unreachable!(),
        };
        if let Some(n) = args.iter().find_map(|a| a.name.as_ref()) {
            return Err(Diagnostic::new("array methods don't take named arguments", n.span));
        }
        let arity = |want: usize| -> Result<()> {
            if args.len() == want {
                return Ok(());
            }
            Err(Diagnostic::new(
                format!("`{}` takes {want} argument(s) but {} were given", member.name, args.len()),
                span,
            ))
        };
        match member.name.as_str() {
            "size" => {
                arity(0)?;
                Ok(mk(K::ArrayLen(Box::new(recv)), Ty::Int))
            }
            "get" => {
                arity(1)?;
                let index = self.value(&args[0].value)?;
                self.get_element(recv, index, args[0].value.span)
            }
            "set" => {
                arity(2)?;
                let index = self.value(&args[0].value)?;
                let value = self.value_expected(&args[1].value, Some(elem))?;
                let stmt = self.set_element(recv, index, value, span)?;
                Ok(mk(K::Block(vec![stmt], Box::new(unit())), Ty::Void))
            }
            // Reset a slot to the zero value of the element type, so it stops keeping an object alive.
            "clear" => {
                arity(1)?;
                let index = self.value(&args[0].value)?;
                let stmt = self.set_element(recv, index, default_expr(elem), span)?;
                Ok(mk(K::Block(vec![stmt], Box::new(unit())), Ty::Void))
            }
            other => Err(Diagnostic::new(format!("an Array has no method `{other}`"), member.span)
                .hint("an Array has `size()`, `get(i)`, `set(i, value)` and `clear(i)`; use a List to grow")),
        }
    }

    /// A `_private` name of another module cannot be used from here.
    fn check_public(&self, module: usize, name: &ast::Ident) -> Result<()> {
        if module != self.module && super::is_private(&name.name) {
            return Err(super::private_error(&name.name, &self.ck.module_names[module], name.span));
        }
        Ok(())
    }

    /// `module.name` where `module` is an imported module alias and `name` a global.
    /// `Color.Red` / `module.Color.Red`: the variable holding an enum's variant.
    fn enum_variant(&mut self, obj: &ast::Expr, field: &ast::Ident) -> Result<Option<GlobalId>> {
        let Some(ty) = self.static_receiver(obj)? else { return Ok(None) };
        let info = self.ck.t.ty(ty);
        if info.variants.is_none() {
            return Ok(None);
        }
        match info.variant_globals.get(&field.name) {
            Some(&g) => Ok(Some(g)),
            None => Err(Diagnostic::new(format!("`{}` has no variant `{}`", info.name, field.name), field.span)
                .hint(format!("its variants are {}", info.variants.iter().flatten().cloned().collect::<Vec<_>>().join(", ")))),
        }
    }

    /// The branch a `when` without `else` falls into.  When it is over an enum, every variant must have
    /// a branch, and falling through is impossible (it panics, so the `when` can be used as a value).
    fn when_check(&mut self, subject: &str, covered: &[ast::Ident], span: Span, out: &mut Vec<hir::Stmt>) -> Result<()> {
        let Some(local) = self.find_local(subject) else { return Ok(()) };
        let Ty::Obj(ty) = self.locals[local].ty else { return Ok(()) };
        let info = self.ck.t.ty(ty);
        let Some(all) = &info.variants else { return Ok(()) };
        let missing: Vec<String> =
            all.iter().filter(|v| !covered.iter().any(|c| &c.name == *v)).map(|v| format!("`{}.{v}`", info.name)).collect();
        if !missing.is_empty() {
            return Err(Diagnostic::new(format!("this `when` does not cover {}", missing.join(", ")), span)
                .hint("add a branch for each, or an `else ->` branch"));
        }
        let message = mk(K::Str("no branch of the `when` matched".into()), Ty::Str);
        out.push(hir::Stmt::Expr(self.prelude_call("panic", vec![message], span)?));
        Ok(())
    }

    fn module_global(&mut self, obj: &ast::Expr, field: &ast::Ident) -> Result<Option<GlobalId>> {
        let A::Name(a) = &obj.kind else { return Ok(None) };
        let Some(Named::Module(m)) = self.resolve(a) else { return Ok(None) };
        self.check_public(m, field)?;
        match self.ck.t.scopes[m].items.get(&field.name) {
            Some(&Sym::Global(g)) => Ok(Some(g)),
            // Functions and types are handled by the callers (calls, function values).
            _ => Ok(None),
        }
    }

    // ── conversions ─────────────────────────────────────────────

    /// Reject expressions that produce no value.
    fn value(&mut self, e: &ast::Expr) -> Result<hir::Expr> {
        self.value_expected(e, None)
    }

    fn value_expected(&mut self, e: &ast::Expr, expected: Option<Ty>) -> Result<hir::Expr> {
        let v = self.expr_expected(e, expected)?;
        if v.ty == Ty::Void {
            return Err(Diagnostic::new("this expression does not produce a value", e.span));
        }
        // `null` only means something where an optional is expected.
        if v.ty == Ty::Null && expected.is_none() {
            return Err(Diagnostic::new("cannot tell what type this `null` has", e.span)
                .hint("give the variable a type: `x: Int? = null`"));
        }
        Ok(v)
    }

    /// Check an expression that is expected to have type `expected`.  Only
    /// lambdas use the hint: it supplies their parameter types.
    fn expr_expected(&mut self, e: &ast::Expr, expected: Option<Ty>) -> Result<hir::Expr> {
        match &e.kind {
            A::Lambda { params, explicit, body } => self.lambda(params, *explicit, body, expected, e.span),
            A::Call { callee, type_args, args } => self.call(callee, type_args, args, e.span, expected),
            A::List(items) => self.list_literal(items, expected, e.span),
            A::Map(entries) => self.map_literal(entries, expected, e.span),
            A::Try { body, catch_name, handler } => {
                self.try_expr(body, catch_name.as_ref(), handler, expected, e.span)
            }
            A::If { cond, then, otherwise } => self.if_value(cond, then, otherwise.as_deref(), e.span, expected),
            A::Block(stmts) => self.block_value(stmts, expected),
            _ => self.expr(e),
        }
    }

    fn coerce(&self, e: hir::Expr, to: Ty, span: Span) -> Result<hir::Expr> {
        match (e.ty, to) {
            (a, b) if a == b => Ok(e),
            (Ty::Int, Ty::Float) => Ok(mk(K::IntToFloat(Box::new(e)), Ty::Float)),
            (Ty::Null, Ty::Opt(_)) => Ok(mk(K::Null, to)),
            // a subtype used as its parent type (same pointer)
            (Ty::Obj(a), Ty::Obj(b)) if self.ck.is_subtype(a, b) => Ok(mk(K::Upcast(Box::new(e)), to)),
            (Ty::Opt(x), Ty::Opt(y))
                if matches!(
                    (self.ck.t.opt_inner(x), self.ck.t.opt_inner(y)),
                    (Ty::Obj(a), Ty::Obj(b)) if self.ck.is_subtype(a, b)
                ) =>
            {
                Ok(mk(K::Upcast(Box::new(e)), to))
            }
            // an object used as an interface it implements (same pointer)
            (Ty::Obj(a), Ty::Obj(b)) if self.is_upcast(a, b) => self.upcast(e, a, b, to, span),
            (Ty::Opt(x), Ty::Opt(y))
                if matches!(
                    (self.ck.t.opt_inner(x), self.ck.t.opt_inner(y)),
                    (Ty::Obj(a), Ty::Obj(b)) if self.is_upcast(a, b)
                ) =>
            {
                let (Ty::Obj(a), Ty::Obj(b)) = (self.ck.t.opt_inner(x), self.ck.t.opt_inner(y)) else { unreachable!() };
                self.upcast(e, a, b, to, span)
            }
            // `T` → `T?` (and `Int` → `Float?`)
            (found, Ty::Opt(id)) if !matches!(found, Ty::Opt(_) | Ty::Null | Ty::Void) => {
                let inner = self.ck.t.opt_inner(id);
                match self.coerce(e, inner, span) {
                    Ok(v) => Ok(mk(K::Wrap(Box::new(v)), to)),
                    Err(_) => Err(self.mismatch(to, found, span)),
                }
            }
            (found, _) => Err(self.mismatch(to, found, span)),
        }
    }

    /// A call to a method that subtypes may replace goes through the object's real type.
    fn virtual_if_open(&self, call: hir::Expr, virtuals: &HashMap<String, (TypeId, usize)>, name: &str) -> hir::Expr {
        match virtuals.get(name) {
            Some(&(root, index)) => self.virtualize(call, root, index),
            None => call,
        }
    }

    /// A call of `name` made on a value of type `ty` through sugar (`xs[i]`, `for`, `a..b` slices): looked up
    /// through the real type when the method is `open`, like a written-out call.
    fn virtual_on(&self, call: hir::Expr, ty: Ty, name: &str) -> hir::Expr {
        match ty {
            Ty::Obj(t) => self.virtual_if_open(call, &self.ck.t.ty(t).virtuals, name),
            _ => call,
        }
    }

    /// Turn a direct call into a call looked up through the receiver's real type.
    fn virtualize(&self, e: hir::Expr, root: TypeId, index: usize) -> hir::Expr {
        match e.kind {
            K::Call { args, .. } => hir::Expr { kind: K::CallIface { iface: root, method: index, args }, ty: e.ty },
            K::Block(stmts, tail) => {
                let ty = e.ty;
                hir::Expr { kind: K::Block(stmts, Box::new(self.virtualize(*tail, root, index))), ty }
            }
            _ => e,
        }
    }

    /// `super.method(args)`: the parent type's version of a method, called directly.
    fn super_call(
        &mut self,
        member: &ast::Ident,
        type_args: &[ast::TypeRef],
        args: &[ast::Arg],
        span: Span,
    ) -> Result<hir::Expr> {
        let Some(this) = self.this else {
            return Err(Diagnostic::new("`super` can only be used inside a method", member.span));
        };
        let own = self.ck.t.ty(this);
        let Some(parent) = own.parent else {
            return Err(Diagnostic::new(format!("`{}` does not extend another type, so it has no `super`", own.name), member.span));
        };
        let p = self.ck.t.ty(parent);
        let Some(&id) = p.methods.get(&member.name) else {
            return Err(Diagnostic::new(format!("`{}` has no method `{}`", p.name, member.name), member.span));
        };
        if !type_args.is_empty() {
            return Err(Diagnostic::new(
                format!("`{}` is not generic, so it takes no type arguments", member.name),
                member.span,
            ));
        }
        let recv = self.this_expr();
        self.call_fn(id, recv, args, span)
    }

    /// The class named on the right of `is` / `as`, checked against what the value can be.
    fn class_target(&mut self, value: &hir::Expr, ty: &ast::TypeRef, span: Span) -> Result<TypeId> {
        let target = match self.ck.resolve_type(self.module, ty, &self.subst)? {
            Ty::Obj(t) if self.ck.t.ty(t).iface.is_none() && self.ck.t.ty(t).array_elem.is_none() => t,
            other => {
                return Err(Diagnostic::new(
                    format!("`is` and `as` can only name a type declared with `type`, not {}", self.name(other)),
                    ty.span,
                ));
            }
        };
        let from = match value.ty {
            Ty::Obj(t) => t,
            Ty::Opt(o) => match self.ck.t.opt_inner(o) {
                Ty::Obj(t) => t,
                other => return Err(Diagnostic::new(format!("a {} is never a {}", self.name(other), self.name(Ty::Obj(target))), span)),
            },
            other => {
                return Err(Diagnostic::new(format!("a {} is never a {}", self.name(other), self.name(Ty::Obj(target))), span));
            }
        };
        let related = from == target
            || self.ck.is_subtype(target, from)
            || self.ck.is_subtype(from, target)
            || self.ck.t.ty(from).iface.is_some();
        if !related {
            return Err(Diagnostic::new(
                format!("a {} can never be a {}", self.name(Ty::Obj(from)), self.name(Ty::Obj(target))),
                span,
            ));
        }
        self.ck.is_targets.borrow_mut().insert(target);
        Ok(target)
    }

    /// `value as T` / `value as? T`
    fn cast_expr(&mut self, value: &ast::Expr, ty: &ast::TypeRef, safe: bool) -> Result<hir::Expr> {
        let v = self.value(value)?;
        let target = self.class_target(&v, ty, value.span.to(ty.span))?;
        let result = if safe { self.ck.t.opt_of(Ty::Obj(target)) } else { Ty::Obj(target) };
        // Going up (or staying) always works, so there is nothing to check.
        let from = match v.ty {
            Ty::Obj(t) => Some(t),
            Ty::Opt(o) => match self.ck.t.opt_inner(o) {
                Ty::Obj(t) => Some(t),
                _ => None,
            },
            _ => None,
        };
        if safe && from.is_some_and(|f| f == target || self.ck.is_subtype(f, target)) {
            return Ok(mk(K::Upcast(Box::new(v)), result));
        }
        Ok(mk(K::Downcast { value: Box::new(v), target, safe }, result))
    }

    /// `value is T` / `value !is T`
    fn is_expr(&mut self, value: &ast::Expr, ty: &ast::TypeRef, negated: bool) -> Result<hir::Expr> {
        let v = self.value(value)?;
        let target = self.class_target(&v, ty, value.span.to(ty.span))?;
        let test = mk(K::IsInstance { value: Box::new(v), target }, Ty::Bool);
        Ok(if negated { mk(K::Not(Box::new(test)), Ty::Bool) } else { test })
    }

    /// The constructor of `ty`; interfaces have none.
    fn ctor_of(&self, ty: TypeId, span: Span) -> Result<FnId> {
        let info = self.ck.t.ty(ty);
        if info.iface.is_some() {
            return Err(Diagnostic::new(format!("`{}` is an interface, so it has no constructor", info.name), span)
                .hint("create a type that implements it"));
        }
        Ok(info.ctor)
    }

    /// `Cache(…)` where `synonym Cache = Map<String, Int>`: a call of the constructor of the type it stands for.
    fn call_alias(&mut self, a: usize, type_args: &[ast::TypeRef], args: &[ast::Arg], span: Span, name_span: Span) -> Result<hir::Expr> {
        let targs = type_args
            .iter()
            .map(|t| self.ck.resolve_value_type(self.module, t, &self.subst))
            .collect::<Result<Vec<_>>>()?;
        match self.ck.resolve_alias(a, targs, name_span)? {
            Ty::Obj(ty) => {
                let ctor = self.ctor_of(ty, name_span)?;
                self.call_fn(ctor, None, args, span)
            }
            other => Err(Diagnostic::new(format!("`{}` is not a type with a constructor", self.ck.t.ty_name(other)), name_span)),
        }
    }

    /// Is `to` an interface and `from` an ordinary type?
    fn is_upcast(&self, from: TypeId, to: TypeId) -> bool {
        self.ck.t.ty(to).iface.is_some() && self.ck.t.ty(from).iface.is_none() && self.ck.t.ty(from).array_elem.is_none()
    }

    fn upcast(&self, e: hir::Expr, from: TypeId, to_iface: TypeId, to: Ty, span: Span) -> Result<hir::Expr> {
        match self.ck.conforms(from, to_iface) {
            Ok(()) => {
                self.ck.register_upcast(from, to_iface);
                Ok(mk(K::Upcast(Box::new(e)), to))
            }
            Err(why) => Err(self.mismatch(to, e.ty, span).hint(why)),
        }
    }

    fn mismatch(&self, expected: Ty, found: Ty, span: Span) -> Diagnostic {
        let d = Diagnostic::new(format!("expected {}, found {}", self.name(expected), self.name(found)), span);
        match found {
            Ty::Opt(id) if self.ck.t.opt_inner(id) == expected => {
                d.hint("it might be null: check for null first, or use `!!`, `?:` or `?.`")
            }
            _ => d,
        }
    }

    /// The type of an `if` whose branches have types `a` and `b`, if there is one.
    fn join(&self, a: Ty, b: Ty) -> Option<Ty> {
        let t = &self.ck.t;
        match (a, b) {
            (a, b) if a == b => Some(a),
            (Ty::Int, Ty::Float) | (Ty::Float, Ty::Int) => Some(Ty::Float),
            // two kinds of the same family: their closest common parent
            (Ty::Obj(x), Ty::Obj(y)) => self.ck.common_parent(x, y).map(Ty::Obj),
            (Ty::Null, x) | (x, Ty::Null) if x != Ty::Void => Some(t.opt_of(x)),
            (Ty::Opt(o), x) | (x, Ty::Opt(o)) if x != Ty::Void && t.opt_inner(o) == x => Some(Ty::Opt(o)),
            _ => None,
        }
    }

    /// Turn a value into text: Strings as they are, numbers and Bools
    /// formatted, objects via their `toString`.
    fn stringify(&mut self, e: hir::Expr, span: Span) -> Result<hir::Expr> {
        match e.ty {
            Ty::Str => Ok(e),
            Ty::Int | Ty::Float | Ty::Bool | Ty::Char => Ok(mk(K::ToStr(Box::new(e)), Ty::Str)),
            Ty::Bytes => self.prelude_call("__bytes_str", vec![e], span),
            Ty::Obj(ty) if self.ck.t.ty(ty).array_elem.is_some() => {
                Err(Diagnostic::new(format!("a {} has no text form", self.name(e.ty)), span)
                    .hint("an Array is a building block; a List can be printed"))
            }
            Ty::Obj(ty) if self.ck.t.ty(ty).iface.is_some() => {
                // dispatch to the real type's `toString`
                self.ck.stringified.borrow_mut().insert(ty);
                let slot = self.ck.t.ty(ty).iface.as_ref().map_or(0, Vec::len);
                let tmp = self.new_local("$obj", e.ty);
                let is_null = mk(K::IsNull(Box::new(self.local_expr(tmp))), Ty::Bool);
                let call = mk(K::CallIface { iface: ty, method: slot, args: vec![self.local_expr(tmp)] }, Ty::Str);
                let cond = mk(
                    K::Cond {
                        cond: Box::new(is_null),
                        then: Box::new(mk(K::Str("null".into()), Ty::Str)),
                        otherwise: Box::new(call),
                    },
                    Ty::Str,
                );
                Ok(mk(K::Block(vec![hir::Stmt::Let(tmp, Some(e))], Box::new(cond)), Ty::Str))
            }
            Ty::Obj(ty) if self.ck.t.ty(ty).open => {
                // an open type's objects may be of a subtype: dispatch to the real type's `toString`
                self.ck.stringified.borrow_mut().insert(ty);
                let slot = self.ck.t.ty(ty).vlist.len();
                let tmp = self.new_local("$obj", e.ty);
                let is_null = mk(K::IsNull(Box::new(self.local_expr(tmp))), Ty::Bool);
                let call = mk(K::CallIface { iface: ty, method: slot, args: vec![self.local_expr(tmp)] }, Ty::Str);
                let cond = mk(
                    K::Cond {
                        cond: Box::new(is_null),
                        then: Box::new(mk(K::Str("null".into()), Ty::Str)),
                        otherwise: Box::new(call),
                    },
                    Ty::Str,
                );
                Ok(mk(K::Block(vec![hir::Stmt::Let(tmp, Some(e))], Box::new(cond)), Ty::Str))
            }
            Ty::Obj(ty) => {
                let f = self.ck.tostring_fn(ty)?;
                let tmp = self.new_local("$obj", e.ty);
                let is_null = mk(
                    K::Binary(hir::BinOp::Eq, Box::new(self.local_expr(tmp)), Box::new(mk(K::Null, e.ty))),
                    Ty::Bool,
                );
                let call = mk(K::Call { func: f, args: vec![self.local_expr(tmp)] }, Ty::Str);
                let cond = mk(
                    K::Cond {
                        cond: Box::new(is_null),
                        then: Box::new(mk(K::Str("null".into()), Ty::Str)),
                        otherwise: Box::new(call),
                    },
                    Ty::Str,
                );
                Ok(mk(K::Block(vec![hir::Stmt::Let(tmp, Some(e))], Box::new(cond)), Ty::Str))
            }
            Ty::Opt(id) => {
                let inner = self.ck.t.opt_inner(id);
                let tmp = self.new_local("$opt", e.ty);
                let is_null = mk(K::IsNull(Box::new(self.local_expr(tmp))), Ty::Bool);
                let value = mk(K::Unwrap(Box::new(self.local_expr(tmp))), inner);
                let text = self.stringify(value, span)?;
                let cond = mk(
                    K::Cond {
                        cond: Box::new(is_null),
                        then: Box::new(mk(K::Str("null".into()), Ty::Str)),
                        otherwise: Box::new(text),
                    },
                    Ty::Str,
                );
                Ok(mk(K::Block(vec![hir::Stmt::Let(tmp, Some(e))], Box::new(cond)), Ty::Str))
            }
            Ty::Null => Ok(mk(K::Str("null".into()), Ty::Str)),
            Ty::Fn(_) => Err(Diagnostic::new("cannot turn a function into text", span)),
            Ty::Void => Err(Diagnostic::new("cannot turn nothing into text", span)),
        }
    }

    // ── expressions ─────────────────────────────────────────────

    fn expr(&mut self, e: &ast::Expr) -> Result<hir::Expr> {
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
                            self.stringify(v, x.span)?
                        }
                    };
                    acc = Some(match acc {
                        None => piece,
                        Some(prev) => mk(K::Binary(hir::BinOp::Add, Box::new(prev), Box::new(piece)), Ty::Str),
                    });
                }
                acc.expect("the parser never produces an empty interpolation")
            }

            A::Name(n) => match self.resolve(n) {
                Some(Named::Local(id)) => {
                    let value = self.local_expr(id);
                    self.narrowed_var(value, self.locals[id].var)
                }
                Some(Named::Field(index, ty)) => {
                    let obj = self.this_expr().expect("fields imply `this`");
                    mk(K::Field { obj: Box::new(obj), index }, ty)
                }
                Some(Named::Global(g)) => {
                    let ty = self.global_use(g, e.span)?;
                    self.narrowed_global(mk(K::Global(g), ty), g)
                }
                Some(Named::Generic) => {
                    return Err(Diagnostic::new(format!("`{n}` is generic, so it can't be used without arguments"), e.span)
                        .hint(format!("call it, e.g. `{n}(…)`, or give its type arguments: `{n}<Int>(…)`")));
                }
                Some(Named::Fn(id)) => self.fn_value(id, e.span)?,
                Some(Named::Type(ty)) => {
                    let ctor = self.ctor_of(ty, e.span)?;
                    self.fn_value(ctor, e.span)?
                }
                Some(Named::Module(_)) => {
                    return Err(Diagnostic::new(format!("`{n}` is a module, not a value"), e.span)
                        .hint(format!("call one of its functions, e.g. `{n}.name(...)`")));
                }
                None => {
                    let d = Diagnostic::new(format!("undefined variable `{n}`"), e.span);
                    return Err(if n == "it" && self.this.is_some() {
                        d.hint("the receiver of a method is called `this`")
                    } else {
                        d
                    });
                }
            },

            A::Field(obj, field) => {
                if let Some(g) = self.enum_variant(obj, field)? {
                    let ty = self.global_use(g, field.span)?;
                    return Ok(mk(K::Global(g), ty));
                }
                if let Some(g) = self.module_global(obj, field)? {
                    let ty = self.global_use(g, field.span)?;
                    return Ok(mk(K::Global(g), ty));
                }
                if let A::Name(a) = &obj.kind
                    && let Some(Named::Module(m)) = self.resolve(a)
                {
                    self.check_public(m, field)?;
                    return match self.ck.t.scopes[m].items.get(&field.name) {
                        Some(&Sym::Fn(id)) => self.fn_value(id, e.span),
                        Some(&Sym::Type(ty)) => {
                            let ctor = self.ctor_of(ty, e.span)?;
                            self.fn_value(ctor, e.span)
                        }
                        _ => Err(Diagnostic::new(format!("module `{a}` has no `{}`", field.name), field.span)),
                    };
                }
                let obj = self.value(obj)?;
                let (index, ty) = self.field_of(&obj, field)?;
                mk(K::Field { obj: Box::new(obj), index }, ty)
            }

            A::Call { callee, type_args, args } => self.call(callee, type_args, args, e.span, None)?,

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
                    // `-v` on your own type is `v.unaryMinus()`
                    (ast::UnOp::Neg, Ty::Obj(t), _) if self.ck.t.ty(t).methods.contains_key("unaryMinus") => {
                        let method = self.ck.t.ty(t).methods["unaryMinus"];
                        let ty = v.ty;
                        let call = self.call_values(method, Some(v), Vec::new(), e.span)?;
                        self.virtual_on(call, ty, "unaryMinus")
                    }
                    (ast::UnOp::Neg, other, _) => {
                        let e = Diagnostic::new(format!("cannot negate a {}", self.name(other)), e.span);
                        return Err(if matches!(other, Ty::Obj(_)) {
                            e.hint("to negate your own type, give it a method `unaryMinus()`")
                        } else {
                            e
                        });
                    }
                    (ast::UnOp::Not, other, _) => {
                        return Err(Diagnostic::new(format!("`!` needs a Bool, found {}", self.name(other)), e.span));
                    }
                }
            }

            // `a && b` checks `b` knowing `a` held; `a || b` knowing it did not.
            A::Binary(op @ (ast::BinOp::And | ast::BinOp::Or), l, r) => {
                let l_val = self.value(l)?;
                let facts = self.facts(l, *op == ast::BinOp::And);
                let r_val = self.with_facts(facts, |cx| cx.value(r))?;
                self.binary(*op, l_val, r_val, e.span)?
            }

            A::Binary(op, l, r) => {
                // `null` can only be compared (`x == null`), not computed with.
                let equality = matches!(op, ast::BinOp::Eq | ast::BinOp::Ne);
                let operand = |cx: &mut Self, x: &ast::Expr| {
                    if equality && matches!(x.kind, A::Null) { Ok(mk(K::Null, Ty::Null)) } else { cx.value(x) }
                };
                let l = operand(self, l)?;
                let r = operand(self, r)?;
                self.binary(*op, l, r, e.span)?
            }

            A::If { cond, then, otherwise } => self.if_value(cond, then, otherwise.as_deref(), e.span, None)?,

            A::Block(stmts) => self.block_value(stmts, None)?,

            A::Index { obj, index } => {
                let o = self.value(obj)?;
                let i = self.value(index)?;
                self.get_element(o, i, e.span)?
            }

            A::Null => mk(K::Null, Ty::Null),

            A::Char(c) => mk(K::Int(i64::from(*c)), Ty::Char),

            A::Is { value, ty, negated } => self.is_expr(value, ty, *negated)?,

            A::Cast { value, ty, safe } => self.cast_expr(value, ty, *safe)?,

            A::Try { body, catch_name, handler } => self.try_expr(body, catch_name.as_ref(), handler, None, e.span)?,

            A::NotNull(inner) => {
                let v = self.value(inner)?;
                let Some((plain, _)) = self.nullable_parts(v.ty) else {
                    return Err(Diagnostic::new(format!("a {} is never null, so `!!` is pointless", self.name(v.ty)), e.span));
                };
                mk(K::Unwrap(Box::new(v)), plain)
            }

            A::Elvis(value, fallback) => self.elvis(value, fallback)?,

            A::Propagate(inner) => {
                let recv = self.value(inner)?;
                self.propagate(recv, e.span)?
            }

            A::SafeField(obj, field) => {
                let recv = self.value(obj)?;
                self.safe_access(recv, e.span, |cx, plain| {
                    let (index, ty) = cx.field_of(&plain, field)?;
                    Ok(mk(K::Field { obj: Box::new(plain), index }, ty))
                })?
            }

            A::Slice { obj, start, end, inclusive } => {
                self.slice_expr(obj, start.as_deref(), end.as_deref(), *inclusive, e.span)?
            }

            A::List(items) => self.list_literal(items, None, e.span)?,
            A::Map(entries) => self.map_literal(entries, None, e.span)?,
            A::Spread(_) => {
                return Err(Diagnostic::new("`*` spreads a list into a call or a list literal", e.span)
                    .hint("e.g. `f(*xs)` or `[0, *xs]`, where `f` takes a `vararg`"));
            }
            // `x ?: return -1`: leaves the function (or loop) instead of producing a value
            A::Return(_) | A::Break | A::Continue => {
                let mut out = Vec::new();
                match &e.kind {
                    A::Return(value) => self.return_stmt(value.as_deref(), e.span, &mut out)?,
                    other => self.jump_stmt(matches!(other, A::Break), e.span, &mut out)?,
                }
                mk(K::Block(out, Box::new(unit())), Ty::Void)
            }

            // A range that is not looped over directly is a `Range` object (see the prelude).
            A::Range { start, end, inclusive } => {
                let from = self.value(start)?;
                let from = self.coerce(from, Ty::Int, start.span)?;
                let to = self.value(end)?;
                let mut to = self.coerce(to, Ty::Int, end.span)?;
                if *inclusive {
                    to = mk(K::Binary(hir::BinOp::Add, Box::new(to), Box::new(mk(K::Int(1), Ty::Int))), Ty::Int);
                }
                let Some(&Sym::Type(range)) = self.ck.t.scopes[0].items.get("Range") else {
                    return Err(Diagnostic::new("the prelude has no `Range`", e.span));
                };
                let ctor = self.ck.t.ty(range).ctor;
                let one = mk(K::Int(1), Ty::Int);
                self.call_values(ctor, None, vec![(from, start.span), (to, end.span), (one, e.span)], e.span)?
            }

            // Without an expected function type, only fully annotated lambdas can be checked.
            A::Lambda { params, explicit, body } => self.lambda(params, *explicit, body, None, e.span)?,
        })
    }

    /// Statements followed by a value: the last expression of the block.
    fn block_value(&mut self, stmts: &[ast::Stmt], expected: Option<Ty>) -> Result<hir::Expr> {
        self.scopes.push(HashMap::new());
        self.block_defers.push(None);
        let saved = self.narrowed.clone();
        let result = (|| {
            let mut out = Vec::new();
            let mut tail = unit();
            for (i, s) in stmts.iter().enumerate() {
                match &s.kind {
                    StmtKind::Expr(e) if i + 1 == stmts.len() => {
                        self.kill_before(s);
                        tail = self.expr_expected(e, expected)?;
                    }
                    _ => self.stmt_narrowing(s, &mut out)?,
                }
            }
            // The block's value is computed before its deferred code runs.
            if let Some(Some(mark)) = self.block_defers.last().copied() {
                if tail.ty == Ty::Void {
                    out.push(hir::Stmt::Expr(tail));
                    tail = unit();
                } else {
                    let tmp = self.new_local("$tail", tail.ty);
                    out.push(hir::Stmt::Let(tmp, Some(tail)));
                    tail = self.local_expr(tmp);
                }
                out.push(self.run_defers(mark)?);
            }
            Ok(if out.is_empty() {
                tail
            } else {
                let ty = tail.ty;
                mk(K::Block(out, Box::new(tail)), ty)
            })
        })();
        self.block_defers.pop();
        self.narrowed = saved;
        self.scopes.pop();
        result
    }

    /// `if` used for its value: both branches must agree on a type.
    fn if_value(
        &mut self,
        cond: &ast::Expr,
        then: &[ast::Stmt],
        otherwise: Option<&[ast::Stmt]>,
        span: Span,
        expected: Option<Ty>,
    ) -> Result<hir::Expr> {
        let test = cond;
        let cond = self.condition(test)?;
        let (when_true, when_false) = (self.facts(test, true), self.facts(test, false));
        let mut then = self.with_facts(when_true, |cx| cx.block_value(then, expected))?;
        let mut other = match otherwise {
            Some(b) => self.with_facts(when_false, |cx| cx.block_value(b, expected))?,
            None => unit(),
        };
        if otherwise.is_none() && then.ty != Ty::Void {
            return Err(Diagnostic::new("an `if` that produces a value needs an `else`", span)
                .hint("for `when`, add an `else ->` branch"));
        }
        // A branch that always leaves (`return`, `break`, `panic(…)`) has no type of its own.
        let (then_leaves, other_leaves) = (diverges(self.ck, &then), diverges(self.ck, &other));
        if then_leaves != other_leaves {
            let (leaving, staying) = if then_leaves { (&mut then, &other) } else { (&mut other, &then) };
            if staying.ty != Ty::Void {
                let ty = staying.ty;
                let gone = replace(leaving, unit());
                *leaving = mk(K::Block(vec![hir::Stmt::Expr(gone)], Box::new(default_expr(ty))), ty);
            }
        }
        let Some(ty) = self.join(then.ty, other.ty) else {
            return Err(Diagnostic::new(
                format!(
                    "the branches have different types: {} and {}",
                    self.name(then.ty),
                    self.name(other.ty)
                ),
                span,
            ));
        };
        then = self.coerce(then, ty, span)?;
        other = self.coerce(other, ty, span)?;
        Ok(mk(K::Cond { cond: Box::new(cond), then: Box::new(then), otherwise: Box::new(other) }, ty))
    }

    fn field_of(&self, obj: &hir::Expr, field: &ast::Ident) -> Result<(usize, Ty)> {
        let Ty::Obj(id) = obj.ty else {
            let d = Diagnostic::new(format!("a {} has no field `{}`", self.name(obj.ty), field.name), field.span);
            return Err(self.null_hint(obj.ty, d));
        };
        let info = self.ck.t.ty(id);
        match info.fields.iter().position(|f| f.name == field.name) {
            Some(i) => Ok((i, info.fields[i].ty)),
            None => Err(Diagnostic::new(format!("`{}` has no field `{}`", info.name, field.name), field.span)),
        }
    }

    /// Explain how to get at a member of something that might be null.
    fn null_hint(&self, ty: Ty, d: Diagnostic) -> Diagnostic {
        match ty {
            Ty::Opt(_) => d.hint("it might be null: use `?.` to access it safely, or `!!` to assert it isn't"),
            _ => d,
        }
    }

    fn binary(&mut self, op: ast::BinOp, l: hir::Expr, r: hir::Expr, span: Span) -> Result<hir::Expr> {
        use ast::BinOp as B;
        if matches!(op, B::And | B::Or) {
            let l = self.coerce(l, Ty::Bool, span)?;
            let r = self.coerce(r, Ty::Bool, span)?;
            return Ok(mk(K::Logical { and: op == B::And, lhs: Box::new(l), rhs: Box::new(r) }, Ty::Bool));
        }

        if matches!(op, B::Eq | B::Ne)
            && (matches!(l.ty, Ty::Opt(_) | Ty::Null) || matches!(r.ty, Ty::Opt(_) | Ty::Null))
        {
            return self.optional_equality(op == B::Eq, l, r, span);
        }
        // A subtype compared with its parent type is compared as the parent type.
        let (l, r) = match (op, l.ty, r.ty) {
            (B::Eq | B::Ne, Ty::Obj(a), Ty::Obj(b)) if a != b && self.ck.is_subtype(a, b) => {
                let to = r.ty;
                (mk(K::Upcast(Box::new(l)), to), r)
            }
            (B::Eq | B::Ne, Ty::Obj(a), Ty::Obj(b)) if a != b && self.ck.is_subtype(b, a) => {
                let to = l.ty;
                (l, mk(K::Upcast(Box::new(r)), to))
            }
            _ => (l, r),
        };
        // Objects compare with their own `equals(other)` if they have one.
        if matches!(op, B::Eq | B::Ne)
            && let (Ty::Obj(a), Ty::Obj(b)) = (l.ty, r.ty)
            && a == b
            && let Some(equals) = self.custom_equals(a)
        {
            return Ok(self.object_equality(op == B::Eq, equals, l, r));
        }

        // Operators on your own types call methods: `a + b` is `a.plus(b)`, `a < b` is `a.compareTo(b) < 0`.
        if let Some((method, name, compare)) = self.operator_method(op, l.ty, r.ty) {
            let ty = l.ty;
            let call = self.call_values(method, Some(l), vec![(r, span)], span)?;
            let call = self.virtual_on(call, ty, name);
            if !compare {
                return Ok(call);
            }
            let order = self.coerce(call, Ty::Int, span)?;
            let against = match op {
                B::Lt => hir::BinOp::Lt,
                B::Le => hir::BinOp::Le,
                B::Gt => hir::BinOp::Gt,
                _ => hir::BinOp::Ge,
            };
            return Ok(mk(K::Binary(against, Box::new(order), Box::new(mk(K::Int(0), Ty::Int))), Ty::Bool));
        }

        // Mixed Int/Float arithmetic and comparison happens in Float.
        let (l, r) = match (l.ty, r.ty) {
            (Ty::Int, Ty::Float) => (mk(K::IntToFloat(Box::new(l)), Ty::Float), r),
            (Ty::Float, Ty::Int) => (l, mk(K::IntToFloat(Box::new(r)), Ty::Float)),
            _ => (l, r),
        };

        // Characters: `c + n`, `n + c` and `c - n` are characters; `c - d` is the distance between two.
        let char_op = match (op, l.ty, r.ty) {
            (B::Add | B::Sub, Ty::Char, Ty::Int) | (B::Add, Ty::Int, Ty::Char) => Some(Ty::Char),
            (B::Sub, Ty::Char, Ty::Char) => Some(Ty::Int),
            _ => None,
        };
        if let Some(result) = char_op {
            let hop = if op == B::Add { hir::BinOp::Add } else { hir::BinOp::Sub };
            return Ok(mk(K::Binary(hop, Box::new(l), Box::new(r)), result));
        }

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

        // Bytes are equal when they hold the same bytes.
        if l.ty == Ty::Bytes && matches!(op, B::Eq | B::Ne) {
            let same = self.prelude_call("__bytes_equals", vec![l, r], span)?;
            return Ok(if op == B::Eq { same } else { mk(K::Not(Box::new(same)), Ty::Bool) });
        }

        // `a + b` on Bytes joins them.
        if l.ty == Ty::Bytes
            && op == B::Add
            && let Some(concat) = self.ck.ext_method(Ty::Bytes, "concat")
        {
            return self.call_values(concat, Some(l), vec![(r, span)], span);
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
            B::Lt | B::Le | B::Gt | B::Ge if matches!(l.ty, Ty::Int | Ty::Float | Ty::Str | Ty::Char) => (
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
                let e = Diagnostic::new(format!("cannot apply `{}` to {} values", op.symbol(), self.name(l.ty)), span);
                return Err(match (l.ty, Self::operator_name(op)) {
                    (Ty::Obj(_), Some((name, _))) => e.hint(format!(
                        "to use `{}` on your own types, give the type a method `{name}`",
                        op.symbol()
                    )),
                    _ => e,
                });
            }
        };
        Ok(mk(K::Binary(hop, Box::new(l), Box::new(r)), result))
    }

    /// The method an operator stands for on an object, and whether it is an ordering (`compareTo`).
    fn operator_name(op: ast::BinOp) -> Option<(&'static str, bool)> {
        use ast::BinOp as B;
        Some(match op {
            B::Add => ("plus", false),
            B::Sub => ("minus", false),
            B::Mul => ("times", false),
            B::Div => ("div", false),
            B::Rem => ("rem", false),
            B::Lt | B::Le | B::Gt | B::Ge => ("compareTo", true),
            _ => return None,
        })
    }

    /// For `l op r` where `l` is an object (or a built-in type with an extension method and `r` is an object):
    /// the method that implements the operator.
    fn operator_method(&self, op: ast::BinOp, l: Ty, r: Ty) -> Option<(FnId, &'static str, bool)> {
        let (name, compare) = Self::operator_name(op)?;
        let method = match l {
            Ty::Obj(t) => {
                let info = self.ck.t.ty(t);
                if info.iface.is_some() || info.array_elem.is_some() {
                    return None;
                }
                info.methods.get(name).copied()
            }
            Ty::Int | Ty::Float | Ty::Str | Ty::Char | Ty::Bool | Ty::Bytes if matches!(r, Ty::Obj(_)) => {
                self.ck.ext_method(l, name)
            }
            _ => None,
        }?;
        Some((method, name, compare))
    }

    /// For a type that can be null: the type without the null, and whether it is a real
    /// optional (as opposed to an object or function reference, which may also be null at run time).
    fn nullable_parts(&self, ty: Ty) -> Option<(Ty, bool)> {
        match ty {
            Ty::Opt(id) => Some((self.ck.t.opt_inner(id), true)),
            Ty::Obj(_) | Ty::Fn(_) => Some((ty, false)),
            _ => None,
        }
    }

    /// `value ?: fallback`
    ///
    /// ```text
    /// { $t := value; if $t == null { fallback } else { $t!! } }
    /// ```
    ///
    /// A fallback that produces nothing (`x ?: panic("…")`) is run for its effect only.
    fn elvis(&mut self, value: &ast::Expr, fallback: &ast::Expr) -> Result<hir::Expr> {
        let v = self.value(value)?;
        let Some((plain, boxed)) = self.nullable_parts(v.ty) else {
            return Err(Diagnostic::new(
                format!("a {} is never null, so `?:` has nothing to fall back from", self.name(v.ty)),
                value.span,
            ));
        };
        let value_ty = v.ty;
        let tmp = self.new_local("$elvis", value_ty);
        let have = |cx: &Self| {
            let t = cx.local_expr(tmp);
            if boxed { mk(K::Unwrap(Box::new(t)), plain) } else { t }
        };
        let is_null = mk(K::IsNull(Box::new(self.local_expr(tmp))), Ty::Bool);
        let bind = hir::Stmt::Let(tmp, Some(v));

        let alt = if matches!(fallback.kind, A::Null) { mk(K::Null, Ty::Null) } else { self.expr_expected(fallback, Some(plain))? };
        if alt.ty == Ty::Void {
            let check = hir::Stmt::Expr(mk(
                K::Cond { cond: Box::new(is_null), then: Box::new(alt), otherwise: Box::new(unit()) },
                Ty::Void,
            ));
            return Ok(mk(K::Block(vec![bind, check], Box::new(have(self))), plain));
        }
        // An optional (or null) fallback keeps the result optional.
        let (alt, ty, keep) = match alt.ty {
            Ty::Null | Ty::Opt(_) => (self.coerce(alt, value_ty, fallback.span)?, value_ty, self.local_expr(tmp)),
            _ => (self.coerce(alt, plain, fallback.span)?, plain, have(self)),
        };
        Ok(mk(
            K::Block(
                vec![bind],
                Box::new(mk(K::Cond { cond: Box::new(is_null), then: Box::new(alt), otherwise: Box::new(keep) }, ty)),
            ),
            ty,
        ))
    }

    /// The `equals(other: T) -> Bool` method of type `ty`, if it has one.
    fn custom_equals(&self, ty: TypeId) -> Option<FnId> {
        let info = self.ck.t.ty(ty);
        if info.iface.is_some() || info.array_elem.is_some() {
            return None;
        }
        let &f = info.methods.get("equals")?;
        let decl = self.ck.func(f);
        let fits = decl.params.len() == 2 && decl.params[1].ty == Ty::Obj(ty);
        (fits && self.ck.ret_of(f, Span::default()).ok()? == Ty::Bool).then_some(f)
    }

    /// `a == b` through `a.equals(b)`; two nulls are equal and a null is never equal to an object.
    fn object_equality(&mut self, equal: bool, equals: FnId, l: hir::Expr, r: hir::Expr) -> hir::Expr {
        let ty = l.ty;
        let (a, b) = (self.new_local("$a", ty), self.new_local("$b", ty));
        let null_a = mk(K::IsNull(Box::new(self.local_expr(a))), Ty::Bool);
        let null_b = mk(K::IsNull(Box::new(self.local_expr(b))), Ty::Bool);
        let present_b = mk(K::Not(Box::new(mk(K::IsNull(Box::new(self.local_expr(b))), Ty::Bool))), Ty::Bool);
        let call = mk(K::Call { func: equals, args: vec![self.local_expr(a), self.local_expr(b)] }, Ty::Bool);
        let call = self.virtual_on(call, ty, "equals");
        let both = mk(K::Logical { and: true, lhs: Box::new(present_b), rhs: Box::new(call) }, Ty::Bool);
        let test = mk(
            K::Cond { cond: Box::new(null_a), then: Box::new(null_b), otherwise: Box::new(both) },
            Ty::Bool,
        );
        let bind = vec![hir::Stmt::Let(a, Some(l)), hir::Stmt::Let(b, Some(r))];
        let result = mk(K::Block(bind, Box::new(test)), Ty::Bool);
        if equal { result } else { mk(K::Not(Box::new(result)), Ty::Bool) }
    }

    /// `==` / `!=` where a side is optional or `null`.
    fn optional_equality(&mut self, equal: bool, l: hir::Expr, r: hir::Expr, span: Span) -> Result<hir::Expr> {
        let not = |e: hir::Expr| if equal { e } else { mk(K::Not(Box::new(e)), Ty::Bool) };
        let is_null = |e: hir::Expr| mk(K::IsNull(Box::new(e)), Ty::Bool);
        // `x == null`: pointers can be checked directly; scalars can never be null.
        match (l.ty, r.ty) {
            (Ty::Null, Ty::Null) => return Ok(not(mk(K::Bool(true), Ty::Bool))),
            (other, Ty::Null) | (Ty::Null, other) => {
                let (value, _) = if r.ty == Ty::Null { (l, r) } else { (r, l) };
                if !matches!(other, Ty::Opt(_) | Ty::Obj(_) | Ty::Fn(_)) {
                    return Err(Diagnostic::new(
                        format!("a {} is never null, so it can't be compared with null", self.name(other)),
                        span,
                    ));
                }
                return Ok(not(is_null(value)));
            }
            _ => {}
        }

        // Two values, at least one optional: equal if both are null, or neither is and the values match.
        let (a, b) = (self.new_local("$a", l.ty), self.new_local("$b", r.ty));
        let (la, lb) = (self.local_expr(a), self.local_expr(b));
        let unwrap = |cx: &Self, e: hir::Expr| match e.ty {
            Ty::Opt(id) => mk(K::Unwrap(Box::new(e)), cx.ck.t.opt_inner(id)),
            _ => e,
        };
        let values = {
            let (ua, ub) = (unwrap(self, self.local_expr(a)), unwrap(self, self.local_expr(b)));
            self.binary(ast::BinOp::Eq, ua, ub, span)?
        };
        let present = |e: hir::Expr| mk(K::Not(Box::new(is_null(e))), Ty::Bool);
        let and = |x: hir::Expr, y: hir::Expr| mk(K::Logical { and: true, lhs: Box::new(x), rhs: Box::new(y) }, Ty::Bool);
        let test = match (l.ty, r.ty) {
            (Ty::Opt(_), Ty::Opt(_)) => mk(
                K::Cond {
                    cond: Box::new(is_null(la)),
                    then: Box::new(is_null(lb)),
                    otherwise: Box::new(and(present(self.local_expr(b)), values)),
                },
                Ty::Bool,
            ),
            (Ty::Opt(_), _) => and(present(la), values),
            _ => and(present(lb), values),
        };
        let bind = vec![hir::Stmt::Let(a, Some(l)), hir::Stmt::Let(b, Some(r))];
        Ok(not(mk(K::Block(bind, Box::new(test)), Ty::Bool)))
    }

    // ── calls ───────────────────────────────────────────────────

    fn call(
        &mut self,
        callee: &ast::Expr,
        type_args: &[ast::TypeRef],
        args: &[ast::Arg],
        span: Span,
        expected: Option<Ty>,
    ) -> Result<hir::Expr> {
        let not_generic = |what: &str| {
            Diagnostic::new(format!("`{what}` is not generic, so it takes no type arguments"), callee.span)
        };
        match &callee.kind {
            A::Name(n) => {
                // A variable holding a function.
                if let Some(id) = self.find_local(n) {
                    // an optional function that was just tested against null can be called
                    let f = self.narrowed_var(self.local_expr(id), self.locals[id].var);
                    return self.call_value(f, args, span, n);
                }
                if let Some(ty) = self.this {
                    // A method of the current object can be called bare…
                    let info = self.ck.t.ty(ty);
                    if let Some(&m) = info.methods.get(n) {
                        if !type_args.is_empty() {
                            return Err(not_generic(n));
                        }
                        let recv = self.this_expr();
                        let call = self.call_fn(m, recv, args, span)?;
                        return Ok(self.virtual_if_open(call, &info.virtuals, n));
                    }
                    if let Some(&g) = info.generic_methods.get(n) {
                        let recv = self.this_expr();
                        return self.call_generic_fn(g, recv, type_args, args, span, expected);
                    }
                    // …and so can a field that holds a function.
                    if let Some(i) = info.fields.iter().position(|f| f.name == *n) {
                        let obj = self.this_expr().expect("fields imply `this`");
                        let f = mk(K::Field { obj: Box::new(obj), index: i }, info.fields[i].ty);
                        return self.call_value(f, args, span, n);
                    }
                }
                // Inside a type (a method or a `static fn`), its static functions can be called bare.
                if let Some(ty) = self.this.or(self.static_owner)
                    && let Some(id) = self.ck.t.static_of(ty, n)
                {
                    if !type_args.is_empty() {
                        return Err(not_generic(n));
                    }
                    return self.call_fn(id, None, args, span);
                }
                // Inside a method added to a built-in type, its siblings can be called bare.
                if self.this.is_none()
                    && let Some(this) = self.find_local("this")
                    && let Some(m) = self.ck.ext_method(self.locals[this].ty, n)
                {
                    let recv = self.local_expr(this);
                    return self.call_fn(m, Some(recv), args, span);
                }
                match self.ck.t.lookup(self.module, n) {
                    Some(Sym::Fn(id)) if type_args.is_empty() => self.call_fn(id, None, args, span),
                    Some(Sym::Type(ty)) if type_args.is_empty() => {
                        let ctor = self.ctor_of(ty, callee.span)?;
                        self.call_fn(ctor, None, args, span)
                    }
                    Some(Sym::Fn(_) | Sym::Type(_)) => Err(not_generic(n)),
                    Some(Sym::GenericFn(g)) => self.call_generic_fn(g, None, type_args, args, span, expected),
                    Some(Sym::GenericType(g)) => self.call_generic_type(g, type_args, args, span, expected),
                    Some(Sym::Alias(a)) => self.call_alias(a, type_args, args, span, callee.span),
                    Some(Sym::GenericIface(_)) => {
                        Err(Diagnostic::new(format!("`{n}` is an interface, so it has no constructor"), callee.span)
                            .hint("create a type that implements it"))
                    }
                    Some(Sym::Global(g)) => {
                        let ty = self.global_use(g, callee.span)?;
                        let f = self.narrowed_global(mk(K::Global(g), ty), g);
                        self.call_value(f, args, span, n)
                    }
                    None if n == "Array" => self.array_new(type_args, args, span, expected),
                    None if n == "hashOf" => self.hash_of(args, span),
                    None if n == "print" || n == "println" => self.print(n == "println", args, span),
                    None => Err(Diagnostic::new(format!("undefined function `{n}`"), callee.span)),
                }
            }

            A::Field(obj, member) if matches!(&obj.kind, A::Name(n) if n == "super") && self.find_local("super").is_none() => {
                self.super_call(member, type_args, args, span)
            }

            A::Field(obj, member) => {
                // `module.func(...)` / `module.Type(...)`
                if let A::Name(a) = &obj.kind {
                    match self.resolve(a) {
                        Some(Named::Module(target)) => {
                            self.check_public(target, member)?;
                            let what = format!("{a}.{}", member.name);
                            return match self.ck.t.scopes[target].items.get(&member.name).copied() {
                                Some(Sym::Fn(id)) if type_args.is_empty() => self.call_fn(id, None, args, span),
                                Some(Sym::Type(ty)) if type_args.is_empty() => {
                                    let ctor = self.ctor_of(ty, callee.span)?;
                                    self.call_fn(ctor, None, args, span)
                                }
                                Some(Sym::Fn(_) | Sym::Type(_)) => Err(not_generic(&what)),
                                Some(Sym::GenericFn(g)) => {
                                    self.call_generic_fn(g, None, type_args, args, span, expected)
                                }
                                Some(Sym::GenericType(g)) => {
                                    self.call_generic_type(g, type_args, args, span, expected)
                                }
                                Some(Sym::Alias(a)) => self.call_alias(a, type_args, args, span, member.span),
                                Some(Sym::GenericIface(_)) => Err(Diagnostic::new(
                                    format!("`{what}` is an interface, so it has no constructor"),
                                    member.span,
                                )),
                                Some(Sym::Global(g)) => {
                                    let ty = self.global_use(g, member.span)?;
                                    self.call_value(mk(K::Global(g), ty), args, span, &what)
                                }
                                None => Err(Diagnostic::new(
                                    format!("module `{a}` has no function or type `{}`", member.name),
                                    member.span,
                                )),
                            };
                        }
                        None => {
                            return Err(Diagnostic::new(format!("undefined name `{a}`"), obj.span)
                                .hint(format!("did you forget `import {a}`?")));
                        }
                        Some(_) => {} // a variable: fall through to a method call
                    }
                }

                // `Type.make(...)` / `module.Type.make(...)`: a static function
                if let Some(ty) = self.static_receiver(obj)? {
                    let Some(id) = self.ck.t.static_of(ty, &member.name) else {
                        let what = self.ck.t.ty(ty).name.clone();
                        let d = Diagnostic::new(format!("`{what}` has no static function `{}`", member.name), member.span);
                        return Err(if self.ck.t.ty(ty).methods.contains_key(&member.name) {
                            d.hint(format!("`{}` is a method: call it on a value, not on the type", member.name))
                        } else {
                            d
                        });
                    };
                    if !type_args.is_empty() {
                        return Err(not_generic(&format!("{}.{}", self.ck.t.ty(ty).name, member.name)));
                    }
                    return self.call_fn(id, None, args, span);
                }

                // `value.method(...)`, or a function stored in a field: `value.callback(...)`
                let recv = self.value(obj)?;
                self.method_call(recv, member, type_args, args, span, expected)
            }

            // `value?.method(...)`: null if `value` is
            A::SafeField(obj, member) => {
                let recv = self.value(obj)?;
                self.safe_access(recv, callee.span, |cx, plain| {
                    cx.method_call(plain, member, type_args, args, span, expected)
                })
            }

            // `make()(x)`, `(f)(x)`, …
            _ => {
                let f = self.value(callee)?;
                self.call_value(f, args, span, "this expression")
            }
        }
    }

    /// `obj` of `obj.name(…)` when it names a (non-generic) type, directly or as `module.Type`.
    fn static_receiver(&mut self, obj: &ast::Expr) -> Result<Option<TypeId>> {
        use ast::ExprKind as A;
        match &obj.kind {
            A::Name(n) => Ok(match self.resolve(n) {
                Some(Named::Type(ty)) => Some(ty),
                _ => None,
            }),
            A::Field(inner, ty_name) => {
                let A::Name(m) = &inner.kind else { return Ok(None) };
                let Some(Named::Module(target)) = self.resolve(m) else { return Ok(None) };
                match self.ck.t.scopes[target].items.get(&ty_name.name).copied() {
                    Some(Sym::Type(ty)) => {
                        self.check_public(target, ty_name)?;
                        Ok(Some(ty))
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    /// `a.and(b)`, `a.shl(3)`, `a.inv()`: the bit operations of an Int.
    fn bit_operation(&mut self, recv: hir::Expr, member: &ast::Ident, args: &[ast::Arg], span: Span) -> Result<hir::Expr> {
        use hir::BinOp as H;
        let op = match member.name.as_str() {
            "and" => H::BitAnd,
            "or" => H::BitOr,
            "xor" => H::BitXor,
            "shl" => H::Shl,
            "shr" => H::Shr,
            "ushr" => H::UShr,
            "inv" => H::BitXor,
            _ => unreachable!("checked by the caller"),
        };
        let want = usize::from(member.name != "inv");
        if args.len() != want || args.iter().any(|a| a.name.is_some()) {
            return Err(Diagnostic::new(format!("`{}` takes {want} argument(s)", member.name), span));
        }
        let operand = match args.first() {
            Some(a) => {
                let v = self.value_expected(&a.value, Some(Ty::Int))?;
                self.coerce(v, Ty::Int, a.value.span)?
            }
            None => mk(K::Int(-1), Ty::Int), // inv() flips every bit
        };
        Ok(mk(K::Binary(op, Box::new(recv), Box::new(operand)), Ty::Int))
    }

    /// `recv.member(args)` for an already evaluated receiver: a method of its type, a method
    /// added to a built-in type, or a function stored in a field.
    fn method_call(
        &mut self,
        recv: hir::Expr,
        member: &ast::Ident,
        type_args: &[ast::TypeRef],
        args: &[ast::Arg],
        span: Span,
        expected: Option<Ty>,
    ) -> Result<hir::Expr> {
        let not_generic = |what: &str| {
            Diagnostic::new(format!("`{what}` is not generic, so it takes no type arguments"), member.span)
        };
        if recv.ty == Ty::Int
            && matches!(member.name.as_str(), "and" | "or" | "xor" | "shl" | "shr" | "ushr" | "inv")
            && self.ck.ext_method(Ty::Int, &member.name).is_none()
        {
            return self.bit_operation(recv, member, args, span);
        }
        if let Some(id) = self.ck.ext_method(recv.ty, &member.name) {
            if !type_args.is_empty() {
                return Err(not_generic(&member.name));
            }
            return self.call_fn(id, Some(recv), args, span);
        }
        let Ty::Obj(ty) = recv.ty else {
            let d = Diagnostic::new(format!("a {} has no method `{}`", self.name(recv.ty), member.name), member.span);
            return Err(self.null_hint(recv.ty, d));
        };
        let info = self.ck.t.ty(ty);
        if let Some(methods) = &info.iface {
            return self.interface_call(recv, ty, methods, member, type_args, args, span);
        }
        if info.array_elem.is_some() {
            if !type_args.is_empty() {
                return Err(not_generic(&member.name));
            }
            return self.array_method(recv, member, args, span);
        }
        if let Some(&id) = info.methods.get(&member.name) {
            if !type_args.is_empty() {
                return Err(not_generic(&member.name));
            }
            let call = self.call_fn(id, Some(recv), args, span)?;
            return Ok(self.virtual_if_open(call, &info.virtuals, &member.name));
        }
        if let Some(&g) = info.generic_methods.get(&member.name) {
            return self.call_generic_fn(g, Some(recv), type_args, args, span, expected);
        }
        if let Some(i) = info.fields.iter().position(|f| f.name == member.name) {
            let f = mk(K::Field { obj: Box::new(recv), index: i }, info.fields[i].ty);
            return self.call_value(f, args, span, &member.name);
        }
        Err(Diagnostic::new(
            format!("`{}` has no method `{}`", info.name, member.name),
            member.span,
        ))
    }

    /// `shape.area()` on a value whose type is an interface.
    #[allow(clippy::too_many_arguments)]
    fn interface_call(
        &mut self,
        recv: hir::Expr,
        iface: TypeId,
        methods: &[super::IfaceMethod],
        member: &ast::Ident,
        type_args: &[ast::TypeRef],
        args: &[ast::Arg],
        span: Span,
    ) -> Result<hir::Expr> {
        let name = &self.ck.t.ty(iface).name;
        let Some(index) = methods.iter().position(|m| m.name == member.name) else {
            return Err(Diagnostic::new(format!("`{name}` has no method `{}`", member.name), member.span));
        };
        if !type_args.is_empty() {
            return Err(Diagnostic::new(
                format!("`{}` is not generic, so it takes no type arguments", member.name),
                member.span,
            ));
        }
        let m = &methods[index];
        if let Some(n) = args.iter().find_map(|a| a.name.as_ref()) {
            return Err(Diagnostic::new("methods called through an interface don't take named arguments", n.span));
        }
        if args.len() != m.params.len() {
            return Err(Diagnostic::new(
                format!("`{}` takes {} argument(s) but {} were given", m.name, m.params.len(), args.len()),
                span,
            ));
        }
        let mut call_args = vec![recv];
        for (a, &want) in args.iter().zip(&m.params) {
            let v = self.value_expected(&a.value, Some(want))?;
            call_args.push(self.coerce(v, want, a.value.span)?);
        }
        Ok(mk(K::CallIface { iface, method: index, args: call_args }, m.ret))
    }

    /// Run `access` on the non-null value of `recv`, or give null: `recv?.…`
    ///
    /// ```text
    /// { $t := recv; if $t == null { null } else { access($t!!) } }
    /// ```
    ///
    /// The result is optional (a plain `R` becomes `R?`); an access that produces nothing runs for effect.
    /// The `Result<T, E>` type behind `ty`, if it is one (the prelude's, whose fields are `ok`, `value`, `error`).
    fn result_type(&self, ty: Ty) -> Option<TypeId> {
        let Ty::Obj(t) = ty else { return None };
        let info = self.ck.t.ty(t);
        let names = ["ok", "value", "error"];
        let shape = info.fields.len() == 3 && info.fields.iter().zip(names).all(|(f, n)| f.name == n);
        (info.name.starts_with("Result<") && shape).then_some(t)
    }

    /// `result?`: the value of an `Ok`; for an `Err`, return from the function with the same error.
    ///
    /// ```text
    /// { $r := result; if !$r.ok { return Result<U, E>(false, null, $r.error) }; $r.value!! }
    /// ```
    fn propagate(&mut self, recv: hir::Expr, span: Span) -> Result<hir::Expr> {
        let Some(from) = self.result_type(recv.ty) else {
            return Err(Diagnostic::new(format!("`?` needs a Result, found a {}", self.name(recv.ty)), span)
                .hint("for a value that may be null, use `?:` or `!!`"));
        };
        let Some((to, Ty::Obj(_))) = self.ret.and_then(|r| self.result_type(r).map(|t| (t, r))) else {
            return Err(Diagnostic::new("`?` can only be used in a function that returns a Result", span)
                .hint("declare it with a result type, e.g. `-> Result<Int, String>`"));
        };
        let (from_fields, to_fields) = (self.ck.t.ty(from).fields.clone(), self.ck.t.ty(to).fields.clone());
        if from_fields[2].ty != to_fields[2].ty {
            let plain = |t: Ty| self.nullable_parts(t).map_or(t, |p| p.0);
            let (a, b) = (self.name(plain(from_fields[2].ty)), self.name(plain(to_fields[2].ty)));
            return Err(Diagnostic::new(format!("this Result has the error type {a}, but the function's has {b}"), span)
                .hint("convert the error first with `mapError`"));
        }
        let tmp = self.new_local("$res", recv.ty);
        let field = |cx: &Self, index: usize| mk(K::Field { obj: Box::new(cx.local_expr(tmp)), index }, from_fields[index].ty);
        let (ok, error) = (field(self, 0), field(self, 2));
        let no_value = self.coerce(mk(K::Null, Ty::Null), to_fields[1].ty, span)?;
        let ctor = self.ctor_of(to, span)?;
        let failure = self.call_values(
            ctor,
            None,
            vec![(mk(K::Bool(false), Ty::Bool), span), (no_value, span), (error, span)],
            span,
        )?;
        let mut leave = Vec::new();
        self.emit_return(Some(failure), &mut leave)?;
        let bail = mk(K::Block(leave, Box::new(unit())), Ty::Void);
        let check = mk(K::Cond { cond: Box::new(mk(K::Not(Box::new(ok)), Ty::Bool)), then: Box::new(bail), otherwise: Box::new(unit()) }, Ty::Void);
        let Some((plain, _)) = self.nullable_parts(from_fields[1].ty) else {
            return Err(Diagnostic::new("a Result's value cannot be read here", span));
        };
        let value = mk(K::Unwrap(Box::new(field(self, 1))), plain);
        Ok(mk(K::Block(vec![hir::Stmt::Let(tmp, Some(recv)), hir::Stmt::Expr(check)], Box::new(value)), plain))
    }

    fn safe_access(
        &mut self,
        recv: hir::Expr,
        span: Span,
        access: impl FnOnce(&mut Self, hir::Expr) -> Result<hir::Expr>,
    ) -> Result<hir::Expr> {
        // `result?.name` is `(result?).name`: a Result is never null, so `?.` can only mean that
        if self.result_type(recv.ty).is_some() {
            let plain = self.propagate(recv, span)?;
            return access(self, plain);
        }
        let Some((plain, boxed)) = self.nullable_parts(recv.ty) else {
            return Err(Diagnostic::new(format!("a {} is never null, so `?.` is pointless", self.name(recv.ty)), span)
                .hint("use `.` instead"));
        };
        let tmp = self.new_local("$safe", recv.ty);
        let present = {
            let t = self.local_expr(tmp);
            if boxed { mk(K::Unwrap(Box::new(t)), plain) } else { t }
        };
        let result = access(self, present)?;
        let is_null = mk(K::IsNull(Box::new(self.local_expr(tmp))), Ty::Bool);
        let bind = hir::Stmt::Let(tmp, Some(recv));
        if result.ty == Ty::Void {
            let check = hir::Stmt::Expr(mk(
                K::Cond { cond: Box::new(is_null), then: Box::new(unit()), otherwise: Box::new(result) },
                Ty::Void,
            ));
            return Ok(mk(K::Block(vec![bind, check], Box::new(unit())), Ty::Void));
        }
        let ty = self.ck.t.opt_of(result.ty);
        let some = if matches!(result.ty, Ty::Opt(_)) { result } else { mk(K::Wrap(Box::new(result)), ty) };
        let cond = mk(
            K::Cond { cond: Box::new(is_null), then: Box::new(mk(K::Null, ty)), otherwise: Box::new(some) },
            ty,
        );
        Ok(mk(K::Block(vec![bind], Box::new(cond)), ty))
    }

    /// Call a function value.
    fn call_value(&mut self, callee: hir::Expr, args: &[ast::Arg], span: Span, what: &str) -> Result<hir::Expr> {
        let Ty::Fn(sig_id) = callee.ty else {
            return Err(Diagnostic::new(
                format!("cannot call `{what}`: it is a {}, not a function", self.name(callee.ty)),
                span,
            ));
        };
        let sig = self.ck.t.fn_sig(sig_id);
        if let Some(n) = args.iter().find_map(|a| a.name.as_ref()) {
            return Err(Diagnostic::new("named arguments need a declared function", n.span)
                .hint("function values take their arguments by position"));
        }
        if args.len() != sig.params.len() {
            return Err(Diagnostic::new(
                format!("this function takes {} argument(s) but {} were given", sig.params.len(), args.len()),
                span,
            ));
        }
        let mut out = Vec::new();
        for (a, &want) in args.iter().zip(&sig.params) {
            let v = self.value_expected(&a.value, Some(want))?;
            out.push(self.coerce(v, want, a.value.span)?);
        }
        Ok(mk(K::CallValue { callee: Box::new(callee), args: out }, sig.ret))
    }

    /// A declared function (or constructor) used as a value.
    fn fn_value(&mut self, id: FnId, span: Span) -> Result<hir::Expr> {
        let ck = self.ck;
        let ret = ck.ret_of(id, span)?;
        let params: Vec<Ty> = ck.func(id).params.iter().map(|p| p.ty).collect();
        let thunk = ck.thunk_for(id, ret);
        Ok(mk(K::Closure { func: thunk }, ck.t.intern_fn(params, ret)))
    }

    /// For each argument, the index of the parameter it fills (by position or name).
    fn map_args(&self, fname: &str, params: &[&str], args: &[ast::Arg], span: Span) -> Result<Vec<usize>> {
        let mut filled = vec![false; params.len()];
        let mut order = Vec::with_capacity(args.len());
        let mut positional = 0;
        let mut named_seen = false;
        for a in args {
            let pi = match &a.name {
                None => {
                    if named_seen {
                        return Err(Diagnostic::new("a positional argument can't follow a named one", a.value.span));
                    }
                    positional += 1;
                    if positional > params.len() {
                        return Err(Diagnostic::new(
                            format!("`{fname}` takes {} argument(s) but {} were given", params.len(), args.len()),
                            span,
                        ));
                    }
                    positional - 1
                }
                Some(n) => {
                    named_seen = true;
                    let Some(pi) = params.iter().position(|p| *p == n.name) else {
                        return Err(Diagnostic::new(
                            format!("`{fname}` has no parameter named `{}`", n.name),
                            n.span,
                        ));
                    };
                    pi
                }
            };
            if filled[pi] {
                return Err(Diagnostic::new(format!("`{}` is given twice", params[pi]), a.value.span));
            }
            filled[pi] = true;
            order.push(pi);
        }
        Ok(order)
    }

    /// Call `id`, matching arguments to parameters by position and name and
    /// filling in defaults.
    fn call_fn(&mut self, id: FnId, recv: Option<hir::Expr>, args: &[ast::Arg], span: Span) -> Result<hir::Expr> {
        self.call_fn_pre(id, recv, args, span, Vec::new())
    }

    /// [`Self::call_fn`], where some arguments (by position in `args`) were
    /// already evaluated — type inference does that.
    fn call_fn_pre(
        &mut self,
        id: FnId,
        recv: Option<hir::Expr>,
        args: &[ast::Arg],
        span: Span,
        mut pre: Vec<Option<hir::Expr>>,
    ) -> Result<hir::Expr> {
        let ck = self.ck;
        let info = ck.func(id);
        let ret = ck.ret_of(id, span)?;
        let params = &info.params[usize::from(recv.is_some())..];
        let names: Vec<&str> = params.iter().map(|p| p.name.as_str()).collect();
        // The arguments from `tail` on are the items of the `vararg` parameter.
        let tail = vararg_tail(params.last().is_some_and(|p| p.vararg).then(|| params.len() - 1), args);
        let mapped = match tail {
            Some(k) => &args[..=k],
            None => args,
        };
        let order = self.map_args(&info.name, &names, mapped, span)?;
        pre.resize_with(args.len(), || None);

        // Evaluate arguments in the order they were written.
        let mut values: Vec<Option<hir::Expr>> = (0..params.len()).map(|_| None).collect();
        for (ai, (a, &pi)) in mapped.iter().zip(&order).enumerate() {
            if tail == Some(ai) {
                let items = (ai..args.len()).map(|i| (&args[i].value, pre[i].take())).collect();
                values[pi] = Some(self.list_from(items, Some(params[pi].ty), a.value.span)?);
                continue;
            }
            let v = match pre[ai].take() {
                Some(v) => v,
                None => self.value_expected(&a.value, Some(params[pi].ty))?,
            };
            values[pi] = Some(self.coerce(v, params[pi].ty, a.value.span)?);
        }

        // If named arguments reorder evaluation, pin each to a temporary first.
        let mut pins = Vec::new();
        if !order.windows(2).all(|w| w[0] < w[1]) {
            for &pi in &order {
                let v = values[pi].take().unwrap();
                let tmp = self.new_local("$arg", v.ty);
                pins.push(hir::Stmt::Let(tmp, Some(v)));
                values[pi] = Some(self.local_expr(tmp));
            }
        }

        // A computed default reads the arguments before it, so those are held in temporaries.
        let computes = params
            .iter()
            .zip(&values)
            .any(|(p, v)| v.is_none() && matches!(p.default, Some(ParamDefault::Computed(_))));
        let mut held: Vec<LocalId> = Vec::new();
        let mut call_args: Vec<hir::Expr> = Vec::new();
        if let Some(r) = recv {
            call_args.push(self.hold(r, computes, &mut pins, &mut held));
        }
        for (p, v) in params.iter().zip(values) {
            let arg = match (v, &p.default) {
                (Some(v), _) => v,
                (None, Some(ParamDefault::Const(c))) => self.coerce(const_expr(c), p.ty, span)?,
                (None, Some(ParamDefault::Computed(f))) => {
                    ck.ret_of(*f, span)?;
                    let args = held.iter().map(|&l| self.local_expr(l)).collect();
                    mk(K::Call { func: *f, args }, p.ty)
                }
                (None, None) if p.vararg => self.list_from(Vec::new(), Some(p.ty), span)?,
                (None, None) => {
                    return Err(Diagnostic::new(
                        format!("`{}` is missing the argument `{}`", info.name, p.name),
                        span,
                    ));
                }
            };
            call_args.push(self.hold(arg, computes, &mut pins, &mut held));
        }
        let call = mk(K::Call { func: id, args: call_args }, ret);
        Ok(if pins.is_empty() { call } else { mk(K::Block(pins, Box::new(call)), ret) })
    }

    /// When `active`, put `e` in a temporary (unless it is a variable already) so it can be read again.
    fn hold(&mut self, e: hir::Expr, active: bool, pins: &mut Vec<hir::Stmt>, held: &mut Vec<LocalId>) -> hir::Expr {
        if !active {
            return e;
        }
        if let K::Local(id) = e.kind {
            held.push(id);
            return e;
        }
        let tmp = self.new_local("$arg", e.ty);
        pins.push(hir::Stmt::Let(tmp, Some(e)));
        held.push(tmp);
        self.local_expr(tmp)
    }

    /// The built-in, type-polymorphic `print` / `println`.
    fn print(&mut self, newline: bool, args: &[ast::Arg], span: Span) -> Result<hir::Expr> {
        if args.is_empty() && !newline {
            return Err(Diagnostic::new("`print` needs something to print", span));
        }
        let mut out = Vec::new();
        for a in args {
            if let Some(n) = &a.name {
                return Err(Diagnostic::new("`print` doesn't take named arguments", n.span));
            }
            // `println(null)` is fine: it prints "null".
            let v = if matches!(a.value.kind, A::Null) { mk(K::Null, Ty::Null) } else { self.value(&a.value)? };
            out.push(match v.ty {
                Ty::Obj(_) | Ty::Opt(_) | Ty::Null | Ty::Bytes => self.stringify(v, a.value.span)?,
                Ty::Fn(_) => return Err(Diagnostic::new("cannot print a function", a.value.span)),
                _ => v,
            });
        }
        Ok(mk(K::Print { newline, args: out }, Ty::Void))
    }
}

/// Where the arguments of a `vararg` parameter (at index `k`) start: from there on, all are unnamed.
fn vararg_tail(k: Option<usize>, args: &[ast::Arg]) -> Option<usize> {
    let k = k?;
    (args.len() > k && args[k..].iter().all(|a| a.name.is_none())).then_some(k)
}

fn default_expr(ty: Ty) -> hir::Expr {
    match ty {
        Ty::Int | Ty::Char => mk(K::Int(0), ty),
        Ty::Float => mk(K::Float(0.0), Ty::Float),
        Ty::Bool => mk(K::Bool(false), Ty::Bool),
        Ty::Str => mk(K::Str(String::new()), Ty::Str),
        Ty::Bytes => mk(K::EmptyBytes, Ty::Bytes),
        Ty::Obj(_) | Ty::Fn(_) | Ty::Opt(_) | Ty::Null => mk(K::Null, ty),
        Ty::Void => unit(),
    }
}

fn const_expr(c: &Const) -> hir::Expr {
    match c {
        Const::Int(v) => mk(K::Int(*v), Ty::Int),
        Const::Float(v) => mk(K::Float(*v), Ty::Float),
        Const::Bool(v) => mk(K::Bool(*v), Ty::Bool),
        Const::Str(s) => mk(K::Str(s.clone()), Ty::Str),
        Const::Null => mk(K::Null, Ty::Null),
    }
}

/// Assigning to a `val` (or to an enum variant).
fn read_only(name: &str, span: Span) -> Diagnostic {
    Diagnostic::new(format!("cannot assign to `{name}`: it was declared with `val`"), span)
        .hint("declare it with `:=` if it has to change")
}
