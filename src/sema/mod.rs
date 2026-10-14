//! Semantic analysis: names and types.
//!
//! 1. **Declare** — register every type, function, method, constructor and
//!    global with its resolved signature ([`Tables`]).  Bodies are not looked at.
//! 2. **Check** — type-check every body against those tables, producing [`hir`].
//!
//! Because signatures are known up front, functions may call each other in any
//! order.  Two things *can't* be known up front and are inferred on demand:
//! the return type of `fn f() = expr` and the type of `x := expr` at the top
//! level.  Whoever first needs such a fact asks the [`Checker`], which checks
//! the defining body right then (and reports a cycle if it depends on itself).

mod body;
pub mod hir;

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use crate::diag::{Diagnostic, Result, Span};
use crate::loader::Loaded;
use crate::syntax::ast::{self, ExprKind, Item, StmtKind, TypeKind};
use hir::{FnId, FnSig, FuncKind, GlobalId, Ty, TypeId};

pub struct TypeInfo {
    pub name: String,
    pub fields: Vec<hir::Field>,
    pub methods: HashMap<String, FnId>,
    pub ctor: FnId,
}

/// A literal usable as a default argument.
#[derive(Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

pub struct ParamInfo {
    pub name: String,
    pub ty: Ty,
    pub default: Option<Const>,
}

pub struct FnInfo {
    pub name: String,
    pub module: usize,
    pub symbol: String,
    pub kind: FuncKind,
    /// Includes the receiver `this` for methods.
    pub params: Vec<ParamInfo>,
    /// `None` until inferred (only for `fn f() = expr` without `->`).
    pub ret: Cell<Option<Ty>>,
}

pub struct GlobalInfo {
    pub name: String,
    pub symbol: String,
    /// `None` until the defining `x := expr` has been checked.
    pub ty: Cell<Option<Ty>>,
    /// The module initialiser that assigns it.
    pub init_fn: FnId,
}

#[derive(Clone, Copy)]
pub enum Sym {
    Fn(FnId),
    Type(TypeId),
    Global(GlobalId),
}

/// Everything visible inside one module.
#[derive(Default)]
pub struct Scope {
    pub items: HashMap<String, Sym>,
    /// `import m` — `alias` → module index, for `alias.name(...)`.
    pub modules: HashMap<String, usize>,
    /// `import m.Name` — brings `Name` in unqualified.
    pub imported: HashMap<String, Sym>,
}

#[derive(Default)]
pub struct Tables {
    pub types: Vec<TypeInfo>,
    pub funcs: Vec<FnInfo>,
    pub globals: Vec<GlobalInfo>,
    pub scopes: Vec<Scope>,
    /// Interned function types, so equal signatures have equal `Ty`s.
    fn_types: RefCell<Vec<FnSig>>,
}

/// What a declared function needs when its body is checked.
pub enum Work<'a> {
    Fn(&'a ast::FnDecl),
    Method(&'a ast::FnDecl),
    Ctor { ty: TypeId, decl: &'a ast::TypeDecl },
    /// A module's top-level statements.
    Init { stmts: Vec<&'a ast::Stmt> },
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum State {
    New,
    InProgress,
    Done,
}

pub struct Checker<'a> {
    t: Tables,
    work: Vec<Work<'a>>,
    state: RefCell<Vec<State>>,
    /// Checked functions by id.  Grows past `work` with synthesised helpers.
    out: RefCell<Vec<Option<hir::Func>>>,
    /// Module initialisers in dependency order.
    inits: Vec<FnId>,
    /// Generated `toString` functions by type.
    tostring: RefCell<HashMap<TypeId, FnId>>,
    /// Adapters that let a plain function be used as a function value.
    thunks: RefCell<HashMap<FnId, FnId>>,
}

impl Tables {
    /// Resolve a bare name as seen from `module`: own items, imports, prelude.
    pub fn lookup(&self, module: usize, name: &str) -> Option<Sym> {
        let s = &self.scopes[module];
        s.items
            .get(name)
            .or_else(|| s.imported.get(name))
            .or_else(|| self.scopes[0].items.get(name))
            .copied()
    }

    /// Resolve a written type.  `Unit` (no value) is only meaningful as a return type.
    pub fn resolve_type(&self, module: usize, ty: &ast::TypeRef) -> Result<Ty> {
        match &ty.kind {
            TypeKind::Named(name) => Ok(match name.as_str() {
                "Int" => Ty::Int,
                "Float" => Ty::Float,
                "Bool" => Ty::Bool,
                "String" => Ty::Str,
                "Unit" => Ty::Void,
                name => match self.lookup(module, name) {
                    Some(Sym::Type(id)) => Ty::Obj(id),
                    _ => return Err(Diagnostic::new(format!("unknown type `{name}`"), ty.span)),
                },
            }),
            TypeKind::Fn { params, ret } => {
                let params = params.iter().map(|p| self.resolve_value_type(module, p)).collect::<Result<Vec<_>>>()?;
                let ret = self.resolve_type(module, ret)?;
                Ok(self.intern_fn(params, ret))
            }
        }
    }

    /// Like [`Self::resolve_type`], for places that hold a value (variables, fields, parameters).
    pub fn resolve_value_type(&self, module: usize, ty: &ast::TypeRef) -> Result<Ty> {
        match self.resolve_type(module, ty)? {
            Ty::Void => Err(Diagnostic::new("`Unit` means \"no value\"; it can only be a return type", ty.span)),
            t => Ok(t),
        }
    }

    pub fn intern_fn(&self, params: Vec<Ty>, ret: Ty) -> Ty {
        let sig = FnSig { params, ret };
        let mut types = self.fn_types.borrow_mut();
        let id = types.iter().position(|s| *s == sig).unwrap_or_else(|| {
            types.push(sig);
            types.len() - 1
        });
        Ty::Fn(id)
    }

    pub fn fn_sig(&self, id: hir::FnTyId) -> FnSig {
        self.fn_types.borrow()[id].clone()
    }

    pub fn ty_name(&self, ty: Ty) -> String {
        match ty {
            Ty::Int => "Int".into(),
            Ty::Float => "Float".into(),
            Ty::Bool => "Bool".into(),
            Ty::Str => "String".into(),
            Ty::Obj(id) => self.types[id].name.clone(),
            Ty::Fn(id) => {
                let sig = self.fn_sig(id);
                let params: Vec<String> = sig.params.iter().map(|&p| self.ty_name(p)).collect();
                let ret = if sig.ret == Ty::Void { "Unit".to_string() } else { self.ty_name(sig.ret) };
                format!("({}) -> {ret}", params.join(", "))
            }
            Ty::Void => "nothing".into(),
        }
    }

    fn add_fn(&mut self, info: FnInfo) -> FnId {
        self.funcs.push(info);
        self.funcs.len() - 1
    }

    fn define(&mut self, module: usize, name: &ast::Ident, sym: Sym) -> Result<()> {
        if self.scopes[module].items.insert(name.name.clone(), sym).is_some() {
            return Err(Diagnostic::new(format!("`{}` is already defined in this module", name.name), name.span));
        }
        Ok(())
    }
}

pub fn analyze(loaded: &Loaded) -> Result<hir::Program> {
    let ck = Checker::declare(loaded)?;
    // Initialisers first: they fix the types of inferred globals in order.
    for &id in &ck.inits {
        ck.ensure(id)?;
    }
    for id in 0..ck.work.len() {
        ck.ensure(id)?;
    }
    ck.finish(loaded.modules.len() - 1)
}

impl<'a> Checker<'a> {
    fn declare(loaded: &'a Loaded) -> Result<Self> {
        let mut ck = Checker {
            t: Tables::default(),
            work: Vec::new(),
            state: RefCell::new(Vec::new()),
            out: RefCell::new(Vec::new()),
            inits: Vec::new(),
            tostring: RefCell::new(HashMap::new()),
            thunks: RefCell::new(HashMap::new()),
        };
        for module in 0..loaded.modules.len() {
            ck.declare_module(loaded, module)?;
        }
        let n = ck.work.len();
        *ck.state.borrow_mut() = vec![State::New; n];
        *ck.out.borrow_mut() = (0..n).map(|_| None).collect();
        Ok(ck)
    }

    fn declare_module(&mut self, loaded: &'a Loaded, mi: usize) -> Result<()> {
        let module = &loaded.modules[mi];
        self.t.scopes.push(Scope::default());

        // 1. Type names, so signatures and fields can refer to any of them.
        let mut own_types: Vec<(TypeId, &ast::TypeDecl)> = Vec::new();
        for item in &module.ast.items {
            if let Item::Type(td) = item {
                let id = self.t.types.len();
                self.t.types.push(TypeInfo {
                    name: td.name.name.clone(),
                    fields: Vec::new(),
                    methods: HashMap::new(),
                    ctor: usize::MAX,
                });
                self.t.define(mi, &td.name, Sym::Type(id))?;
                own_types.push((id, td));
            }
        }

        // 2. Imports (all imported modules are already declared).
        for imp in &module.imports {
            if let Some(alias) = &imp.alias {
                match self.t.scopes[mi].modules.insert(alias.clone(), imp.module) {
                    Some(prev) if prev != imp.module => {
                        return Err(Diagnostic::new(
                            format!("`{alias}` is imported twice for different modules"),
                            imp.span,
                        ));
                    }
                    _ => {}
                }
            }
            if let Some(sym) = &imp.symbol {
                let Some(found) = self.t.scopes[imp.module].items.get(&sym.name).copied() else {
                    return Err(Diagnostic::new(
                        format!("module `{}` has no item `{}`", loaded.modules[imp.module].name, sym.name),
                        sym.span,
                    ));
                };
                self.t.scopes[mi].imported.insert(sym.name.clone(), found);
            }
        }

        // 3. Field types: primary-constructor parameters first, then body fields.
        for &(id, td) in &own_types {
            let mut fields: Vec<hir::Field> = Vec::new();
            let declared = td
                .primary
                .iter()
                .flatten()
                .map(|p| (&p.name, &p.ty))
                .chain(td.fields.iter().map(|f| (&f.name, &f.ty)));
            for (name, ty) in declared {
                if fields.iter().any(|g| g.name == name.name) {
                    return Err(Diagnostic::new(format!("duplicate field `{}`", name.name), name.span));
                }
                fields.push(hir::Field { name: name.name.clone(), ty: self.t.resolve_value_type(mi, ty)? });
            }
            if fields.len() > 64 {
                return Err(Diagnostic::new("a type can have at most 64 fields", td.name.span));
            }
            if td.primary.is_some()
                && let Some(c) = &td.ctor
            {
                return Err(Diagnostic::new(
                    "a type with a primary constructor cannot also declare `constructor`",
                    c.params.first().map_or(td.name.span, |p| p.name.span),
                ));
            }
            self.t.types[id].fields = fields;
        }

        // 4. Functions, methods and constructors.
        let mut top: Vec<&'a ast::Stmt> = Vec::new();
        for item in &module.ast.items {
            match item {
                Item::Fn(decl) => {
                    let info = self.signature(mi, decl, FuncKind::Free, None, symbol_for(&module.name, None, decl))?;
                    let id = self.t.add_fn(info);
                    self.work.push(Work::Fn(decl));
                    self.t.define(mi, &decl.name, Sym::Fn(id))?;
                }
                Item::Method { owner, decl } => {
                    let ty = match self.t.lookup(mi, &owner.name) {
                        Some(Sym::Type(id)) => id,
                        _ => return Err(Diagnostic::new(format!("unknown type `{}`", owner.name), owner.span)),
                    };
                    self.declare_method(&module.name, mi, ty, decl)?;
                }
                Item::Stmt(s) => top.push(s),
                Item::Type(_) => {}
            }
        }
        for &(id, td) in &own_types {
            for m in &td.methods {
                self.declare_method(&module.name, mi, id, m)?;
            }
            self.declare_ctor(&module.name, mi, id, td)?;
        }

        // 5. Top-level statements: an initialiser function plus a global per `x := …`.
        if !top.is_empty() {
            let init_fn = self.t.add_fn(FnInfo {
                name: "<init>".into(),
                module: mi,
                symbol: format!("{}__init", module.name),
                kind: FuncKind::Free,
                params: Vec::new(),
                ret: Cell::new(Some(Ty::Void)),
            });
            for s in &top {
                if let StmtKind::Let { name, ty, .. } = &s.kind {
                    let declared = ty.as_ref().map(|t| self.t.resolve_value_type(mi, t)).transpose()?;
                    let gid = self.t.globals.len();
                    self.t.globals.push(GlobalInfo {
                        name: name.name.clone(),
                        symbol: format!("{}__g_{}", module.name, name.name),
                        ty: Cell::new(declared),
                        init_fn,
                    });
                    self.t.define(mi, name, Sym::Global(gid))?;
                }
            }
            self.work.push(Work::Init { stmts: top });
            self.inits.push(init_fn);
        }
        Ok(())
    }

    fn param_info(&self, mi: usize, p: &ast::Param) -> Result<ParamInfo> {
        let ty = self.t.resolve_value_type(mi, &p.ty)?;
        let default = p.default.as_ref().map(|e| const_default(e, ty, &self.t)).transpose()?;
        Ok(ParamInfo { name: p.name.name.clone(), ty, default })
    }

    fn params(&self, mi: usize, params: &[ast::Param], receiver: Option<TypeId>) -> Result<Vec<ParamInfo>> {
        let mut out: Vec<ParamInfo> = Vec::new();
        if let Some(ty) = receiver {
            out.push(ParamInfo { name: "this".into(), ty: Ty::Obj(ty), default: None });
        }
        for p in params {
            if out.iter().any(|q| q.name == p.name.name) {
                return Err(Diagnostic::new(format!("duplicate parameter `{}`", p.name.name), p.name.span));
            }
            out.push(self.param_info(mi, p)?);
        }
        Ok(out)
    }

    /// Resolve a function's parameter and return types.
    fn signature(
        &self,
        mi: usize,
        decl: &ast::FnDecl,
        kind: FuncKind,
        receiver: Option<TypeId>,
        symbol: String,
    ) -> Result<FnInfo> {
        let params = self.params(mi, &decl.params, receiver)?;
        let ret = match &decl.ret {
            Some(r) => Some(self.t.resolve_type(mi, r)?),
            // Only `fn f() = expr` can infer its return type; anything else returns nothing.
            None if matches!(decl.body, ast::FnBody::Expr(_)) => None,
            None => Some(Ty::Void),
        };
        if matches!(decl.body, ast::FnBody::Extern { .. }) {
            let c_types_only =
                params.iter().map(|p| p.ty).chain(ret).all(|ty| !matches!(ty, Ty::Obj(_)));
            if !c_types_only {
                return Err(Diagnostic::new("`extern fn` can only use Int, Float, Bool and String", decl.span));
            }
        }
        Ok(FnInfo { name: decl.name.name.clone(), module: mi, symbol, kind, params, ret: Cell::new(ret) })
    }

    fn declare_method(&mut self, module_name: &str, mi: usize, ty: TypeId, decl: &'a ast::FnDecl) -> Result<()> {
        if matches!(decl.body, ast::FnBody::Extern { .. }) {
            return Err(Diagnostic::new("methods cannot be `extern`", decl.span));
        }
        let owner = self.t.types[ty].name.clone();
        let symbol = symbol_for(module_name, Some(&owner), decl);
        let info = self.signature(mi, decl, FuncKind::Method, Some(ty), symbol)?;
        let id = self.t.add_fn(info);
        if self.t.types[ty].methods.insert(decl.name.name.clone(), id).is_some() {
            return Err(Diagnostic::new(
                format!("`{owner}` already has a method `{}`", decl.name.name),
                decl.name.span,
            ));
        }
        self.work.push(Work::Method(decl));
        Ok(())
    }

    fn declare_ctor(&mut self, module_name: &str, mi: usize, ty: TypeId, td: &'a ast::TypeDecl) -> Result<()> {
        let params = match (&td.ctor, &td.primary) {
            (Some(c), _) => self.params(mi, &c.params, None)?,
            (None, Some(p)) => self.params(mi, p, None)?,
            (None, None) => Vec::new(),
        };
        let id = self.t.add_fn(FnInfo {
            name: td.name.name.clone(),
            module: mi,
            symbol: format!("{module_name}__{}__new", td.name.name),
            kind: FuncKind::Constructor,
            params,
            ret: Cell::new(Some(Ty::Obj(ty))),
        });
        self.t.types[ty].ctor = id;
        self.work.push(Work::Ctor { ty, decl: td });
        Ok(())
    }

    // ── on-demand checking ──────────────────────────────────────

    /// Check function `id` (once).  Re-entrant requests for a function that is
    /// currently being checked return immediately; callers that need its
    /// result use [`Self::ret_of`], which reports the cycle.
    fn ensure(&self, id: FnId) -> Result<()> {
        if self.state.borrow()[id] != State::New {
            return Ok(());
        }
        self.state.borrow_mut()[id] = State::InProgress;
        let func = body::check_fn(self, id, &self.work[id])?;
        self.out.borrow_mut()[id] = Some(func);
        self.state.borrow_mut()[id] = State::Done;
        Ok(())
    }

    /// Allocate an id for a function created while checking another (a lambda or adapter).
    fn reserve_fn(&self) -> FnId {
        let mut out = self.out.borrow_mut();
        out.push(None);
        out.len() - 1
    }

    /// The return type of `id`, inferring it from the body if necessary.
    fn ret_of(&self, id: FnId, span: Span) -> Result<Ty> {
        let info = &self.t.funcs[id];
        if let Some(ty) = info.ret.get() {
            return Ok(ty);
        }
        if self.state.borrow()[id] == State::InProgress {
            return Err(Diagnostic::new(
                format!("cannot infer the return type of `{}`: it depends on itself", info.name),
                span,
            )
            .hint("add `-> Type` to its signature"));
        }
        self.ensure(id)?;
        Ok(info.ret.get().expect("checking a function fixes its return type"))
    }

    /// The type of a global, checking its module's initialiser if needed.
    fn global_ty(&self, gid: GlobalId, span: Span) -> Result<Ty> {
        let g = &self.t.globals[gid];
        if let Some(ty) = g.ty.get() {
            return Ok(ty);
        }
        if self.state.borrow()[g.init_fn] == State::InProgress {
            return Err(Diagnostic::new(format!("cannot infer the type of `{}` here", g.name), span)
                .hint("it is used before its definition has been seen; annotate it, e.g. `name: Int := …`"));
        }
        self.ensure(g.init_fn)?;
        Ok(g.ty.get().expect("checking the initialiser fixes global types"))
    }

    fn finish(self, entry_module: usize) -> Result<hir::Program> {
        let main = match self.t.scopes[entry_module].items.get("main") {
            Some(&Sym::Fn(id)) => Some(id),
            _ => None,
        };
        if let Some(id) = main {
            let m = &self.t.funcs[id];
            if !m.params.is_empty() || !matches!(m.ret.get(), Some(Ty::Void | Ty::Int)) {
                return Err(Diagnostic::general("`main` must take no parameters and return nothing or Int"));
            }
        }
        let entry_has_script = self.inits.iter().any(|&i| self.t.funcs[i].module == entry_module);
        if main.is_none() && !entry_has_script {
            return Err(Diagnostic::general("the program has no `fn main()` and no top-level statements to run")
                .hint("write statements directly in the file, or define `fn main() { … }`"));
        }

        let globals = self
            .t
            .globals
            .iter()
            .map(|g| hir::Global {
                name: g.name.clone(),
                symbol: g.symbol.clone(),
                ty: g.ty.get().expect("every global is typed by its initialiser"),
            })
            .collect();
        let types = self.t.types.into_iter().map(|ti| hir::TypeDef { name: ti.name, fields: ti.fields }).collect();
        let funcs = self.out.into_inner().into_iter().map(|f| f.expect("every function was checked")).collect();
        Ok(hir::Program { types, globals, funcs, inits: self.inits, main })
    }
}

fn symbol_for(module: &str, owner: Option<&str>, decl: &ast::FnDecl) -> String {
    match (&decl.body, owner) {
        (ast::FnBody::Extern { symbol }, _) => symbol.clone().unwrap_or_else(|| decl.name.name.clone()),
        (_, Some(ty)) => format!("{module}__{ty}__{}", decl.name.name),
        (_, None) => format!("{module}__{}", decl.name.name),
    }
}

/// Default arguments must be literals (possibly negated), matching the parameter type.
fn const_default(e: &ast::Expr, ty: Ty, t: &Tables) -> Result<Const> {
    let lit = match &e.kind {
        ExprKind::Int(v) => Some((Const::Int(*v), Ty::Int)),
        ExprKind::Float(v) => Some((Const::Float(*v), Ty::Float)),
        ExprKind::Bool(v) => Some((Const::Bool(*v), Ty::Bool)),
        ExprKind::Str(s) => Some((Const::Str(s.clone()), Ty::Str)),
        ExprKind::Unary(ast::UnOp::Neg, inner) => match &inner.kind {
            ExprKind::Int(v) => Some((Const::Int(v.wrapping_neg()), Ty::Int)),
            ExprKind::Float(v) => Some((Const::Float(-v), Ty::Float)),
            _ => None,
        },
        _ => None,
    };
    match lit {
        None => Err(Diagnostic::new("default values must be literals", e.span)),
        Some((c, found)) if found == ty => Ok(c),
        Some((Const::Int(v), Ty::Int)) if ty == Ty::Float => Ok(Const::Float(v as f64)),
        Some((_, found)) => Err(Diagnostic::new(
            format!("expected a {} default, found {}", t.ty_name(ty), t.ty_name(found)),
            e.span,
        )),
    }
}
