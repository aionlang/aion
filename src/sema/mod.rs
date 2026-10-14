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
mod generics;
pub mod hir;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::diag::{Diagnostic, Result, Span};
use crate::loader::Loaded;
use crate::syntax::ast::{self, ExprKind, Item, StmtKind};
use hir::{FnId, FnSig, FuncKind, GlobalId, Ty, TypeId};

/// Type parameters bound to concrete types, e.g. `{T → Int}`.
pub type Subst = Rc<HashMap<String, Ty>>;

#[derive(Clone)]
pub struct TypeInfo {
    pub name: String,
    pub fields: Vec<hir::Field>,
    pub methods: HashMap<String, FnId>,
    /// Methods with their own type parameters, by name → index into the checker's templates.
    pub generic_methods: HashMap<String, usize>,
    pub ctor: FnId,
    /// `Some(T)` for the built-in `Array<T>`, whose operations are intrinsics.
    pub array_elem: Option<Ty>,
}

/// A generic function (or a generic method of a generic type's instance):
/// a template checked afresh for each combination of type arguments.
pub struct GenericFn<'a> {
    pub module: usize,
    pub decl: &'a ast::FnDecl,
    /// Parameters of the enclosing generic type, already bound.
    pub base: Subst,
    pub tparams: Vec<String>,
    /// The type whose method this is.
    pub owner: Option<TypeId>,
    pub instances: RefCell<HashMap<Vec<Ty>, FnId>>,
}

/// A generic type: a template instantiated per list of type arguments.
pub struct GenericType<'a> {
    pub module: usize,
    pub decl: &'a ast::TypeDecl,
    pub tparams: Vec<String>,
    /// `fn List<T>.first()` written outside the type: (module it is written in,
    /// names for the type's parameters, declaration).
    pub extra_methods: RefCell<Vec<(usize, Vec<String>, &'a ast::FnDecl)>>,
    pub instances: RefCell<HashMap<Vec<Ty>, TypeId>>,
}

/// A literal usable as a default argument.
#[derive(Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

#[derive(Clone)]
pub struct ParamInfo {
    pub name: String,
    pub ty: Ty,
    pub default: Option<Const>,
}

#[derive(Clone)]
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
    /// Index into the checker's generic function templates.
    GenericFn(usize),
    /// Index into the checker's generic type templates.
    GenericType(usize),
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
    /// Growable: generic instantiation adds types while bodies are checked.
    types: RefCell<Vec<Rc<TypeInfo>>>,
    pub globals: Vec<GlobalInfo>,
    pub scopes: Vec<Scope>,
    /// Interned function types, so equal signatures have equal `Ty`s.
    fn_types: RefCell<Vec<FnSig>>,
    /// The `Array<T>` type for each element type.
    arrays: RefCell<HashMap<Ty, TypeId>>,
    /// Interned inner types of optionals: `Ty::Opt(i)` is `opt_types[i]?`.
    opt_types: RefCell<Vec<Ty>>,
}

/// What a declared function needs when its body is checked.
#[derive(Clone)]
pub enum Work<'a> {
    Fn { decl: &'a ast::FnDecl, subst: Subst },
    Method { decl: &'a ast::FnDecl, subst: Subst },
    Ctor { ty: TypeId, decl: &'a ast::TypeDecl, subst: Subst },
    /// A module's top-level statements.
    Init { stmts: Vec<&'a ast::Stmt> },
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum State {
    New,
    InProgress,
    Done,
}

/// Everything known about one function id.  Declared functions have `info` and
/// `work`; lambdas and generated adapters only get a finished `func`.
struct FnSlot<'a> {
    info: Option<Rc<FnInfo>>,
    work: Option<Work<'a>>,
    state: State,
    func: Option<hir::Func>,
    /// Members of generic instances are only checked when something uses them
    /// (a `List<Fn>` is fine as long as nobody calls `join` on it).
    lazy: bool,
    used: bool,
}

pub struct Checker<'a> {
    t: Tables,
    /// One slot per function id; grows as lambdas, adapters and instances appear.
    fns: RefCell<Vec<FnSlot<'a>>>,
    /// Module initialisers in dependency order.
    inits: Vec<FnId>,
    /// Generated `toString` functions by type.
    tostring: RefCell<HashMap<TypeId, FnId>>,
    /// Adapters that let a plain function be used as a function value.
    thunks: RefCell<HashMap<FnId, FnId>>,
    generic_fns: RefCell<Vec<Rc<GenericFn<'a>>>>,
    generic_types: RefCell<Vec<Rc<GenericType<'a>>>>,
    /// For a type that is an instance of a generic one: which, and with what arguments.
    instance_of: RefCell<HashMap<TypeId, (usize, Vec<Ty>)>>,
    module_names: Vec<String>,
    empty: Subst,
    /// Nesting depth of generic instantiation, to stop runaway recursion.
    depth: Cell<u32>,
    /// Methods added to the built-in types: `fn String.shout() = …`.
    ext_methods: RefCell<HashMap<(u8, String), FnId>>,
    /// Names of module variables that functions, lambdas or other modules assign:
    /// these are never narrowed by null checks.
    unstable_globals: HashSet<String>,
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

    pub fn intern_fn(&self, params: Vec<Ty>, ret: Ty) -> Ty {
        let sig = FnSig { params, ret };
        let mut types = self.fn_types.borrow_mut();
        let id = types.iter().position(|s| *s == sig).unwrap_or_else(|| {
            types.push(sig);
            types.len() - 1
        });
        Ty::Fn(id)
    }

    /// The built-in type `Array<elem>`.
    pub fn array_of(&self, elem: Ty) -> Ty {
        if let Some(&id) = self.arrays.borrow().get(&elem) {
            return Ty::Obj(id);
        }
        let id = self.add_type(TypeInfo {
            name: format!("Array<{}>", self.ty_name(elem)),
            fields: Vec::new(),
            methods: HashMap::new(),
            generic_methods: HashMap::new(),
            ctor: usize::MAX,
            array_elem: Some(elem),
        });
        self.arrays.borrow_mut().insert(elem, id);
        Ty::Obj(id)
    }

    /// `inner?`.  An optional of an optional is the same optional.
    pub fn opt_of(&self, inner: Ty) -> Ty {
        if matches!(inner, Ty::Opt(_)) {
            return inner;
        }
        let mut types = self.opt_types.borrow_mut();
        let id = types.iter().position(|&t| t == inner).unwrap_or_else(|| {
            types.push(inner);
            types.len() - 1
        });
        Ty::Opt(id)
    }

    /// The `T` of `T?`.
    pub fn opt_inner(&self, id: hir::OptId) -> Ty {
        self.opt_types.borrow()[id]
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
            Ty::Obj(id) => self.ty(id).name.clone(),
            Ty::Fn(id) => {
                let sig = self.fn_sig(id);
                let params: Vec<String> = sig.params.iter().map(|&p| self.ty_name(p)).collect();
                let ret = if sig.ret == Ty::Void { "Unit".to_string() } else { self.ty_name(sig.ret) };
                format!("({}) -> {ret}", params.join(", "))
            }
            Ty::Opt(id) => match self.opt_inner(id) {
                inner @ Ty::Fn(_) => format!("({})?", self.ty_name(inner)),
                inner => format!("{}?", self.ty_name(inner)),
            },
            Ty::Null => "null".into(),
            Ty::Void => "nothing".into(),
        }
    }

    pub fn ty(&self, id: TypeId) -> Rc<TypeInfo> {
        self.types.borrow()[id].clone()
    }

    fn add_type(&self, info: TypeInfo) -> TypeId {
        let mut types = self.types.borrow_mut();
        types.push(Rc::new(info));
        types.len() - 1
    }

    fn update_type(&self, id: TypeId, f: impl FnOnce(&mut TypeInfo)) {
        f(Rc::make_mut(&mut self.types.borrow_mut()[id]));
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
    // Checking can create more functions (lambdas, instances) and mark lazy ones as used,
    // so keep sweeping until nothing new needs checking.
    loop {
        let mut checked = false;
        let mut id = 0;
        while id < ck.fns.borrow().len() {
            let wanted = {
                let fns = ck.fns.borrow();
                fns[id].state == State::New && (!fns[id].lazy || fns[id].used)
            };
            if wanted {
                ck.ensure(id)?;
                checked = true;
            }
            id += 1;
        }
        if !checked {
            break;
        }
    }
    ck.finish(loaded.modules.len() - 1)
}

impl<'a> Checker<'a> {
    fn declare(loaded: &'a Loaded) -> Result<Self> {
        let mut ck = Checker {
            t: Tables::default(),
            fns: RefCell::new(Vec::new()),
            inits: Vec::new(),
            tostring: RefCell::new(HashMap::new()),
            thunks: RefCell::new(HashMap::new()),
            generic_fns: RefCell::new(Vec::new()),
            generic_types: RefCell::new(Vec::new()),
            instance_of: RefCell::new(HashMap::new()),
            module_names: loaded.modules.iter().map(|m| m.name.clone()).collect(),
            empty: Rc::new(HashMap::new()),
            depth: Cell::new(0),
            ext_methods: RefCell::new(HashMap::new()),
            unstable_globals: unstable_globals(loaded),
        };
        for module in 0..loaded.modules.len() {
            ck.declare_module(loaded, module)?;
        }
        Ok(ck)
    }

    fn declare_module(&mut self, loaded: &'a Loaded, mi: usize) -> Result<()> {
        let module = &loaded.modules[mi];
        let empty = self.empty.clone();
        self.t.scopes.push(Scope::default());

        // 1. Names of types and generic items, so any signature or field can mention them.
        let mut own_types: Vec<(TypeId, &ast::TypeDecl)> = Vec::new();
        for item in &module.ast.items {
            match item {
                Item::Type(td) if td.type_params.is_empty() => {
                    let id = self.t.add_type(TypeInfo {
                        name: td.name.name.clone(),
                        fields: Vec::new(),
                        methods: HashMap::new(),
                        generic_methods: HashMap::new(),
                        ctor: usize::MAX,
                        array_elem: None,
                    });
                    self.t.define(mi, &td.name, Sym::Type(id))?;
                    own_types.push((id, td));
                }
                Item::Type(td) => {
                    let g = self.add_generic_type(mi, td);
                    self.t.define(mi, &td.name, Sym::GenericType(g))?;
                }
                Item::Fn(decl) if !decl.type_params.is_empty() => {
                    if matches!(decl.body, ast::FnBody::Extern { .. }) {
                        return Err(Diagnostic::new("an `extern fn` cannot be generic", decl.name.span));
                    }
                    let g = self.add_generic_fn(GenericFn {
                        module: mi,
                        decl,
                        base: empty.clone(),
                        tparams: decl.type_params.iter().map(|t| t.name.clone()).collect(),
                        owner: None,
                        instances: RefCell::new(HashMap::new()),
                    });
                    self.t.define(mi, &decl.name, Sym::GenericFn(g))?;
                }
                _ => {}
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

        // Methods written outside a generic type join its template (and its existing instances).
        for item in &module.ast.items {
            if let Item::Method { owner, owner_params, decl } = item
                && let Some(Sym::GenericType(g)) = self.t.lookup(mi, &owner.name)
            {
                self.add_extra_method(g, mi, owner, owner_params, decl)?;
            }
        }

        // 3. Field types: primary-constructor parameters first, then body fields.
        for &(id, td) in &own_types {
            let fields = self.type_fields(mi, td, &empty)?;
            self.t.update_type(id, |ti| ti.fields = fields);
        }

        // 4. Functions, methods and constructors.
        let mut top: Vec<&'a ast::Stmt> = Vec::new();
        for item in &module.ast.items {
            match item {
                Item::Fn(decl) if decl.type_params.is_empty() => {
                    let info =
                        self.signature(mi, decl, FuncKind::Free, None, symbol_for(&module.name, None, decl), &empty)?;
                    let id = self.declare_fn(info, Work::Fn { decl, subst: empty.clone() });
                    self.t.define(mi, &decl.name, Sym::Fn(id))?;
                }
                Item::Method { owner, owner_params, decl } if builtin_type(&owner.name).is_some() => {
                    if let Some(p) = owner_params.first() {
                        return Err(Diagnostic::new(format!("`{}` is not generic", owner.name), p.span));
                    }
                    self.declare_builtin_method(&module.name, mi, builtin_type(&owner.name).unwrap(), decl)?;
                }
                Item::Method { owner, owner_params, decl } => match self.t.lookup(mi, &owner.name) {
                    Some(Sym::Type(ty)) => {
                        if let Some(p) = owner_params.first() {
                            return Err(Diagnostic::new(
                                format!("`{}` is not generic", owner.name),
                                p.span,
                            ));
                        }
                        self.declare_method(&module.name, mi, ty, decl, &empty)?;
                    }
                    Some(Sym::GenericType(_)) => {} // attached above
                    _ => return Err(Diagnostic::new(format!("unknown type `{}`", owner.name), owner.span)),
                },
                Item::Stmt(s) => top.push(s),
                _ => {}
            }
        }
        for &(id, td) in &own_types {
            for m in &td.methods {
                self.declare_method(&module.name, mi, id, m, &empty)?;
            }
            self.declare_ctor(&module.name, mi, id, td, &empty)?;
        }

        // 5. Top-level statements: an initialiser function plus a global per `x := …`.
        if !top.is_empty() {
            let init_fn = self.declare_fn(
                FnInfo {
                    name: "<init>".into(),
                    module: mi,
                    symbol: format!("{}__init", module.name),
                    kind: FuncKind::Free,
                    params: Vec::new(),
                    ret: Cell::new(Some(Ty::Void)),
                },
                Work::Init { stmts: top.clone() },
            );
            for s in &top {
                if let StmtKind::Let { name, ty, .. } = &s.kind {
                    let declared = ty.as_ref().map(|t| self.resolve_value_type(mi, t, &empty)).transpose()?;
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
            self.inits.push(init_fn);
        }
        Ok(())
    }

    /// The fields of a type declaration: primary-constructor parameters, then body fields.
    fn type_fields(&self, mi: usize, td: &ast::TypeDecl, subst: &Subst) -> Result<Vec<hir::Field>> {
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
            fields.push(hir::Field { name: name.name.clone(), ty: self.resolve_value_type(mi, ty, subst)? });
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
        Ok(fields)
    }

    fn param_info(&self, mi: usize, p: &ast::Param, subst: &Subst) -> Result<ParamInfo> {
        let ty = self.resolve_value_type(mi, &p.ty, subst)?;
        let default = p.default.as_ref().map(|e| const_default(e, ty, &self.t)).transpose()?;
        Ok(ParamInfo { name: p.name.name.clone(), ty, default })
    }

    fn params(
        &self,
        mi: usize,
        params: &[ast::Param],
        receiver: Option<Ty>,
        subst: &Subst,
    ) -> Result<Vec<ParamInfo>> {
        let mut out: Vec<ParamInfo> = Vec::new();
        if let Some(ty) = receiver {
            out.push(ParamInfo { name: "this".into(), ty, default: None });
        }
        for p in params {
            if out.iter().any(|q| q.name == p.name.name) {
                return Err(Diagnostic::new(format!("duplicate parameter `{}`", p.name.name), p.name.span));
            }
            out.push(self.param_info(mi, p, subst)?);
        }
        Ok(out)
    }

    /// Resolve a function's parameter and return types.
    fn signature(
        &self,
        mi: usize,
        decl: &ast::FnDecl,
        kind: FuncKind,
        receiver: Option<Ty>,
        symbol: String,
        subst: &Subst,
    ) -> Result<FnInfo> {
        let params = self.params(mi, &decl.params, receiver, subst)?;
        let ret = match &decl.ret {
            Some(r) => Some(self.resolve_type(mi, r, subst)?),
            // Only `fn f() = expr` can infer its return type; anything else returns nothing.
            None if matches!(decl.body, ast::FnBody::Expr(_)) => None,
            None => Some(Ty::Void),
        };
        if matches!(decl.body, ast::FnBody::Extern { .. }) {
            // Function values may be passed to C (as an opaque closure pointer), never returned.
            let plain = |ty: Ty| !matches!(ty, Ty::Obj(_) | Ty::Opt(_) | Ty::Null);
            let ok = params.iter().all(|p| plain(p.ty)) && ret.is_none_or(|r| plain(r) && !matches!(r, Ty::Fn(_)));
            if !ok {
                return Err(Diagnostic::new(
                    "`extern fn` can only use Int, Float, Bool and String (and take function values)",
                    decl.span,
                ));
            }
        }
        Ok(FnInfo { name: decl.name.name.clone(), module: mi, symbol, kind, params, ret: Cell::new(ret) })
    }

    /// Declare method `decl` on type `ty`.  Under `subst` for instances of generic types.
    fn declare_method(
        &self,
        module_name: &str,
        mi: usize,
        ty: TypeId,
        decl: &'a ast::FnDecl,
        subst: &Subst,
    ) -> Result<()> {
        if matches!(decl.body, ast::FnBody::Extern { .. }) {
            return Err(Diagnostic::new("methods cannot be `extern`", decl.span));
        }
        let owner = self.t.ty(ty).name.clone();
        let existing = self.t.ty(ty);
        if existing.methods.contains_key(&decl.name.name) || existing.generic_methods.contains_key(&decl.name.name) {
            return Err(Diagnostic::new(
                format!("`{owner}` already has a method `{}`", decl.name.name),
                decl.name.span,
            ));
        }
        // A method with its own type parameters is a template.
        if !decl.type_params.is_empty() {
            let g = self.add_generic_fn(GenericFn {
                module: mi,
                decl,
                base: subst.clone(),
                tparams: decl.type_params.iter().map(|t| t.name.clone()).collect(),
                owner: Some(ty),
                instances: RefCell::new(HashMap::new()),
            });
            self.t.update_type(ty, |ti| {
                ti.generic_methods.insert(decl.name.name.clone(), g);
            });
            return Ok(());
        }
        let symbol = method_symbol(module_name, &owner, ty, &decl.name.name);
        let info = self.signature(mi, decl, FuncKind::Method, Some(Ty::Obj(ty)), symbol, subst)?;
        let id = self.declare_fn(info, Work::Method { decl, subst: subst.clone() });
        self.t.update_type(ty, |ti| {
            ti.methods.insert(decl.name.name.clone(), id);
        });
        Ok(())
    }

    fn declare_ctor(
        &self,
        module_name: &str,
        mi: usize,
        ty: TypeId,
        td: &'a ast::TypeDecl,
        subst: &Subst,
    ) -> Result<()> {
        let params = match (&td.ctor, &td.primary) {
            (Some(c), _) => self.params(mi, &c.params, None, subst)?,
            (None, Some(p)) => self.params(mi, p, None, subst)?,
            (None, None) => Vec::new(),
        };
        let owner = self.t.ty(ty).name.clone();
        let id = self.declare_fn(
            FnInfo {
                name: td.name.name.clone(),
                module: mi,
                symbol: method_symbol(module_name, &owner, ty, "new"),
                kind: FuncKind::Constructor,
                params,
                ret: Cell::new(Some(Ty::Obj(ty))),
            },
            Work::Ctor { ty, decl: td, subst: subst.clone() },
        );
        self.t.update_type(ty, |ti| ti.ctor = id);
        Ok(())
    }

    /// `fn String.shout() = …` / `extern fn String.length() -> Int`: a method on a built-in type.
    fn declare_builtin_method(&self, module_name: &str, mi: usize, ty: Ty, decl: &'a ast::FnDecl) -> Result<()> {
        let key = (builtin_key(ty), decl.name.name.clone());
        if self.ext_methods.borrow().contains_key(&key) {
            return Err(Diagnostic::new(
                format!("`{}` already has a method `{}`", self.t.ty_name(ty), decl.name.name),
                decl.name.span,
            ));
        }
        if !decl.type_params.is_empty() {
            return Err(Diagnostic::new("methods on built-in types cannot be generic", decl.name.span));
        }
        let symbol = symbol_for(module_name, Some(&self.t.ty_name(ty)), decl);
        let info = self.signature(mi, decl, FuncKind::Method, Some(ty), symbol, &self.empty)?;
        let id = self.declare_fn(info, Work::Method { decl, subst: self.empty.clone() });
        self.ext_methods.borrow_mut().insert(key, id);
        Ok(())
    }

    /// The method `name` that was added to built-in type `ty`, if any.
    pub fn ext_method(&self, ty: Ty, name: &str) -> Option<FnId> {
        self.ext_methods.borrow().get(&(builtin_key(ty), name.to_string())).copied()
    }

    fn add_generic_fn(&self, g: GenericFn<'a>) -> usize {
        let mut v = self.generic_fns.borrow_mut();
        v.push(Rc::new(g));
        v.len() - 1
    }

    fn add_generic_type(&self, module: usize, td: &'a ast::TypeDecl) -> usize {
        let mut v = self.generic_types.borrow_mut();
        v.push(Rc::new(GenericType {
            module,
            decl: td,
            tparams: td.type_params.iter().map(|t| t.name.clone()).collect(),
            extra_methods: RefCell::new(Vec::new()),
            instances: RefCell::new(HashMap::new()),
        }));
        v.len() - 1
    }

    // ── function table ──────────────────────────────────────────

    /// Register a function that has a declaration and a body to check.
    fn declare_fn(&self, info: FnInfo, work: Work<'a>) -> FnId {
        let mut fns = self.fns.borrow_mut();
        let lazy = match &work {
            Work::Fn { subst, .. } | Work::Method { subst, .. } | Work::Ctor { subst, .. } => !subst.is_empty(),
            Work::Init { .. } => false,
        };
        fns.push(FnSlot { info: Some(Rc::new(info)), work: Some(work), state: State::New, func: None, lazy, used: false });
        fns.len() - 1
    }

    /// Allocate an id for a function created while checking another (a lambda or adapter).
    fn reserve_fn(&self) -> FnId {
        let mut fns = self.fns.borrow_mut();
        fns.push(FnSlot { info: None, work: None, state: State::Done, func: None, lazy: false, used: false });
        fns.len() - 1
    }

    fn set_func(&self, id: FnId, func: hir::Func) {
        self.fns.borrow_mut()[id].func = Some(func);
    }

    /// The declaration of a function (panics for lambdas and adapters, which have none).
    pub fn func(&self, id: FnId) -> Rc<FnInfo> {
        self.fns.borrow()[id].info.clone().expect("only declared functions have a declaration")
    }

    fn state(&self, id: FnId) -> State {
        self.fns.borrow()[id].state
    }

    // ── on-demand checking ──────────────────────────────────────

    /// Check function `id` (once).  Re-entrant requests for a function that is
    /// currently being checked return immediately; callers that need its
    /// result use [`Self::ret_of`], which reports the cycle.
    fn ensure(&self, id: FnId) -> Result<()> {
        let work = {
            let mut fns = self.fns.borrow_mut();
            if fns[id].state != State::New {
                return Ok(());
            }
            fns[id].state = State::InProgress;
            fns[id].work.clone().expect("a function awaiting a check has work")
        };
        let func = body::check_fn(self, id, &work).map_err(|d| self.note_instance(d, &work))?;
        let mut fns = self.fns.borrow_mut();
        fns[id].func = Some(func);
        fns[id].state = State::Done;
        Ok(())
    }

    /// An error inside an instance of a generic function should say which one.
    fn note_instance(&self, mut d: Diagnostic, work: &Work) -> Diagnostic {
        let subst = match work {
            Work::Fn { subst, .. } | Work::Method { subst, .. } | Work::Ctor { subst, .. } => subst,
            Work::Init { .. } => return d,
        };
        if subst.is_empty() {
            return d;
        }
        let mut bound: Vec<String> = subst.iter().map(|(n, &t)| format!("{n} = {}", self.t.ty_name(t))).collect();
        bound.sort();
        let note = format!("while checking the instance with {}", bound.join(", "));
        d.hint = Some(match d.hint.take() {
            Some(h) => format!("{h}\n{note}"),
            None => note,
        });
        d
    }

    /// The return type of `id`, inferring it from the body if necessary.
    fn ret_of(&self, id: FnId, span: Span) -> Result<Ty> {
        self.fns.borrow_mut()[id].used = true;
        let info = self.func(id);
        if let Some(ty) = info.ret.get() {
            return Ok(ty);
        }
        if self.state(id) == State::InProgress {
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
        if self.state(g.init_fn) == State::InProgress {
            return Err(Diagnostic::new(format!("cannot infer the type of `{}` here", g.name), span)
                .hint("it is used before its definition has been seen; annotate it, e.g. `name: Int = …`"));
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
            let m = self.func(id);
            if !m.params.is_empty() || !matches!(m.ret.get(), Some(Ty::Void | Ty::Int)) {
                return Err(Diagnostic::general("`main` must take no parameters and return nothing or Int"));
            }
        }
        let entry_has_script = self.inits.iter().any(|&i| self.func(i).module == entry_module);
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
        let types = self
            .t
            .types
            .into_inner()
            .into_iter()
            .map(|ti| hir::TypeDef { name: ti.name.clone(), fields: ti.fields.clone() })
            .collect();
        let funcs = self
            .fns
            .into_inner()
            .into_iter()
            .map(|slot| slot.func.unwrap_or_else(unused_func))
            .collect();
        Ok(hir::Program { types, globals, funcs, inits: self.inits, main })
    }
}

/// Names that some function body, lambda or `module.name = …` assigns.  Top-level
/// statements assigning their own variables in sequence do not count.
fn unstable_globals(loaded: &Loaded) -> HashSet<String> {
    use ast::FnBody;
    let mut names = HashSet::new();
    let mut anywhere = |a: body::Assign<'_>| {
        names.insert(a.name.to_string());
    };
    let fn_decl = |d: &ast::FnDecl, note: &mut dyn FnMut(body::Assign<'_>)| match &d.body {
        FnBody::Block(stmts) => body::walk_stmts(stmts, false, note),
        FnBody::Expr(e) => body::walk_expr(e, false, note),
        FnBody::Extern { .. } => {}
    };
    let mut top_level = Vec::new();
    for module in &loaded.modules {
        for item in &module.ast.items {
            match item {
                Item::Fn(d) | Item::Method { decl: d, .. } => fn_decl(d, &mut anywhere),
                Item::Type(td) => {
                    td.fields.iter().filter_map(|f| f.init.as_ref()).for_each(|e| body::walk_expr(e, false, &mut anywhere));
                    td.inits.iter().for_each(|b| body::walk_stmts(b, false, &mut anywhere));
                    if let Some(c) = &td.ctor {
                        body::walk_stmts(&c.body, false, &mut anywhere);
                    }
                    td.methods.iter().for_each(|m| fn_decl(m, &mut anywhere));
                }
                Item::Stmt(s) => top_level.push(s),
            }
        }
    }
    // Plain top-level assignments are straight-line code; only lambdas and `a.b = …` count.
    for s in top_level {
        body::walk_stmt(s, false, &mut |a| {
            if a.in_lambda || a.field {
                names.insert(a.name.to_string());
            }
        });
    }
    names
}

/// Stands in for a function that was never checked (see [`FnSlot::lazy`]), so ids stay valid.
fn unused_func() -> hir::Func {
    hir::Func {
        symbol: String::new(),
        kind: FuncKind::Unused,
        ret: Ty::Void,
        n_params: 0,
        locals: Vec::new(),
        body: None,
        captures: Vec::new(),
    }
}

/// A linker-safe symbol for a method of `owner`; the type id keeps instances of
/// generic types (`List<Int>` vs `List<String>`) apart.
fn method_symbol(module: &str, owner: &str, ty: TypeId, method: &str) -> String {
    let clean: String = owner.chars().map(|c| if c.is_ascii_alphanumeric() { c } else { '_' }).collect();
    format!("{module}__{clean}_{ty}__{method}")
}

/// The built-in type named `name`, if it is one that can have methods added.
fn builtin_type(name: &str) -> Option<Ty> {
    Some(match name {
        "Int" => Ty::Int,
        "Float" => Ty::Float,
        "Bool" => Ty::Bool,
        "String" => Ty::Str,
        _ => return None,
    })
}

fn builtin_key(ty: Ty) -> u8 {
    match ty {
        Ty::Int => 0,
        Ty::Float => 1,
        Ty::Bool => 2,
        Ty::Str => 3,
        _ => u8::MAX,
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
