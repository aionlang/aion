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
use std::collections::{BTreeSet, HashMap, HashSet};
use std::rc::Rc;

use crate::diag::{Diagnostic, Result, SourceMap, Span};
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
    /// Methods the program's own code declared with the name of one the standard library declared: user code
    /// calls these, while the standard library's code keeps calling its own (`methods`).
    pub user_methods: HashMap<String, FnId>,
    /// `static fn`s: called as `Type.name(…)`, with no `this`.
    pub statics: HashMap<String, FnId>,
    /// Methods with their own type parameters, by name → index into the checker's templates.
    pub generic_methods: HashMap<String, usize>,
    pub ctor: FnId,
    /// `Some(T)` for the built-in `Array<T>`, whose operations are intrinsics.
    pub array_elem: Option<Ty>,
    /// For an `interface`: the methods a type needs to be used as it.
    pub iface: Option<Vec<IfaceMethod>>,
    /// `type Dog : Animal(…)`: the type this one extends.
    pub parent: Option<TypeId>,
    /// Declared `open type`, so others may extend it.
    pub open: bool,
    /// The initialiser (everything a constructor does after allocating) of a type that
    /// is open or extends another, so a subtype's constructor can run it on its own object.
    pub init: Option<FnId>,
    /// Methods taken over from the parent and not replaced.
    pub inherited: HashSet<String>,
    /// Methods that subtypes may replace: name → (the type that first declared it `open`,
    /// its position in that type's `vlist`).  Calls to them look the real type up at run time.
    pub virtuals: HashMap<String, (TypeId, usize)>,
    /// The methods this type declared `open`, in order.
    pub vlist: Vec<String>,
    /// `Some` for an `enum`: its variants, in order.
    pub variants: Option<Vec<String>>,
    /// An enum's variants and the module variables that hold them.
    pub variant_globals: HashMap<String, GlobalId>,
}

impl TypeInfo {
    /// A type with nothing declared yet.
    fn new(name: String) -> Self {
        TypeInfo {
            name,
            fields: Vec::new(),
            methods: HashMap::new(),
            user_methods: HashMap::new(),
            statics: HashMap::new(),
            generic_methods: HashMap::new(),
            ctor: usize::MAX,
            array_elem: None,
            iface: None,
            parent: None,
            open: false,
            init: None,
            inherited: HashSet::new(),
            virtuals: HashMap::new(),
            vlist: Vec::new(),
            variants: None,
            variant_globals: HashMap::new(),
        }
    }
}

/// One method an interface requires (the receiver is not included in `params`).
#[derive(Clone)]
pub struct IfaceMethod {
    pub name: String,
    pub params: Vec<Ty>,
    pub ret: Ty,
    /// The version types use when they have no method of this name: its receiver is the interface itself.
    pub default: Option<FnId>,
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

/// `synonym Name<T> = Type`: resolved where it is used, with its parameters filled in.
pub struct Alias<'a> {
    pub module: usize,
    pub decl: &'a ast::AliasDecl,
}

/// A generic interface: a template instantiated per list of type arguments.
pub struct GenericIface<'a> {
    pub module: usize,
    pub decl: &'a ast::InterfaceDecl,
    pub tparams: Vec<String>,
    pub instances: RefCell<HashMap<Vec<Ty>, TypeId>>,
}

/// A literal usable as a default argument.
#[derive(Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    /// `null`, the default of an optional parameter.
    Null,
}

/// What a call fills in for an argument that was left out.
#[derive(Clone)]
pub enum ParamDefault {
    /// A literal: `n: Int = 0`.
    Const(Const),
    /// Any other expression: `b: Int = a * 2`.  A function of the parameters before it (and `this`).
    Computed(FnId),
}

#[derive(Clone)]
pub struct ParamInfo {
    pub name: String,
    pub ty: Ty,
    pub default: Option<ParamDefault>,
    /// `vararg xs: Int`: `ty` is the `List<Int>` the function sees; callers pass the items one by one.
    pub vararg: bool,
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
    /// Declared with `val` (or the variable of an enum variant): it cannot be assigned.
    pub immutable: bool,
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
    /// Index into the checker's generic interface templates.
    GenericIface(usize),
    /// Index into the checker's `synonym` declarations.
    Alias(usize),
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
    /// `owner` is the type of a `static fn`.
    Fn { decl: &'a ast::FnDecl, subst: Subst, owner: Option<TypeId> },
    Method { decl: &'a ast::FnDecl, subst: Subst },
    /// A computed default argument (see [`ParamDefault::Computed`]); its parameters are those before it.
    Default { expr: &'a ast::Expr, subst: Subst },
    Ctor { ty: TypeId, decl: &'a ast::TypeDecl, subst: Subst },
    /// The initialiser of a type that is open or extends another (`this`, then the constructor's parameters).
    TypeInit { ty: TypeId, decl: &'a ast::TypeDecl, subst: Subst },
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
    generic_ifaces: RefCell<Vec<Rc<GenericIface<'a>>>>,
    aliases: RefCell<Vec<Rc<Alias<'a>>>>,
    /// The synonyms being resolved right now, to catch one that is defined through itself.
    resolving_aliases: RefCell<Vec<usize>>,
    /// For a type that is an instance of a generic one: which, and with what arguments.
    instance_of: RefCell<HashMap<TypeId, (usize, Vec<Ty>)>>,
    /// The same for instances of generic interfaces.
    iface_instance_of: RefCell<HashMap<TypeId, (usize, Vec<Ty>)>>,
    module_names: Vec<String>,
    /// Whether the program keeps a stack trace, and what it needs to know about the sources.
    trace: bool,
    files: Vec<FileLines>,
    module_files: Vec<u32>,
    empty: Subst,
    /// Nesting depth of generic instantiation, to stop runaway recursion.
    depth: Cell<u32>,
    /// Methods added to the built-in types: `fn String.shout() = …`.
    ext_methods: RefCell<HashMap<(u8, String), FnId>>,
    /// Functions and methods that share a name: the first one declared maps to all of them, in order.
    overloads: RefCell<HashMap<FnId, Vec<FnId>>>,
    /// Methods of built-in types that the program's own code declared over one of the standard library's.
    ext_overrides: RefCell<HashMap<(u8, String), FnId>>,
    /// Which modules are the standard library's.
    std_modules: Vec<bool>,
    /// Names of module variables that functions, lambdas or other modules assign:
    /// these are never narrowed by null checks.
    unstable_globals: HashSet<(usize, String)>,
    /// Types named on the right of `is` / `as`.
    is_targets: RefCell<BTreeSet<TypeId>>,
    /// `override fn`s with the method each replaces, to check that the signatures agree.
    overrides: RefCell<Vec<(FnId, FnId, Span)>>,
    /// (concrete type, interface) pairs where a value of the first is used as the second.
    upcasts: RefCell<BTreeSet<(TypeId, TypeId)>>,
    /// Interfaces whose values are turned into text (they then dispatch `toString` too).
    stringified: RefCell<BTreeSet<TypeId>>,
}

/// A top-level name that starts with `_` (but not `__`, the prelude's helpers, or just `_`) is private
/// to the file that defines it.
pub fn is_private(name: &str) -> bool {
    name.starts_with('_') && !name.starts_with("__") && name != "_"
}

impl Tables {
    /// Resolve a bare name as seen from `module`: own items, imports, prelude.
    pub fn lookup(&self, module: usize, name: &str) -> Option<Sym> {
        let s = &self.scopes[module];
        // `alias.Name`: an item of an imported module
        if let Some((alias, item)) = name.split_once('.') {
            let &m = s.modules.get(alias)?;
            if is_private(item) {
                return None;
            }
            return self.scopes[m].items.get(item).copied();
        }
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
            array_elem: Some(elem),
            ..TypeInfo::new(format!("Array<{}>", self.ty_name(elem)))
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
            Ty::Char => "Char".into(),
            Ty::Int32 => "Int32".into(),
            Ty::Byte => "Byte".into(),
            Ty::Str => "String".into(),
            Ty::Bytes => "Bytes".into(),
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

    /// The `static fn` called `name` of type `ty`, or of the nearest type it extends that has one.
    pub fn static_of(&self, ty: TypeId, name: &str) -> Option<FnId> {
        let mut cur = Some(ty);
        while let Some(t) = cur {
            let info = self.ty(t);
            if let Some(&id) = info.statics.get(name) {
                return Some(id);
            }
            cur = info.parent;
        }
        None
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

/// Where the lines of a source file start, to turn a [`Span`] into a line number.
struct FileLines {
    name: String,
    starts: Vec<u32>,
    /// Part of the standard library, whose functions stay out of stack traces.
    std: bool,
}

/// `trace` makes the program keep a stack of the user's own functions and lines, which a panic prints.
pub fn analyze(loaded: &Loaded, map: &SourceMap, trace: bool) -> Result<hir::Program> {
    let mut ck = Checker::declare(loaded, map)?;
    ck.trace = trace;
    ck.files = (0..map.len() as u32)
        .map(|id| {
            let f = map.file(id);
            let starts = std::iter::once(0).chain(f.text.match_indices('\n').map(|(i, _)| i as u32 + 1)).collect();
            FileLines { name: f.name.clone(), starts, std: f.name.starts_with("<std>") }
        })
        .collect();
    ck.module_files = loaded.modules.iter().map(|m| m.file).collect();
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
        // A method that subtypes may replace is called through a table naming every version, so all are needed
        // (the methods of generic instances are otherwise only checked when something calls them).
        for root in 0..ck.types_len() {
            let vlist = ck.t.ty(root).vlist.clone();
            for name in &vlist {
                for s in (0..ck.types_len()).filter(|&s| s == root || ck.is_subtype(s, root)) {
                    let Some(&f) = ck.t.ty(s).methods.get(name) else { continue };
                    let mut fns = ck.fns.borrow_mut();
                    if fns[f].state == State::New && !fns[f].used {
                        fns[f].used = true;
                        checked = true;
                    }
                }
            }
        }
        // The methods interfaces come with are called through dispatch tables, so they are all needed.
        let upcast_ifaces: BTreeSet<TypeId> = ck.upcasts.borrow().iter().map(|&(_, i)| i).collect();
        for iface in upcast_ifaces {
            let defaults: Vec<FnId> =
                ck.t.ty(iface).iface.iter().flatten().filter_map(|m| m.default).collect();
            for f in defaults {
                let mut fns = ck.fns.borrow_mut();
                if fns[f].state == State::New && !fns[f].used {
                    fns[f].used = true;
                    checked = true;
                }
            }
        }
        // Types used as an interface that gets printed need their `toString` compiled.
        let pending: Vec<TypeId> = {
            let (ups, shown) = (ck.upcasts.borrow(), ck.stringified.borrow());
            let count = ck.types_len();
            let mut pending: Vec<TypeId> = ups.iter().filter(|(_, i)| shown.contains(i)).map(|&(t, _)| t).collect();
            // a printed open type prints each object as its own kind
            for &root in shown.iter().filter(|&&r| ck.t.ty(r).iface.is_none()) {
                pending.extend((0..count).filter(|&s| s == root || ck.is_subtype(s, root)));
            }
            pending
        };
        for t in pending {
            if !ck.tostring.borrow().contains_key(&t) {
                ck.tostring_fn(t)?;
                checked = true;
            }
        }
        if !checked {
            break;
        }
    }
    ck.check_overrides()?;
    ck.finish(loaded.modules.len() - 1)
}

impl<'a> Checker<'a> {
    /// The 1-based line a span starts on.
    fn line_of(&self, span: Span) -> u32 {
        match self.files.get(span.file as usize) {
            Some(f) => f.starts.partition_point(|&s| s <= span.start) as u32,
            None => 0,
        }
    }

    fn declare(loaded: &'a Loaded, map: &SourceMap) -> Result<Self> {
        let mut ck = Checker {
            t: Tables::default(),
            fns: RefCell::new(Vec::new()),
            inits: Vec::new(),
            tostring: RefCell::new(HashMap::new()),
            thunks: RefCell::new(HashMap::new()),
            generic_fns: RefCell::new(Vec::new()),
            generic_types: RefCell::new(Vec::new()),
            generic_ifaces: RefCell::new(Vec::new()),
            aliases: RefCell::new(Vec::new()),
            resolving_aliases: RefCell::new(Vec::new()),
            instance_of: RefCell::new(HashMap::new()),
            iface_instance_of: RefCell::new(HashMap::new()),
            module_names: loaded.modules.iter().map(|m| m.name.clone()).collect(),
            trace: false,
            files: Vec::new(),
            module_files: Vec::new(),
            empty: Rc::new(HashMap::new()),
            depth: Cell::new(0),
            ext_methods: RefCell::new(HashMap::new()),
            overloads: RefCell::new(HashMap::new()),
            ext_overrides: RefCell::new(HashMap::new()),
            std_modules: Vec::new(),
            unstable_globals: unstable_globals(loaded),
            overrides: RefCell::new(Vec::new()),
            is_targets: RefCell::new(BTreeSet::new()),
            upcasts: RefCell::new(BTreeSet::new()),
            stringified: RefCell::new(BTreeSet::new()),
        };
        ck.std_modules = loaded.modules.iter().map(|m| map.file(m.file).name.starts_with("<std>")).collect();
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
        let mut own_ifaces: Vec<(TypeId, &ast::InterfaceDecl)> = Vec::new();
        for item in &module.ast.items {
            match item {
                Item::Type(td) if td.type_params.is_empty() => {
                    let variants = td.variants.as_ref().map(|vs| vs.iter().map(|v| v.name.clone()).collect());
                    let id = self.t.add_type(TypeInfo { open: td.open, variants, ..TypeInfo::new(td.name.name.clone()) });
                    self.t.define(mi, &td.name, Sym::Type(id))?;
                    own_types.push((id, td));
                }
                Item::Interface(d) if !d.type_params.is_empty() => {
                    let mut v = self.generic_ifaces.borrow_mut();
                    v.push(Rc::new(GenericIface {
                        module: mi,
                        decl: d,
                        tparams: d.type_params.iter().map(|t| t.name.clone()).collect(),
                        instances: RefCell::new(HashMap::new()),
                    }));
                    let g = v.len() - 1;
                    drop(v);
                    self.t.define(mi, &d.name, Sym::GenericIface(g))?;
                }
                Item::Alias(decl) => {
                    let mut v = self.aliases.borrow_mut();
                    v.push(Rc::new(Alias { module: mi, decl }));
                    let a = v.len() - 1;
                    drop(v);
                    self.t.define(mi, &decl.name, Sym::Alias(a))?;
                }
                Item::Interface(d) => {
                    let id = self.t.add_type(TypeInfo {
                        iface: Some(Vec::new()),
                        ..TypeInfo::new(d.name.name.clone())
                    });
                    self.t.define(mi, &d.name, Sym::Type(id))?;
                    own_ifaces.push((id, d));
                }
                Item::Type(td) => {
                    if let Some(m) = td.methods.iter().find(|m| m.is_static) {
                        return Err(Diagnostic::new("`static fn` is not supported on generic types", m.name.span)
                            .hint("write a top-level function instead"));
                    }
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
                if is_private(&sym.name) {
                    return Err(private_error(&sym.name, &loaded.modules[imp.module].name, sym.span));
                }
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

        // 3a. What each interface requires.
        for &(id, d) in &own_ifaces {
            let methods = self.iface_methods(mi, d, &empty)?;
            self.t.update_type(id, |ti| ti.iface = Some(methods));
            self.declare_iface_defaults(mi, id, d, &empty)?;
        }

        // 3a. Parents: `type Dog : Animal(…)`.  Parents come first, so a child sees their fields and methods.
        for &(id, td) in &own_types {
            let Some(pid) = self.parent_of(mi, td, &empty)? else { continue };
            self.t.update_type(id, |ti| ti.parent = Some(pid));
        }
        let own_types = self.parents_first(own_types)?;

        // 3. Field types: the parent's, then primary-constructor parameters, then body fields.
        for &(id, td) in &own_types {
            let inherited = self.t.ty(id).parent.map(|p| self.t.ty(p).fields.clone()).unwrap_or_default();
            let fields = self.type_fields(mi, td, &empty, &inherited)?;
            self.t.update_type(id, |ti| ti.fields = fields);
        }

        // 4. Functions, methods and constructors.
        let mut top: Vec<&'a ast::Stmt> = Vec::new();
        for item in &module.ast.items {
            match item {
                Item::Fn(decl) if decl.type_params.is_empty() => {
                    let mut info =
                        self.signature(mi, decl, FuncKind::Free, None, symbol_for(&module.name, None, decl), &empty)?;
                    // A second function of the same name is another version of it when the parameters differ.
                    let first = match self.t.scopes[mi].items.get(&decl.name.name) {
                        Some(&Sym::Fn(first))
                            if !self.is_extern_fn(first) && !matches!(decl.body, ast::FnBody::Extern { .. }) =>
                        {
                            Some(first)
                        }
                        _ => None,
                    };
                    if let Some(first) = first {
                        let duplicate = format!("`{}` is already defined in this module", decl.name.name);
                        let suffix = self.overload_suffix(first, &info, decl.name.span, duplicate)?;
                        info.symbol.push_str(&suffix);
                    }
                    let id = self.declare_fn(info, Work::Fn { decl, subst: empty.clone(), owner: None });
                    match first {
                        Some(first) => self.add_overload(first, id),
                        None => self.t.define(mi, &decl.name, Sym::Fn(id))?,
                    }
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
            self.inherit_methods(id);
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
            let read_only: HashSet<&str> =
                top.iter().filter_map(|s| if let StmtKind::Val(n) = &s.kind { Some(n.name.as_str()) } else { None }).collect();
            for s in &top {
                if let StmtKind::Let { name, ty, .. } = &s.kind {
                    let declared = ty.as_ref().map(|t| self.resolve_value_type(mi, t, &empty)).transpose()?;
                    let gid = self.t.globals.len();
                    self.t.globals.push(GlobalInfo {
                        name: name.name.clone(),
                        symbol: format!("{}__g_{}", module.name, name.name),
                        ty: Cell::new(declared),
                        init_fn,
                        immutable: name.name.contains('$') || read_only.contains(name.name.as_str()),
                    });
                    self.t.define(mi, name, Sym::Global(gid))?;
                    // `Color$Red` is the variable of an enum's variant
                    if let Some((owner, variant)) = name.name.split_once('$')
                        && let Some(Sym::Type(ty)) = self.t.scopes[mi].items.get(owner).copied()
                    {
                        self.t.update_type(ty, |ti| {
                            ti.variant_globals.insert(variant.to_string(), gid);
                        });
                    }
                }
            }
            self.inits.push(init_fn);
        }
        Ok(())
    }

    /// The type that `td` extends (`: Animal(…)` or `: Box<T>(…)`, with the type arguments filled in from `subst`),
    /// after checking that it can be extended.  A generic parent is instantiated.
    fn parent_of(&self, mi: usize, td: &ast::TypeDecl, subst: &Subst) -> Result<Option<TypeId>> {
        let Some(parent) = &td.parent else { return Ok(None) };
        let pid = match self.t.lookup(mi, &parent.name.name) {
            Some(Sym::Type(pid)) => {
                if let Some(arg) = parent.type_args.first() {
                    return Err(Diagnostic::new(format!("`{}` is not generic", parent.name.name), arg.span));
                }
                pid
            }
            Some(Sym::GenericType(g)) => {
                if parent.type_args.is_empty() {
                    return Err(Diagnostic::new(format!("`{}` needs type arguments", parent.name.name), parent.name.span)
                        .hint(format!("write it like `: {}<Int>(…)`", parent.name.name)));
                }
                let targs = parent
                    .type_args
                    .iter()
                    .map(|a| self.resolve_value_type(mi, a, subst))
                    .collect::<Result<Vec<_>>>()?;
                self.instantiate_type(g, targs, parent.name.span)?
            }
            _ => return Err(Diagnostic::new(format!("unknown type `{}`", parent.name.name), parent.name.span)),
        };
        let p = self.t.ty(pid);
        if p.iface.is_some() {
            return Err(Diagnostic::new(format!("`{}` is an interface, which a type implements by having its methods, not by extending it", p.name), parent.name.span));
        }
        if !p.open {
            return Err(Diagnostic::new(format!("`{}` cannot extend `{}`: it is not `open`", td.name.name, p.name), parent.name.span)
                .hint(format!("declare it `open type {}`", td.parent.as_ref().map_or("", |p| p.name.name.as_str()))));
        }
        Ok(Some(pid))
    }

    /// What interface `d` requires, with its type parameters bound by `subst`.
    fn iface_methods(&self, mi: usize, d: &ast::InterfaceDecl, subst: &Subst) -> Result<Vec<IfaceMethod>> {
        let mut methods: Vec<IfaceMethod> = Vec::new();
        for m in &d.methods {
            if methods.iter().any(|x| x.name == m.name.name) {
                return Err(Diagnostic::new(format!("duplicate method `{}`", m.name.name), m.name.span));
            }
            let mut params = Vec::new();
            for p in &m.params {
                if p.default.is_some() {
                    return Err(Diagnostic::new("interface methods cannot have default arguments", p.name.span));
                }
                params.push(self.resolve_value_type(mi, &p.ty, subst)?);
            }
            let ret = match &m.ret {
                Some(r) => self.resolve_type(mi, r, subst)?,
                None => Ty::Void,
            };
            methods.push(IfaceMethod { name: m.name.name.clone(), params, ret, default: None });
        }
        Ok(methods)
    }

    /// The methods of interface `id` that have a body become functions whose receiver is the interface; a type
    /// without a method of that name is dispatched to them.
    fn declare_iface_defaults(&self, mi: usize, id: TypeId, d: &'a ast::InterfaceDecl, subst: &Subst) -> Result<()> {
        for decl in &d.defaults {
            if !decl.type_params.is_empty() {
                return Err(Diagnostic::new("an interface method cannot be generic", decl.name.span));
            }
            if decl.ret.is_none() && matches!(decl.body, ast::FnBody::Expr(_)) {
                return Err(Diagnostic::new(
                    format!("the method `{}` of an interface needs its result type", decl.name.name),
                    decl.name.span,
                )
                .hint(format!("write it like `fn {}() -> Type = …`", decl.name.name)));
            }
            let symbol = method_symbol(&self.module_names[mi], &self.t.ty(id).name, id, &decl.name.name);
            let info = self.signature(mi, decl, FuncKind::Method, Some(Ty::Obj(id)), symbol, subst)?;
            let f = self.declare_fn(info, Work::Method { decl, subst: subst.clone() });
            self.t.update_type(id, |ti| {
                if let Some(m) = ti.iface.as_mut().and_then(|ms| ms.iter_mut().find(|m| m.name == decl.name.name)) {
                    m.default = Some(f);
                }
            });
        }
        Ok(())
    }

    /// The fields of a type declaration: primary-constructor parameters, then body fields.
    fn type_fields(
        &self,
        mi: usize,
        td: &ast::TypeDecl,
        subst: &Subst,
        inherited: &[hir::Field],
    ) -> Result<Vec<hir::Field>> {
        let mut fields: Vec<hir::Field> = inherited.to_vec();
        let declared = td
            .primary
            .iter()
            .flatten()
            .map(|p| (&p.name, &p.ty, true))
            .chain(td.fields.iter().map(|f| (&f.name, &f.ty, false)));
        for (name, ty, is_param) in declared {
            let ty = self.resolve_value_type(mi, ty, subst)?;
            if let Some(prev) = fields.iter().position(|g| g.name == name.name) {
                // A constructor parameter named like an inherited field just forwards its value to the parent.
                if is_param && prev < inherited.len() && fields[prev].ty == ty {
                    continue;
                }
                let msg = if prev < inherited.len() {
                    format!("`{}` is already a field of the parent type", name.name)
                } else {
                    format!("duplicate field `{}`", name.name)
                };
                return Err(Diagnostic::new(msg, name.span));
            }
            fields.push(hir::Field { name: name.name.clone(), ty });
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

    /// `earlier` are the parameters before `p`, which a computed default may use.
    fn param_info(&self, mi: usize, p: &'a ast::Param, subst: &Subst, earlier: &[ParamInfo]) -> Result<ParamInfo> {
        let ty = self.resolve_value_type(mi, &p.ty, subst)?;
        let default = match &p.default {
            None => None,
            Some(e) => Some(match const_default(e, ty, &self.t)? {
                Some(c) => ParamDefault::Const(c),
                None => {
                    // Any other expression becomes a function of the parameters before it.
                    let params = earlier.iter().map(|q| ParamInfo { default: None, ..q.clone() }).collect();
                    let info = FnInfo {
                        name: format!("the default of `{}`", p.name.name),
                        module: mi,
                        symbol: format!("{}__default_{}__{}", self.module_names[mi], p.name.name, self.fns.borrow().len()),
                        kind: FuncKind::Free,
                        params,
                        ret: Cell::new(Some(ty)),
                    };
                    ParamDefault::Computed(self.declare_fn(info, Work::Default { expr: e, subst: subst.clone() }))
                }
            }),
        };
        if p.vararg {
            let Some(&Sym::GenericType(list)) = self.t.scopes[0].items.get("List") else {
                return Err(Diagnostic::new("the prelude has no `List`", p.name.span));
            };
            let ty = Ty::Obj(self.instantiate_type(list, vec![ty], p.ty.span)?);
            return Ok(ParamInfo { name: p.name.name.clone(), ty, default: None, vararg: true });
        }
        Ok(ParamInfo { name: p.name.name.clone(), ty, default, vararg: false })
    }

    fn params(
        &self,
        mi: usize,
        params: &'a [ast::Param],
        receiver: Option<Ty>,
        subst: &Subst,
    ) -> Result<Vec<ParamInfo>> {
        let mut out: Vec<ParamInfo> = Vec::new();
        if let Some(ty) = receiver {
            out.push(ParamInfo { name: "this".into(), ty, default: None, vararg: false });
        }
        for p in params {
            if out.iter().any(|q| q.name == p.name.name) {
                return Err(Diagnostic::new(format!("duplicate parameter `{}`", p.name.name), p.name.span));
            }
            let info = self.param_info(mi, p, subst, &out)?;
            out.push(info);
        }
        Ok(out)
    }

    /// Resolve a function's parameter and return types.
    fn signature(
        &self,
        mi: usize,
        decl: &'a ast::FnDecl,
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
            // A `String?` is the same `const char *`, with NULL for null.
            let plain = |ty: Ty| match ty {
                Ty::Opt(o) => matches!(self.t.opt_inner(o), Ty::Str | Ty::Bytes),
                Ty::Obj(_) | Ty::Null => false,
                _ => true,
            };
            let ok = params.iter().all(|p| plain(p.ty)) && ret.is_none_or(|r| plain(r) && !matches!(r, Ty::Fn(_)));
            if !ok {
                return Err(Diagnostic::new(
                    "`extern fn` can only use Int, Float, Bool, String and String? (and take function values)",
                    decl.span,
                ));
            }
        }
        Ok(FnInfo { name: decl.name.name.clone(), module: mi, symbol, kind, params, ret: Cell::new(ret) })
    }

    /// Order types so that every type comes after the one it extends (when both are in this module).
    fn parents_first(
        &self,
        types: Vec<(TypeId, &'a ast::TypeDecl)>,
    ) -> Result<Vec<(TypeId, &'a ast::TypeDecl)>> {
        let mut done = Vec::new();
        let mut pending = types;
        while !pending.is_empty() {
            let ids: Vec<TypeId> = pending.iter().map(|&(id, _)| id).collect();
            let (ready, rest): (Vec<_>, Vec<_>) =
                pending.into_iter().partition(|&(id, _)| self.t.ty(id).parent.is_none_or(|p| !ids.contains(&p)));
            if ready.is_empty() {
                let (_, td) = rest[0];
                return Err(Diagnostic::new(
                    format!("`{}` extends itself, directly or through other types", td.name.name),
                    td.name.span,
                ));
            }
            done.extend(ready);
            pending = rest;
        }
        Ok(done)
    }

    /// A type starts with its parent's methods (the ones it does not replace).
    fn inherit_methods(&self, id: TypeId) {
        let Some(parent) = self.t.ty(id).parent else { return };
        let p = self.t.ty(parent);
        self.t.update_type(id, |ti| {
            for (name, &f) in &p.methods {
                if !ti.methods.contains_key(name) {
                    ti.methods.insert(name.clone(), f);
                    ti.inherited.insert(name.clone());
                }
            }
            for (name, &g) in &p.generic_methods {
                ti.generic_methods.entry(name.clone()).or_insert(g);
            }
            for (name, &v) in &p.virtuals {
                ti.virtuals.entry(name.clone()).or_insert(v);
            }
        });
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
        if decl.is_static {
            return self.declare_static(module_name, mi, ty, decl, subst);
        }
        let owner = self.t.ty(ty).name.clone();
        let existing = self.t.ty(ty);
        let name = &decl.name.name;
        let inherited = existing.inherited.contains(name);
        let parent_name = existing.parent.map(|p| self.t.ty(p).name.clone()).unwrap_or_default();
        match (decl.modifier, inherited) {
            (ast::Modifier::None, false) => {}
            (ast::Modifier::Override, false) => {
                return Err(Diagnostic::new(format!("`{name}` overrides nothing: `{owner}` inherits no such method"), decl.name.span));
            }
            (ast::Modifier::Open, false) if !existing.open => {
                return Err(Diagnostic::new(format!("`open fn` needs an `open type`; `{owner}` is not open"), decl.name.span));
            }
            (ast::Modifier::Open, false) => {}
            (modifier, true) => {
                if !existing.virtuals.contains_key(name) {
                    return Err(Diagnostic::new(format!("`{parent_name}.{name}` is not `open`, so `{owner}` cannot replace it"), decl.name.span)
                        .hint(format!("declare it `open fn {name}` in `{parent_name}`")));
                }
                if modifier != ast::Modifier::Override {
                    return Err(Diagnostic::new(format!("`{owner}.{name}` replaces a method of `{parent_name}`"), decl.name.span)
                        .hint(format!("write `override fn {name}`")));
                }
                if !decl.type_params.is_empty() {
                    return Err(Diagnostic::new("a generic method cannot be overridden", decl.name.span));
                }
            }
        }
        if decl.modifier != ast::Modifier::None && !decl.type_params.is_empty() {
            return Err(Diagnostic::new("a generic method cannot be `open`", decl.name.span));
        }
        // The program's own method of a name the standard library's methods already use hides the library's, for
        // the program (the library's own code keeps calling its own).
        let std_first = existing.methods.get(name).copied().filter(|&f| {
            !inherited
                && !self.is_std_module(mi)
                && self.is_std_module(self.func(f).module)
                && decl.modifier == ast::Modifier::None
                && decl.type_params.is_empty()
                && !existing.open
                && existing.parent.is_none()
                && !existing.virtuals.contains_key(name)
                && !existing.generic_methods.contains_key(name)
                && !existing.statics.contains_key(name)
        });
        // A second method of the same name is another version of it, unless a subtype could replace the method.
        let first = existing.methods.get(name).copied().filter(|_| {
            !inherited
                && decl.modifier == ast::Modifier::None
                && decl.type_params.is_empty()
                && !existing.open
                && existing.parent.is_none()
                && !existing.virtuals.contains_key(name)
                && !existing.generic_methods.contains_key(name)
                && !existing.statics.contains_key(name)
        });
        if !inherited
            && first.is_none()
            && std_first.is_none()
            && (existing.methods.contains_key(name)
                || existing.generic_methods.contains_key(name)
                || existing.statics.contains_key(name))
        {
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
        let mut info = self.signature(mi, decl, FuncKind::Method, Some(Ty::Obj(ty)), symbol, subst)?;
        if let Some(std_first) = std_first {
            let mine = self.t.ty(ty).user_methods.get(name).copied();
            let duplicate = format!("`{owner}` already has a method `{}`", decl.name.name);
            let (suffix, mut versions) = self.override_versions(mine, std_first, &info, decl.name.span, duplicate)?;
            info.symbol.push_str(&suffix);
            let id = self.declare_fn(info, Work::Method { decl, subst: subst.clone() });
            let primary = mine.unwrap_or(id);
            versions.push(id);
            self.set_versions(primary, versions);
            self.t.update_type(ty, |ti| {
                ti.user_methods.insert(name.clone(), primary);
            });
            return Ok(());
        }
        if let Some(first) = first {
            let duplicate = format!("`{owner}` already has a method `{}`", decl.name.name);
            info.symbol.push_str(&self.overload_suffix(first, &info, decl.name.span, duplicate)?);
        }
        let id = self.declare_fn(info, Work::Method { decl, subst: subst.clone() });
        if let Some(first) = first {
            self.add_overload(first, id);
            return Ok(());
        }
        let replaced = self.t.ty(ty).methods.get(name).copied();
        self.t.update_type(ty, |ti| {
            ti.methods.insert(name.clone(), id);
            ti.inherited.remove(name);
            if decl.modifier == ast::Modifier::Open {
                ti.virtuals.insert(name.clone(), (ty, ti.vlist.len()));
                ti.vlist.push(name.clone());
            }
        });
        if let (ast::Modifier::Override, Some(parent_fn)) = (decl.modifier, replaced) {
            self.overrides.borrow_mut().push((id, parent_fn, decl.name.span));
        }
        Ok(())
    }

    /// Declare `static fn` `decl` of type `ty`: an ordinary function with no `this`, reached as `Type.name(…)`.
    fn declare_static(&self, module_name: &str, mi: usize, ty: TypeId, decl: &'a ast::FnDecl, subst: &Subst) -> Result<()> {
        let owner = self.t.ty(ty).name.clone();
        let name = &decl.name.name;
        if !subst.is_empty() {
            return Err(Diagnostic::new("`static fn` is not supported on generic types", decl.name.span)
                .hint("write a top-level function instead"));
        }
        if !decl.type_params.is_empty() {
            return Err(Diagnostic::new("a `static fn` cannot be generic", decl.name.span));
        }
        let existing = self.t.ty(ty);
        if existing.statics.contains_key(name) || existing.methods.contains_key(name) || existing.generic_methods.contains_key(name) {
            return Err(Diagnostic::new(format!("`{owner}` already has a method `{name}`"), decl.name.span));
        }
        let symbol = method_symbol(module_name, &owner, ty, name);
        let info = self.signature(mi, decl, FuncKind::Free, None, symbol, subst)?;
        let id = self.declare_fn(info, Work::Fn { decl, subst: subst.clone(), owner: Some(ty) });
        self.t.update_type(ty, |ti| {
            ti.statics.insert(name.clone(), id);
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
        // Open types and subtypes split construction in two: allocate, then run the initialiser.
        if td.open || td.parent.is_some() {
            let mut init_params = vec![ParamInfo { name: "this".into(), ty: Ty::Obj(ty), default: None, vararg: false }];
            init_params.extend(params.iter().cloned());
            let init = self.declare_fn(
                FnInfo {
                    name: td.name.name.clone(),
                    module: mi,
                    symbol: method_symbol(module_name, &owner, ty, "init"),
                    kind: FuncKind::Method,
                    params: init_params,
                    ret: Cell::new(Some(Ty::Void)),
                },
                Work::TypeInit { ty, decl: td, subst: subst.clone() },
            );
            self.t.update_type(ty, |ti| ti.init = Some(init));
        }
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
        let mut first = self.ext_methods.borrow().get(&key).copied();
        // The program's own method of a name the standard library already uses hides it, for the program.
        let std_first = first.filter(|&f| !self.is_std_module(mi) && self.is_std_module(self.func(f).module));
        if std_first.is_some() {
            first = self.ext_overrides.borrow().get(&key).copied();
        }
        let duplicate = format!("`{}` already has a method `{}`", self.t.ty_name(ty), decl.name.name);
        if decl.is_static {
            return Err(Diagnostic::new("`static fn` is not supported on built-in types", decl.name.span)
                .hint("write a top-level function instead"));
        }
        if !decl.type_params.is_empty() {
            return Err(Diagnostic::new("methods on built-in types cannot be generic", decl.name.span));
        }
        let symbol = symbol_for(module_name, Some(&self.t.ty_name(ty)), decl);
        let mut info = self.signature(mi, decl, FuncKind::Method, Some(ty), symbol, &self.empty)?;
        if let Some(std_first) = std_first {
            let (suffix, mut versions) = self.override_versions(first, std_first, &info, decl.name.span, duplicate)?;
            info.symbol.push_str(&suffix);
            let id = self.declare_fn(info, Work::Method { decl, subst: self.empty.clone() });
            let primary = first.unwrap_or(id);
            versions.push(id);
            self.set_versions(primary, versions);
            self.ext_overrides.borrow_mut().insert(key, primary);
            return Ok(());
        }
        if let Some(first) = first {
            info.symbol.push_str(&self.overload_suffix(first, &info, decl.name.span, duplicate)?);
        }
        let id = self.declare_fn(info, Work::Method { decl, subst: self.empty.clone() });
        match first {
            Some(first) => self.add_overload(first, id),
            None => {
                self.ext_methods.borrow_mut().insert(key, id);
            }
        }
        Ok(())
    }

    pub fn is_std_module(&self, module: usize) -> bool {
        self.std_modules.get(module).copied().unwrap_or(false)
    }

    /// What printing a function value shows: `<function add(a: Int, b: Int) -> Int>` for a declared function
    /// (`<function shapes.area(…)>` from another module), `<stdlib function strings.trim(…)>` for the standard
    /// library's, and `<anonymous function (x: Int) -> Int>` for a lambda.  `name` is `None` for a lambda, whose text has no module in it.
    pub fn function_text(&self, module: usize, kind: FuncKind, name: Option<&str>, params: &[(&str, Ty)], ret: Ty) -> String {
        // `print` and the like are built into the compiler; the prelude's `__builtin_print` stands in for them as a value
        let builtin = name.and_then(|n| n.strip_prefix("__builtin_"));
        let word = match (name, kind) {
            (None, _) => "anonymous function",
            (_, FuncKind::Constructor) => "constructor",
            _ => "function",
        };
        let origin = match builtin {
            Some(_) => "built-in ",
            None if self.is_std_module(module) => "stdlib ",
            None => "",
        };
        let name = builtin.or(name);
        // the entry module is the last one; the prelude's names are in scope everywhere, so they go unqualified
        let qualifier = match self.module_names.get(module) {
            Some(m) if name.is_some() && module + 1 != self.module_names.len() && m != "prelude" => format!("{m}."),
            _ => String::new(),
        };
        let params: Vec<String> = params.iter().map(|(n, t)| format!("{n}: {}", self.t.ty_name(*t))).collect();
        let ret = if ret == Ty::Void { "Unit".to_string() } else { self.t.ty_name(ret) };
        format!("<{origin}{word} {qualifier}{}({}) -> {ret}>", name.unwrap_or(""), params.join(", "))
    }

    /// A method the program declared over a standard-library one of the same name, made from the versions
    /// the program wrote and the standard library's versions that they do not replace.
    /// `mine` is the program's first version so far (if any), `std_first` the standard library's, and `info`
    /// the new declaration: gives the text for its symbol, and the versions it leaves in place.
    fn override_versions(
        &self,
        mine: Option<FnId>,
        std_first: FnId,
        info: &FnInfo,
        span: Span,
        duplicate: String,
    ) -> Result<(String, Vec<FnId>)> {
        let same = |v: FnId| {
            let other = self.func(v);
            other.params.len() == info.params.len() && other.params.iter().zip(&info.params).all(|(a, b)| a.ty == b.ty)
        };
        let mut versions = match mine {
            Some(first) => self.overloads_of(first).unwrap_or_else(|| vec![first]),
            None => self.overloads_of(std_first).unwrap_or_else(|| vec![std_first]),
        };
        let written = versions.iter().filter(|&&v| !self.is_std_module(self.func(v).module)).count();
        if versions.iter().any(|&v| same(v) && !self.is_std_module(self.func(v).module)) {
            return Err(Diagnostic::new(duplicate, span));
        }
        // the program's own version replaces the standard library's with the same parameters
        versions.retain(|&v| !same(v));
        Ok((if written == 0 { String::new() } else { format!("__v{written}") }, versions))
    }

    /// Every version of an overloaded function or method (the first declared, which is what a lookup by
    /// name finds, comes first); `None` when `id` is the only one of its name.
    pub fn overloads_of(&self, id: FnId) -> Option<Vec<FnId>> {
        self.overloads.borrow().get(&id).cloned()
    }

    /// `info` is another version of `first`.  Gives the text that keeps its symbol apart from the others,
    /// or `duplicate` as the error when an existing version already takes the same parameter types.
    fn overload_suffix(&self, first: FnId, info: &FnInfo, span: Span, duplicate: String) -> Result<String> {
        let versions = self.overloads_of(first).unwrap_or_else(|| vec![first]);
        for &v in &versions {
            let other = self.func(v);
            if other.params.len() == info.params.len() && other.params.iter().zip(&info.params).all(|(a, b)| a.ty == b.ty) {
                return Err(Diagnostic::new(duplicate, span));
            }
        }
        Ok(format!("__v{}", versions.len()))
    }

    /// Of the versions of the method `first`, the one whose parameters (without the receiver) are `params`;
    /// `first` itself when none is, or when it has no other versions.
    fn version_with(&self, first: FnId, params: &[Ty]) -> FnId {
        let Some(versions) = self.overloads_of(first) else { return first };
        versions
            .into_iter()
            .find(|&v| self.func(v).params[1..].iter().map(|p| p.ty).eq(params.iter().copied()))
            .unwrap_or(first)
    }

    /// Make `primary` stand for `versions` (or for itself alone when that is all).
    fn set_versions(&self, primary: FnId, versions: Vec<FnId>) {
        let mut table = self.overloads.borrow_mut();
        let mut all = vec![primary];
        all.extend(versions.into_iter().filter(|&v| v != primary));
        if all.len() > 1 {
            table.insert(primary, all);
        } else {
            table.remove(&primary);
        }
    }

    fn add_overload(&self, first: FnId, id: FnId) {
        self.overloads.borrow_mut().entry(first).or_insert_with(|| vec![first]).push(id);
    }

    /// Does `id` have its body in C?  Such functions keep their C name, so they cannot be overloaded.
    fn is_extern_fn(&self, id: FnId) -> bool {
        matches!(
            &self.fns.borrow()[id].work,
            Some(Work::Fn { decl, .. } | Work::Method { decl, .. }) if matches!(decl.body, ast::FnBody::Extern { .. })
        )
    }

    /// [`Self::ext_method`] as code in `module` sees it: the program's own methods hide the standard library's
    /// of the same name, but the standard library's own code still calls its own.
    pub fn ext_method_from(&self, module: usize, ty: Ty, name: &str) -> Option<FnId> {
        if !self.is_std_module(module)
            && let Some(&mine) = self.ext_overrides.borrow().get(&(builtin_key(ty), name.to_string()))
        {
            return Some(mine);
        }
        self.ext_method(ty, name)
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
            Work::Fn { subst, .. } | Work::Method { subst, .. } | Work::Ctor { subst, .. } | Work::Default { subst, .. } => {
                !subst.is_empty()
            }
            Work::Init { .. } | Work::TypeInit { .. } => false,
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
            Work::Fn { subst, .. } | Work::Method { subst, .. } | Work::Ctor { subst, .. } | Work::Default { subst, .. } => subst,
            Work::Init { .. } | Work::TypeInit { .. } => return d,
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

    /// An `override fn` must take and return exactly what the method it replaces does.
    fn check_overrides(&self) -> Result<()> {
        for (child, parent, span) in self.overrides.borrow().clone() {
            let shape = |f: FnId| -> Result<(Vec<Ty>, Ty)> {
                let params = self.func(f).params[1..].iter().map(|p| p.ty).collect();
                Ok((params, self.ret_of(f, span)?))
            };
            let ((cp, cr), (pp, pr)) = (shape(child)?, shape(parent)?);
            if (cp.clone(), cr) != (pp.clone(), pr) {
                let show = |ps: &[Ty], r: Ty| {
                    let ps: Vec<String> = ps.iter().map(|&p| self.t.ty_name(p)).collect();
                    let r = if r == Ty::Void { "nothing".to_string() } else { self.t.ty_name(r) };
                    format!("takes ({}) and returns {r}", ps.join(", "))
                };
                return Err(Diagnostic::new(
                    format!(
                        "`{}` {}, but the method it overrides {}",
                        self.func(child).name,
                        show(&cp, cr),
                        show(&pp, pr)
                    ),
                    span,
                ));
            }
        }
        Ok(())
    }

    /// Why a value of type `ty` cannot be used as interface `iface`, if it can't.
    pub fn conforms(&self, ty: TypeId, iface: TypeId) -> std::result::Result<(), String> {
        self.satisfies(Ty::Obj(ty), iface)
    }

    /// Does `ty` — any type, including `Int` with added methods — have everything `iface` requires?
    pub fn satisfies(&self, ty: Ty, iface: TypeId) -> std::result::Result<(), String> {
        let i = self.t.ty(iface);
        let Some(required) = &i.iface else { return Err(format!("`{}` is not an interface", i.name)) };
        if ty == Ty::Obj(iface) {
            return Ok(());
        }
        let name = self.t.ty_name(ty);
        let show = |m: &IfaceMethod| {
            let params: Vec<String> = m.params.iter().map(|&p| self.t.ty_name(p)).collect();
            let ret = if m.ret == Ty::Void { String::new() } else { format!(" -> {}", self.t.ty_name(m.ret)) };
            format!("{}({}){ret}", m.name, params.join(", "))
        };
        for m in required {
            // the method's parameter types (without the receiver) and result type, if `ty` has it
            let found: Option<(Vec<Ty>, Ty)> = match ty {
                Ty::Obj(t) if self.t.ty(t).generic_methods.contains_key(&m.name) => {
                    return Err(format!("`{name}.{}` is generic, so it can't implement `{}`", m.name, i.name));
                }
                Ty::Obj(t) if self.t.ty(t).iface.is_some() => self
                    .t
                    .ty(t)
                    .iface
                    .as_ref()
                    .and_then(|ms| ms.iter().find(|x| x.name == m.name))
                    .map(|x| (x.params.clone(), x.ret)),
                Ty::Obj(t) => match self.t.ty(t).methods.get(&m.name) {
                    Some(&f) => Some(self.signature_of(self.version_with(f, &m.params))?),
                    None => None,
                },
                other => match self.ext_method(other, &m.name) {
                    Some(f) => Some(self.signature_of(self.version_with(f, &m.params))?),
                    None => None,
                },
            };
            let Some((actual, ret)) = found else {
                // a method that comes with a body need not be there
                if m.default.is_some() {
                    continue;
                }
                return Err(format!("`{name}` does not implement `{}`: it needs `fn {}`", i.name, show(m)));
            };
            if actual != m.params {
                let have: Vec<String> = actual.iter().map(|&p| self.t.ty_name(p)).collect();
                return Err(format!(
                    "`{name}.{}` takes ({}), but `{}` needs `fn {}`",
                    m.name,
                    have.join(", "),
                    i.name,
                    show(m)
                ));
            }
            if ret != m.ret {
                return Err(format!(
                    "`{name}.{}` returns {}, but `{}` needs `fn {}`",
                    m.name,
                    self.t.ty_name(ret),
                    i.name,
                    show(m)
                ));
            }
        }
        Ok(())
    }

    /// Parameter types (without the receiver) and result type of method `f`.
    fn signature_of(&self, f: FnId) -> std::result::Result<(Vec<Ty>, Ty), String> {
        let info = self.func(f);
        let params = info.params[1..].iter().map(|p| p.ty).collect();
        let ret = self.ret_of(f, Span::default()).map_err(|d| d.message)?;
        Ok((params, ret))
    }

    fn types_len(&self) -> usize {
        self.t.types.borrow().len()
    }

    /// Is `ancestor` a proper parent, grandparent… of `ty`?
    pub fn is_subtype(&self, ty: TypeId, ancestor: TypeId) -> bool {
        let mut cur = self.t.ty(ty).parent;
        while let Some(p) = cur {
            if p == ancestor {
                return true;
            }
            cur = self.t.ty(p).parent;
        }
        false
    }

    /// The closest type that both `a` and `b` are (or extend), if they share one.
    pub fn common_parent(&self, a: TypeId, b: TypeId) -> Option<TypeId> {
        let mut cur = Some(a);
        while let Some(t) = cur {
            if t == b || self.is_subtype(b, t) {
                return Some(t);
            }
            cur = self.t.ty(t).parent;
        }
        None
    }

    /// Is the module variable `g` (named `name`) assigned somewhere that makes null checks on it unreliable?
    pub fn is_unstable_global(&self, g: GlobalId, name: &str) -> bool {
        let module = self.func(self.t.globals[g].init_fn).module;
        [module, usize::MAX].into_iter().any(|m| self.unstable_globals.contains(&(m, name.to_string())))
    }

    /// The prelude's `Any`.
    pub fn any_type(&self) -> Option<TypeId> {
        match self.t.scopes[0].items.get("Any") {
            Some(&Sym::Type(id)) => Some(id),
            _ => None,
        }
    }

    /// Is `ty` `Any` or `Any?`?
    pub fn is_any(&self, ty: Ty) -> bool {
        let inner = match ty {
            Ty::Opt(o) => self.t.opt_inner(o),
            other => other,
        };
        matches!(inner, Ty::Obj(t) if Some(t) == self.any_type())
    }

    /// A call of the prelude function `name`, if it exists (its result type is known from its signature).
    pub fn prelude_fn(&self, name: &str, span: Span) -> Option<Result<FnId>> {
        match self.t.scopes[0].items.get(name) {
            Some(&Sym::Fn(id)) => Some(self.ret_of(id, span).map(|_| id)),
            _ => None,
        }
    }

    /// Note that values of `ty` are used as `iface`, so dispatch tables include it.
    pub fn register_upcast(&self, ty: TypeId, iface: TypeId) {
        self.upcasts.borrow_mut().insert((ty, iface));
    }

    /// The dispatch tables for interface calls.
    fn dispatch_tables(&self) -> Vec<hir::Dispatch> {
        let ups = self.upcasts.borrow();
        let shown = self.stringified.borrow();
        let mut tables = Vec::new();
        // Methods that subtypes may replace: one table per method of the type that opened it.
        let count = self.t.types.borrow().len();
        for root in 0..count {
            let info = self.t.ty(root);
            let family = |s: TypeId| s == root || self.is_subtype(s, root);
            for (index, name) in info.vlist.iter().enumerate() {
                let entries = (0..count).filter(|&s| family(s)).map(|s| (s, self.t.ty(s).methods[name])).collect();
                tables.push(hir::Dispatch { iface: root, method: index, entries });
            }
            if shown.contains(&root) && info.iface.is_none() {
                let entries = (0..count).filter(|&s| family(s)).map(|s| (s, self.tostring.borrow()[&s])).collect();
                tables.push(hir::Dispatch { iface: root, method: info.vlist.len(), entries });
            }
        }
        let ifaces: BTreeSet<TypeId> = ups.iter().map(|&(_, i)| i).collect();
        for iface in ifaces {
            let info = self.t.ty(iface);
            let methods = info.iface.as_ref().expect("only interfaces are upcast to");
            for (index, m) in methods.iter().enumerate() {
                let entries = ups
                    .iter()
                    .filter(|&&(_, i)| i == iface)
                    .map(|&(t, _)| match self.t.ty(t).methods.get(&m.name) {
                        Some(&f) => (t, self.version_with(f, &m.params)),
                        None => (t, m.default.expect("a type without the method conforms only if there is a default")),
                    })
                    .collect();
                tables.push(hir::Dispatch { iface, method: index, entries });
            }
            if shown.contains(&iface) {
                let entries = ups
                    .iter()
                    .filter(|&&(_, i)| i == iface)
                    .map(|&(t, _)| (t, self.tostring.borrow()[&t]))
                    .collect();
                tables.push(hir::Dispatch { iface, method: methods.len(), entries });
            }
        }
        tables
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
        let nothing_to_run = main.is_none() && !entry_has_script;

        let dispatch = self.dispatch_tables();
        let count = self.types_len();
        let instance_checks = self
            .is_targets
            .borrow()
            .iter()
            .map(|&target| hir::InstanceCheck {
                target,
                members: (0..count).filter(|&s| s == target || self.is_subtype(s, target)).collect(),
            })
            .collect();
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
        Ok(hir::Program { types, dispatch, instance_checks, globals, funcs, inits: self.inits, main, nothing_to_run })
    }
}

/// Module variables that are assigned somewhere other than straight-line top-level code, so a null check on
/// them proves nothing later.  Keyed by the module the assignment is in: the prelude (module 0) cannot assign
/// a variable of the program, so its assignments only count against its own variables; an assignment in any
/// other module counts for every module (`MAX`), as does an assignment to a field (`a.name = …`).
fn unstable_globals(loaded: &Loaded) -> HashSet<(usize, String)> {
    use ast::FnBody;
    const EVERYWHERE: usize = usize::MAX;
    let mut names = HashSet::new();
    let fn_decl = |d: &ast::FnDecl, note: &mut dyn FnMut(body::Assign<'_>)| match &d.body {
        FnBody::Block(stmts) => body::walk_stmts(stmts, false, note),
        FnBody::Expr(e) => body::walk_expr(e, false, note),
        FnBody::Extern { .. } => {}
    };
    let mut top_level = Vec::new();
    for (mi, module) in loaded.modules.iter().enumerate() {
        let scope = if mi == 0 { 0 } else { EVERYWHERE };
        let mut anywhere = |a: body::Assign<'_>| {
            names.insert((if a.field { EVERYWHERE } else { scope }, a.name.to_string()));
        };
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
                Item::Stmt(s) => top_level.push((scope, s)),
                Item::Interface(_) | Item::Alias(_) => {}
            }
        }
    }
    // Plain top-level assignments are straight-line code; only lambdas and `a.b = …` count.
    for (scope, s) in top_level {
        body::walk_stmt(s, false, &mut |a| {
            if a.in_lambda || a.field {
                names.insert((if a.field { EVERYWHERE } else { scope }, a.name.to_string()));
            }
        });
    }
    names
}

/// Stands in for a function that was never checked (see [`FnSlot::lazy`]), so ids stay valid.
fn private_error(name: &str, module: &str, span: Span) -> Diagnostic {
    Diagnostic::new(format!("`{name}` is private to module `{module}`"), span)
        .hint("names starting with `_` are only visible inside their own file")
}

fn unused_func() -> hir::Func {
    hir::Func {
        trace: None,
        symbol: String::new(),
        kind: FuncKind::Unused,
        ret: Ty::Void,
        n_params: 0,
        locals: Vec::new(),
        body: None,
        captures: Vec::new(),
        text: None,
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
        "Char" => Ty::Char,
        "Int32" => Ty::Int32,
        "Byte" => Ty::Byte,
        "String" => Ty::Str,
        "Bytes" => Ty::Bytes,
        _ => return None,
    })
}

fn builtin_key(ty: Ty) -> u8 {
    match ty {
        Ty::Int => 0,
        Ty::Float => 1,
        Ty::Bool => 2,
        Ty::Str => 3,
        Ty::Char => 4,
        Ty::Bytes => 5,
        Ty::Int32 => 6,
        Ty::Byte => 7,
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

/// A default argument that is a literal (possibly negated) matching the parameter type, or `null` for
/// an optional parameter.  `None` for any other expression, which is computed at the call.
fn const_default(e: &ast::Expr, ty: Ty, t: &Tables) -> Result<Option<Const>> {
    let (ty, optional) = match ty {
        Ty::Opt(o) => (t.opt_inner(o), true),
        other => (other, false),
    };
    if matches!(e.kind, ExprKind::Null) {
        return if optional {
            Ok(Some(Const::Null))
        } else {
            Err(Diagnostic::new(format!("a {} can't default to null", t.ty_name(ty)), e.span)
                .hint(format!("declare the parameter as `{}?`", t.ty_name(ty))))
        };
    }
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
        None => Ok(None),
        Some((c, found)) if found == ty => Ok(Some(c)),
        Some((Const::Int(v), Ty::Int)) if ty == Ty::Float => Ok(Some(Const::Float(v as f64))),
        Some((Const::Int(v), Ty::Int)) if matches!(ty, Ty::Byte | Ty::Int32) && int_fits(ty, v) => Ok(Some(Const::Int(v))),
        Some((_, found)) => Err(Diagnostic::new(
            format!("expected a {} default, found {}", t.ty_name(ty), t.ty_name(found)),
            e.span,
        )),
    }
}

/// The name by which the prelude's `Any` and cast helpers know a basic type: `Int`, `Float`, `Bool`, `Char`,
/// `String` or `Bytes`.
pub fn basic_name(ty: Ty) -> Option<&'static str> {
    Some(match ty {
        Ty::Int => "Int",
        Ty::Float => "Float",
        Ty::Bool => "Bool",
        Ty::Char => "Char",
        Ty::Int32 => "Int32",
        Ty::Byte => "Byte",
        Ty::Str => "String",
        Ty::Bytes => "Bytes",
        _ => return None,
    })
}

/// How wide an integer type is, for deciding which side of `a + b` widens: `Byte`, `Int32`, `Int`.
pub fn int_rank(ty: Ty) -> Option<u8> {
    match ty {
        Ty::Byte => Some(0),
        Ty::Int32 => Some(1),
        Ty::Int => Some(2),
        _ => None,
    }
}

/// Does `v` fit in `ty`?  Only `Byte` and `Int32` have limits.
pub fn int_fits(ty: Ty, v: i64) -> bool {
    match ty {
        Ty::Byte => (0..=255).contains(&v),
        Ty::Int32 => (i64::from(i32::MIN)..=i64::from(i32::MAX)).contains(&v),
        _ => true,
    }
}
