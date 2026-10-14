//! Semantic analysis: names and types.
//!
//! Runs in two phases over the loaded modules (dependencies first):
//!
//! 1. **Declare** — register every type, function, method and constructor with
//!    its resolved signature ([`Tables`]).  Bodies are not looked at.
//! 2. **Check** — type-check each body against those tables, producing [`hir`].
//!
//! Splitting the phases is what lets functions call each other regardless of
//! definition order, with no forward-declaration bookkeeping.

mod body;
pub mod hir;

use std::collections::HashMap;

use crate::diag::{Diagnostic, Result, Span};
use crate::loader::Loaded;
use crate::syntax::ast::{self, Item};
use hir::{FnId, FuncKind, Ty, TypeId};

pub struct TypeInfo {
    pub name: String,
    pub fields: Vec<hir::Field>,
    pub methods: HashMap<String, FnId>,
    pub ctor: FnId,
}

pub struct FnInfo {
    pub name: String,
    pub module: usize,
    pub symbol: String,
    pub kind: FuncKind,
    /// Includes the receiver `it` for methods.
    pub params: Vec<(String, Ty)>,
    pub ret: Ty,
}

#[derive(Clone, Copy)]
pub enum Sym {
    Fn(FnId),
    Type(TypeId),
}

/// Everything visible inside one module.
#[derive(Default)]
pub struct Scope {
    pub items: HashMap<String, Sym>,
    /// `import m;` — `alias` → module index, for `alias.name(...)`.
    pub modules: HashMap<String, usize>,
    /// `import m.Name;` — brings `Name` in unqualified.
    pub imported: HashMap<String, Sym>,
}

#[derive(Default)]
pub struct Tables {
    pub types: Vec<TypeInfo>,
    pub funcs: Vec<FnInfo>,
    pub scopes: Vec<Scope>,
}

/// What a declared function needs when its body is checked.
pub enum Work<'a> {
    Fn(&'a ast::FnDecl),
    Method(&'a ast::FnDecl),
    Ctor {
        ty: TypeId,
        decl: Option<&'a ast::CtorDecl>,
        /// Shorthand constructors: the field each parameter initialises.
        shorthand: Vec<usize>,
    },
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

    pub fn resolve_type(&self, module: usize, ty: &ast::TypeRef) -> Result<Ty> {
        Ok(match ty.name.as_str() {
            "Int" => Ty::Int,
            "Float" => Ty::Float,
            "Bool" => Ty::Bool,
            "String" => Ty::Str,
            name => match self.lookup(module, name) {
                Some(Sym::Type(id)) => Ty::Obj(id),
                _ => return Err(Diagnostic::new(format!("unknown type `{name}`"), ty.span)),
            },
        })
    }

    pub fn ty_name(&self, ty: Ty) -> String {
        match ty {
            Ty::Int => "Int".into(),
            Ty::Float => "Float".into(),
            Ty::Bool => "Bool".into(),
            Ty::Str => "String".into(),
            Ty::Obj(id) => self.types[id].name.clone(),
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
    let mut t = Tables::default();
    let mut work: Vec<Work> = Vec::new();
    for module in 0..loaded.modules.len() {
        declare_module(&mut t, &mut work, loaded, module)?;
    }

    let funcs = work
        .iter()
        .enumerate()
        .map(|(id, w)| body::check_fn(&t, id, w))
        .collect::<Result<Vec<_>>>()?;

    let entry_module = loaded.modules.len() - 1;
    let entry = match t.scopes[entry_module].items.get("main") {
        Some(&Sym::Fn(id)) => id,
        _ => return Err(Diagnostic::general("the program has no `fn main()`")),
    };
    let main = &t.funcs[entry];
    if !main.params.is_empty() || !matches!(main.ret, Ty::Void | Ty::Int) {
        return Err(Diagnostic::general("`main` must take no parameters and return nothing or Int"));
    }

    let types = t
        .types
        .into_iter()
        .map(|ti| hir::TypeDef { name: ti.name, fields: ti.fields })
        .collect();
    Ok(hir::Program { types, funcs, entry })
}

fn declare_module<'a>(
    t: &mut Tables,
    work: &mut Vec<Work<'a>>,
    loaded: &'a Loaded,
    mi: usize,
) -> Result<()> {
    let module = &loaded.modules[mi];
    t.scopes.push(Scope::default());

    // 1. Type names, so signatures and fields can refer to any of them.
    let mut own_types: Vec<(TypeId, &ast::TypeDecl)> = Vec::new();
    for item in &module.ast.items {
        if let Item::Type(td) = item {
            let id = t.types.len();
            t.types.push(TypeInfo {
                name: td.name.name.clone(),
                fields: Vec::new(),
                methods: HashMap::new(),
                ctor: usize::MAX,
            });
            t.define(mi, &td.name, Sym::Type(id))?;
            own_types.push((id, td));
        }
    }

    // 2. Imports (all imported modules are already declared).
    for imp in &module.imports {
        match t.scopes[mi].modules.insert(imp.alias.clone(), imp.module) {
            Some(prev) if prev != imp.module => {
                return Err(Diagnostic::new(format!("`{}` is imported twice for different modules", imp.alias), imp.span));
            }
            _ => {}
        }
        if let Some(sym) = &imp.symbol {
            let found = t.scopes[imp.module].items.get(&sym.name).copied();
            match found {
                Some(found) => {
                    t.scopes[mi].imported.insert(sym.name.clone(), found);
                }
                None => {
                    return Err(Diagnostic::new(
                        format!("module `{}` has no item `{}`", imp.alias, sym.name),
                        sym.span,
                    ));
                }
            }
        }
    }

    // 3. Field types.
    for &(id, td) in &own_types {
        let mut fields: Vec<hir::Field> = Vec::new();
        for f in &td.fields {
            if fields.iter().any(|g| g.name == f.name.name) {
                return Err(Diagnostic::new(format!("duplicate field `{}`", f.name.name), f.name.span));
            }
            let ty = t.resolve_type(mi, &f.ty)?;
            fields.push(hir::Field { name: f.name.name.clone(), ty });
        }
        if fields.len() > 64 {
            return Err(Diagnostic::new("a type can have at most 64 fields", td.name.span));
        }
        t.types[id].fields = fields;
    }

    // 4. Functions, methods and constructors.
    for item in &module.ast.items {
        match item {
            Item::Fn(decl) => {
                let info = signature(t, mi, decl, FuncKind::Free, None, symbol_for(&module.name, None, decl))?;
                let id = t.add_fn(info);
                work.push(Work::Fn(decl));
                t.define(mi, &decl.name, Sym::Fn(id))?;
            }
            Item::Method { owner, decl } => {
                let ty = match t.lookup(mi, &owner.name) {
                    Some(Sym::Type(id)) => id,
                    _ => return Err(Diagnostic::new(format!("unknown type `{}`", owner.name), owner.span)),
                };
                declare_method(t, work, &module.name, mi, ty, decl)?;
            }
            Item::Type(_) => {}
        }
    }
    for &(id, td) in &own_types {
        for m in &td.methods {
            declare_method(t, work, &module.name, mi, id, m)?;
        }
        declare_ctor(t, work, &module.name, mi, id, td)?;
    }
    Ok(())
}

fn symbol_for(module: &str, owner: Option<&str>, decl: &ast::FnDecl) -> String {
    match (&decl.body, owner) {
        (ast::FnBody::Extern { symbol }, _) => symbol.clone().unwrap_or_else(|| decl.name.name.clone()),
        (_, Some(ty)) => format!("{module}__{ty}__{}", decl.name.name),
        (_, None) => format!("{module}__{}", decl.name.name),
    }
}

/// Resolve a function's parameter and return types.
fn signature(
    t: &Tables,
    mi: usize,
    decl: &ast::FnDecl,
    kind: FuncKind,
    receiver: Option<TypeId>,
    symbol: String,
) -> Result<FnInfo> {
    let mut params: Vec<(String, Ty)> = Vec::new();
    if let Some(ty) = receiver {
        params.push(("it".into(), Ty::Obj(ty)));
    }
    for p in &decl.params {
        if params.iter().any(|(n, _)| *n == p.name.name) {
            return Err(Diagnostic::new(format!("duplicate parameter `{}`", p.name.name), p.name.span));
        }
        params.push((p.name.name.clone(), t.resolve_type(mi, &p.ty)?));
    }
    let ret = match &decl.ret {
        Some(r) => t.resolve_type(mi, r)?,
        None => Ty::Void,
    };
    if matches!(decl.body, ast::FnBody::Extern { .. }) {
        let c_types_only = params.iter().map(|(_, ty)| *ty).chain([ret]).all(|ty| !matches!(ty, Ty::Obj(_)));
        if !c_types_only {
            return Err(Diagnostic::new("`extern fn` can only use Int, Float, Bool and String", decl.span));
        }
    }
    Ok(FnInfo { name: decl.name.name.clone(), module: mi, symbol, kind, params, ret })
}

fn declare_method<'a>(
    t: &mut Tables,
    work: &mut Vec<Work<'a>>,
    module_name: &str,
    mi: usize,
    ty: TypeId,
    decl: &'a ast::FnDecl,
) -> Result<()> {
    if matches!(decl.body, ast::FnBody::Extern { .. }) {
        return Err(Diagnostic::new("methods cannot be `extern`", decl.span));
    }
    let owner = t.types[ty].name.clone();
    let symbol = symbol_for(module_name, Some(&owner), decl);
    let info = signature(t, mi, decl, FuncKind::Method, Some(ty), symbol)?;
    let id = t.add_fn(info);
    if t.types[ty].methods.insert(decl.name.name.clone(), id).is_some() {
        return Err(Diagnostic::new(format!("`{owner}` already has a method `{}`", decl.name.name), decl.name.span));
    }
    work.push(Work::Method(decl));
    Ok(())
}

fn declare_ctor<'a>(
    t: &mut Tables,
    work: &mut Vec<Work<'a>>,
    module_name: &str,
    mi: usize,
    ty: TypeId,
    td: &'a ast::TypeDecl,
) -> Result<()> {
    let mut shorthand = Vec::new();
    let params: Vec<(String, Ty)> = if let Some(c) = &td.ctor {
        let mut ps: Vec<(String, Ty)> = Vec::new();
        for p in &c.params {
            if ps.iter().any(|(n, _)| *n == p.name.name) {
                return Err(Diagnostic::new(format!("duplicate parameter `{}`", p.name.name), p.name.span));
            }
            ps.push((p.name.name.clone(), t.resolve_type(mi, &p.ty)?));
        }
        ps
    } else {
        // `type A(it.x, it.y)` or, with no list, every field in order.
        let wanted: Vec<(String, Span)> = match &td.ctor_fields {
            Some(list) => list.iter().map(|i| (i.name.clone(), i.span)).collect(),
            None => t.types[ty].fields.iter().map(|f| (f.name.clone(), td.name.span)).collect(),
        };
        let mut ps = Vec::new();
        for (name, span) in wanted {
            let Some(idx) = t.types[ty].fields.iter().position(|f| f.name == name) else {
                return Err(Diagnostic::new(format!("`{}` has no field `{name}`", td.name.name), span));
            };
            shorthand.push(idx);
            ps.push((name, t.types[ty].fields[idx].ty));
        }
        ps
    };

    let info = FnInfo {
        name: td.name.name.clone(),
        module: mi,
        symbol: format!("{module_name}__{}__new", td.name.name),
        kind: FuncKind::Constructor,
        params,
        ret: Ty::Obj(ty),
    };
    let id = t.add_fn(info);
    t.types[ty].ctor = id;
    work.push(Work::Ctor { ty, decl: td.ctor.as_ref(), shorthand });
    Ok(())
}
