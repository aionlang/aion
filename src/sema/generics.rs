//! Types as written in source, and generics.
//!
//! Generics are **monomorphized**: a generic type or function is a template
//! (its syntax tree), and each distinct list of type arguments gets its own
//! concrete copy — `List<Int>` and `List<String>` are different types with
//! different methods, each checked as ordinary non-generic code.  That keeps
//! the rest of the compiler (and the garbage collector's exact pointer maps)
//! free of any notion of "unknown type".

use std::collections::HashMap;
use std::rc::Rc;

use super::hir::{FuncKind, Ty, TypeId};
use super::{Checker, FnId, FnInfo, Subst, Sym, TypeInfo, Work};
use crate::diag::{Diagnostic, Result, Span};
use crate::syntax::ast::{self, TypeKind};

/// How deep generic instantiation may nest before we assume it is runaway
/// (`fn f<T>() = f<List<T>>()`).
const MAX_DEPTH: u32 = 64;

impl<'a> Checker<'a> {
    /// Resolve a written type.  `Unit` (no value) is only meaningful as a return type.
    pub fn resolve_type(&self, module: usize, ty: &ast::TypeRef, subst: &Subst) -> Result<Ty> {
        match &ty.kind {
            TypeKind::Named { name, args } => {
                if args.is_empty()
                    && let Some(&t) = subst.get(name)
                {
                    return Ok(t);
                }
                let builtin = match name.as_str() {
                    "Int" => Some(Ty::Int),
                    "Float" => Some(Ty::Float),
                    "Bool" => Some(Ty::Bool),
                    "Char" => Some(Ty::Char),
                    "String" => Some(Ty::Str),
                    "Unit" => Some(Ty::Void),
                    _ => None,
                };
                if let Some(t) = builtin {
                    if !args.is_empty() {
                        return Err(Diagnostic::new(format!("`{name}` takes no type arguments"), ty.span));
                    }
                    return Ok(t);
                }
                match self.t.lookup(module, name) {
                    Some(Sym::Type(id)) => {
                        if !args.is_empty() {
                            return Err(Diagnostic::new(format!("`{name}` is not generic"), ty.span));
                        }
                        Ok(Ty::Obj(id))
                    }
                    Some(Sym::GenericType(g)) => {
                        if args.is_empty() {
                            return Err(Diagnostic::new(format!("`{name}` needs type arguments"), ty.span)
                                .hint(format!("write it like `{name}<Int>`")));
                        }
                        let targs = args
                            .iter()
                            .map(|a| self.resolve_value_type(module, a, subst))
                            .collect::<Result<Vec<_>>>()?;
                        Ok(Ty::Obj(self.instantiate_type(g, targs, ty.span)?))
                    }
                    None if name == "Array" => {
                        let [elem] = args.as_slice() else {
                            return Err(Diagnostic::new("`Array` takes 1 type argument, e.g. `Array<Int>`", ty.span));
                        };
                        Ok(self.t.array_of(self.resolve_value_type(module, elem, subst)?))
                    }
                    _ => Err(Diagnostic::new(format!("unknown type `{name}`"), ty.span)),
                }
            }
            TypeKind::Nullable(inner) => {
                let inner = self.resolve_value_type(module, inner, subst)?;
                Ok(self.t.opt_of(inner))
            }
            TypeKind::Fn { params, ret } => {
                let params =
                    params.iter().map(|p| self.resolve_value_type(module, p, subst)).collect::<Result<Vec<_>>>()?;
                let ret = self.resolve_type(module, ret, subst)?;
                Ok(self.t.intern_fn(params, ret))
            }
        }
    }

    /// Like [`Self::resolve_type`], for places that hold a value (variables, fields, parameters).
    pub fn resolve_value_type(&self, module: usize, ty: &ast::TypeRef, subst: &Subst) -> Result<Ty> {
        match self.resolve_type(module, ty, subst)? {
            Ty::Void => Err(Diagnostic::new("`Unit` means \"no value\"; it can only be a return type", ty.span)),
            t => Ok(t),
        }
    }

    /// `fn List<T>.first()` written outside the generic type `List`.
    pub(super) fn add_extra_method(
        &self,
        g: usize,
        module: usize,
        owner: &ast::Ident,
        owner_params: &[ast::Ident],
        decl: &'a ast::FnDecl,
    ) -> Result<()> {
        let gt = self.generic_types.borrow()[g].clone();
        if owner_params.len() != gt.tparams.len() {
            return Err(Diagnostic::new(
                format!(
                    "`{}` has {} type parameter(s); write `fn {}<{}>.{}`",
                    owner.name,
                    gt.tparams.len(),
                    owner.name,
                    gt.tparams.join(", "),
                    decl.name.name
                ),
                owner.span,
            ));
        }
        let names: Vec<String> = owner_params.iter().map(|p| p.name.clone()).collect();
        gt.extra_methods.borrow_mut().push((module, names.clone(), decl));
        // Instances created before this declaration was seen get the method too.
        let existing: Vec<(Vec<Ty>, TypeId)> = gt.instances.borrow().iter().map(|(a, &t)| (a.clone(), t)).collect();
        for (args, ty) in existing {
            self.declare_extra_on_instance(module, ty, &names, &args, decl)?;
        }
        Ok(())
    }

    fn declare_extra_on_instance(
        &self,
        module: usize,
        ty: TypeId,
        names: &[String],
        args: &[Ty],
        decl: &'a ast::FnDecl,
    ) -> Result<()> {
        let subst: Subst = Rc::new(names.iter().cloned().zip(args.iter().copied()).collect());
        self.declare_method(&self.module_names[module], module, ty, decl, &subst)
    }

    /// `fn f<T: Shape>(…)`: check that the type given for `T` has what `Shape` requires.
    fn check_bounds(
        &self,
        module: usize,
        tparams: &[String],
        bounds: &[Option<ast::TypeRef>],
        args: &[Ty],
        span: Span,
    ) -> Result<()> {
        for ((name, bound), &arg) in tparams.iter().zip(bounds).zip(args) {
            let Some(bound) = bound else { continue };
            let want = match self.resolve_type(module, bound, &self.empty)? {
                Ty::Obj(b) if self.t.ty(b).iface.is_some() => b,
                other => {
                    return Err(Diagnostic::new(
                        format!("the bound of `{name}` must be an interface, not {}", self.t.ty_name(other)),
                        bound.span,
                    ));
                }
            };
            self.satisfies(arg, want).map_err(|why| {
                Diagnostic::new(
                    format!("{} can't be used for `{name}: {}`", self.t.ty_name(arg), self.t.ty_name(Ty::Obj(want))),
                    span,
                )
                .hint(why)
            })?;
        }
        Ok(())
    }

    /// The concrete type `List<Int>` for template `g` and arguments `[Int]`.
    pub(super) fn instantiate_type(&self, g: usize, args: Vec<Ty>, span: Span) -> Result<TypeId> {
        let gt = self.generic_types.borrow()[g].clone();
        if args.len() != gt.tparams.len() {
            return Err(Diagnostic::new(
                format!(
                    "`{}` takes {} type argument(s) but {} were given",
                    gt.decl.name.name,
                    gt.tparams.len(),
                    args.len()
                ),
                span,
            ));
        }
        if let Some(&id) = gt.instances.borrow().get(&args) {
            return Ok(id);
        }
        self.check_bounds(gt.module, &gt.tparams, &gt.decl.bounds, &args, span)?;
        if self.depth.get() > MAX_DEPTH {
            return Err(Diagnostic::new("generic types nest too deeply", span));
        }
        self.depth.set(self.depth.get() + 1);
        let result = self.build_instance(&gt, g, args);
        self.depth.set(self.depth.get() - 1);
        result
    }

    fn build_instance(&self, gt: &Rc<super::GenericType<'a>>, g: usize, args: Vec<Ty>) -> Result<TypeId> {
        let td: &'a ast::TypeDecl = gt.decl;
        let arg_names: Vec<String> = args.iter().map(|&a| self.t.ty_name(a)).collect();
        let id = self.t.add_type(TypeInfo {
            name: format!("{}<{}>", td.name.name, arg_names.join(", ")),
            fields: Vec::new(),
            methods: HashMap::new(),
            generic_methods: HashMap::new(),
            ctor: usize::MAX,
            array_elem: None,
            iface: None,
        });
        // Register before resolving anything, so a self-referential type (`next: Node<T>`) finds itself.
        gt.instances.borrow_mut().insert(args.clone(), id);
        self.instance_of.borrow_mut().insert(id, (g, args.clone()));

        let subst: Subst = Rc::new(gt.tparams.iter().cloned().zip(args.iter().copied()).collect());
        let fields = self.type_fields(gt.module, td, &subst)?;
        self.t.update_type(id, |ti| ti.fields = fields);

        let module_name = &self.module_names[gt.module];
        for m in &td.methods {
            self.declare_method(module_name, gt.module, id, m, &subst)?;
        }
        let extras: Vec<_> = gt.extra_methods.borrow().clone();
        for (module, names, decl) in extras {
            self.declare_extra_on_instance(module, id, &names, &args, decl)?;
        }
        self.declare_ctor(module_name, gt.module, id, td, &subst)?;
        Ok(id)
    }

    /// The concrete function for generic template `g` and type arguments `targs`.
    pub(super) fn instantiate_fn(&self, g: usize, targs: Vec<Ty>, span: Span) -> Result<FnId> {
        let gf = self.generic_fns.borrow()[g].clone();
        if let Some(&id) = gf.instances.borrow().get(&targs) {
            return Ok(id);
        }
        self.check_bounds(gf.module, &gf.tparams, &gf.decl.bounds, &targs, span)?;
        if self.depth.get() > MAX_DEPTH {
            return Err(Diagnostic::new("generic functions nest too deeply", span));
        }
        self.depth.set(self.depth.get() + 1);
        let result = (|| {
            let mut map: HashMap<String, Ty> = (*gf.base).clone();
            for (name, &ty) in gf.tparams.iter().zip(&targs) {
                map.insert(name.clone(), ty);
            }
            let subst: Subst = Rc::new(map);
            let symbol = format!(
                "{}__{}__g{}",
                self.module_names[gf.module],
                gf.decl.name.name,
                self.fns.borrow().len()
            );
            let (kind, receiver) = match gf.owner {
                Some(t) => (FuncKind::Method, Some(Ty::Obj(t))),
                None => (FuncKind::Free, None),
            };
            let info: FnInfo = self.signature(gf.module, gf.decl, kind, receiver, symbol, &subst)?;
            let work = if gf.owner.is_some() {
                Work::Method { decl: gf.decl, subst }
            } else {
                Work::Fn { decl: gf.decl, subst }
            };
            let id = self.declare_fn(info, work);
            gf.instances.borrow_mut().insert(targs, id);
            Ok(id)
        })();
        self.depth.set(self.depth.get() - 1);
        result
    }
}
