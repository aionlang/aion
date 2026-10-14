//! Calling generic functions and constructing generic types.
//!
//! The type arguments are *inferred*: first from the arguments that are not
//! lambdas, then from the type the surrounding context expects, and finally
//! lambdas are checked against the parameter types that are known by then, which
//! in turn reveals the rest (`xs.map { it.length }` learns `R` from the lambda).
//! The matching concrete instance is then called like any other function.

use super::*;
use crate::syntax::ast::{TypeKind, TypeRef};

/// What inference needs to know about a generic callee.
struct Template<'t> {
    name: &'t str,
    module: usize,
    tparams: &'t [String],
    /// Parameters of the enclosing generic type, already bound.
    base: Subst,
    params: &'t [ast::Param],
    /// The result type as written, to use the context's expectation.
    ret: Option<TypeRef>,
}

impl Cx<'_, '_> {
    /// Call a generic function or method.  `recv` is the receiver of a method.
    pub(super) fn call_generic_fn(
        &mut self,
        g: usize,
        recv: Option<hir::Expr>,
        type_args: &[TypeRef],
        args: &[ast::Arg],
        span: Span,
        expected: Option<Ty>,
    ) -> Result<hir::Expr> {
        let gf = self.ck.generic_fns.borrow()[g].clone();
        let tpl = Template {
            name: &gf.decl.name.name,
            module: gf.module,
            tparams: &gf.tparams,
            base: gf.base.clone(),
            params: &gf.decl.params,
            ret: gf.decl.ret.clone(),
        };
        let (targs, pre) = self.infer(&tpl, type_args, args, span, expected)?;
        let id = self.ck.instantiate_fn(g, targs, span)?;
        self.call_fn_pre(id, recv, args, span, pre)
    }

    /// Construct an instance of a generic type: `Box(5)`, `List<Int>()`, `xs: List<Int> = List()`.
    pub(super) fn call_generic_type(
        &mut self,
        g: usize,
        type_args: &[TypeRef],
        args: &[ast::Arg],
        span: Span,
        expected: Option<Ty>,
    ) -> Result<hir::Expr> {
        let gt = self.ck.generic_types.borrow()[g].clone();
        let decl = gt.decl;
        // The constructor's parameters are the template's parameters.
        let params: &[ast::Param] = match (&decl.ctor, &decl.primary) {
            (Some(c), _) => &c.params,
            (None, Some(p)) => p,
            (None, None) => &[],
        };
        // Its result is `Name<T, …>`.
        let named = |name: &str, args: Vec<TypeRef>| TypeRef { kind: TypeKind::Named { name: name.to_string(), args }, span };
        let ret = named(&decl.name.name, gt.tparams.iter().map(|t| named(t, Vec::new())).collect());
        let tpl = Template {
            name: &decl.name.name,
            module: gt.module,
            tparams: &gt.tparams,
            base: self.ck.empty.clone(),
            params,
            ret: Some(ret),
        };
        let (targs, pre) = self.infer(&tpl, type_args, args, span, expected)?;
        let ty = self.ck.instantiate_type(g, targs, span)?;
        let ctor = self.ck.t.ty(ty).ctor;
        self.call_fn_pre(ctor, None, args, span, pre)
    }

    /// Work out the type arguments; also returns the arguments already evaluated.
    fn infer(
        &mut self,
        tpl: &Template<'_>,
        type_args: &[TypeRef],
        args: &[ast::Arg],
        span: Span,
        expected: Option<Ty>,
    ) -> Result<(Vec<Ty>, Vec<Option<hir::Expr>>)> {
        let n = tpl.tparams.len();
        let mut bind: Vec<Option<Ty>> = vec![None; n];
        if !type_args.is_empty() {
            if type_args.len() != n {
                return Err(Diagnostic::new(
                    format!("`{}` takes {n} type argument(s) but {} were given", tpl.name, type_args.len()),
                    span,
                ));
            }
            for (slot, t) in bind.iter_mut().zip(type_args) {
                *slot = Some(self.ck.resolve_value_type(self.module, t, &self.subst)?);
            }
        }

        let pnames: Vec<&str> = tpl.params.iter().map(|p| p.name.name.as_str()).collect();
        let order = self.map_args(tpl.name, &pnames, args, span)?;
        let mut pre: Vec<Option<hir::Expr>> = (0..args.len()).map(|_| None).collect();

        // 1. Arguments that are not lambdas say what their types are.
        for (ai, a) in args.iter().enumerate() {
            if matches!(a.value.kind, A::Lambda { .. }) {
                continue;
            }
            let v = self.value(&a.value)?;
            self.unify(tpl, &tpl.params[order[ai]].ty, v.ty, &mut bind);
            pre[ai] = Some(v);
        }

        // 2. What the context expects can settle parameters the arguments didn't.
        if let (Some(want), Some(ret)) = (expected, &tpl.ret)
            && bind.iter().any(Option::is_none)
        {
            let mut trial = bind.clone();
            self.unify(tpl, ret, want, &mut trial);
            for (slot, found) in bind.iter_mut().zip(trial) {
                slot.get_or_insert_with(|| found.unwrap_or(Ty::Void));
            }
            for slot in &mut bind {
                if *slot == Some(Ty::Void) {
                    *slot = None;
                }
            }
        }

        // 3. Lambdas, now that the parameter types they need are (usually) known.
        for (ai, a) in args.iter().enumerate() {
            let A::Lambda { params, explicit, body } = &a.value.kind else { continue };
            let written = &tpl.params[order[ai]].ty;
            let sig = self.lambda_expectation(tpl, written, &bind);
            let v = self.lambda_sig(params, *explicit, body, sig, a.value.span)?;
            self.unify(tpl, written, v.ty, &mut bind);
            pre[ai] = Some(v);
        }

        let mut targs = Vec::with_capacity(n);
        for (name, b) in tpl.tparams.iter().zip(&bind) {
            match b {
                Some(t) => targs.push(*t),
                None => {
                    return Err(Diagnostic::new(
                        format!("cannot infer the type parameter `{name}` of `{}`", tpl.name),
                        span,
                    )
                    .hint(format!("give it explicitly, e.g. `{}<Int>(…)`, or annotate the result", tpl.name)));
                }
            }
        }
        Ok((targs, pre))
    }

    /// Match a parameter type as written (`List<T>`, `(T) -> R`) against the
    /// type an argument actually has, recording what each type parameter is.
    /// Mismatches are ignored here; they surface when the call is type-checked.
    fn unify(&self, tpl: &Template<'_>, written: &TypeRef, actual: Ty, bind: &mut [Option<Ty>]) {
        match &written.kind {
            TypeKind::Named { name, args } => {
                if args.is_empty() {
                    if let Some(i) = tpl.tparams.iter().position(|t| t == name)
                        && actual != Ty::Void
                    {
                        bind[i] = match (bind[i], actual) {
                            (None, a) => Some(a),
                            // `max(1, 2.5)`: the parameter becomes Float and the Int widens.
                            (Some(Ty::Int), Ty::Float) => Some(Ty::Float),
                            (Some(b), _) => Some(b),
                        };
                    }
                    return;
                }
                // `List<T>` against the instance `List<Int>`.
                let Ty::Obj(id) = actual else { return };
                let Some((g, targs)) = self.ck.instance_of.borrow().get(&id).cloned() else { return };
                let Some(Sym::GenericType(want)) = self.ck.t.lookup(tpl.module, name) else { return };
                if want != g || targs.len() != args.len() {
                    return;
                }
                for (a, t) in args.iter().zip(targs) {
                    self.unify(tpl, a, t, bind);
                }
            }
            // `T?` against `Int?` binds T = Int, and against a plain `Int` too.
            TypeKind::Nullable(inner) => match actual {
                Ty::Opt(id) => self.unify(tpl, inner, self.ck.t.opt_inner(id), bind),
                Ty::Null => {}
                other => self.unify(tpl, inner, other, bind),
            },
            TypeKind::Fn { params, ret } => {
                let Ty::Fn(id) = actual else { return };
                let sig = self.ck.t.fn_sig(id);
                if sig.params.len() != params.len() {
                    return;
                }
                for (p, a) in params.iter().zip(sig.params) {
                    self.unify(tpl, p, a, bind);
                }
                self.unify(tpl, ret, sig.ret, bind);
            }
        }
    }

    /// What a lambda passed for parameter type `written` must look like, as far
    /// as the type parameters bound so far tell.
    fn lambda_expectation(&self, tpl: &Template<'_>, written: &TypeRef, bind: &[Option<Ty>]) -> Option<LamSig> {
        let TypeKind::Fn { params, ret } = &written.kind else { return None };
        let mut map: HashMap<String, Ty> = (*tpl.base).clone();
        for (name, b) in tpl.tparams.iter().zip(bind) {
            if let Some(t) = b {
                map.insert(name.clone(), *t);
            }
        }
        let partial: Subst = Rc::new(map);
        let params = params
            .iter()
            .map(|p| self.ck.resolve_value_type(tpl.module, p, &partial))
            .collect::<Result<Vec<_>>>()
            .ok()?;
        // The result type may still depend on a parameter that only the lambda reveals.
        let ret = self.ck.resolve_type(tpl.module, ret, &partial).ok();
        Some(LamSig { params, ret })
    }
}
