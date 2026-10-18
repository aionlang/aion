//! Reflection: for a program that imports `reflect`, each of its modules gets a piece of ordinary Aion code that
//! registers the module's types with the library (their fields, methods and constructors, as closures).
//!
//! The code is generated as text from the syntax tree, parsed, and appended to the module before type checking, so
//! the checker sees nothing special and a mistake in it would show as an ordinary error in the file
//! `<reflect>/<module>`.  What a type has that reflection cannot express (generic types, function types, `vararg`s,
//! type parameters of methods) is left out rather than turned into code that would not compile.

use std::fmt::Write as _;

use crate::diag::{Diagnostic, SourceMap};
use crate::loader::{Loaded, ResolvedImport};
use crate::syntax::{self, ast};

use ast::{FnBody, Item, TypeKind, TypeRef};

/// Add the registration code to every module of the program, if `reflect` is among the modules.
pub fn instrument(loaded: &mut Loaded, map: &mut SourceMap) -> Result<(), Diagnostic> {
    let is_std = |map: &SourceMap, file: u32| map.file(file).name.starts_with("<std>");
    let Some(found) = loaded.modules.iter().position(|m| map.file(m.file).name == "<std>/reflect.aion") else {
        return Ok(());
    };
    let reflect = move_up(loaded, found);
    for i in 0..loaded.modules.len() {
        if i == reflect || is_std(map, loaded.modules[i].file) {
            continue;
        }
        let code = generate(&loaded.modules[i].ast);
        if code.is_empty() {
            continue;
        }
        let file = map.add(format!("<reflect>/{}", loaded.modules[i].name), code.clone());
        let parsed = syntax::parse_module(file, &code)?;
        let module = &mut loaded.modules[i];
        // first, so that the types are registered before the module's own statements run
        module.ast.items.splice(0..0, parsed.items);
        if !module.imports.iter().any(|r| r.module == reflect && r.alias.as_deref() == Some("reflect")) {
            module.imports.push(ResolvedImport {
                module: reflect,
                alias: Some("reflect".into()),
                symbol: None,
                span: crate::diag::Span::new(file, 0, 0),
            });
        }
    }
    Ok(())
}

/// Put module `from` right after the prelude, so that `reflect` starts up before every module that registers with it
/// (modules run their top-level code in the order they are listed).  Gives its new index.
fn move_up(loaded: &mut Loaded, from: usize) -> usize {
    if from <= 1 {
        return from;
    }
    let n = loaded.modules.len();
    let order: Vec<usize> = std::iter::once(0).chain(std::iter::once(from)).chain((1..n).filter(|&i| i != from)).collect();
    let mut new_index = vec![0; n];
    for (new, &old) in order.iter().enumerate() {
        new_index[old] = new;
    }
    let mut slots: Vec<Option<crate::loader::LoadedModule>> = std::mem::take(&mut loaded.modules).into_iter().map(Some).collect();
    loaded.modules = order.iter().map(|&old| slots[old].take().expect("each module once")).collect();
    for m in &mut loaded.modules {
        for import in &mut m.imports {
            import.module = new_index[import.module];
        }
    }
    1
}

fn render(t: &TypeRef) -> String {
    match &t.kind {
        TypeKind::Named { name, args } if args.is_empty() => name.clone(),
        TypeKind::Named { name, args } => format!("{name}<{}>", args.iter().map(render).collect::<Vec<_>>().join(", ")),
        TypeKind::Fn { params, ret } => {
            format!("({}) -> {}", params.iter().map(render).collect::<Vec<_>>().join(", "), render(ret))
        }
        TypeKind::Nullable(inner) => match inner.kind {
            TypeKind::Fn { .. } => format!("({})?", render(inner)),
            _ => format!("{}?", render(inner)),
        },
    }
}

/// Does the type mention a function type, which cannot be turned into an `Any`?
fn has_fn(t: &TypeRef) -> bool {
    match &t.kind {
        TypeKind::Named { args, .. } => args.iter().any(has_fn),
        TypeKind::Fn { .. } => true,
        TypeKind::Nullable(inner) => has_fn(inner),
    }
}

fn named(t: &TypeRef) -> Option<(&str, bool)> {
    match &t.kind {
        TypeKind::Named { name, args } => Some((name.as_str(), !args.is_empty())),
        _ => None,
    }
}

/// A test that the `Any?` expression `value` can be used as type `t` (true when it cannot be checked).
fn accepts_test(t: &TypeRef, value: &str) -> String {
    match &t.kind {
        TypeKind::Nullable(inner) => format!("({value} == null || {})", accepts_test(inner, value)),
        TypeKind::Named { name, args } if name == "Any" && args.is_empty() => format!("{value} != null"),
        TypeKind::Named { name, args } if args.is_empty() && name == "Float" => {
            format!("({value} is Float || {value} is Int)")
        }
        TypeKind::Named { name, args } if args.is_empty() => format!("{value} is {name}"),
        _ => "true".into(),
    }
}

/// The expression that turns argument `i` of the list `a` into type `t`.
fn argument(t: &TypeRef, i: usize) -> String {
    match &t.kind {
        TypeKind::Nullable(inner) => match named(inner) {
            Some(("Any", false)) => format!("reflect.argOrNull(a, {i})"),
            Some(("Float", false)) => format!("reflect.argFloatOrNull(a, {i})"),
            _ => format!("reflect.argOrNull(a, {i}) as? {}", render(inner)),
        },
        TypeKind::Named { name, args } if name == "Any" && args.is_empty() => format!("reflect.arg(a, {i})"),
        TypeKind::Named { name, args } if name == "Float" && args.is_empty() => format!("reflect.argFloat(a, {i})"),
        _ => format!("reflect.arg(a, {i}) as {}", render(t)),
    }
}

struct Member<'a> {
    name: &'a str,
    params: &'a [ast::Param],
    ret: Option<&'a TypeRef>,
    void: bool,
}

fn usable_params(params: &[ast::Param]) -> bool {
    params.iter().all(|p| !p.vararg && !has_fn(&p.ty))
}

/// How many of the parameters must be given (the ones before the first with a default).
fn required(params: &[ast::Param]) -> usize {
    params.iter().take_while(|p| p.default.is_none()).count()
}

fn private(name: &str) -> bool {
    name.starts_with('_')
}

/// Note a method that reflection can call.
fn collect<'a>(d: &'a ast::FnDecl, methods: &mut Vec<Member<'a>>) {
    if d.is_static || !d.type_params.is_empty() || private(&d.name.name) || !usable_params(&d.params) {
        return;
    }
    if d.ret.as_ref().is_some_and(has_fn) {
        return;
    }
    let void = match (&d.ret, &d.body) {
        (Some(r), _) => matches!(named(r), Some(("Unit", false))),
        (None, FnBody::Expr(e)) => {
            // a method that answers with a lambda has a function type, which cannot be an `Any`
            if matches!(e.kind, ast::ExprKind::Lambda { .. }) {
                return;
            }
            false
        }
        (None, _) => true,
    };
    methods.push(Member { name: &d.name.name, params: &d.params, ret: d.ret.as_ref(), void });
}

fn generate(module: &ast::Module) -> String {
    let mut out = String::new();
    let mut counter = 0usize;
    for item in &module.items {
        let Item::Type(td) = item else { continue };
        if !td.type_params.is_empty() || td.variants.is_some() || private(&td.name.name) {
            continue;
        }
        let tname = &td.name.name;
        let id = counter;
        counter += 1;

        // fields: the primary constructor's parameters, then the ones declared in the body
        let mut fields: Vec<(&str, &TypeRef)> = Vec::new();
        for p in td.primary.iter().flatten() {
            fields.push((&p.name.name, &p.ty));
        }
        for f in &td.fields {
            fields.push((&f.name.name, &f.ty));
        }

        // methods: declared in the type, and `fn Type.name()` extensions in this module
        let mut methods: Vec<Member> = Vec::new();
        for m in &td.methods {
            collect(m, &mut methods);
        }
        for other in &module.items {
            if let Item::Method { owner, owner_params, decl } = other
                && owner.name == *tname
                && owner_params.is_empty()
            {
                collect(decl, &mut methods);
            }
        }

        // constructors
        let ctor_params: &[ast::Param] = match (&td.primary, &td.ctor) {
            (Some(p), _) => p,
            (None, Some(c)) => &c.params,
            (None, None) => &[],
        };
        let constructible = usable_params(ctor_params);

        // ---- getter and setter ----
        let _ = writeln!(out, "fn __reflect_get_{id}(o: Any, i: Int) -> Any? {{");
        let _ = writeln!(out, "    p := o as {tname}");
        for (k, (name, ty)) in fields.iter().enumerate() {
            if !has_fn(ty) {
                let _ = writeln!(out, "    if i == {k} {{ return reflect.box(p.{name}) }}");
            }
        }
        let _ = writeln!(out, "    return null\n}}");

        let _ = writeln!(out, "fn __reflect_set_{id}(o: Any, i: Int, v: Any?) -> Bool {{");
        let _ = writeln!(out, "    p := o as {tname}");
        for (k, (name, ty)) in fields.iter().enumerate() {
            let (inner, nullable) = match &ty.kind {
                TypeKind::Nullable(inner) => (inner.as_ref(), true),
                _ => (*ty, false),
            };
            let Some((type_name, generic)) = named(inner) else { continue };
            if generic || type_name == "Any" {
                continue;
            }
            let _ = writeln!(out, "    if i == {k} {{");
            if nullable {
                let _ = writeln!(out, "        if v == null {{\n            p.{name} = null\n            return true\n        }}");
            }
            let _ = writeln!(out, "        if v is {type_name} {{\n            p.{name} = v\n            return true\n        }}");
            if type_name == "Float" {
                let _ = writeln!(out, "        if v is Int {{\n            p.{name} = v as Float\n            return true\n        }}");
            }
            let _ = writeln!(out, "        return false\n    }}");
        }
        let _ = writeln!(out, "    return false\n}}");

        // ---- methods ----
        let mut method_infos = Vec::new();
        for (m_index, m) in methods.iter().enumerate() {
            let total = m.params.len();
            let least = required(m.params);
            let mut test = format!("a.size() >= {least} && a.size() <= {total}");
            for (i, p) in m.params.iter().enumerate() {
                let _ = write!(test, " && (a.size() <= {i} || {})", accepts_test(&p.ty, &format!("a[{i}]")));
            }
            let _ = writeln!(out, "fn __reflect_ok_{id}_{m_index}(a: List<Any?>) -> Bool = {test}");
            let _ = writeln!(out, "fn __reflect_call_{id}_{m_index}(o: Any, a: List<Any?>) -> Any? {{");
            let _ = writeln!(out, "    p := o as {tname}");
            for count in least..=total {
                let args: Vec<String> = m.params[..count].iter().enumerate().map(|(i, p)| argument(&p.ty, i)).collect();
                let call = format!("p.{}({})", m.name, args.join(", "));
                let _ = writeln!(out, "    if a.size() == {count} {{");
                if m.void {
                    let _ = writeln!(out, "        {call}\n        return null");
                } else {
                    let _ = writeln!(out, "        return reflect.box({call})");
                }
                let _ = writeln!(out, "    }}");
            }
            let _ = writeln!(out, "    return null\n}}");
            let ptypes: Vec<String> = m.params.iter().map(|p| format!("\"{}\"", render(&p.ty))).collect();
            let returns = if m.void { "Unit".to_string() } else { m.ret.map_or("?".to_string(), render) };
            method_infos.push(format!(
                "reflect.MethodInfo(\"{}\", [{}], \"{}\", __reflect_ok_{id}_{m_index}, __reflect_call_{id}_{m_index})",
                m.name,
                ptypes.join(", "),
                returns
            ));
        }

        // ---- constructors ----
        let mut ctor_infos = Vec::new();
        if constructible {
            let total = ctor_params.len();
            let least = required(ctor_params);
            let mut test = format!("a.size() >= {least} && a.size() <= {total}");
            for (i, p) in ctor_params.iter().enumerate() {
                let _ = write!(test, " && (a.size() <= {i} || {})", accepts_test(&p.ty, &format!("a[{i}]")));
            }
            let _ = writeln!(out, "fn __reflect_cok_{id}(a: List<Any?>) -> Bool = {test}");
            let _ = writeln!(out, "fn __reflect_new_{id}(a: List<Any?>) -> Any? {{");
            for count in least..=total {
                let args: Vec<String> = ctor_params[..count].iter().enumerate().map(|(i, p)| argument(&p.ty, i)).collect();
                let _ = writeln!(out, "    if a.size() == {count} {{ return reflect.box({tname}({})) }}", args.join(", "));
            }
            let _ = writeln!(out, "    return null\n}}");
            let ptypes: Vec<String> = ctor_params.iter().map(|p| format!("\"{}\"", render(&p.ty))).collect();
            ctor_infos.push(format!("reflect.CtorInfo([{}], __reflect_cok_{id}, __reflect_new_{id})", ptypes.join(", ")));
        }

        // ---- the registration ----
        let quoted = |items: Vec<String>| items.iter().map(|s| format!("\"{s}\"")).collect::<Vec<_>>().join(", ");
        let field_names = quoted(fields.iter().map(|(n, _)| n.to_string()).collect());
        let field_types = quoted(fields.iter().map(|(_, t)| render(t)).collect());
        let parent = td.parent.as_ref().map_or(String::new(), |p| p.name.name.clone());
        let _ = writeln!(
            out,
            "reflect.register(reflect.TypeInfo(\"{tname}\", \"{parent}\", [{field_names}], [{field_types}], __reflect_get_{id}, __reflect_set_{id}, [{}], [{}]))",
            method_infos.join(", "),
            ctor_infos.join(", ")
        );
    }
    out
}
