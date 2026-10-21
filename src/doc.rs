//! `aion doc`: an API reference, in Markdown, made from the declarations of a program and the `/** … */`
//! comments in front of them.

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use crate::project;
use crate::syntax::ast::*;
use crate::syntax::{doc_comments, parse_module};
use crate::testing::aion_files;

/// A declaration worth a heading: a function, a type, one of a type's members, ...
struct Entry {
    /// `fn`, `type`, `enum`, `interface`, `synonym`, `field`, `property`
    kind: &'static str,
    /// What the heading says: `area`, `Circle`, `Circle.area`
    title: String,
    /// Where the declared name is in the source: a comment in front of it belongs to it.
    at: usize,
    signature: String,
    doc: Option<String>,
    members: Vec<Entry>,
}

/// `aion doc [path…] [-o file]`: the documentation of the given files and folders (default: this project, or
/// this folder), to the standard output or to a file.
pub fn run(args: &[String]) -> ExitCode {
    let mut paths: Vec<PathBuf> = Vec::new();
    let mut output: Option<PathBuf> = None;
    let mut it = args.iter();
    while let Some(a) = it.next() {
        if a == "-o" {
            match it.next() {
                Some(file) => output = Some(PathBuf::from(file)),
                None => {
                    eprintln!("error: -o needs a file name");
                    return ExitCode::from(2);
                }
            }
        } else if a.starts_with('-') {
            eprintln!("error: unknown option {a}");
            return ExitCode::from(2);
        } else {
            paths.push(PathBuf::from(a));
        }
    }
    let by_default = paths.is_empty();
    let root = if by_default {
        let root = project::find(Path::new("."))
            .and_then(|p| p.parent().map(Path::to_path_buf))
            .filter(|p| !p.as_os_str().is_empty())
            .unwrap_or_else(|| PathBuf::from("."));
        paths.push(root.clone());
        root
    } else {
        PathBuf::from(".")
    };

    let mut files = Vec::new();
    for p in &paths {
        if !p.exists() {
            eprintln!("error: {} does not exist", p.display());
            return ExitCode::FAILURE;
        }
        aion_files(p, false, &mut files);
    }
    if by_default {
        // the tests and the examples are programs, not an API
        files.retain(|f| !f.strip_prefix(&root).unwrap_or(f).components().any(|c| matches!(c.as_os_str().to_str(), Some("tests" | "examples"))));
    }

    let (mut text, mut broken) = (String::new(), false);
    for file in &files {
        let Ok(source) = std::fs::read_to_string(file) else { continue };
        match document(&source, &name_of(file, &root)) {
            Ok(section) => text.push_str(&section),
            Err(message) => {
                eprintln!("skipped {}: {message}", file.display());
                broken = true;
            }
        }
    }
    match output {
        Some(file) => {
            if let Err(e) = std::fs::write(&file, text) {
                eprintln!("error: cannot write {}: {e}", file.display());
                return ExitCode::FAILURE;
            }
            eprintln!("wrote {}", file.display());
        }
        None => print!("{text}"),
    }
    if broken { ExitCode::FAILURE } else { ExitCode::SUCCESS }
}

/// The module a file is, as the heading names it: its path inside the project without `.aion`.
fn name_of(file: &Path, root: &Path) -> String {
    let relative = match file.strip_prefix(root) {
        Ok(inside) => inside,
        // a file named by itself: its own name will do
        Err(_) if file.is_absolute() => Path::new(file.file_name().unwrap_or(file.as_os_str())),
        Err(_) => file,
    };
    let text = relative.with_extension("").to_string_lossy().replace('\\', "/");
    text.trim_start_matches("./").to_string()
}

/// The documentation of one source file; empty if it declares nothing public.
fn document(source: &str, module_name: &str) -> Result<String, String> {
    let module = parse_module(0, source).map_err(|d| d.message)?;
    let mut entries = entries(&module, source);
    let mut module_doc = None;
    for (n, (start, end)) in doc_comments(source).into_iter().enumerate() {
        let text = clean(&source[start..end]);
        if !attach(&mut entries, source, end, &text) && n == 0 {
            module_doc = Some(text);
        }
    }
    if entries.is_empty() {
        return Ok(String::new());
    }
    let mut out = format!("# {module_name}\n\n");
    if let Some(d) = module_doc {
        out.push_str(&render_doc(&d));
    }
    for e in &entries {
        render(e, 2, &mut out);
    }
    Ok(out)
}

// ── reading the declarations ─────────────────────────────────────────────────

fn private(name: &str) -> bool {
    name.starts_with('_')
}

fn entries(module: &Module, src: &str) -> Vec<Entry> {
    let mut out = Vec::new();
    for item in &module.items {
        match item {
            Item::Fn(f) if !private(&f.name.name) => out.push(function(f, "", src)),
            Item::Method { owner, owner_params, decl } if !private(&decl.name.name) && !private(&owner.name) => {
                let owner_text = if owner_params.is_empty() {
                    owner.name.clone()
                } else {
                    format!("{}<{}>", owner.name, owner_params.iter().map(|p| p.name.as_str()).collect::<Vec<_>>().join(", "))
                };
                out.push(function(decl, &format!("{owner_text}."), src));
            }
            Item::Type(t) if !private(&t.name.name) => out.push(type_entry(t, src)),
            Item::Interface(i) if !private(&i.name.name) => out.push(interface_entry(i, src)),
            Item::Alias(a) if !private(&a.name.name) => out.push(Entry {
                kind: "synonym",
                title: a.name.name.clone(),
                at: a.name.span.start as usize,
                signature: format!("synonym {}{} = {}", a.name.name, generics(&a.type_params), type_text(&a.target)),
                doc: None,
                members: Vec::new(),
            }),
            _ => {}
        }
    }
    out
}

fn generics(params: &[Ident]) -> String {
    if params.is_empty() { String::new() } else { format!("<{}>", params.iter().map(|p| p.name.as_str()).collect::<Vec<_>>().join(", ")) }
}

fn params_text(params: &[Param], src: &str) -> String {
    let parts: Vec<String> = params
        .iter()
        .map(|p| {
            let mut s = format!("{}{}: {}", if p.vararg { "vararg " } else { "" }, p.name.name, type_text(&p.ty));
            if let Some(d) = &p.default {
                s.push_str(" = ");
                s.push_str(src.get(d.span.start as usize..d.span.end as usize).unwrap_or("…"));
            }
            s
        })
        .collect();
    parts.join(", ")
}

fn function(f: &FnDecl, owner: &str, src: &str) -> Entry {
    let mut signature = String::new();
    if f.is_static {
        signature.push_str("static ");
    }
    match f.modifier {
        Modifier::Open => signature.push_str("open "),
        Modifier::Override => signature.push_str("override "),
        Modifier::None => {}
    }
    let bounded: Vec<String> = f
        .type_params
        .iter()
        .enumerate()
        .map(|(i, p)| match f.bounds.get(i) {
            Some(Some(b)) => format!("{}: {}", p.name, type_text(b)),
            _ => p.name.clone(),
        })
        .collect();
    let generic = if bounded.is_empty() { String::new() } else { format!("<{}>", bounded.join(", ")) };
    signature.push_str(&format!("fn {owner}{}{generic}({})", f.name.name, params_text(&f.params, src)));
    if let Some(r) = &f.ret {
        signature.push_str(&format!(" -> {}", type_text(r)));
    }
    Entry { kind: "fn", title: format!("{owner}{}", f.name.name), at: f.name.span.start as usize, signature, doc: None, members: Vec::new() }
}

fn type_entry(t: &TypeDecl, src: &str) -> Entry {
    let name = &t.name.name;
    // the members of a data type are made by the parser and point at the name
    let generated = |m: &FnDecl| m.span == t.name.span || m.name.name == "$init";
    let is_data = t.methods.iter().any(|m| m.name.name == "component1" && generated(m));
    let is_enum = t.variants.is_some();
    let kind = if is_enum { "enum" } else { "type" };
    let mut signature = String::new();
    if t.open {
        signature.push_str("open ");
    }
    if is_data {
        signature.push_str("data ");
    }
    signature.push_str(&format!("{kind} {name}{}", generics(&t.type_params)));
    if let (Some(ps), false) = (&t.primary, is_enum) {
        signature.push_str(&format!("({})", params_text(ps, src)));
    }
    if let Some(p) = &t.parent {
        signature.push_str(&format!(" : {}", p.name.name));
    }
    let mut members = Vec::new();
    if let Some(vs) = &t.variants {
        signature.push_str(&format!(" {{ {} }}", vs.iter().map(|v| v.name.as_str()).collect::<Vec<_>>().join(", ")));
    }
    for f in &t.fields {
        if !private(&f.name.name) {
            members.push(Entry {
                kind: "field",
                title: format!("{name}.{}", f.name.name),
                at: f.name.span.start as usize,
                signature: format!("{}: {}", f.name.name, type_text(&f.ty)),
                doc: None,
                members: Vec::new(),
            });
        }
    }
    for m in &t.methods {
        if generated(m) || m.name.name.starts_with('$') && !m.name.name.starts_with("$get_") {
            continue;
        }
        if let Some(property) = m.name.name.strip_prefix("$get_") {
            if !private(property) {
                let ret = m.ret.as_ref().map_or(String::new(), |r| format!(": {}", type_text(r)));
                members.push(Entry {
                    kind: "property",
                    title: format!("{name}.{property}"),
                    at: m.name.span.start as usize,
                    signature: format!("{property}{ret}"),
                    doc: None,
                    members: Vec::new(),
                });
            }
        } else if !private(&m.name.name) {
            members.push(function(m, &format!("{name}."), src));
        }
    }
    members.sort_by_key(|m| m.at);
    Entry { kind, title: name.clone(), at: t.name.span.start as usize, signature, doc: None, members }
}

fn interface_entry(i: &InterfaceDecl, src: &str) -> Entry {
    let name = &i.name.name;
    let mut members: Vec<Entry> = i
        .methods
        .iter()
        .filter(|m| !m.default && !private(&m.name.name))
        .map(|m| {
            let ret = m.ret.as_ref().map_or(String::new(), |r| format!(" -> {}", type_text(r)));
            Entry {
                kind: "fn",
                title: format!("{name}.{}", m.name.name),
                at: m.name.span.start as usize,
                signature: format!("fn {}({}){ret}", m.name.name, params_text(&m.params, src)),
                doc: None,
                members: Vec::new(),
            }
        })
        .collect();
    members.extend(i.defaults.iter().filter(|m| !private(&m.name.name)).map(|m| function(m, &format!("{name}."), src)));
    members.sort_by_key(|m| m.at);
    Entry {
        kind: "interface",
        title: name.clone(),
        at: i.name.span.start as usize,
        signature: format!("interface {name}{}", generics(&i.type_params)),
        doc: None,
        members,
    }
}

fn type_text(t: &TypeRef) -> String {
    match &t.kind {
        TypeKind::Named { name, args } if args.is_empty() => name.clone(),
        TypeKind::Named { name, args } if name.starts_with("Tuple") => {
            format!("({})", args.iter().map(type_text).collect::<Vec<_>>().join(", "))
        }
        TypeKind::Named { name, args } => format!("{name}<{}>", args.iter().map(type_text).collect::<Vec<_>>().join(", ")),
        TypeKind::Nullable(inner) if matches!(inner.kind, TypeKind::Fn { .. }) => format!("({})?", type_text(inner)),
        TypeKind::Nullable(inner) => format!("{}?", type_text(inner)),
        TypeKind::Fn { params, ret } => format!("({}) -> {}", params.iter().map(type_text).collect::<Vec<_>>().join(", "), type_text(ret)),
    }
}

// ── the comments ─────────────────────────────────────────────────────────────

/// The words that can stand between a doc comment and the name it describes.
const BEFORE_NAME: [&str; 10] = ["fn", "type", "data", "open", "static", "override", "enum", "interface", "synonym", "syn"];

/// Give the comment that ends at `end` to the declaration right after it.  False if no declaration is.
fn attach(entries: &mut [Entry], src: &str, end: usize, text: &str) -> bool {
    // the first name after the comment, anywhere: a type's members come after the type
    fn locate(entries: &[Entry], end: usize, path: &mut Vec<usize>, best: &mut Option<(usize, Vec<usize>)>) {
        for (i, e) in entries.iter().enumerate() {
            path.push(i);
            if e.at >= end && best.as_ref().is_none_or(|(at, _)| e.at < *at) {
                *best = Some((e.at, path.clone()));
            }
            locate(&e.members, end, path, best);
            path.pop();
        }
    }
    fn at_path<'a>(entries: &'a mut [Entry], path: &[usize]) -> &'a mut Entry {
        let (first, rest) = path.split_first().expect("a path to an entry");
        let e = &mut entries[*first];
        if rest.is_empty() { e } else { at_path(&mut e.members, rest) }
    }
    let mut best = None;
    locate(entries, end, &mut Vec::new(), &mut best);
    let Some((at, path)) = best else { return false };
    // only keywords between the comment and the name; `fn Point.` of an extension method ends in a dot
    let adjacent = src[end..at].split_whitespace().all(|w| BEFORE_NAME.contains(&w) || w.ends_with('.'));
    if adjacent {
        at_path(entries, &path).doc = Some(text.to_string());
    }
    adjacent
}

/// The text of a `/** … */` comment: without the stars that frame it.
fn clean(comment: &str) -> String {
    let inner = comment.strip_prefix("/**").unwrap_or(comment);
    let inner = inner.strip_suffix("*/").unwrap_or(inner);
    let lines: Vec<&str> = inner
        .lines()
        .map(|line| {
            let t = line.trim_start();
            match t.strip_prefix('*') {
                Some(rest) => rest.strip_prefix(' ').unwrap_or(rest),
                None => t,
            }
            .trim_end()
        })
        .collect();
    let first = lines.iter().position(|l| !l.is_empty()).unwrap_or(lines.len());
    let last = lines.iter().rposition(|l| !l.is_empty()).map_or(first, |i| i + 1);
    lines[first..last].join("\n")
}

/// A comment as Markdown: `@param name text` and `@return text` become lists after the description.
fn render_doc(text: &str) -> String {
    let (mut body, mut params, mut returns) = (Vec::new(), Vec::new(), None);
    for line in text.lines() {
        let t = line.trim();
        if let Some(rest) = t.strip_prefix("@param ") {
            let (name, what) = rest.trim().split_once(char::is_whitespace).unwrap_or((rest.trim(), ""));
            params.push(format!("- `{name}`: {}", what.trim()));
        } else if let Some(rest) = t.strip_prefix("@returns ").or_else(|| t.strip_prefix("@return ")) {
            returns = Some(rest.trim().to_string());
        } else {
            body.push(line);
        }
    }
    let mut out = body.join("\n").trim_end().to_string();
    out.push_str("\n\n");
    if !params.is_empty() {
        out.push_str(&format!("**Parameters**\n\n{}\n\n", params.join("\n")));
    }
    if let Some(r) = returns {
        out.push_str(&format!("**Returns** {r}\n\n"));
    }
    if out.trim().is_empty() { String::new() } else { out }
}

fn render(e: &Entry, level: usize, out: &mut String) {
    out.push_str(&format!("{} {} {}\n\n```aion\n{}\n```\n\n", "#".repeat(level.min(6)), e.kind, e.title, e.signature));
    if let Some(d) = &e.doc {
        out.push_str(&render_doc(d));
    }
    for m in &e.members {
        render(m, level + 1, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_comment_loses_its_frame() {
        assert_eq!(clean("/** One line. */"), "One line.");
        assert_eq!(clean("/**\n * First.\n *\n *     code()\n * @param x the x\n */"), "First.\n\n    code()\n@param x the x");
    }

    #[test]
    fn comments_belong_to_the_declaration_that_follows() {
        let src = "/** Top. */\nimport json\n\n/** Adds. @param a x */\nfn add(a: Int) = a\n\n// a plain comment\nfn plain() = 1\n\n/** Not for plain. */\n\n// because of this\nfn other() = 1\n\n/** The Point. */\ndata type Point(x: Int) {\n    /** Its norm. */\n    fn norm() = x\n}\n";
        let docs = document(src, "m").unwrap();
        assert!(docs.starts_with("# m\n\nTop.\n\n## fn add\n"), "{docs}");
        assert!(docs.contains("```aion\nfn add(a: Int)\n```\n\nAdds. @param a x\n"), "{docs}");
        assert!(docs.contains("## fn plain\n\n```aion\nfn plain()\n```\n\n## fn other\n\n```aion\nfn other()\n```\n\n## type Point\n\n```aion\ndata type Point(x: Int)\n```\n\nThe Point.\n"), "{docs}");
        assert!(!docs.contains("Not for plain"), "{docs}");
        assert!(docs.contains("### fn Point.norm\n\n```aion\nfn Point.norm()\n```\n\nIts norm.\n"), "{docs}");
        // none of the made-up members of a data type
        assert!(!docs.contains("equals") && !docs.contains("component1") && !docs.contains("copy"), "{docs}");
    }

    #[test]
    fn private_names_and_empty_files_have_no_documentation() {
        assert_eq!(document("/** Hidden. */\nfn _secret() = 1\nx := 1\n", "m").unwrap(), "");
        assert!(document("fn (", "m").is_err());
    }

    #[test]
    fn tags_become_lists() {
        let docs = render_doc("Adds.\n@param a the first\n@param b the second\n@return the sum");
        assert_eq!(docs, "Adds.\n\n**Parameters**\n\n- `a`: the first\n- `b`: the second\n\n**Returns** the sum\n\n");
    }
}
