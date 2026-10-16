//! Source tracking and error reporting.
//!
//! Every compiler stage returns `Result<T, Diagnostic>`; only `main` prints.

use std::fmt::Write as _;

/// A byte range inside one source file.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Span {
    pub file: u32,
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(file: u32, start: usize, end: usize) -> Self {
        Span { file, start: start as u32, end: end as u32 }
    }

    /// The smallest span covering both `self` and `other`.
    pub fn to(self, other: Span) -> Span {
        Span { file: self.file, start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}

pub struct SourceFile {
    pub name: String,
    pub text: String,
}

#[derive(Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn add(&mut self, name: impl Into<String>, text: impl Into<String>) -> u32 {
        self.files.push(SourceFile { name: name.into(), text: text.into() });
        (self.files.len() - 1) as u32
    }

    pub fn file(&self, id: u32) -> &SourceFile {
        &self.files[id as usize]
    }

    pub fn len(&self) -> usize {
        self.files.len()
    }

}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: Option<Span>,
    pub hint: Option<String>,
    /// Reported but not fatal.
    pub warning: bool,
}

pub type Result<T> = std::result::Result<T, Diagnostic>;

impl Diagnostic {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Diagnostic { message: message.into(), span: Some(span), hint: None, warning: false }
    }

    /// Something worth saying that does not stop the build.
    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Diagnostic { warning: true, ..Diagnostic::new(message, span) }
    }

    /// An error that is not tied to a place in the source.
    pub fn general(message: impl Into<String>) -> Self {
        Diagnostic { message: message.into(), span: None, hint: None, warning: false }
    }

    pub fn hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = Some(hint.into());
        self
    }

    /// Render in the familiar `file:line:col` + source excerpt format.
    pub fn render(&self, map: &SourceMap) -> String {
        let mut out = format!("{}: {}\n", if self.warning { "warning" } else { "error" }, self.message);
        if let Some(span) = self.span {
            let file = map.file(span.file);
            let start = (span.start as usize).min(file.text.len());
            let line_start = file.text[..start].rfind('\n').map_or(0, |i| i + 1);
            let line_end = file.text[start..].find('\n').map_or(file.text.len(), |i| start + i);
            let line_no = file.text[..start].matches('\n').count() + 1;
            let col = file.text[line_start..start].chars().count() + 1;
            let width = file.text[start..(span.end as usize).clamp(start, line_end)]
                .chars()
                .count()
                .max(1);
            let gutter = line_no.to_string();
            let pad = " ".repeat(gutter.len());
            let _ = writeln!(out, "{pad}--> {}:{line_no}:{col}", file.name);
            let _ = writeln!(out, "{pad} |");
            let _ = writeln!(out, "{gutter} | {}", &file.text[line_start..line_end]);
            let _ = writeln!(out, "{pad} | {}{}", " ".repeat(col - 1), "^".repeat(width));
        }
        if let Some(hint) = &self.hint {
            let _ = writeln!(out, "hint: {hint}");
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn renders_location_excerpt_and_hint() {
        let mut map = SourceMap::default();
        let f = map.add("demo.aion", "fn main() {\n    x := oops\n}\n");
        let d = Diagnostic::new("undefined variable `oops`", Span::new(f, 21, 25)).hint("declare it first");
        let text = d.render(&map);
        assert!(text.starts_with("error: undefined variable `oops`\n"));
        assert!(text.contains("--> demo.aion:2:10"), "{text}");
        assert!(text.contains("2 |     x := oops"), "{text}");
        assert!(text.contains("|          ^^^^"), "{text}");
        assert!(text.ends_with("hint: declare it first\n"));
    }

    #[test]
    fn errors_without_a_span_render_plainly() {
        let text = Diagnostic::general("no such file").render(&SourceMap::default());
        assert_eq!(text, "error: no such file\n");
    }
}
