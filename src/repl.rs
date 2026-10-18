//! `aion repl`: an interactive prompt.
//!
//! There is no incremental compiler, so every entry is compiled together with the session so far: the
//! imports and declarations you have entered, then the statements, then the new entry.  The earlier
//! statements run again each time, but the program prints a marker before the new entry, and only what
//! comes after it is shown.  A side effect of an earlier statement (a file written, a request sent) happens
//! again; `:reset` starts over.  Compiling and building a small program takes a fraction of a second.

use std::io::{BufRead, IsTerminal, Write};
use std::path::Path;
use std::process::{Command, ExitCode, Stdio};

use crate::diag::SourceMap;
use crate::syntax::{self, Token, ast};
use crate::toolchain;

/// Printed between the replayed statements and the new entry.
const MARK: &str = "@@aion-repl-mark@@";

const HELP: &str = "\
  :help          this text
  :show          the session so far, as a program
  :reset         forget everything entered
  :quit, :q      leave (so does Ctrl-D)

An expression prints its value.  Enter a function or type again to replace it.
Earlier statements run again with every entry (their output is hidden), so a side effect repeats.";

#[derive(Default, Clone)]
struct Session {
    imports: Vec<String>,
    items: Vec<String>,
    stmts: Vec<String>,
}

#[derive(PartialEq, Clone, Copy)]
enum Kind {
    Import,
    Declaration,
    Statement,
}

impl Session {
    /// The whole session as a program, with `entry` (if any) after the marker.
    fn program(&self, entry: &str) -> String {
        let mut out = String::new();
        for part in self.imports.iter().chain(&self.items).chain(&self.stmts) {
            out.push_str(part);
            out.push('\n');
        }
        out.push_str(&format!("println(\"{MARK}\")\n"));
        out.push_str(entry);
        out.push('\n');
        out
    }

    /// The session with `entry` added: a declaration replaces one with the same head.
    fn with(&self, kind: Kind, entry: &str) -> Session {
        let mut next = self.clone();
        match kind {
            Kind::Import => {
                if !next.imports.iter().any(|i| i == entry) {
                    next.imports.push(entry.to_string());
                }
            }
            Kind::Declaration => {
                let head = head_of(entry);
                match next.items.iter().position(|i| head_of(i) == head) {
                    Some(at) => next.items[at] = entry.to_string(),
                    None => next.items.push(entry.to_string()),
                }
            }
            Kind::Statement => next.stmts.push(entry.to_string()),
        }
        next
    }
}

fn kind_of(entry: &str) -> Kind {
    match entry.split_whitespace().next().unwrap_or("") {
        "import" | "from" => Kind::Import,
        "fn" | "type" | "data" | "open" | "enum" | "interface" | "synonym" | "syn" | "typealias" | "extern" | "static" => {
            Kind::Declaration
        }
        _ => Kind::Statement,
    }
}

/// What names a declaration: its first line up to the body (`fn area(r: Float)`, `type Point`), so that
/// entering it again replaces it, while a function of the same name with other parameters is another version.
fn head_of(declaration: &str) -> String {
    let line = declaration.lines().next().unwrap_or("");
    let end = ["{", "=", "->"].iter().filter_map(|m| line.find(m)).min().unwrap_or(line.len());
    line[..end].split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Is `entry` a single expression, whose value can be shown?
fn is_expression(entry: &str) -> bool {
    match syntax::parse_module(0, entry) {
        Ok(module) => {
            module.imports.is_empty()
                && matches!(&module.items[..], [ast::Item::Stmt(s)] if matches!(s.kind, ast::StmtKind::Expr(_)))
        }
        Err(_) => false,
    }
}

/// Does `text` stop in the middle of something (an open bracket or string), so that more lines are expected?
fn is_incomplete(text: &str) -> bool {
    match syntax::lex(0, text, 0) {
        Err(d) => d.message.contains("unterminated"),
        Ok(tokens) => {
            let mut depth = 0i32;
            for t in tokens {
                match t.kind {
                    Token::LParen | Token::LBracket | Token::LBrace => depth += 1,
                    Token::RParen | Token::RBracket | Token::RBrace => depth -= 1,
                    _ => {}
                }
            }
            depth > 0
        }
    }
}

/// Compile `program` (the session file at `file`); the error is ready to print.
fn check(program: &str, file: &Path) -> Result<crate::Compiled, String> {
    std::fs::write(file, program).map_err(|e| format!("error: cannot write {}: {e}", file.display()))?;
    let mut map = SourceMap::default();
    crate::compile(&mut map, file, None, true, true).map_err(|(d, _)| {
        // the position is in the session file, which means nothing to the person typing
        let text = d.render(&map);
        text.lines().filter(|l| !l.trim_start().starts_with("-->")).collect::<Vec<_>>().join("\n")
    })
}

/// Run `entry` in `session`.  Returns the session to keep (the old one when the entry failed).
fn evaluate(session: &Session, entry: &str, file: &Path, opts: &toolchain::Options) -> Session {
    let kind = kind_of(entry);
    if kind != Kind::Statement {
        // nothing to run: compiling is enough to know the declaration is fine
        let next = session.with(kind, entry);
        return match check(&next.program(""), file) {
            Ok(_) => next,
            Err(message) => {
                eprintln!("{message}");
                session.clone()
            }
        };
    }
    // An expression is shown with `println`; one that has no value (a call of a function that returns
    // nothing) does not compile that way, so it is run as written.
    let mut candidates = Vec::new();
    if is_expression(entry) {
        candidates.push(format!("println({})", entry.trim()));
    }
    candidates.push(entry.to_string());

    let mut failure = String::new();
    for candidate in candidates {
        match check(&session.program(&candidate), file) {
            Ok(compiled) => return execute(session, candidate, compiled, opts),
            Err(message) => failure = message,
        }
    }
    eprintln!("{failure}");
    session.clone()
}

fn execute(session: &Session, entry: String, compiled: crate::Compiled, opts: &toolchain::Options) -> Session {
    let (exe, temporary) = match crate::build_program(&compiled, opts) {
        Ok(built) => built,
        Err(d) => {
            eprint!("{}", d.render(&SourceMap::default()));
            return session.clone();
        }
    };
    let output = Command::new(&exe).stdin(Stdio::null()).output();
    if temporary {
        let _ = std::fs::remove_file(&exe);
    }
    let output = match output {
        Ok(o) => o,
        Err(e) => {
            eprintln!("error: could not run the program: {e}");
            return session.clone();
        }
    };
    let stdout = String::from_utf8_lossy(&output.stdout);
    let marker = format!("{MARK}\n");
    let shown = stdout.split_once(&marker).map_or("", |(_, after)| after);
    print!("{shown}");
    if !shown.is_empty() && !shown.ends_with('\n') {
        println!();
    }
    let _ = std::io::stdout().flush();
    // a panic names the session file and a line in it, which mean nothing to the person typing
    let errors = String::from_utf8_lossy(&output.stderr);
    for line in errors.lines().filter(|l| !l.contains("session.aion")) {
        eprintln!("{line}");
    }
    if output.status.success() {
        session.with(Kind::Statement, &entry)
    } else {
        session.clone()
    }
}

/// Where the lines come from: a terminal gets line editing (the arrow keys recall earlier lines); a pipe
/// or a file is read as it is.
enum Input {
    Terminal(rustyline::DefaultEditor),
    Plain(std::io::StdinLock<'static>),
}

enum Line {
    Text(String),
    /// Ctrl-C: drop what has been typed of this entry.
    Interrupted,
    End,
}

impl Input {
    fn open() -> Input {
        if std::io::stdin().is_terminal()
            && std::io::stdout().is_terminal()
            && let Ok(editor) = rustyline::DefaultEditor::new()
        {
            return Input::Terminal(editor);
        }
        Input::Plain(std::io::stdin().lock())
    }

    fn line(&mut self, prompt: &str) -> Line {
        match self {
            Input::Terminal(editor) => match editor.readline(prompt) {
                Ok(text) => {
                    if !text.trim().is_empty() {
                        let _ = editor.add_history_entry(text.as_str());
                    }
                    Line::Text(text)
                }
                Err(rustyline::error::ReadlineError::Interrupted) => Line::Interrupted,
                Err(_) => Line::End,
            },
            Input::Plain(input) => {
                print!("{prompt}");
                let _ = std::io::stdout().flush();
                let mut text = String::new();
                match input.read_line(&mut text) {
                    Ok(0) | Err(_) => Line::End,
                    Ok(_) => Line::Text(text.trim_end_matches(['\n', '\r']).to_string()),
                }
            }
        }
    }
}

/// Read one entry: lines until the brackets and strings are closed.  `None` at the end of the input.
fn read_entry(input: &mut Input) -> Option<String> {
    let mut entry = String::new();
    loop {
        let line = match input.line(if entry.is_empty() { ">>> " } else { "... " }) {
            Line::Text(line) => line,
            Line::Interrupted => {
                entry.clear();
                continue;
            }
            Line::End => return if entry.trim().is_empty() { None } else { Some(entry) },
        };
        let line = line.as_str();
        if entry.is_empty() && line.trim().is_empty() {
            continue;
        }
        // a blank line ends an entry that is still open, so a typo cannot trap you
        if !entry.is_empty() && line.trim().is_empty() {
            return Some(entry);
        }
        if !entry.is_empty() {
            entry.push('\n');
        }
        entry.push_str(line);
        if !is_incomplete(&entry) {
            return Some(entry);
        }
    }
}

pub fn run(opts: &toolchain::Options) -> ExitCode {
    let mut opts = toolchain::Options { opt_level: opts.opt_level.clone(), keep_temps: false, trace: opts.trace };
    // Builds are quicker without optimisation, and a prompt is about quick answers.
    if opts.opt_level == "2" {
        opts.opt_level = "0".into();
    }
    let dir = std::env::temp_dir().join(format!("aion-repl-{}", std::process::id()));
    if let Err(e) = std::fs::create_dir_all(&dir) {
        eprintln!("error: cannot create {}: {e}", dir.display());
        return ExitCode::FAILURE;
    }
    let file = dir.join("session.aion");
    println!("Aion {} — :help for commands, Ctrl-D to leave", env!("CARGO_PKG_VERSION"));

    let mut session = Session::default();
    let mut input = Input::open();
    while let Some(entry) = read_entry(&mut input) {
        match entry.trim() {
            ":quit" | ":q" | ":exit" => break,
            ":help" => println!("{HELP}"),
            ":reset" => {
                session = Session::default();
                println!("session cleared");
            }
            ":show" => print!("{}", session.program("").replace(&format!("println(\"{MARK}\")\n"), "")),
            command if command.starts_with(':') => eprintln!("unknown command `{command}`; type `:help`"),
            _ => session = evaluate(&session, &entry, &file, &opts),
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    ExitCode::SUCCESS
}
