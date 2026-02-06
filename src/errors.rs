//! Aion error reporting — pretty, coloured diagnostics.

use std::fmt;

/// The phase of compilation where an error occurred.
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Phase {
    Lexer,
    Parser,
    Compiler,
    Linker,
}

impl fmt::Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Phase::Lexer    => write!(f, "lex"),
            Phase::Parser   => write!(f, "parse"),
            Phase::Compiler => write!(f, "compile"),
            Phase::Linker   => write!(f, "link"),
        }
    }
}

/// A structured compiler error.
#[derive(Debug, Clone)]
pub struct AionError {
    pub phase: Phase,
    pub message: String,
    pub hint: Option<String>,
}

impl fmt::Display for AionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.phase, self.message)?;
        if let Some(hint) = &self.hint {
            write!(f, "\n  hint: {hint}")?;
        }
        Ok(())
    }
}

impl std::error::Error for AionError {}

/// Print an error to stderr with red colouring (ANSI).
pub fn report(err: &AionError) {
    // Red bold: \x1b[1;31m   Reset: \x1b[0m
    eprintln!(
        "\x1b[1;31merror\x1b[0m\x1b[1m[{}]:\x1b[0m {}",
        err.phase, err.message,
    );
    if let Some(hint) = &err.hint {
        eprintln!("  \x1b[1;36mhint:\x1b[0m {hint}");
    }
}

/// Shorthand — build an error, print it red, and exit.
pub fn fatal(phase: Phase, message: impl Into<String>) -> ! {
    fatal_with_hint(phase, message, None)
}

/// Shorthand — build an error with a hint, print it red, and exit.
pub fn fatal_with_hint(phase: Phase, message: impl Into<String>, hint: Option<String>) -> ! {
    let err = AionError {
        phase,
        message: message.into(),
        hint,
    };
    report(&err);
    std::process::exit(1);
}

// ═══════════════════════════════════════════════════════════════════
// Warnings (yellow)
// ═══════════════════════════════════════════════════════════════════

/// Print a yellow warning to stderr.
pub fn warn(phase: Phase, message: impl Into<String>) {
    eprintln!(
        "\x1b[1;33mwarning\x1b[0m\x1b[1m[{}]:\x1b[0m {}",
        phase,
        message.into(),
    );
}

/// Print a yellow warning with a hint to stderr.
#[allow(dead_code)]
pub fn warn_with_hint(phase: Phase, message: impl Into<String>, hint: impl Into<String>) {
    eprintln!(
        "\x1b[1;33mwarning\x1b[0m\x1b[1m[{}]:\x1b[0m {}",
        phase,
        message.into(),
    );
    eprintln!("  \x1b[1;36mhint:\x1b[0m {}", hint.into());
}

// ═══════════════════════════════════════════════════════════════════
// Info messages (cyan [aion] tag)
// ═══════════════════════════════════════════════════════════════════

/// Print a status/info message with a coloured `[aion]` prefix.
pub fn info(message: impl std::fmt::Display) {
    eprintln!("\x1b[1;34m[aion]\x1b[0m {message}");
}

/// Print a success message in green.
pub fn success(message: impl std::fmt::Display) {
    eprintln!("\x1b[1;32m[aion]\x1b[0m {message}");
}
