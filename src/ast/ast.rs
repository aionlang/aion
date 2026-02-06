/// An import declaration: `import aion.math;`
#[derive(Debug, Clone)]
pub struct Import {
    /// The dot-separated path segments, e.g. `["aion", "math"]`.
    pub path: Vec<String>,
}

impl Import {
    /// Return the short module name (last segment), e.g. `"math"`.
    pub fn module_name(&self) -> &str {
        self.path.last().expect("empty import path")
    }

    /// Returns `true` for C-backed imports (`import aion.math;`).
    pub fn is_stdlib(&self) -> bool {
        self.path.first().map(|s| s.as_str()) == Some("aion")
    }

    /// Returns `true` for embedded Aion stdlib (`import std.io;`).
    pub fn is_std(&self) -> bool {
        self.path.first().map(|s| s.as_str()) == Some("std")
    }

    /// Returns `true` for user-local imports (`import utils;`).
    pub fn is_user(&self) -> bool {
        !self.is_stdlib() && !self.is_std()
    }
}

/// An expression in function bodies.
#[derive(Debug)]
pub enum Expr {
    /// `print("...")`  — print a string literal
    PrintStr(String),

    /// `print(<expr>)` — print the result of an expression (float)
    PrintExpr(Box<Expr>),

    /// `math.sqrt(144.0)` — call a module function
    ModuleCall {
        module: String,
        func: String,
        args: Vec<Expr>,
    },

    /// A floating-point literal like `3.14`
    FloatLiteral(f64),

    /// An integer literal like `42`
    IntLiteral(i64),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Vec<Expr>,
}

/// A user-written Aion module loaded via `import utils;`.
///
/// Contains the functions defined in the external `.aion` file.
#[derive(Debug)]
pub struct UserModule {
    pub name: String,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Program {
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    /// User-written modules resolved from non-stdlib imports.
    pub user_modules: Vec<UserModule>,
}