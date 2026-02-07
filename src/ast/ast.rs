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
    #[allow(dead_code)]
    pub fn is_user(&self) -> bool {
        !self.is_stdlib() && !self.is_std()
    }
}

/// An expression in function bodies.
#[derive(Debug)]
pub enum Expr {
    /// A string literal: `"hello"`
    StringLiteral(String),

    /// `math.sqrt(144.0)` — call a module function
    ModuleCall {
        module: String,
        func: String,
        args: Vec<Expr>,
    },

    IfExpr {
        condition: Box<Expr>,
        then_branch: Vec<Expr>,
        else_branch: Option<Vec<Expr>>,
    },

    /// A floating-point literal like `3.14`
    FloatLiteral(f64),

    BooleanLiteral(bool),

    /// An integer literal like `42`
    IntLiteral(i64),

    /// Variable definition: `a: Int := 5` or `a: Int` or `a := 5`
    VarDef {
        name: String,
        type_annotation: Option<String>,  // Some("Int") or None
        value: Option<Box<Expr>>,         // Some(expr) or None
    },
    
    VarAssign {
        name: String,
        value: Box<Expr>,
        op: Option<AssignBinOperator>, // Some(op) for `a += 5`, None for `a = 5`
    },
    /// Variable reference: just `a`
    VarRef(String),

    /// Unqualified function call: `println()`, `separator()`
    FuncCall {
        name: String,
        args: Vec<Expr>,
    },

    /// Binary operation
    BinaryOp {
        op: BinOperator, 
        left: Box<Expr>,
        right: Box<Expr>,
    },

    WhileExpr {
        condition: Box<Expr>,
        body: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum AssignBinOperator {
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    DotAssign, // .= method call assignment, e.g. `s .=concat(" world")` → `s = s.concat(" world")`
}

#[derive(Debug)]
pub enum BinOperator {
    Add,
    Sub,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

/// A single function parameter: `name: Type`.
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    /// e.g. `"Int"`, `"Float"`, `"String"`.
    pub type_annotation: String,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Vec<Expr>,
    /// `true` for arrow functions: `fn run() => expr`
    pub is_arrow: bool,
    /// Optional return type annotation: `fn pi() -> Float => 3.14`
    pub return_type: Option<String>,
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
