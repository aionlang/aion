/// An import declaration.
///
/// Full module:   `import aion.math;`       → path=["aion","math"],  symbol=None
/// Selective:     `import aion.math.sqrt;`  → path=["aion","math"],  symbol=Some("sqrt")
#[derive(Debug, Clone)]
pub struct Import {
    /// The dot-separated module path, e.g. `["aion", "math"]`.
    pub path: Vec<String>,
    /// An optional specific symbol imported from that module.
    /// When present, the symbol can be used unqualified.
    pub symbol: Option<String>,
}

impl Import {
    /// Return the short module name (last segment of the *module* path), e.g. `"math"`.
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

    /// Returns `true` when this import targets a specific symbol
    /// rather than the whole module.
    pub fn is_selective(&self) -> bool {
        self.symbol.is_some()
    }
}

/// An expression in function bodies.
#[derive(Debug)]
pub enum Expr {
    /// A string literal: `"hello"`
    StringLiteral(String),
    
    /// A string interpolation: `"Hello, ${name}!"`
    /// Parts can be either literal strings or expressions.
    InterpolatedString {
        parts: Vec<StringInterpolationPart>,
    },

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

    /// Field access: `a.name`
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },

    /// Return statement: `return expr` or `return`
    ReturnExpr {
        value: Option<Box<Expr>>,
    },

    /// Field assignment: `it.name = value`
    FieldAssign {
        object: Box<Expr>,
        field: String,
        value: Box<Expr>,
    },

    /// Chained method call: `expr.method(args…)`
    ///
    /// Unlike `ModuleCall` (where the receiver is a bare identifier),
    /// the receiver here is an arbitrary expression, e.g.
    /// `it.stream.send(data)`.
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum StringInterpolationPart {
    Literal(String),
    Expr(Box<Expr>),
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

/// A field inside a type definition: `name: String?`
#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: String,
    /// The base type name, e.g. `"String"`, `"Int"`.
    pub type_name: String,
    /// `true` when declared with `?`, e.g. `String?`.
    pub nullable: bool,
}

/// A user-defined type (struct):
///
/// ```aion
/// type Animal(it.name, it.age) {
///     name: String?
///     age: Int?
/// }
/// ```
#[derive(Debug)]
pub struct TypeDef {
    pub name: String,
    /// Optional parent type for inheritance-like composition.
    pub parent: Option<String>,
    /// The field names the constructor accepts, in order.
    pub constructor_params: Vec<String>,
    /// All field definitions.
    pub fields: Vec<FieldDef>,
    /// Methods defined inside the type body.
    pub methods: Vec<Function>,
    /// Optional explicit constructor.
    pub constructor: Option<ConstructorDef>,
}

/// An explicit constructor for a user-defined type:
///
/// ```aion
/// constructor(name: String, age: Int) {
///     it.name = name
///     it.age = age
/// }
/// ```
#[derive(Debug)]
pub struct ConstructorDef {
    pub params: Vec<Param>,
    pub body: Vec<Expr>,
}

/// Distinguishes regular, extern, and abstract functions.
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionKind {
    /// `fn foo() { … }` — has a body.
    Regular,
    /// `extern fn foo()` — C-backed, no body.
    Extern,
    /// `abstract fn foo()` — must be overridden, no body.
    Abstract,
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
    /// Whether this is a regular, extern, or abstract function.
    pub kind: FunctionKind,
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
    /// User-defined types (`type Animal { … }`).
    pub type_defs: Vec<TypeDef>,
    /// Methods defined outside the type body: `fn Animal::speak() { … }`.
    pub impl_methods: Vec<(String, Function)>,
    /// User-written modules resolved from non-stdlib imports.
    pub user_modules: Vec<UserModule>,
}
