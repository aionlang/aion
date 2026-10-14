//! Typed, name-resolved program representation.
//!
//! Everything the code generator needs and nothing it has to guess:
//! every expression carries its type, calls point at function ids, and
//! implicit conversions (`Int` → `Float`, value → `String`) are explicit nodes.

pub type FnId = usize;
pub type TypeId = usize;
pub type LocalId = usize;
pub type GlobalId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Int,
    Float,
    Bool,
    Str,
    /// A reference to an instance of a user-defined `type`.
    Obj(TypeId),
    Void,
}

impl Ty {
    /// Values of this type live on the garbage-collected heap.
    pub fn is_gc(self) -> bool {
        matches!(self, Ty::Str | Ty::Obj(_))
    }
}

pub struct Program {
    pub types: Vec<TypeDef>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
    /// Module initialisers (top-level statements), in dependency order.
    pub inits: Vec<FnId>,
    /// The entry module's `fn main()`, run after all initialisers.
    pub main: Option<FnId>,
}

/// A module-level variable.
pub struct Global {
    pub name: String,
    pub symbol: String,
    pub ty: Ty,
}

pub struct TypeDef {
    pub name: String,
    pub fields: Vec<Field>,
}

pub struct Field {
    pub name: String,
    pub ty: Ty,
}

pub struct Local {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FuncKind {
    Free,
    /// Takes the receiver (`it`) as its first parameter.
    Method,
    Constructor,
}

pub struct Func {
    /// Linker-level name (for externs, the C symbol).
    pub symbol: String,
    pub kind: FuncKind,
    pub ret: Ty,
    /// The first `n_params` locals are the parameters.
    pub n_params: usize,
    pub locals: Vec<Local>,
    /// `None` for `extern fn`.
    pub body: Option<Vec<Stmt>>,
}

impl Func {
    pub fn params(&self) -> &[Local] {
        &self.locals[..self.n_params]
    }
}

pub enum Stmt {
    /// Declare a local; `None` means "zero value of its type".
    Let(LocalId, Option<Expr>),
    Set(LocalId, Expr),
    SetGlobal(GlobalId, Expr),
    SetField { obj: Expr, index: usize, value: Expr },
    Expr(Expr),
    /// `step` runs after the body and is where `continue` jumps to.
    While { cond: Expr, body: Vec<Stmt>, step: Vec<Stmt> },
    Break,
    Continue,
    Return(Option<Expr>),
}

pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Local(LocalId),
    Global(GlobalId),
    /// The null reference.
    Null,
    /// Nothing (the value of a block that ends in a statement).
    Unit,
    Field { obj: Box<Expr>, index: usize },
    Call { func: FnId, args: Vec<Expr> },
    /// Allocate a zeroed instance (string fields start as `""`).
    New(TypeId),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    /// Both operands have the same type; `Str + Str` is concatenation.
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// Short-circuiting `&&` / `||`.
    Logical { and: bool, lhs: Box<Expr>, rhs: Box<Expr> },
    IntToFloat(Box<Expr>),
    /// Run the statements, then yield the value of the last expression.
    Block(Vec<Stmt>, Box<Expr>),
    /// `if`/`else`; both branches have the expression's type (or it is `Void`).
    Cond { cond: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr> },
    /// Render an Int/Float/Bool as a String.
    ToStr(Box<Expr>),
    /// Prints the arguments separated by spaces (each Int, Float, Bool or String).
    Print { newline: bool, args: Vec<Expr> },
}
