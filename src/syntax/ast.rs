//! Untyped syntax tree produced by the parser.

use crate::diag::Span;

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

/// A type written in source: `Int`, `List<Int>`, `(Int, String) -> Bool`.
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    /// `Int`, `Animal`, `List<Int>`, or a type parameter such as `T`.
    Named { name: String, args: Vec<TypeRef> },
    /// `(A, B) -> R`
    Fn { params: Vec<TypeRef>, ret: Box<TypeRef> },
    /// `T?`: a `T` or `null`.
    Nullable(Box<TypeRef>),
}

#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub items: Vec<Item>,
}

/// `import math`, `import utils as u`, `from sockets import TcpListener, TcpStream`
#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<Ident>,
    /// `from <path> import a, b` — the names to bring in (the module itself isn't bound).
    pub names: Option<Vec<Ident>>,
    /// `import <path> as alias`
    pub alias: Option<Ident>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Item {
    Fn(FnDecl),
    Type(TypeDecl),
    Interface(InterfaceDecl),
    /// `fn Animal.speak() { ... }` — a method declared outside its type.
    /// For a generic type, `fn List<T>.first()` names the type's parameters.
    Method { owner: Ident, owner_params: Vec<Ident>, decl: FnDecl },
    /// A top-level statement: runs when the module is loaded.
    Stmt(Stmt),
}

#[derive(Debug)]
pub struct Param {
    pub name: Ident,
    pub ty: TypeRef,
    /// `x: Int = 0`
    pub default: Option<Expr>,
    /// `vararg xs: Int`: takes any number of arguments; `xs` is a `List<Int>` inside.  `ty` is the element type.
    pub vararg: bool,
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: Ident,
    /// `fn map<T, R>(…)`
    pub type_params: Vec<Ident>,
    /// `fn show<T: Shape>(…)`: one entry per type parameter.
    pub bounds: Vec<Option<TypeRef>>,
    pub params: Vec<Param>,
    pub ret: Option<TypeRef>,
    pub body: FnBody,
    pub span: Span,
    /// `open fn` / `override fn` inside a type.
    pub modifier: Modifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Modifier {
    None,
    /// May be overridden by subtypes.
    Open,
    /// Replaces a method of the parent type.
    Override,
}

#[derive(Debug)]
pub enum FnBody {
    Block(Vec<Stmt>),
    /// `fn f() = expr`
    Expr(Expr),
    /// `extern fn f(...) -> T` or `... = "c_symbol"`; implemented in C.
    Extern { symbol: Option<String> },
}

#[derive(Debug)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: TypeRef,
    /// `count: Int = 0`
    pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct CtorDecl {
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct TypeDecl {
    pub name: Ident,
    /// `type Box<T>(…)`
    pub type_params: Vec<Ident>,
    pub bounds: Vec<Option<TypeRef>>,
    /// `type User(name: String, age: Int = 0)` — each parameter is also a field.
    pub primary: Option<Vec<Param>>,
    pub fields: Vec<FieldDecl>,
    /// An explicit `constructor(...) { ... }` (only without a primary one).
    pub ctor: Option<CtorDecl>,
    /// `init { ... }` blocks, run after the fields are set up.
    pub inits: Vec<Vec<Stmt>>,
    pub methods: Vec<FnDecl>,
    /// `open type Animal(…)`: other types may extend it.
    pub open: bool,
    /// `type Dog(…) : Animal(name)`
    pub parent: Option<ParentRef>,
}

/// The `: Animal(name)` of a type that extends another.
#[derive(Debug)]
pub struct ParentRef {
    pub name: Ident,
    /// Arguments for the parent's constructor, evaluated with the child's parameters in scope.
    pub args: Vec<Arg>,
    pub span: Span,
}

/// `interface Shape { fn area() -> Float }`: the methods a type must have to be used as a `Shape`.
#[derive(Debug)]
pub struct InterfaceDecl {
    pub name: Ident,
    /// `interface Comparable<T> { … }`
    pub type_params: Vec<Ident>,
    pub methods: Vec<MethodSig>,
}

/// `fn area() -> Float` — a signature without a body.
#[derive(Debug)]
pub struct MethodSig {
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret: Option<TypeRef>,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    /// `a := 1`, `a: Int := 1`, `a: Int`
    Let { name: Ident, ty: Option<TypeRef>, init: Option<Expr> },
    /// `a = 1`, `this.age += 1`
    Assign { target: Expr, op: AssignOp, value: Expr },
    Expr(Expr),
    While { cond: Expr, body: Vec<Stmt> },
    /// `for i in 0..10 { ... }`
    For { var: Ident, iter: Expr, body: Vec<Stmt> },
    Break,
    Continue,
    Return(Option<Expr>),
    /// `defer cleanup()` / `defer { … }`: runs when the enclosing block is left.
    Defer(Vec<Stmt>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Set,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// A call argument, optionally named: `f(1, flag: true)`.
#[derive(Debug)]
pub struct Arg {
    pub name: Option<Ident>,
    pub value: Expr,
}

#[derive(Debug)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    /// `'a'`: a Unicode code point.
    Char(u32),
    /// `"hi ${name} and $other"`
    Interp(Vec<InterpPart>),
    Name(String),
    /// `obj.field` — also `module.name` before name resolution.
    Field(Box<Expr>, Ident),
    /// `obj?.field`: null if `obj` is null.  With a call, `obj?.method(…)`.
    SafeField(Box<Expr>, Ident),
    /// `expr!!`: the value, or a panic if it is null.
    NotNull(Box<Expr>),
    /// `a ?: b`: `a` unless it is null.
    Elvis(Box<Expr>, Box<Expr>),
    /// The `null` literal.
    Null,
    /// `x is Dog` / `x !is Dog`
    Is { value: Box<Expr>, ty: TypeRef, negated: bool },
    /// `x as Dog` (fails if it is not one) / `x as? Dog` (null if it is not one)
    Cast { value: Box<Expr>, ty: TypeRef, safe: bool },
    /// `try { … } catch e { … }`: the value of the block, or of the handler if something panicked.
    Try { body: Vec<Stmt>, catch_name: Option<Ident>, handler: Vec<Stmt> },
    /// `f(x)`, `List<Int>()`: explicit type arguments are optional.
    Call { callee: Box<Expr>, type_args: Vec<TypeRef>, args: Vec<Arg> },
    /// `xs[i]`
    Index { obj: Box<Expr>, index: Box<Expr> },
    /// `xs[1..3]`, `xs[2..]`, `xs[..=4]`, `xs[..]`: a slice, either end optional.
    Slice { obj: Box<Expr>, start: Option<Box<Expr>>, end: Option<Box<Expr>>, inclusive: bool },
    /// `[1, 2, 3]`
    List(Vec<Expr>),
    /// `*xs` in a call or a list: the elements of a list, one by one.
    Spread(Box<Expr>),
    /// `return v`, `break`, `continue` where a value is expected, as in `x ?: return -1`.
    Return(Option<Box<Expr>>),
    Break,
    Continue,
    /// `{"a": 1, "b": 2}`
    Map(Vec<(Expr, Expr)>),
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// `if c { a } else { b }`; the value of a branch is its last expression.
    If { cond: Box<Expr>, then: Vec<Stmt>, otherwise: Option<Vec<Stmt>> },
    /// A sequence of statements whose value is the last expression
    /// (produced by desugaring `when`).
    Block(Vec<Stmt>),
    /// `{ x, y: Int -> body }`; the value of the body is its last expression.
    /// Without `->` and parameters, a one-parameter lambda gets `it`.
    Lambda { params: Vec<LambdaParam>, explicit: bool, body: Vec<Stmt> },
    /// `a..b` (exclusive) or `a..=b` (inclusive).
    Range { start: Box<Expr>, end: Box<Expr>, inclusive: bool },
}

#[derive(Debug)]
pub struct LambdaParam {
    pub name: Ident,
    pub ty: Option<TypeRef>,
}

#[derive(Debug)]
pub enum InterpPart {
    Lit(String),
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    And,
    Or,
}

impl BinOp {
    pub fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Rem => "%",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
        }
    }
}
