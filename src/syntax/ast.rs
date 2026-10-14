//! Untyped syntax tree produced by the parser.

use crate::diag::Span;

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

/// A type written in source: `Int`, `String`, `Animal`.
#[derive(Debug, Clone)]
pub struct TypeRef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub items: Vec<Item>,
}

/// `import aion.math;` / `import aion.sockets.TcpListener;` / `import utils;`
#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Item {
    Fn(FnDecl),
    Type(TypeDecl),
    /// `fn Animal::speak() { ... }` — a method declared outside its type.
    Method { owner: Ident, decl: FnDecl },
}

#[derive(Debug)]
pub struct Param {
    pub name: Ident,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret: Option<TypeRef>,
    pub body: FnBody,
    pub span: Span,
}

#[derive(Debug)]
pub enum FnBody {
    Block(Vec<Stmt>),
    /// `fn f() => expr`
    Arrow(Expr),
    /// `extern fn f(...) -> T` or `... = "c_symbol"`; implemented in C.
    Extern { symbol: Option<String> },
}

#[derive(Debug)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: TypeRef,
}

#[derive(Debug)]
pub struct CtorDecl {
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct TypeDecl {
    pub name: Ident,
    /// `type Animal(it.name, it.age) { ... }` shorthand constructor.
    pub ctor_fields: Option<Vec<Ident>>,
    pub fields: Vec<FieldDecl>,
    pub ctor: Option<CtorDecl>,
    pub methods: Vec<FnDecl>,
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
    /// `a = 1`, `it.age += 1`
    Assign { target: Expr, op: AssignOp, value: Expr },
    Expr(Expr),
    If { cond: Expr, then: Vec<Stmt>, otherwise: Option<Vec<Stmt>> },
    While { cond: Expr, body: Vec<Stmt> },
    Return(Option<Expr>),
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

#[derive(Debug)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    /// `"hi ${name}!"`
    Interp(Vec<InterpPart>),
    Name(String),
    /// `obj.field` — also `module.name` before name resolution.
    Field(Box<Expr>, Ident),
    Call { callee: Box<Expr>, args: Vec<Expr> },
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
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
