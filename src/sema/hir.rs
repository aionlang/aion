//! Typed, name-resolved program representation.
//!
//! Everything the code generator needs and nothing it has to guess:
//! every expression carries its type, calls point at function ids, and
//! implicit conversions (`Int` → `Float`, value → `String`) are explicit nodes.

pub type FnId = usize;
pub type TypeId = usize;
pub type LocalId = usize;
pub type GlobalId = usize;
pub type FnTyId = usize;
pub type OptId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    Int,
    Float,
    Bool,
    /// A Unicode code point, held like an `Int`.
    Char,
    Str,
    /// A run of raw bytes, held like a `String` but never null and not NUL-terminated.
    Bytes,
    /// A reference to an instance of a user-defined `type`.
    Obj(TypeId),
    /// A function value (a closure), identified by its interned signature.
    Fn(FnTyId),
    /// `T?`: a `T` or null, identified by its interned inner type.  A pointer
    /// type's optional is the same pointer (null = absent); `Int?`, `Float?` and
    /// `Bool?` point to a one-slot box.
    Opt(OptId),
    /// The type of the `null` literal; it converts to any `T?` and is never stored.
    Null,
    Void,
}

impl Ty {
    /// Values of this type live on the garbage-collected heap.
    pub fn is_gc(self) -> bool {
        matches!(self, Ty::Str | Ty::Bytes | Ty::Obj(_) | Ty::Fn(_) | Ty::Opt(_))
    }
}

/// The signature of a function type such as `(Int, String) -> Bool`.
#[derive(Clone, PartialEq, Eq)]
pub struct FnSig {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

pub struct Program {
    pub types: Vec<TypeDef>,
    /// Dispatch tables for calls through interface-typed values.
    pub dispatch: Vec<Dispatch>,
    /// For each type tested with `is` / `as`: the types whose objects pass.
    pub instance_checks: Vec<InstanceCheck>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
    /// Module initialisers (top-level statements), in dependency order.
    pub inits: Vec<FnId>,
    /// The entry module's `fn main()`, run after all initialisers.
    pub main: Option<FnId>,
}

/// How one method of an interface is found at run time: for each concrete type
/// that is used as the interface, the function that implements it.
pub struct Dispatch {
    pub iface: TypeId,
    /// Index into the interface's methods; one past the end means `toString`.
    pub method: usize,
    pub entries: Vec<(TypeId, FnId)>,
}

/// The objects that count as `target` for `is` and `as`: its own and its subtypes'.
pub struct InstanceCheck {
    pub target: TypeId,
    pub members: Vec<TypeId>,
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

#[derive(Clone)]
pub struct Field {
    pub name: String,
    pub ty: Ty,
}

pub struct Local {
    pub name: String,
    pub ty: Ty,
    /// Captured by a closure *and* reassigned somewhere, so it lives in a
    /// shared heap cell instead of directly in the function's frame.
    pub boxed: bool,
}

/// A variable a closure takes from the function that created it.
pub struct Capture {
    /// The variable in the creating function.
    pub outer: LocalId,
    /// The closure's own local standing for it.
    pub inner: LocalId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FuncKind {
    Free,
    /// Takes the receiver (`it`) as its first parameter.
    Method,
    Constructor,
    /// Called through a function value; receives its closure object as a hidden first argument.
    Closure,
    /// A method of a generic instance that nothing calls; it was never checked and has no code.
    Unused,
}

/// What a stack trace shows for a function of the user's own code.
#[derive(Clone)]
pub struct Trace {
    /// `describe`, `Counter.inc`, `<lambda>`, `<top level>`.
    pub name: String,
    /// The source file as errors show it.
    pub file: String,
    /// The line the function starts on.
    pub line: u32,
}

pub struct Func {
    /// `Some` for functions that keep a frame in the stack trace.
    pub trace: Option<Trace>,
    /// Linker-level name (for externs, the C symbol).
    pub symbol: String,
    pub kind: FuncKind,
    pub ret: Ty,
    /// The first `n_params` locals are the parameters.
    pub n_params: usize,
    pub locals: Vec<Local>,
    /// `None` for `extern fn`.
    pub body: Option<Vec<Stmt>>,
    /// Closures only: the variables copied out of the closure object on entry.
    pub captures: Vec<Capture>,
}

impl Func {
    pub fn params(&self) -> &[Local] {
        &self.locals[..self.n_params]
    }
}

pub enum Stmt {
    /// The following statements come from this source line (for stack traces; only in traced functions).
    Line(u32),
    /// Declare a local; `None` means "zero value of its type".
    Let(LocalId, Option<Expr>),
    Set(LocalId, Expr),
    SetGlobal(GlobalId, Expr),
    SetField { obj: Expr, index: usize, value: Expr },
    /// `array[index] = value`, with a bounds check.
    SetElem { array: Expr, index: Expr, value: Expr },
    Expr(Expr),
    /// `step` runs after the body and is where `continue` jumps to.
    While { cond: Expr, body: Vec<Stmt>, step: Vec<Stmt> },
    Break,
    Continue,
    /// `break@label` / `continue@label`: `levels` loops outward from the innermost (0 is the same as a plain one).
    JumpOut { levels: usize, is_break: bool },
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
    // Int only
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    /// Arithmetic shift right (keeps the sign).
    Shr,
    /// Logical shift right (fills with zeros).
    UShr,
}

pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    /// The zero-length `Bytes` value (the default of a `Bytes` variable).
    EmptyBytes,
    Local(LocalId),
    Global(GlobalId),
    /// The null reference.
    Null,
    /// Nothing (the value of a block that ends in a statement).
    Unit,
    Field { obj: Box<Expr>, index: usize },
    Call { func: FnId, args: Vec<Expr> },
    /// Call a function value.
    CallValue { callee: Box<Expr>, args: Vec<Expr> },
    /// Create a closure of `func`, capturing the variables listed in its `captures`.
    Closure { func: FnId },
    /// Allocate a zeroed instance (string fields start as `""`).
    New(TypeId),
    /// Is the object (not null) an instance of `target` or of a type that extends it?
    IsInstance { value: Box<Expr>, target: TypeId },
    /// `value as target` (panics if it is not one) or `value as? target` (null if it is not one).
    Downcast { value: Box<Expr>, target: TypeId, safe: bool },
    /// A value of a type that implements an interface, used as that interface
    /// (the same pointer; the object header says which type it really is).
    Upcast(Box<Expr>),
    /// Call a method of an interface through the object's real type; `args[0]` is the receiver.
    CallIface { iface: TypeId, method: usize, args: Vec<Expr> },
    /// `T` → `T?` (boxes an Int, Float or Bool).
    Wrap(Box<Expr>),
    /// `T?` → `T`; panics if it is null.
    Unwrap(Box<Expr>),
    /// Is the optional null?
    IsNull(Box<Expr>),
    /// A new `Array<elem>` of `len` slots, each set to `fill`.
    ArrayNew { elem: Ty, len: Box<Expr>, fill: Box<Expr> },
    /// `array[index]`, with a bounds check.
    Elem { array: Box<Expr>, index: Box<Expr> },
    ArrayLen(Box<Expr>),
    /// `hashOf(value)`: a non-negative hash of an Int, Float, Bool, String or object identity.
    Hash(Box<Expr>),
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
