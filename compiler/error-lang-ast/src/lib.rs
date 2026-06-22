// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! Abstract Syntax Tree definitions for Error-Lang.
//!
//! Error-Lang is an educational programming language where errors are features,
//! not failures. This crate defines the canonical Rust AST used by the WASM
//! backend, LSP server, static analysers, and the TypeLL bridge.
//!
//! # Core concepts
//!
//! - **Stability scores**: Every expression carries an optional stability
//!   annotation (0-100) that tracks how "stable" the code is. Mutations, type
//!   instability, null propagation, unhandled errors, and other factors lower
//!   the score.
//!
//! - **Gutter blocks**: Error recovery regions (`gutter ... end`) where errors
//!   are intentionally injected. The parser enters error-tolerant mode inside a
//!   gutter and recovers after `end`. Each gutter decreases stability.
//!
//! - **Ternary expressions**: Three-way branching (`cond ? ok_branch : err_branch`)
//!   as well as match-style three-valued logic over Ok/Err/Pending states.
//!
//! - **Stability factors**: Structured annotations on declarations describing
//!   *why* stability is what it is (mutation count, IO, exception paths, etc.).
//!
//! # Serde support
//!
//! Enable the `serde` feature to derive `Serialize` and `Deserialize` on all
//! AST types. This is used for the JSON bridge between the ReScript frontend
//! and Rust backends.
//!
//! ```toml
//! error-lang-ast = { path = "../error-lang-ast", features = ["serde"] }
//! ```

// ============================================================================
// Span
// ============================================================================

/// A byte-offset span in source text.
///
/// `start` is inclusive, `end` is exclusive — the same convention used by
/// `std::ops::Range<u32>`.  A span where `start == end` is zero-width (e.g.
/// inserted by error recovery).
#![forbid(unsafe_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Span {
    /// Inclusive start byte offset.
    pub start: u32,
    /// Exclusive end byte offset.
    pub end: u32,
}

impl Span {
    /// Create a new span from byte offsets.
    #[inline]
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    /// A synthetic span with no source location (e.g. compiler-generated nodes).
    #[inline]
    pub fn synthetic() -> Self {
        Self {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    /// Returns `true` if this span was produced by [`Span::synthetic`].
    #[inline]
    pub fn is_synthetic(&self) -> bool {
        self.start == u32::MAX && self.end == u32::MAX
    }

    /// Merge two spans into one that covers both.
    #[inline]
    pub fn merge(self, other: Self) -> Self {
        if self.is_synthetic() {
            return other;
        }
        if other.is_synthetic() {
            return self;
        }
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Byte length of the span.
    #[inline]
    pub fn len(&self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    /// Whether the span is empty (zero-width).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

// ============================================================================
// Identifiers
// ============================================================================

/// A source identifier with its span.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Ident {
    /// The identifier text.
    pub name: String,
    /// Where this identifier appears in source.
    pub span: Span,
}

// ============================================================================
// Type Expressions
// ============================================================================

/// A type expression in Error-Lang source.
///
/// Error-Lang has a small set of built-in types plus named user types (structs).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TypeExpr {
    /// The `Int` type.
    Int(Span),
    /// The `Float` type.
    Float(Span),
    /// The `String` type.
    Str(Span),
    /// The `Bool` type.
    Bool(Span),
    /// `Array<T>` — a homogeneous dynamic array.
    Array {
        elem: Box<TypeExpr>,
        span: Span,
    },
    /// A user-defined type name (struct, alias, etc.).
    Named {
        name: Ident,
        span: Span,
    },
    /// A function type: `(params) -> return_type`.
    Function {
        params: Vec<TypeExpr>,
        ret: Box<TypeExpr>,
        span: Span,
    },
    /// An error-recovery placeholder type inserted by the parser.
    Error(Span),
}

impl TypeExpr {
    /// Return the span of this type expression.
    pub fn span(&self) -> Span {
        match self {
            Self::Int(s)
            | Self::Float(s)
            | Self::Str(s)
            | Self::Bool(s)
            | Self::Error(s) => *s,
            Self::Array { span, .. }
            | Self::Named { span, .. }
            | Self::Function { span, .. } => *span,
        }
    }
}

// ============================================================================
// Patterns
// ============================================================================

/// A pattern for destructuring in `match` arms and `let` bindings.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Pattern {
    /// `_` — matches anything, binds nothing.
    Wildcard(Span),
    /// A variable binding: `x`.
    Var(Ident),
    /// A literal pattern: `42`, `"hello"`, `true`.
    Literal(Literal),
    /// A constructor pattern: `Some(x)`, `None`.
    Constructor {
        name: Ident,
        fields: Vec<Pattern>,
        span: Span,
    },
    /// An error-recovery placeholder pattern.
    Error(Span),
}

impl Pattern {
    /// Return the span of this pattern.
    pub fn span(&self) -> Span {
        match self {
            Self::Wildcard(s) | Self::Error(s) => *s,
            Self::Var(id) => id.span,
            Self::Literal(lit) => lit.span(),
            Self::Constructor { span, .. } => *span,
        }
    }
}

// ============================================================================
// Literals
// ============================================================================

/// A literal value with its source span.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Literal {
    /// An integer literal: `42`, `0xFF`, `0b1010`.
    Int(i64, Span),
    /// A floating-point literal: `3.14`, `1e10`.
    Float(f64, Span),
    /// A string literal: `"hello"`.
    Str(String, Span),
    /// A boolean literal: `true` or `false`.
    Bool(bool, Span),
    /// The `nil` literal.
    Nil(Span),
}

impl Literal {
    /// Return the span of this literal.
    pub fn span(&self) -> Span {
        match self {
            Self::Int(_, s) | Self::Float(_, s) | Self::Str(_, s) | Self::Bool(_, s) | Self::Nil(s) => *s,
        }
    }
}

// ============================================================================
// Operators
// ============================================================================

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    // Logical
    LogAnd,
    LogOr,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum UnaryOp {
    /// Arithmetic negation: `-x`.
    Neg,
    /// Logical not: `not x`.
    LogNot,
    /// Bitwise not: `~x`.
    BitNot,
}

// ============================================================================
// Stability
// ============================================================================

/// A stability factor that contributes to a stability score.
///
/// Stability factors describe *why* a piece of code has a particular stability
/// rating. These are attached to declarations and expressions by the analyser.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum StabilityFactor {
    /// Mutable state detected.
    MutableState {
        /// Number of mutation sites.
        mutations: u32,
        /// Number of read sites.
        readers: u32,
    },
    /// A variable's type changed across reassignments.
    TypeInstability {
        /// Number of type-changing reassignments.
        reassignments: u32,
    },
    /// A null/nil value propagated through code.
    NullPropagation {
        /// How many levels deep the null propagated.
        depth: u32,
    },
    /// Global state was mutated.
    GlobalState {
        /// Number of mutation sites.
        mutations: u32,
        /// Number of dependent functions.
        dependencies: u32,
    },
    /// Error paths left unhandled.
    UnhandledError {
        /// Number of unhandled error paths.
        paths: u32,
    },
    /// IO operations (network, file, console).
    Io {
        /// Number of IO operations.
        operations: u32,
    },
    /// Exception throwing or catching.
    Exception {
        /// Number of throw sites.
        throws: u32,
        /// Number of catch sites.
        catches: u32,
    },
    /// Algorithm complexity concern.
    AlgorithmComplexity {
        /// Estimated time in milliseconds (amplified).
        time_ms: f64,
    },
    /// Memory leaked without cleanup.
    MemoryLeak {
        /// Estimated leaked bytes.
        bytes: u32,
    },
    /// Race condition potential.
    RaceCondition {
        /// Number of conflicting accesses.
        conflicts: u32,
    },
}

/// A stability annotation on an expression or declaration.
///
/// Contains the overall score (0-100) and the factors that produced it.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StabilityAnnotation {
    /// Overall stability score in the range 0..=100.
    pub score: u32,
    /// The individual factors contributing to the score.
    pub factors: Vec<StabilityFactor>,
}

// ============================================================================
// Gutter
// ============================================================================

/// A handler arm in a gutter block — describes what to do for a specific error
/// class.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GutterHandler {
    /// Pattern to match the error (e.g. a specific error code, wildcard).
    pub pattern: Pattern,
    /// Body to execute when this handler matches.
    pub body: Vec<Stmt>,
    /// Span of the entire handler arm.
    pub span: Span,
}

// ============================================================================
// Expressions
// ============================================================================

/// A function parameter.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Param {
    /// Parameter name.
    pub name: Ident,
    /// Optional type annotation.
    pub ty: Option<TypeExpr>,
    /// Span covering the whole parameter.
    pub span: Span,
}

/// The body of a lambda expression.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum LambdaBody {
    /// A single expression body: `fn(x) -> x + 1`.
    Expr(Box<Expr>),
    /// A block body: `fn(x) ... end`.
    Block(Vec<Stmt>),
}

/// An expression in Error-Lang.
///
/// Every variant carries a [`Span`] so that error messages, IDE features, and
/// the stability analyser can always refer back to source locations.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr {
    /// A literal value.
    Literal {
        value: Literal,
        span: Span,
    },

    /// A variable reference.
    Var {
        name: Ident,
        span: Span,
    },

    /// An array literal: `[1, 2, 3]`.
    Array {
        elements: Vec<Expr>,
        span: Span,
    },

    /// A binary operation: `a + b`, `x == y`.
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
        span: Span,
    },

    /// A unary operation: `-x`, `not flag`, `~bits`.
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
        span: Span,
    },

    /// A function call: `foo(a, b)`.
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },

    /// Array indexing: `arr[i]`.
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },

    /// Member access: `obj.field`.
    Member {
        object: Box<Expr>,
        field: Ident,
        span: Span,
    },

    /// Ternary conditional: `cond ? then_branch : else_branch`.
    ///
    /// In Error-Lang this doubles as three-valued logic when combined with
    /// stability-aware evaluation (Ok / Err / Pending).
    Ternary {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
        span: Span,
    },

    /// A lambda (anonymous function): `fn(x) -> x * 2` or `fn(x) ... end`.
    Lambda {
        params: Vec<Param>,
        return_type: Option<TypeExpr>,
        body: LambdaBody,
        span: Span,
    },

    /// An expression annotated with a stability score and factors.
    ///
    /// This is inserted by the stability analyser to record the computed
    /// stability of a sub-expression.
    Stability {
        inner: Box<Expr>,
        annotation: StabilityAnnotation,
        span: Span,
    },

    /// A gutter expression — an error recovery region.
    ///
    /// The `body` is the code that may contain (or have injected) errors.
    /// The optional `handlers` describe structured recovery paths.
    /// After a gutter block the program continues; stability decreases.
    Gutter {
        body: Vec<Stmt>,
        handlers: Vec<GutterHandler>,
        /// Whether the parser successfully recovered.
        recovered: bool,
        span: Span,
    },

    /// A grouped (parenthesised) expression — `(expr)`.
    ///
    /// Semantically transparent; preserved so round-trip formatting is exact.
    Group {
        inner: Box<Expr>,
        span: Span,
    },

    /// An error-recovery placeholder expression.
    ///
    /// Inserted by the parser when it cannot form a valid expression inside a
    /// gutter block or after a syntax error. Carries diagnostic information for
    /// downstream consumers.
    Error {
        /// Human-readable description of what went wrong.
        message: String,
        span: Span,
    },
}

impl Expr {
    /// Return the span of this expression.
    pub fn span(&self) -> Span {
        match self {
            Self::Literal { span, .. }
            | Self::Var { span, .. }
            | Self::Array { span, .. }
            | Self::Binary { span, .. }
            | Self::Unary { span, .. }
            | Self::Call { span, .. }
            | Self::Index { span, .. }
            | Self::Member { span, .. }
            | Self::Ternary { span, .. }
            | Self::Lambda { span, .. }
            | Self::Stability { span, .. }
            | Self::Gutter { span, .. }
            | Self::Group { span, .. }
            | Self::Error { span, .. } => *span,
        }
    }
}

// ============================================================================
// Statements
// ============================================================================

/// A statement in Error-Lang.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Stmt {
    /// `let [mutable] name [: type] = value`
    Let {
        mutable: bool,
        name: Ident,
        ty: Option<TypeExpr>,
        value: Expr,
        span: Span,
    },

    /// `target = value` (assignment to variable, index, or member).
    Assign {
        target: Expr,
        value: Expr,
        span: Span,
    },

    /// `if cond ... [elseif cond ...] [else ...] end`
    If {
        condition: Expr,
        then_body: Vec<Stmt>,
        elseif_clauses: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
        span: Span,
    },

    /// `while cond ... end`
    While {
        condition: Expr,
        body: Vec<Stmt>,
        span: Span,
    },

    /// `for var in iter ... end`
    For {
        var: Ident,
        iter: Expr,
        body: Vec<Stmt>,
        span: Span,
    },

    /// `return [value]`
    Return {
        value: Option<Expr>,
        span: Span,
    },

    /// `break`
    Break(Span),

    /// `continue`
    Continue(Span),

    /// `print(args)` or `println(args)`
    Print {
        println: bool,
        args: Vec<Expr>,
        span: Span,
    },

    /// A gutter block at the statement level.
    ///
    /// `gutter ... end` — an error injection / recovery zone.
    Gutter {
        body: Vec<Stmt>,
        handlers: Vec<GutterHandler>,
        recovered: bool,
        span: Span,
    },

    /// A bare expression used as a statement.
    Expr(Expr),

    /// An error-recovery placeholder statement.
    Error {
        message: String,
        span: Span,
    },
}

impl Stmt {
    /// Return the span of this statement.
    pub fn span(&self) -> Span {
        match self {
            Self::Let { span, .. }
            | Self::Assign { span, .. }
            | Self::If { span, .. }
            | Self::While { span, .. }
            | Self::For { span, .. }
            | Self::Return { span, .. }
            | Self::Break(span)
            | Self::Continue(span)
            | Self::Print { span, .. }
            | Self::Gutter { span, .. }
            | Self::Error { span, .. } => *span,
            Self::Expr(e) => e.span(),
        }
    }
}

// ============================================================================
// Declarations / Items
// ============================================================================

/// A top-level declaration (item) in an Error-Lang program.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Decl {
    /// `function name(params) [: return_type] ... end`
    Function {
        name: Ident,
        params: Vec<Param>,
        return_type: Option<TypeExpr>,
        body: Vec<Stmt>,
        /// Optional stability annotation computed by the analyser.
        stability: Option<StabilityAnnotation>,
        span: Span,
    },

    /// `struct Name field1: Type1 field2: Type2 end`
    Struct {
        name: Ident,
        fields: Vec<(Ident, TypeExpr)>,
        span: Span,
    },

    /// `main ... end`
    Main {
        body: Vec<Stmt>,
        /// Optional stability annotation computed by the analyser.
        stability: Option<StabilityAnnotation>,
        span: Span,
    },

    /// A stability declaration: attaches explicit stability metadata to a name.
    ///
    /// This allows authors to annotate functions or modules with expected
    /// stability characteristics that the analyser can verify.
    StabilityDecl {
        /// The name being annotated.
        target: Ident,
        /// The declared stability annotation.
        annotation: StabilityAnnotation,
        span: Span,
    },

    /// A bare statement at the top level.
    Stmt(Stmt),
}

impl Decl {
    /// Return the span of this declaration.
    pub fn span(&self) -> Span {
        match self {
            Self::Function { span, .. }
            | Self::Struct { span, .. }
            | Self::Main { span, .. }
            | Self::StabilityDecl { span, .. } => *span,
            Self::Stmt(s) => s.span(),
        }
    }
}

// ============================================================================
// Program
// ============================================================================

/// A complete Error-Lang program — the root of the AST.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Program {
    /// Top-level declarations.
    pub declarations: Vec<Decl>,
    /// Span covering the entire source file.
    pub span: Span,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_synthetic() {
        let s = Span::synthetic();
        assert!(s.is_synthetic());
        assert_ne!(s, Span::new(0, 0));
    }

    #[test]
    fn test_span_merge() {
        let a = Span::new(10, 20);
        let b = Span::new(15, 30);
        let merged = a.merge(b);
        assert_eq!(merged, Span::new(10, 30));
    }

    #[test]
    fn test_span_merge_with_synthetic() {
        let real = Span::new(5, 10);
        let synth = Span::synthetic();
        assert_eq!(real.merge(synth), real);
        assert_eq!(synth.merge(real), real);
    }

    #[test]
    fn test_span_len() {
        assert_eq!(Span::new(0, 42).len(), 42);
        assert_eq!(Span::new(10, 10).len(), 0);
        assert!(Span::new(10, 10).is_empty());
    }

    #[test]
    fn test_expr_span() {
        let expr = Expr::Literal {
            value: Literal::Int(42, Span::new(0, 2)),
            span: Span::new(0, 2),
        };
        assert_eq!(expr.span(), Span::new(0, 2));
    }

    #[test]
    fn test_error_expr() {
        let err = Expr::Error {
            message: "unexpected token".to_string(),
            span: Span::new(5, 10),
        };
        assert_eq!(err.span(), Span::new(5, 10));
    }

    #[test]
    fn test_gutter_expr() {
        let gutter = Expr::Gutter {
            body: vec![],
            handlers: vec![],
            recovered: true,
            span: Span::new(0, 50),
        };
        assert_eq!(gutter.span(), Span::new(0, 50));
    }

    #[test]
    fn test_stability_expr() {
        let inner = Expr::Literal {
            value: Literal::Int(1, Span::new(5, 6)),
            span: Span::new(5, 6),
        };
        let annotated = Expr::Stability {
            inner: Box::new(inner),
            annotation: StabilityAnnotation {
                score: 85,
                factors: vec![StabilityFactor::MutableState {
                    mutations: 2,
                    readers: 3,
                }],
            },
            span: Span::new(0, 20),
        };
        assert_eq!(annotated.span(), Span::new(0, 20));
    }

    #[test]
    fn test_ternary_expr() {
        let cond = Expr::Literal {
            value: Literal::Bool(true, Span::new(0, 4)),
            span: Span::new(0, 4),
        };
        let then_br = Expr::Literal {
            value: Literal::Int(1, Span::new(7, 8)),
            span: Span::new(7, 8),
        };
        let else_br = Expr::Literal {
            value: Literal::Int(0, Span::new(11, 12)),
            span: Span::new(11, 12),
        };
        let ternary = Expr::Ternary {
            condition: Box::new(cond),
            then_branch: Box::new(then_br),
            else_branch: Box::new(else_br),
            span: Span::new(0, 12),
        };
        assert_eq!(ternary.span(), Span::new(0, 12));
    }

    #[test]
    fn test_pattern_variants() {
        let w = Pattern::Wildcard(Span::new(0, 1));
        assert_eq!(w.span(), Span::new(0, 1));

        let v = Pattern::Var(Ident {
            name: "x".to_string(),
            span: Span::new(0, 1),
        });
        assert_eq!(v.span(), Span::new(0, 1));

        let c = Pattern::Constructor {
            name: Ident {
                name: "Some".to_string(),
                span: Span::new(0, 4),
            },
            fields: vec![Pattern::Wildcard(Span::new(5, 6))],
            span: Span::new(0, 7),
        };
        assert_eq!(c.span(), Span::new(0, 7));
    }

    #[test]
    fn test_type_expr_variants() {
        assert_eq!(TypeExpr::Int(Span::new(0, 3)).span(), Span::new(0, 3));
        assert_eq!(TypeExpr::Error(Span::new(0, 0)).span(), Span::new(0, 0));

        let arr = TypeExpr::Array {
            elem: Box::new(TypeExpr::Int(Span::new(6, 9))),
            span: Span::new(0, 10),
        };
        assert_eq!(arr.span(), Span::new(0, 10));
    }

    #[test]
    fn test_decl_function() {
        let decl = Decl::Function {
            name: Ident {
                name: "greet".to_string(),
                span: Span::new(9, 14),
            },
            params: vec![],
            return_type: None,
            body: vec![],
            stability: Some(StabilityAnnotation {
                score: 100,
                factors: vec![],
            }),
            span: Span::new(0, 50),
        };
        assert_eq!(decl.span(), Span::new(0, 50));
    }

    #[test]
    fn test_program_round_trip() {
        let program = Program {
            declarations: vec![
                Decl::Main {
                    body: vec![Stmt::Print {
                        println: true,
                        args: vec![Expr::Literal {
                            value: Literal::Str("Hello!".to_string(), Span::new(14, 22)),
                            span: Span::new(14, 22),
                        }],
                        span: Span::new(6, 23),
                    }],
                    stability: None,
                    span: Span::new(0, 27),
                },
            ],
            span: Span::new(0, 27),
        };
        assert_eq!(program.declarations.len(), 1);
    }

    #[test]
    fn test_gutter_handler() {
        let handler = GutterHandler {
            pattern: Pattern::Wildcard(Span::new(0, 1)),
            body: vec![Stmt::Print {
                println: true,
                args: vec![Expr::Literal {
                    value: Literal::Str("recovered".to_string(), Span::new(10, 21)),
                    span: Span::new(10, 21),
                }],
                span: Span::new(2, 22),
            }],
            span: Span::new(0, 22),
        };
        assert_eq!(handler.span, Span::new(0, 22));
    }

    #[test]
    fn test_stability_decl() {
        let decl = Decl::StabilityDecl {
            target: Ident {
                name: "process_data".to_string(),
                span: Span::new(10, 22),
            },
            annotation: StabilityAnnotation {
                score: 70,
                factors: vec![
                    StabilityFactor::Io { operations: 3 },
                    StabilityFactor::Exception {
                        throws: 1,
                        catches: 1,
                    },
                ],
            },
            span: Span::new(0, 60),
        };
        assert_eq!(decl.span(), Span::new(0, 60));
    }
}
