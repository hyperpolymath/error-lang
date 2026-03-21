// SPDX-License-Identifier: PMPL-1.0-or-later
// Error-Lang Playground - Core Type Definitions
// Types for tokens, AST nodes, errors, and the stability system

// Position tracking for error reporting
type position = {
  line: int,
  column: int,
  offset: int,
}

type span = {
  start: position,
  end_: position,
  source: string,
}

// Error severity levels for pedagogical feedback
type errorSeverity =
  | Hint      // Gentle suggestion
  | Info      // Informational
  | Warning   // Non-fatal issue
  | Error     // Compilation error
  | Fatal     // Unrecoverable

// Error codes follow E0XXX pattern
// E0001-E0099: Syntax errors (lexical/parse phase)
// E0101-E0199: Runtime errors
// E0201-E0299: Logical errors
// E0301-E0399: Semantic errors
type errorCode =
  | E0001  // Unexpected token
  | E0002  // Unterminated string
  | E0003  // Invalid escape sequence
  | E0004  // Illegal character
  | E0005  // Missing 'end' keyword
  | E0006  // Unmatched parenthesis
  | E0007  // Smart quote detected
  | E0008  // Invalid identifier
  | E0009  // Reserved keyword misuse
  | E0010  // Whitespace issue
  | E0101  // Division by zero
  | E0102  // Nil dereference
  | E0103  // Index out of bounds
  | E0104  // Stack overflow
  | E0105  // Type mismatch at runtime
  | E0106  // Undefined variable
  | E0107  // Function not found
  | E0201  // Unreachable code
  | E0202  // Infinite loop detected
  | E0203  // Unused variable
  | E0204  // Shadowed binding
  | E0205  // Off-by-one potential
  | E0206  // Comparison always true/false
  | E0207  // Integer overflow potential
  | E0301  // Type mismatch
  | E0302  // Arity mismatch
  | E0303  // Undefined function
  | E0304  // Duplicate definition
  | E0305  // Visibility violation

// Diagnostic with learning objective
type diagnostic = {
  code: errorCode,
  severity: errorSeverity,
  message: string,
  span: span,
  learningObjective: string,
  recoveryHint: string,
  curriculumLesson: option<int>,
}

// Token types for the lexer
type tokenKind =
  // Keywords
  | Main | End | Let | Mutable | Function | Struct
  | If | Elseif | Else | While | For | In
  | Return | Break | Continue
  | And | Or | Not
  | True | False | Nil
  | Gutter | Fn
  | Print | Println
  // Types
  | TInt | TFloat | TString | TBool | TArray
  // Literals
  | IntLiteral(int)
  | FloatLiteral(float)
  | StringLiteral(string)
  | Identifier(string)
  // Operators
  | Plus | Minus | Star | Slash | Percent
  | Eq | EqEq | BangEq
  | Lt | Gt | LtEq | GtEq
  | Amp | Pipe | Caret | Tilde
  | LtLt | GtGt
  | Question | Colon
  | Arrow  // ->
  // Delimiters
  | LParen | RParen
  | LBracket | RBracket
  | LBrace | RBrace
  | Comma | Dot | Semicolon
  // Special
  | Comment(string)
  | Whitespace
  | Newline
  | Eof
  | Error(string)

type token = {
  kind: tokenKind,
  lexeme: string,
  span: span,
}

// AST node types
type rec expr =
  | IntExpr(int, span)
  | FloatExpr(float, span)
  | StringExpr(string, span)
  | BoolExpr(bool, span)
  | NilExpr(span)
  | IdentExpr(string, span)
  | BinaryExpr(binaryOp, expr, expr, span)
  | UnaryExpr(unaryOp, expr, span)
  | CallExpr(expr, array<expr>, span)
  | IndexExpr(expr, expr, span)
  | MemberExpr(expr, string, span)
  | ArrayExpr(array<expr>, span)
  | LambdaExpr(array<param>, option<typeExpr>, lambdaBody, span)
  | TernaryExpr(expr, expr, expr, span)
  | GroupExpr(expr, span)
  | ErrorExpr(string, span)  // For error recovery

and binaryOp =
  | Add | Sub | Mul | Div | Mod
  | Eq_ | Neq | Lt_ | Gt_ | Lte | Gte
  | And_ | Or_
  | BitAnd | BitOr | BitXor | Shl | Shr

and unaryOp = Neg | Not_ | BitNot

and param = {
  name: string,
  typeAnnotation: option<typeExpr>,
}

and typeExpr =
  | IntType | FloatType | StringType | BoolType
  | ArrayType(typeExpr)
  | CustomType(string)

and lambdaBody =
  | ExprBody(expr)
  | BlockBody(array<stmt>)

and stmt =
  | LetStmt(string, bool, option<typeExpr>, expr, span)  // name, mutable, type, value
  | AssignStmt(string, option<expr>, expr, span)         // name, index, value
  | IfStmt(expr, array<stmt>, array<elseifClause>, option<array<stmt>>, span)
  | WhileStmt(expr, array<stmt>, span)
  | ForStmt(string, expr, array<stmt>, span)
  | ReturnStmt(option<expr>, span)
  | BreakStmt(span)
  | ContinueStmt(span)
  | PrintStmt(bool, array<expr>, span)  // newline, args
  | ExprStmt(expr, span)
  | GutterStmt(array<token>, span)  // Raw tokens in gutter (will have errors)
  | ErrorStmt(string, span)  // For error recovery

and elseifClause = {
  condition: expr,
  body: array<stmt>,
  span: span,
}

// Top-level declarations
type decl =
  | FunctionDecl(string, array<param>, option<typeExpr>, array<stmt>, span)
  | StructDecl(string, array<structField>, span)
  | MainBlock(array<stmt>, span)
  | StmtDecl(stmt)

and structField = {
  fieldName: string,
  fieldType: typeExpr,
}

// Complete program
type program = {
  declarations: array<decl>,
  diagnostics: array<diagnostic>,
  stabilityScore: int,  // 0-100, decreases with errors
}

// Error injection configuration
type injectionConfig = {
  seed: option<int>,
  errorProbability: float,  // 0.0-1.0
  maxErrorsPerGutter: int,
  enabledErrorCodes: array<errorCode>,
}

// Curriculum mapping
type lesson = {
  number: int,
  title: string,
  errorCodes: array<errorCode>,
  learningObjectives: array<string>,
  prerequisiteLessons: array<int>,
}
