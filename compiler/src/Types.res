// SPDX-License-Identifier: AGPL-3.0-or-later
// Types.res - Core type definitions for Error-Lang

// Position in source code
type position = {
  line: int,
  column: int,
  offset: int,
}

// Location span (start to end)
type location = {
  start: position,
  end_: position,
  file: string,
}

// Token types
type tokenType =
  // Keywords
  | Main
  | End
  | Let
  | Mutable
  | Function
  | Struct
  | If
  | Elseif
  | Else
  | While
  | For
  | In
  | Break
  | Continue
  | Return
  | And
  | Or
  | Not
  | True
  | False
  | Nil
  | Gutter
  | Fn
  // Types
  | TInt
  | TFloat
  | TString
  | TBool
  | TArray
  // Literals
  | Integer(int)
  | Float(float)
  | String(string)
  | Identifier(string)
  // Operators
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | EqualEqual
  | BangEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | Ampersand
  | Pipe
  | Caret
  | Tilde
  | LessLess
  | GreaterGreater
  | Equal
  | Arrow
  | Question
  | Colon
  // Delimiters
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Dot
  // Special
  | Newline
  | EOF
  | Error(string)

// Token with location
type token = {
  type_: tokenType,
  lexeme: string,
  loc: location,
}

// ============================================
// AST Node Types
// ============================================

type rec expr =
  | IntLit(int, location)
  | FloatLit(float, location)
  | StringLit(string, location)
  | BoolLit(bool, location)
  | NilLit(location)
  | Ident(string, location)
  | Array(array<expr>, location)
  | Binary(expr, binaryOp, expr, location)
  | Unary(unaryOp, expr, location)
  | Call(expr, array<expr>, location)
  | Index(expr, expr, location)
  | Member(expr, string, location)
  | Ternary(expr, expr, expr, location)
  | Lambda(array<param>, option<typeExpr>, lambdaBody, location)

and binaryOp =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Lte | Gte
  | BAnd | BOr | BXor | Shl | Shr
  | LAnd | LOr

and unaryOp = Neg | LNot | BNot

and param = {
  name: string,
  type_: option<typeExpr>,
  loc: location,
}

and typeExpr =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyArray(typeExpr)
  | TyIdent(string)

and lambdaBody =
  | LambdaExpr(expr)
  | LambdaBlock(array<stmt>)

and stmt =
  | LetStmt({mutable_: bool, name: string, type_: option<typeExpr>, value: expr, loc: location})
  | AssignStmt({target: expr, value: expr, loc: location})
  | IfStmt({
      cond: expr,
      then_: array<stmt>,
      elseifs: array<(expr, array<stmt>)>,
      else_: option<array<stmt>>,
      loc: location,
    })
  | WhileStmt({cond: expr, body: array<stmt>, loc: location})
  | ForStmt({var: string, iter: expr, body: array<stmt>, loc: location})
  | ReturnStmt({value: option<expr>, loc: location})
  | BreakStmt(location)
  | ContinueStmt(location)
  | PrintStmt({println: bool, args: array<expr>, loc: location})
  | GutterBlock({tokens: array<token>, recovered: bool, loc: location})
  | ExprStmt(expr)

type decl =
  | FunctionDecl({
      name: string,
      params: array<param>,
      returnType: option<typeExpr>,
      body: array<stmt>,
      loc: location,
    })
  | StructDecl({name: string, fields: array<(string, typeExpr)>, loc: location})
  | MainBlock({body: array<stmt>, loc: location})
  | StmtDecl(stmt)

type program = {
  declarations: array<decl>,
  loc: location,
}

// ============================================
// Error Types
// ============================================

type errorCode =
  | E0001 // Unexpected token
  | E0002 // Unterminated string
  | E0003 // Invalid escape sequence
  | E0004 // Illegal character
  | E0005 // Missing 'end'
  | E0006 // Unmatched parenthesis
  | E0007 // Unicode/smart quote
  | E0008 // Identifier rules violation
  | E0009 // Reserved keyword misuse
  | E0010 // Whitespace significance

type diagnostic = {
  code: errorCode,
  message: string,
  loc: location,
  runNumber: int,
  hint: option<string>,
}

// ============================================
// Runtime State
// ============================================

type runtimeState = {
  mutable runCounter: int,
  mutable stabilityScore: int,
  mutable lastError: option<errorCode>,
  mutable seed: int,
}

let makeDefaultState = () => {
  runCounter: 0,
  stabilityScore: 100,
  lastError: None,
  seed: 0,
}

// ============================================
// Helper Functions
// ============================================

let errorCodeToString = code =>
  switch code {
  | E0001 => "E0001"
  | E0002 => "E0002"
  | E0003 => "E0003"
  | E0004 => "E0004"
  | E0005 => "E0005"
  | E0006 => "E0006"
  | E0007 => "E0007"
  | E0008 => "E0008"
  | E0009 => "E0009"
  | E0010 => "E0010"
  }

let formatDiagnostic = (diag: diagnostic): string => {
  let codeStr = errorCodeToString(diag.code)
  let loc = diag.loc
  `Error-LangError: ${loc.file}:${loc.start.line->Int.toString}:${loc.start.column->Int.toString}: ${diag.message} [code=${codeStr} run=${diag.runNumber->Int.toString}]`
}
