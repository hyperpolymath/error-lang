// SPDX-License-Identifier: MPL-2.0
// Pretty.res - Pretty-printer for Error-Lang AST
//
// Converts AST nodes into formatted source code strings. Handles
// main/end blocks, gutter blocks, let/if/while/for statements,
// function declarations, and struct definitions.
//
// Used for the `fmt` command, code generation output, and REPL display.

open Types

// ============================================================
// Configuration
// ============================================================

type config = {
  indentWidth: int, // Spaces per indent level (2 or 4)
  maxWidth: int, // Advisory max line width
}

let defaultConfig: config = {indentWidth: 2, maxWidth: 100}

// ============================================================
// Internal printer
// ============================================================

type printer = {
  mutable buf: string,
  mutable indent: int,
  config: config,
}

let makePrinter = config => {buf: "", indent: 0, config}

let emit = (p, s) => p.buf = p.buf ++ s

let newline = p => {
  p.buf = p.buf ++ "\n"
  for _ in 1 to p.indent * p.config.indentWidth {
    p.buf = p.buf ++ " "
  }
}

let indented = (p, f) => {
  p.indent = p.indent + 1
  f()
  p.indent = p.indent - 1
}

// ============================================================
// All printer functions (mutually recursive)
// ============================================================

let rec ppTypeExpr = (p, ty) =>
  switch ty {
  | TyInt => emit(p, "int")
  | TyFloat => emit(p, "float")
  | TyString => emit(p, "string")
  | TyBool => emit(p, "bool")
  | TyArray(inner) =>
    emit(p, "[")
    ppTypeExpr(p, inner)
    emit(p, "]")
  | TyIdent(name) => emit(p, name)
  }

and ppBinaryOp = (p, op) =>
  emit(
    p,
    switch op {
    | Add => "+"
    | Sub => "-"
    | Mul => "*"
    | Div => "/"
    | Mod => "%"
    | Eq => "=="
    | Neq => "!="
    | Lt => "<"
    | Gt => ">"
    | Lte => "<="
    | Gte => ">="
    | BAnd => "&"
    | BOr => "|"
    | BXor => "^"
    | Shl => "<<"
    | Shr => ">>"
    | LAnd => "and"
    | LOr => "or"
    },
  )

and ppUnaryOp = (p, op) =>
  switch op {
  | Neg => emit(p, "-")
  | LNot => emit(p, "not ")
  | BNot => emit(p, "~")
  }

and ppExpr = (p, expr) =>
  switch expr {
  | IntLit(n, _) => emit(p, Int.toString(n))
  | FloatLit(f, _) => emit(p, Float.toString(f))
  | StringLit(s, _) =>
    emit(p, "\"")
    emit(p, s->String.replaceAll("\\", "\\\\")->String.replaceAll("\"", "\\\""))
    emit(p, "\"")
  | BoolLit(b, _) => emit(p, b ? "true" : "false")
  | NilLit(_) => emit(p, "nil")
  | Ident(name, _) => emit(p, name)

  | Array(elems, _) =>
    emit(p, "[")
    elems->Array.forEachWithIndex((elem, i) => {
      if i > 0 {
        emit(p, ", ")
      }
      ppExpr(p, elem)
    })
    emit(p, "]")

  | Binary(lhs, op, rhs, _) =>
    emit(p, "(")
    ppExpr(p, lhs)
    emit(p, " ")
    ppBinaryOp(p, op)
    emit(p, " ")
    ppExpr(p, rhs)
    emit(p, ")")

  | Unary(op, operand, _) =>
    ppUnaryOp(p, op)
    ppExpr(p, operand)

  | Call(callee, args, _) =>
    ppExpr(p, callee)
    emit(p, "(")
    args->Array.forEachWithIndex((arg, i) => {
      if i > 0 {
        emit(p, ", ")
      }
      ppExpr(p, arg)
    })
    emit(p, ")")

  | Index(target, index, _) =>
    ppExpr(p, target)
    emit(p, "[")
    ppExpr(p, index)
    emit(p, "]")

  | Member(target, field, _) =>
    ppExpr(p, target)
    emit(p, ".")
    emit(p, field)

  | Ternary(cond, then_, else_, _) =>
    ppExpr(p, cond)
    emit(p, " ? ")
    ppExpr(p, then_)
    emit(p, " : ")
    ppExpr(p, else_)

  | Lambda(params, retTy, body, _) =>
    emit(p, "fn(")
    params->Array.forEachWithIndex((param, i) => {
      if i > 0 {
        emit(p, ", ")
      }
      emit(p, param.name)
      switch param.type_ {
      | Some(ty) =>
        emit(p, ": ")
        ppTypeExpr(p, ty)
      | None => ()
      }
    })
    emit(p, ")")
    switch retTy {
    | Some(ty) =>
      emit(p, " -> ")
      ppTypeExpr(p, ty)
    | None => ()
    }
    switch body {
    | LambdaExpr(e) =>
      emit(p, " => ")
      ppExpr(p, e)
    | LambdaBlock(stmts) =>
      emit(p, " {")
      ppStmtBlock(p, stmts)
      newline(p)
      emit(p, "}")
    }
  }

and ppStmt = (p, stmt) =>
  switch stmt {
  | LetStmt({mutable_, name, type_, value, _}) =>
    emit(p, "let ")
    if mutable_ {
      emit(p, "mutable ")
    }
    emit(p, name)
    switch type_ {
    | Some(ty) =>
      emit(p, ": ")
      ppTypeExpr(p, ty)
    | None => ()
    }
    emit(p, " = ")
    ppExpr(p, value)

  | AssignStmt({target, value, _}) =>
    ppExpr(p, target)
    emit(p, " = ")
    ppExpr(p, value)

  | IfStmt({cond, then_, elseifs, else_, _}) =>
    emit(p, "if ")
    ppExpr(p, cond)
    ppStmtBlock(p, then_)
    elseifs->Array.forEach(((eifCond, eifBody)) => {
      newline(p)
      emit(p, "elseif ")
      ppExpr(p, eifCond)
      ppStmtBlock(p, eifBody)
    })
    switch else_ {
    | Some(elseBody) =>
      newline(p)
      emit(p, "else")
      ppStmtBlock(p, elseBody)
    | None => ()
    }
    newline(p)
    emit(p, "end")

  | WhileStmt({cond, body, _}) =>
    emit(p, "while ")
    ppExpr(p, cond)
    ppStmtBlock(p, body)
    newline(p)
    emit(p, "end")

  | ForStmt({var, iter, body, _}) =>
    emit(p, "for ")
    emit(p, var)
    emit(p, " in ")
    ppExpr(p, iter)
    ppStmtBlock(p, body)
    newline(p)
    emit(p, "end")

  | ReturnStmt({value, _}) =>
    emit(p, "return")
    switch value {
    | Some(v) =>
      emit(p, " ")
      ppExpr(p, v)
    | None => ()
    }

  | BreakStmt(_) => emit(p, "break")

  | ContinueStmt(_) => emit(p, "continue")

  | PrintStmt({println, args, _}) =>
    emit(p, println ? "println" : "print")
    emit(p, "(")
    args->Array.forEachWithIndex((arg, i) => {
      if i > 0 {
        emit(p, ", ")
      }
      ppExpr(p, arg)
    })
    emit(p, ")")

  | GutterBlock({loc: _, _}) => emit(p, "gutter { ... }")

  | ExprStmt(e) => ppExpr(p, e)
  }

and ppStmtBlock = (p, stmts) =>
  indented(p, () =>
    stmts->Array.forEach(stmt => {
      newline(p)
      ppStmt(p, stmt)
    })
  )

and ppDecl = (p, decl) =>
  switch decl {
  | FunctionDecl({name, params, returnType, body, _}) =>
    emit(p, "function ")
    emit(p, name)
    emit(p, "(")
    params->Array.forEachWithIndex((param, i) => {
      if i > 0 {
        emit(p, ", ")
      }
      emit(p, param.name)
      switch param.type_ {
      | Some(ty) =>
        emit(p, ": ")
        ppTypeExpr(p, ty)
      | None => ()
      }
    })
    emit(p, ")")
    switch returnType {
    | Some(ty) =>
      emit(p, " -> ")
      ppTypeExpr(p, ty)
    | None => ()
    }
    ppStmtBlock(p, body)
    newline(p)
    emit(p, "end")

  | StructDecl({name, fields, _}) =>
    emit(p, "struct ")
    emit(p, name)
    indented(p, () =>
      fields->Array.forEach(((fname, ftype)) => {
        newline(p)
        emit(p, fname)
        emit(p, ": ")
        ppTypeExpr(p, ftype)
      })
    )
    newline(p)
    emit(p, "end")

  | MainBlock({body, _}) =>
    emit(p, "main")
    ppStmtBlock(p, body)
    newline(p)
    emit(p, "end")

  | StmtDecl(stmt) => ppStmt(p, stmt)
  }

and ppProgram = (p, prog) => {
  prog.declarations->Array.forEachWithIndex((decl, i) => {
    if i > 0 {
      newline(p)
      newline(p)
    }
    ppDecl(p, decl)
  })
  emit(p, "\n")
}

// ============================================================
// Public API
// ============================================================

/** Pretty-print a program to a string with default configuration. */
let programToString = prog => {
  let p = makePrinter(defaultConfig)
  ppProgram(p, prog)
  p.buf
}

/** Pretty-print a program with custom configuration. */
let programToStringWithConfig = (prog, config) => {
  let p = makePrinter(config)
  ppProgram(p, prog)
  p.buf
}

/** Pretty-print a single expression to a string. */
let exprToString = expr => {
  let p = makePrinter(defaultConfig)
  ppExpr(p, expr)
  p.buf
}

/** Pretty-print a single statement to a string. */
let stmtToString = stmt => {
  let p = makePrinter(defaultConfig)
  ppStmt(p, stmt)
  p.buf
}

/** Pretty-print a single declaration to a string. */
let declToString = decl => {
  let p = makePrinter(defaultConfig)
  ppDecl(p, decl)
  p.buf
}
