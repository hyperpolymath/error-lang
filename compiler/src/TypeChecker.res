// SPDX-License-Identifier: MPL-2.0
// TypeChecker.res - Static type checker for Error-Lang
//
// Typing rules:
// - `let x = expr` — infer type from expr
// - `let x: Type = expr` — check expr against Type
// - `mutable` variables can be reassigned (same type only)
// - `if/elseif/else` — condition must be boolean, branches unified
// - `while` — condition must be boolean
// - `for x in expr` — expr must be Array
// - `function` — standard arrow type
// - `gutter` blocks — skip type checking (error injection zone)
// - `print/println` — accept any type
// - Operators: arithmetic on Int/Float, comparison returns Bool, logical on Bool

open Types

// ============================================
// Internal Type Representation
// ============================================

type rec ty =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyNil
  | TyArray(ty)
  | TyFun(array<ty>, ty)
  | TyStruct(string)
  | TyAny
  | TyVar(int)

// ============================================
// Type Environment
// ============================================

type binding = {
  ty: ty,
  mutable_: bool,
}

type env = {
  bindings: Dict.t<string, binding>,
  parent: option<env>,
  mutable nextVar: int,
  mutable substitutions: Dict.t<int, ty>,
}

let makeEnv = (): env => {
  bindings: Dict.make(),
  parent: None,
  nextVar: 0,
  substitutions: Dict.make(),
}

let extendEnv = (parent: env): env => {
  bindings: Dict.make(),
  parent: Some(parent),
  nextVar: parent.nextVar,
  substitutions: parent.substitutions,
}

let bindVar = (env: env, name: string, ty: ty, mutable_: bool): unit => {
  Dict.set(env.bindings, name, {ty, mutable_})
}

let rec lookupVar = (env: env, name: string): option<binding> => {
  switch Dict.get(env.bindings, name) {
  | Some(b) => Some(b)
  | None =>
    switch env.parent {
    | Some(p) => lookupVar(p, name)
    | None => None
    }
  }
}

let freshVar = (env: env): ty => {
  let id = env.nextVar
  env.nextVar = env.nextVar + 1
  TyVar(id)
}

// ============================================
// Type Errors
// ============================================

type typeError = {
  message: string,
  loc: location,
}

type checkResult = {
  errors: array<typeError>,
}

let makeResult = (): checkResult => {
  errors: [],
}

let addError = (result: checkResult, message: string, loc: location): unit => {
  Array.push(result.errors, {message, loc})
}

// ============================================
// AST Type → Internal Type
// ============================================

let rec typeExprToTy = (te: typeExpr): ty => {
  switch te {
  | Types.TyInt => TyInt
  | Types.TyFloat => TyFloat
  | Types.TyString => TyString
  | Types.TyBool => TyBool
  | Types.TyArray(inner) => TyArray(typeExprToTy(inner))
  | Types.TyIdent(name) => TyStruct(name)
  }
}

// ============================================
// Type Display
// ============================================

let rec tyToString = (t: ty): string => {
  switch t {
  | TyInt => "Int"
  | TyFloat => "Float"
  | TyString => "String"
  | TyBool => "Bool"
  | TyNil => "Nil"
  | TyArray(inner) => `Array<${tyToString(inner)}>`
  | TyFun(params, ret) =>
    let paramStr = params->Array.map(tyToString)->Array.joinWith(", ")
    `(${paramStr}) -> ${tyToString(ret)}`
  | TyStruct(name) => name
  | TyAny => "Any"
  | TyVar(id) => `?${Int.toString(id)}`
  }
}

// ============================================
// Unification
// ============================================

let rec resolve = (env: env, t: ty): ty => {
  switch t {
  | TyVar(id) =>
    switch Dict.get(env.substitutions, id) {
    | Some(resolved) => resolve(env, resolved)
    | None => t
    }
  | TyArray(inner) => TyArray(resolve(env, inner))
  | TyFun(params, ret) =>
    TyFun(params->Array.map(p => resolve(env, p)), resolve(env, ret))
  | _ => t
  }
}

let rec unify = (env: env, a: ty, b: ty): bool => {
  let a = resolve(env, a)
  let b = resolve(env, b)

  switch (a, b) {
  | (TyAny, _) | (_, TyAny) => true
  | (TyNil, _) | (_, TyNil) => true
  | (TyInt, TyInt) => true
  | (TyFloat, TyFloat) => true
  | (TyString, TyString) => true
  | (TyBool, TyBool) => true
  | (TyVar(id), _) =>
    Dict.set(env.substitutions, id, b)
    true
  | (_, TyVar(id)) =>
    Dict.set(env.substitutions, id, a)
    true
  | (TyArray(ia), TyArray(ib)) => unify(env, ia, ib)
  | (TyStruct(na), TyStruct(nb)) => na == nb
  | (TyFun(pa, ra), TyFun(pb, rb)) =>
    if Array.length(pa) != Array.length(pb) {
      false
    } else {
      let paramsOk = Array.zip(pa, pb)->Array.every(((x, y)) => unify(env, x, y))
      paramsOk && unify(env, ra, rb)
    }
  | _ => false
  }
}

// Numeric widening: Int can widen to Float
let isNumeric = (t: ty): bool => {
  switch t {
  | TyInt | TyFloat => true
  | _ => false
  }
}

let widenNumeric = (a: ty, b: ty): ty => {
  switch (a, b) {
  | (TyFloat, _) | (_, TyFloat) => TyFloat
  | _ => TyInt
  }
}

// ============================================
// Expression Type Inference
// ============================================

let rec inferExpr = (env: env, result: checkResult, e: expr): ty => {
  switch e {
  | IntLit(_, _) => TyInt
  | FloatLit(_, _) => TyFloat
  | StringLit(_, _) => TyString
  | BoolLit(_, _) => TyBool
  | NilLit(_) => TyNil

  | Ident(name, loc) =>
    switch lookupVar(env, name) {
    | Some({ty, _}) => ty
    | None =>
      addError(result, `Undefined variable '${name}'`, loc)
      TyAny
    }

  | Array(elements, _loc) =>
    if Array.length(elements) == 0 {
      TyArray(freshVar(env))
    } else {
      let elemTy = inferExpr(env, result, elements->Array.getUnsafe(0))
      elements
      ->Array.sliceToEnd(~start=1)
      ->Array.forEach(el => {
        let t = inferExpr(env, result, el)
        if !unify(env, elemTy, t) {
          let loc = exprLoc(el)
          addError(
            result,
            `Array element type mismatch: expected ${tyToString(elemTy)}, got ${tyToString(t)}`,
            loc,
          )
        }
      })
      TyArray(resolve(env, elemTy))
    }

  | Binary(left, op, right, loc) =>
    inferBinaryOp(env, result, left, op, right, loc)

  | Unary(op, operand, loc) =>
    inferUnaryOp(env, result, op, operand, loc)

  | Call(callee, args, loc) =>
    let calleeTy = inferExpr(env, result, callee)
    switch resolve(env, calleeTy) {
    | TyFun(paramTys, retTy) =>
      if Array.length(args) != Array.length(paramTys) {
        addError(
          result,
          `Function expects ${Int.toString(Array.length(paramTys))} arguments, got ${Int.toString(Array.length(args))}`,
          loc,
        )
      } else {
        Array.zip(args, paramTys)->Array.forEach(((arg, pty)) => {
          let argTy = inferExpr(env, result, arg)
          if !unify(env, argTy, pty) {
            addError(
              result,
              `Argument type mismatch: expected ${tyToString(pty)}, got ${tyToString(argTy)}`,
              exprLoc(arg),
            )
          }
        })
      }
      retTy
    | TyAny => TyAny
    | other =>
      addError(result, `Cannot call non-function type ${tyToString(other)}`, loc)
      TyAny
    }

  | Index(base, idx, loc) =>
    let baseTy = inferExpr(env, result, base)
    let idxTy = inferExpr(env, result, idx)
    if !unify(env, idxTy, TyInt) {
      addError(result, `Array index must be Int, got ${tyToString(idxTy)}`, loc)
    }
    switch resolve(env, baseTy) {
    | TyArray(elemTy) => elemTy
    | TyAny => TyAny
    | other =>
      addError(result, `Cannot index non-array type ${tyToString(other)}`, loc)
      TyAny
    }

  | Member(base, _field, _loc) =>
    let _ = inferExpr(env, result, base)
    // Struct field access requires a type database; return Any for now.
    TyAny

  | Ternary(cond, thenExpr, elseExpr, loc) =>
    let condTy = inferExpr(env, result, cond)
    if !unify(env, condTy, TyBool) {
      addError(result, `Ternary condition must be Bool, got ${tyToString(condTy)}`, loc)
    }
    let thenTy = inferExpr(env, result, thenExpr)
    let elseTy = inferExpr(env, result, elseExpr)
    if !unify(env, thenTy, elseTy) {
      addError(
        result,
        `Ternary branches have different types: ${tyToString(thenTy)} vs ${tyToString(elseTy)}`,
        loc,
      )
    }
    resolve(env, thenTy)

  | Lambda(params, retAnn, body, _loc) =>
    let lambdaEnv = extendEnv(env)
    let paramTys = params->Array.map(p => {
      let ty = switch p.type_ {
      | Some(te) => typeExprToTy(te)
      | None => freshVar(lambdaEnv)
      }
      bindVar(lambdaEnv, p.name, ty, false)
      ty
    })
    let bodyTy = switch body {
    | LambdaExpr(expr) => inferExpr(lambdaEnv, result, expr)
    | LambdaBlock(stmts) => checkStmts(lambdaEnv, result, stmts)
    }
    switch retAnn {
    | Some(ann) =>
      let annTy = typeExprToTy(ann)
      if !unify(env, bodyTy, annTy) {
        addError(
          result,
          `Lambda return type mismatch: declared ${tyToString(annTy)}, body is ${tyToString(bodyTy)}`,
          exprLoc(e),
        )
      }
    | None => ()
    }
    // Propagate state back
    env.nextVar = lambdaEnv.nextVar
    env.substitutions = lambdaEnv.substitutions
    TyFun(paramTys, resolve(env, bodyTy))
  }
}

and inferBinaryOp = (
  env: env,
  result: checkResult,
  left: expr,
  op: binaryOp,
  right: expr,
  loc: location,
): ty => {
  let lt = inferExpr(env, result, left)
  let rt = inferExpr(env, result, right)
  let ltR = resolve(env, lt)
  let rtR = resolve(env, rt)

  switch op {
  // Arithmetic
  | Add | Sub | Mul | Div | Mod =>
    if isNumeric(ltR) && isNumeric(rtR) {
      widenNumeric(ltR, rtR)
    } else if op == Add && ltR == TyString && rtR == TyString {
      TyString
    } else {
      addError(
        result,
        `Operator requires numeric operands, got ${tyToString(ltR)} and ${tyToString(rtR)}`,
        loc,
      )
      TyAny
    }

  // Comparison
  | Eq | Neq | Lt | Gt | Lte | Gte =>
    if !unify(env, lt, rt) {
      addError(
        result,
        `Cannot compare ${tyToString(ltR)} with ${tyToString(rtR)}`,
        loc,
      )
    }
    TyBool

  // Logical
  | LAnd | LOr =>
    if ltR != TyBool {
      addError(result, `Logical operator requires Bool, got ${tyToString(ltR)}`, loc)
    }
    if rtR != TyBool {
      addError(result, `Logical operator requires Bool, got ${tyToString(rtR)}`, loc)
    }
    TyBool

  // Bitwise
  | BAnd | BOr | BXor | Shl | Shr =>
    if ltR != TyInt {
      addError(result, `Bitwise operator requires Int, got ${tyToString(ltR)}`, loc)
    }
    if rtR != TyInt {
      addError(result, `Bitwise operator requires Int, got ${tyToString(rtR)}`, loc)
    }
    TyInt
  }
}

and inferUnaryOp = (
  env: env,
  result: checkResult,
  op: unaryOp,
  operand: expr,
  loc: location,
): ty => {
  let t = inferExpr(env, result, operand)
  let tR = resolve(env, t)

  switch op {
  | Neg =>
    if !isNumeric(tR) {
      addError(result, `Negation requires numeric type, got ${tyToString(tR)}`, loc)
    }
    tR

  | LNot =>
    if tR != TyBool {
      addError(result, `Logical NOT requires Bool, got ${tyToString(tR)}`, loc)
    }
    TyBool

  | BNot =>
    if tR != TyInt {
      addError(result, `Bitwise NOT requires Int, got ${tyToString(tR)}`, loc)
    }
    TyInt
  }
}

// ============================================
// Statement Type Checking
// ============================================

and checkStmt = (env: env, result: checkResult, stmt: stmt): unit => {
  switch stmt {
  | LetStmt({mutable_, name, type_, value, loc}) =>
    let valueTy = inferExpr(env, result, value)
    switch type_ {
    | Some(ann) =>
      let annTy = typeExprToTy(ann)
      if !unify(env, valueTy, annTy) {
        addError(
          result,
          `Type annotation mismatch for '${name}': declared ${tyToString(annTy)}, value is ${tyToString(valueTy)}`,
          loc,
        )
      }
      bindVar(env, name, annTy, mutable_)
    | None => bindVar(env, name, resolve(env, valueTy), mutable_)
    }

  | AssignStmt({target, value, loc}) =>
    switch target {
    | Ident(name, _) =>
      switch lookupVar(env, name) {
      | Some({ty, mutable_: true}) =>
        let valueTy = inferExpr(env, result, value)
        if !unify(env, valueTy, ty) {
          addError(
            result,
            `Cannot assign ${tyToString(valueTy)} to ${tyToString(ty)} variable '${name}'`,
            loc,
          )
        }
      | Some({mutable_: false, _}) =>
        addError(result, `Cannot reassign immutable variable '${name}'`, loc)
        let _ = inferExpr(env, result, value)
      | None =>
        addError(result, `Undefined variable '${name}'`, loc)
        let _ = inferExpr(env, result, value)
      }
    | _ =>
      // Complex assignment targets (index, member) — infer both sides.
      let _ = inferExpr(env, result, target)
      let _ = inferExpr(env, result, value)
    }

  | IfStmt({cond, then_, elseifs, else_, loc}) =>
    let condTy = inferExpr(env, result, cond)
    if !unify(env, condTy, TyBool) {
      addError(result, `If condition must be Bool, got ${tyToString(condTy)}`, loc)
    }
    let thenEnv = extendEnv(env)
    Array.forEach(then_, s => checkStmt(thenEnv, result, s))
    Array.forEach(elseifs, ((eifCond, eifBody)) => {
      let eifTy = inferExpr(env, result, eifCond)
      if !unify(env, eifTy, TyBool) {
        addError(result, `Elseif condition must be Bool, got ${tyToString(eifTy)}`, loc)
      }
      let eifEnv = extendEnv(env)
      Array.forEach(eifBody, s => checkStmt(eifEnv, result, s))
    })
    switch else_ {
    | Some(elseBody) =>
      let elseEnv = extendEnv(env)
      Array.forEach(elseBody, s => checkStmt(elseEnv, result, s))
    | None => ()
    }

  | WhileStmt({cond, body, loc}) =>
    let condTy = inferExpr(env, result, cond)
    if !unify(env, condTy, TyBool) {
      addError(result, `While condition must be Bool, got ${tyToString(condTy)}`, loc)
    }
    let bodyEnv = extendEnv(env)
    Array.forEach(body, s => checkStmt(bodyEnv, result, s))

  | ForStmt({var: varName, iter, body, loc}) =>
    let iterTy = inferExpr(env, result, iter)
    let elemTy = switch resolve(env, iterTy) {
    | TyArray(inner) => inner
    | TyAny => TyAny
    | other =>
      addError(
        result,
        `For loop requires Array, got ${tyToString(other)}`,
        loc,
      )
      TyAny
    }
    let bodyEnv = extendEnv(env)
    bindVar(bodyEnv, varName, elemTy, false)
    Array.forEach(body, s => checkStmt(bodyEnv, result, s))

  | ReturnStmt({value, _}) =>
    switch value {
    | Some(expr) => let _ = inferExpr(env, result, expr)
    | None => ()
    }

  | BreakStmt(_) => ()
  | ContinueStmt(_) => ()

  | PrintStmt({args, _}) =>
    // print/println accept any type.
    Array.forEach(args, arg => {
      let _ = inferExpr(env, result, arg)
    })

  | GutterBlock(_) =>
    // Gutter blocks are intentional error injection zones — skip type checking.
    ()

  | ExprStmt(expr) =>
    let _ = inferExpr(env, result, expr)
  }
}

and checkStmts = (env: env, result: checkResult, stmts: array<stmt>): ty => {
  Array.forEach(stmts, s => checkStmt(env, result, s))
  TyNil
}

// ============================================
// Declaration Checking
// ============================================

let checkDecl = (env: env, result: checkResult, decl: decl): unit => {
  switch decl {
  | FunctionDecl({name, params, returnType, body, loc}) =>
    let fnEnv = extendEnv(env)
    let paramTys = params->Array.map(p => {
      let ty = switch p.type_ {
      | Some(te) => typeExprToTy(te)
      | None => freshVar(fnEnv)
      }
      bindVar(fnEnv, p.name, ty, false)
      ty
    })
    // Infer body type from the last statement or return.
    Array.forEach(body, s => checkStmt(fnEnv, result, s))
    let retTy = switch returnType {
    | Some(ann) => typeExprToTy(ann)
    | None => TyNil
    }
    // Bind the function in the outer environment.
    bindVar(env, name, TyFun(paramTys, retTy), false)

  | StructDecl({name, _}) =>
    // Register the struct name as a type.
    bindVar(env, name, TyStruct(name), false)

  | MainBlock({body, _}) =>
    let mainEnv = extendEnv(env)
    Array.forEach(body, s => checkStmt(mainEnv, result, s))

  | StmtDecl(stmt) =>
    checkStmt(env, result, stmt)
  }
}

// ============================================
// Program Entry Point
// ============================================

/// Type-check a complete Error-Lang program.
/// Returns a result with any type errors found.
let checkProgram = (prog: program): checkResult => {
  let env = makeEnv()
  let result = makeResult()

  Array.forEach(prog.declarations, decl => checkDecl(env, result, decl))

  result
}

// ============================================
// Utility: Extract Location from Expression
// ============================================

let exprLoc = (e: expr): location => {
  let dummy: location = {
    start: {line: 0, column: 0, offset: 0},
    end_: {line: 0, column: 0, offset: 0},
    file: "<unknown>",
  }
  switch e {
  | IntLit(_, loc) => loc
  | FloatLit(_, loc) => loc
  | StringLit(_, loc) => loc
  | BoolLit(_, loc) => loc
  | NilLit(loc) => loc
  | Ident(_, loc) => loc
  | Array(_, loc) => loc
  | Binary(_, _, _, loc) => loc
  | Unary(_, _, loc) => loc
  | Call(_, _, loc) => loc
  | Index(_, _, loc) => loc
  | Member(_, _, loc) => loc
  | Ternary(_, _, _, loc) => loc
  | Lambda(_, _, _, loc) => loc
  }
}
