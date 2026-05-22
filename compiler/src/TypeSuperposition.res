// SPDX-License-Identifier: MPL-2.0
// TypeSuperposition.res - Quantum type system implementation

open Types

/**
 * Type Quantum Superposition
 *
 * Variables exist in multiple types simultaneously until "observed"
 * (printed, used in arithmetic, compared, etc.)
 *
 * This demonstrates:
 * - Type inference is contextual, not magical
 * - The cost of dynamic typing
 * - How observation collapses superposition
 */

// Quantum type states
type quantumType =
  | Collapsed(typeExpr)  // Type is determined
  | Superposition({
      possibleTypes: array<typeExpr>,
      seed: int,
      declaredAt: location,
    })

// Type observation contexts
type observationContext =
  | Arithmetic  // Used in +, -, *, /
  | StringOp    // Used in ++, interpolation
  | Comparison  // Used in ==, !=, <, >
  | Print       // Used in println
  | Assignment  // Assigned to typed variable
  | FunctionArg // Passed to typed parameter

// Variable with quantum type state
type quantumVariable = {
  name: string,
  quantumType: quantumType,
  observedAt: option<(location, observationContext)>,
  declaredAt: location,
}

/**
 * Determine possible types for a literal based on its form
 */
let possibleTypesForLiteral = (expr: expr): array<typeExpr> => {
  switch expr {
  | IntLit(_, _) =>
    // Integer literal could be Int, Float (via coercion), or String
    [TyInt, TyFloat, TyString]

  | FloatLit(_, _) =>
    // Float could be Float or String
    [TyFloat, TyString]

  | StringLit(_, _) =>
    // String is always String (but might be coercible to Int/Float)
    [TyString, TyInt, TyFloat]

  | BoolLit(_, _) =>
    // Bool could be Bool, Int (0/1), or String
    [TyBool, TyInt, TyString]

  | _ =>
    // Unknown expression - full superposition
    [TyInt, TyFloat, TyString, TyBool]
  }
}

/**
 * Collapse quantum type based on observation context and seed
 */
let collapseType = (
  qt: quantumType,
  context: observationContext,
  seed: int,
): typeExpr => {
  switch qt {
  | Collapsed(t) => t  // Already collapsed

  | Superposition({possibleTypes, seed: varSeed, _}) =>
    // Collapse is deterministic but depends on:
    // 1. Context (what operation is being performed)
    // 2. Seed (for reproducibility)
    // 3. Position in code (via hash)

    let contextHash = switch context {
    | Arithmetic => 0
    | StringOp => 1
    | Comparison => 2
    | Print => 3
    | Assignment => 4
    | FunctionArg => 5
    }

    let hash = (varSeed + seed + contextHash) mod Array.length(possibleTypes)
    possibleTypes[hash]->Option.getOr(TyInt)
  }
}

/**
 * Check if an observation would cause type mismatch
 */
let wouldCauseMismatch = (
  qt: quantumType,
  context: observationContext,
  expectedType: typeExpr,
  seed: int,
): bool => {
  let collapsedType = collapseType(qt, context, seed)
  collapsedType != expectedType
}

/**
 * Create quantum variable from let statement
 */
let createQuantumVariable = (
  name: string,
  value: expr,
  typeAnnotation: option<typeExpr>,
  loc: location,
  seed: int,
): quantumVariable => {
  let quantumType = switch typeAnnotation {
  | Some(annotatedType) =>
    // Explicit type annotation - collapsed from the start
    Collapsed(annotatedType)

  | None =>
    // No type annotation - quantum superposition!
    let possibleTypes = possibleTypesForLiteral(value)
    Superposition({
      possibleTypes: possibleTypes,
      seed: seed,
      declaredAt: loc,
    })
  }

  {
    name: name,
    quantumType: quantumType,
    observedAt: None,
    declaredAt: loc,
  }
}

/**
 * Observe a quantum variable (collapse its type)
 */
let observeVariable = (
  qvar: quantumVariable,
  context: observationContext,
  observeLoc: location,
  seed: int,
): (quantumVariable, option<typeExpr>) => {
  switch qvar.quantumType {
  | Collapsed(t) =>
    // Already collapsed - just return it
    (qvar, Some(t))

  | Superposition({possibleTypes, seed: varSeed, declaredAt}) =>
    // COLLAPSE THE WAVEFUNCTION!
    let collapsedType = collapseType(qvar.quantumType, context, seed)

    let updatedVar = {
      ...qvar,
      quantumType: Collapsed(collapsedType),
      observedAt: Some((observeLoc, context)),
    }

    (updatedVar, Some(collapsedType))
  }
}

/**
 * Detect type superposition paradox in program
 */
let detectSuperposition = (prog: program, seed: int): array<(string, quantumVariable)> => {
  let quantumVars = []

  let rec analyzeDecl = (decl: decl): unit => {
    switch decl {
    | MainBlock({body, _}) => Array.forEach(body, analyzeStmt)
    | FunctionDecl({body, _}) => Array.forEach(body, analyzeStmt)
    | _ => ()
    }
  }

  and analyzeStmt = (stmt: stmt): unit => {
    switch stmt {
    | LetStmt({name, type_, value, loc, _}) =>
      // Check if this is a quantum variable
      if Option.isNone(type_) {
        let qvar = createQuantumVariable(name, value, type_, loc, seed)
        switch qvar.quantumType {
        | Superposition(_) =>
          Array.push(quantumVars, (name, qvar))
        | _ => ()
        }
      }

    | IfStmt({then_, elseifs, else_, _}) =>
      Array.forEach(then_, analyzeStmt)
      Array.forEach(elseifs, ((_, body)) => Array.forEach(body, analyzeStmt))
      Option.forEach(else_, body => Array.forEach(body, analyzeStmt))

    | _ => ()
    }
  }

  Array.forEach(prog.declarations, analyzeDecl)
  quantumVars
}

/**
 * Format quantum type for display
 */
let formatQuantumType = (qt: quantumType): string => {
  switch qt {
  | Collapsed(t) =>
    switch t {
    | TyInt => "Int"
    | TyFloat => "Float"
    | TyString => "String"
    | TyBool => "Bool"
    | _ => "Unknown"
    }

  | Superposition({possibleTypes, _}) =>
    possibleTypes
    ->Array.map(t => switch t {
      | TyInt => "Int"
      | TyFloat => "Float"
      | TyString => "String"
      | TyBool => "Bool"
      | _ => "Unknown"
    })
    ->Array.joinWith(" | ")
  }
}

/**
 * Predict collapse target based on context
 */
let predictCollapse = (
  qt: quantumType,
  context: observationContext,
  seed: int,
): (typeExpr, string) => {
  let collapsed = collapseType(qt, context, seed)

  let reason = switch context {
  | Arithmetic => "Used in arithmetic operation"
  | StringOp => "Used in string operation"
  | Comparison => "Used in comparison"
  | Print => "Printed to console"
  | Assignment => "Assigned to typed variable"
  | FunctionArg => "Passed as function argument"
  }

  (collapsed, reason)
}

/**
 * Generate visualization of type collapse
 */
let visualizeCollapse = (
  varName: string,
  before: quantumType,
  after: typeExpr,
  context: observationContext,
): string => {
  let beforeStr = formatQuantumType(before)
  let afterStr = formatQuantumType(Collapsed(after))
  let contextStr = switch context {
  | Arithmetic => "arithmetic"
  | StringOp => "string operation"
  | Comparison => "comparison"
  | Print => "println"
  | Assignment => "assignment"
  | FunctionArg => "function call"
  }

  `
🌀 TYPE COLLAPSE DETECTED

Variable: ${varName}
Before:   ${beforeStr} (superposition)
After:    ${afterStr} (collapsed)
Context:  ${contextStr}

The act of observation collapsed the type!
`
}
