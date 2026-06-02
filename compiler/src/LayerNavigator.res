// SPDX-License-Identifier: MPL-2.0
// LayerNavigator.res - Navigate through abstraction layers

open Types

/**
 * Layer Navigator
 *
 * Shows how code transforms through the 5 layers of abstraction:
 * Grammar → Parser → AST → Semantics → Runtime
 *
 * Students can click any code element and see it across all layers.
 * This teaches how abstractions work and where problems originate.
 */

// The five layers of abstraction
type layer =
  | Grammar    // EBNF rules that define what's valid
  | Parser     // How text becomes structure (parse tree)
  | AST        // Abstract syntax tree (simplified structure)
  | Semantics  // Type checking, scope analysis
  | Runtime    // Actual execution values

// Layer view representation
type layerView = {
  layer: layer,
  content: string,
  highlighted: option<(int, int)>,  // Start/end position in this layer
  metadata: dict<string, string>,
}

// Navigation state - which layer we're currently viewing
type navigationState = {
  currentLayer: layer,
  selectedNode: option<string>,  // ID of selected AST node
  layerViews: array<layerView>,
}

/**
 * Create layer views for a given AST node
 */
let createLayerViews = (node: stmt, sourceCode: string): array<layerView> => {
  let views = []

  // Layer 1: Grammar (EBNF rule that matched)
  let grammarRule = getGrammarRule(node)
  Array.push(views, {
    layer: Grammar,
    content: grammarRule,
    highlighted: None,
    metadata: Dict.fromArray([("rule-type", "statement")]),
  })

  // Layer 2: Parser (parse tree structure)
  let parseTree = formatParseTree(node)
  Array.push(views, {
    layer: Parser,
    content: parseTree,
    highlighted: None,
    metadata: Dict.fromArray([("tree-depth", "3")]),
  })

  // Layer 3: AST (abstract syntax tree node)
  let astRepr = formatASTNode(node)
  Array.push(views, {
    layer: AST,
    content: astRepr,
    highlighted: None,
    metadata: Dict.fromArray([("node-type", nodeTypeName(node))]),
  })

  // Layer 4: Semantics (type analysis)
  let semanticInfo = analyzeSemantics(node)
  Array.push(views, {
    layer: Semantics,
    content: semanticInfo,
    highlighted: None,
    metadata: Dict.fromArray([("type-check", "pending")]),
  })

  // Layer 5: Runtime (execution trace)
  Array.push(views, {
    layer: Runtime,
    content: "Execution: [not yet run]",
    highlighted: None,
    metadata: Dict.fromArray([("state", "pending")]),
  })

  views
}

/**
 * Get EBNF grammar rule for a statement
 */
let getGrammarRule = (stmt: stmt): string => {
  switch stmt {
  | LetStmt({mutable_, _}) =>
    if mutable_ {
      `letStmt ::= "let" "mut" identifier "=" expression`
    } else {
      `letStmt ::= "let" identifier "=" expression`
    }

  | IfStmt(_) =>
    `ifStmt ::= "if" expression
                statement*
                ("elseif" expression statement*)*
                ("else" statement*)?
                "end"`

  | WhileStmt(_) =>
    `whileStmt ::= "while" expression
                   statement*
                   "end"`

  | ForStmt(_) =>
    `forStmt ::= "for" identifier "in" expression
                 statement*
                 "end"`

  | PrintStmt({println, _}) =>
    if println {
      `printStmt ::= "println" "(" expression ("," expression)* ")"`
    } else {
      `printStmt ::= "print" "(" expression ("," expression)* ")"`
    }

  | GutterBlock(_) =>
    `gutterBlock ::= "gutter"
                     statement*
                     "end"`

  | ExprStmt(_) =>
    `exprStmt ::= expression`

  | _ =>
    `statement ::= /* unknown */`
  }
}

/**
 * Format parse tree for display
 */
let formatParseTree = (stmt: stmt): string => {
  switch stmt {
  | LetStmt({name, value, mutable_, _}) =>
    let mutStr = if mutable_ { "mut " } else { "" }
    `letStmt
├─ "let"
├─ ${mutStr}identifier("${name}")
├─ "="
└─ ${formatExprParseTree(value, 1)}`

  | PrintStmt({println, args, _}) =>
    let fname = if println { "println" } else { "print" }
    `printStmt
├─ "${fname}"
├─ "("
├─ args: ${Int.toString(Array.length(args))}
└─ ")"`

  | _ =>
    `statement (simplified)`
  }
}

and formatExprParseTree = (expr: expr, depth: int): string => {
  let indent = String.repeat("   ", depth)

  switch expr {
  | IntLit(n, _) =>
    `literal(${Int.toString(n)})`

  | StringLit(s, _) =>
    `literal("${s}")`

  | Ident(name, _) =>
    `identifier("${name}")`

  | Binary(left, op, right, _) =>
    `binary
${indent}├─ ${formatExprParseTree(left, depth + 1)}
${indent}├─ operator(${binaryOpToString(op)})
${indent}└─ ${formatExprParseTree(right, depth + 1)}`

  | _ =>
    `expression`
  }
}

/**
 * Format AST node for display
 */
let formatASTNode = (stmt: stmt): string => {
  switch stmt {
  | LetStmt({name, value, type_, mutable_, loc}) =>
    let typeStr = switch type_ {
    | Some(t) => `: ${typeExprToString(t)}`
    | None => ""
    }

    `LetStmt {
  name: "${name}",
  mutable: ${mutable_ ? "true" : "false"},
  type: ${typeStr},
  value: ${formatExprAST(value)},
  loc: line ${Int.toString(loc.start.line)}
}`

  | PrintStmt({println, args, _}) =>
    `PrintStmt {
  println: ${println ? "true" : "false"},
  args: [${Int.toString(Array.length(args))} expressions]
}`

  | _ =>
    `Statement { ... }`
  }
}

and formatExprAST = (expr: expr): string => {
  switch expr {
  | IntLit(n, _) => `IntLit(${Int.toString(n)})`
  | StringLit(s, _) => `StringLit("${s}")`
  | Ident(name, _) => `Ident("${name}")`
  | Binary(left, op, right, _) =>
    `Binary(${formatExprAST(left)}, ${binaryOpToString(op)}, ${formatExprAST(right)})`
  | _ => `Expr(...)`
  }
}

/**
 * Analyze semantics for a node
 */
let analyzeSemantics = (stmt: stmt): string => {
  switch stmt {
  | LetStmt({name, value, type_, _}) =>
    let inferredType = inferExprType(value)
    let typeCheck = switch type_ {
    | Some(annotated) =>
      if typeExprsMatch(annotated, inferredType) {
        `✓ Type check passed: ${typeExprToString(annotated)}`
      } else {
        `✗ Type mismatch: expected ${typeExprToString(annotated)}, got ${typeExprToString(inferredType)}`
      }
    | None =>
      `Type inferred: ${typeExprToString(inferredType)}`
    }

    `Variable: ${name}
Scope: local
${typeCheck}`

  | _ =>
    `Semantic analysis: [pending]`
  }
}

/**
 * Infer type of an expression (simplified)
 */
let inferExprType = (expr: expr): typeExpr => {
  switch expr {
  | IntLit(_, _) => TyInt
  | FloatLit(_, _) => TyFloat
  | StringLit(_, _) => TyString
  | BoolLit(_, _) => TyBool
  | _ => TyInt  // Simplified
  }
}

/**
 * Check if two type expressions match
 */
let typeExprsMatch = (t1: typeExpr, t2: typeExpr): bool => {
  switch (t1, t2) {
  | (TyInt, TyInt) => true
  | (TyFloat, TyFloat) => true
  | (TyString, TyString) => true
  | (TyBool, TyBool) => true
  | _ => false
  }
}

/**
 * Convert type expression to string
 */
let typeExprToString = (t: typeExpr): string => {
  switch t {
  | TyInt => "Int"
  | TyFloat => "Float"
  | TyString => "String"
  | TyBool => "Bool"
  | TyArray(inner) => `Array<${typeExprToString(inner)}>`
  | TyEcho(a, b) =>
    switch (a, b) {
    | (None, _) => "Echo"
    | (Some(ta), None) => `Echo<${typeExprToString(ta)}>`
    | (Some(ta), Some(tb)) => `Echo<${typeExprToString(ta)}, ${typeExprToString(tb)}>`
    }
  | TyEchoResidue(a, b) =>
    switch (a, b) {
    | (None, _) => "EchoR"
    | (Some(ta), None) => `EchoR<${typeExprToString(ta)}>`
    | (Some(ta), Some(tb)) => `EchoR<${typeExprToString(ta)}, ${typeExprToString(tb)}>`
    }
  | TyIdent(name) => name
  }
}

/**
 * Get node type name
 */
let nodeTypeName = (stmt: stmt): string => {
  switch stmt {
  | LetStmt(_) => "LetStmt"
  | AssignStmt(_) => "AssignStmt"
  | IfStmt(_) => "IfStmt"
  | WhileStmt(_) => "WhileStmt"
  | ForStmt(_) => "ForStmt"
  | PrintStmt(_) => "PrintStmt"
  | GutterBlock(_) => "GutterBlock"
  | ExprStmt(_) => "ExprStmt"
  | _ => "Statement"
  }
}

/**
 * Binary operator to string
 */
let binaryOpToString = (op: binaryOp): string => {
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
  | _ => "op"
  }
}

/**
 * Navigate to a specific layer
 */
let navigateToLayer = (state: navigationState, targetLayer: layer): navigationState => {
  {
    ...state,
    currentLayer: targetLayer,
  }
}

/**
 * Get layer name for display
 */
let layerName = (layer: layer): string => {
  switch layer {
  | Grammar => "Grammar (EBNF)"
  | Parser => "Parser (Parse Tree)"
  | AST => "AST (Abstract Syntax)"
  | Semantics => "Semantics (Type Check)"
  | Runtime => "Runtime (Execution)"
  }
}

/**
 * Get layer index (for visualization)
 */
let layerIndex = (layer: layer): int => {
  switch layer {
  | Grammar => 0
  | Parser => 1
  | AST => 2
  | Semantics => 3
  | Runtime => 4
  }
}
