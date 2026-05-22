// SPDX-License-Identifier: MPL-2.0
// Analyzer.res - Static analysis and paradox detection

open Types
open Stability

// ============================================
// Program Analysis
// ============================================

type analysisResult = {
  paradoxes: array<paradox>,
  consequences: array<consequence>,
  stabilityReport: stabilityReport,
  discoveries: array<discovery>,
}

// Analyze AST for paradoxes and instabilities
let analyzeProgram = (prog: program, state: runtimeState): analysisResult => {
  let paradoxes = []
  let consequences = []

  // Walk the AST and detect patterns
  let rec analyzeDecl = (decl: decl): unit => {
    switch decl {
    | MainBlock({body, _}) => Array.forEach(body, analyzeStmt)
    | FunctionDecl({body, _}) => Array.forEach(body, analyzeStmt)
    | StructDecl(_) => ()
    | StmtDecl(stmt) => analyzeStmt(stmt)
    }
  }

  and analyzeStmt = (stmt: stmt): unit => {
    switch stmt {
    | LetStmt({mutable_, name, value, loc, _}) =>
      // Check for context-collapse keywords
      if isContextCollapse(name, 0, loc.start.line, loc.start.column) {
        Array.push(paradoxes, ContextCollapseKeyword({
          keyword: name,
          line: loc.start.line,
          depth: 0,
          isKeyword: false,
          reason: "Variable name is also a keyword in other contexts",
        }))
      }

      // Check for mutable state
      if mutable_ {
        Array.push(state.stabilityFactors, MutableState({
          mutations: 1,
          readers: 0, // Will be updated during dependency analysis
        }))
      }

      analyzeExpr(value)

    | IfStmt({cond, then_, elseifs, else_, _}) =>
      analyzeExpr(cond)
      Array.forEach(then_, analyzeStmt)
      Array.forEach(elseifs, ((cond, body)) => {
        analyzeExpr(cond)
        Array.forEach(body, analyzeStmt)
      })
      Option.forEach(else_, body => Array.forEach(body, analyzeStmt))

    | WhileStmt({cond, body, _}) =>
      analyzeExpr(cond)
      Array.forEach(body, analyzeStmt)

    | ForStmt({iter, body, _}) =>
      analyzeExpr(iter)
      Array.forEach(body, analyzeStmt)

    | GutterBlock(_) => ()  // Gutter blocks are meant to be unstable
    | ExprStmt(expr) => analyzeExpr(expr)
    | _ => ()
    }
  }

  and analyzeExpr = (expr: expr): unit => {
    switch expr {
    | Binary(left, op, right, loc) =>
      // Check for positional operator ambiguity
      let opStr = switch op {
      | Add => "+"
      | Sub => "-"
      | Mul => "*"
      | _ => ""
      }

      if opStr != "" {
        let behavior = operatorBehaviorFromPosition(opStr, loc.start.line, loc.start.column)
        let alternatives = alternativeOperatorBehaviors(opStr, loc.start.line, loc.start.column)

        Array.push(paradoxes, PositionalOperator({
          operator: opStr,
          line: loc.start.line,
          column: loc.start.column,
          behavior: behavior,
          alternatives: alternatives,
        }))
      }

      analyzeExpr(left)
      analyzeExpr(right)

    | Call(func, args, _) =>
      analyzeExpr(func)
      Array.forEach(args, analyzeExpr)

    | Array(elements, _) =>
      Array.forEach(elements, analyzeExpr)

    | _ => ()
    }
  }

  // Analyze all declarations
  Array.forEach(prog.declarations, analyzeDecl)

  // Generate stability report
  let report = generateReport(state)

  // Check for discoveries
  let discoveries = switch checkDiscovery(state, report) {
  | Some(d) => [d]
  | None => []
  }

  {
    paradoxes: paradoxes,
    consequences: consequences,
    stabilityReport: report,
    discoveries: discoveries,
  }
}

// ============================================
// Causality Tracing
// ============================================

type causalityChain = {
  symptom: string,
  symptomLocation: location,
  chain: array<(string, location)>,
  rootCause: string,
  rootLocation: location,
}

let traceCausality = (
  symptom: string,
  symptomLoc: location,
  prog: program,
): causalityChain => {
  // Simplified causality tracing - in real implementation,
  // would track data flow through AST

  {
    symptom: symptom,
    symptomLocation: symptomLoc,
    chain: [
      ("Type mismatch propagated from", symptomLoc),
    ],
    rootCause: "Unvalidated user input",
    rootLocation: symptomLoc,
  }
}

// ============================================
// Alternative Code Generation
// ============================================

type alternative = {
  description: string,
  code: string,
  stabilityScore: int,
  improvements: array<string>,
}

let generateAlternatives = (stmt: stmt, currentStability: int): array<alternative> => {
  switch stmt {
  | LetStmt({mutable_: true, name, value, _}) =>
    // Suggest immutable alternative
    [
      {
        description: "Use immutable binding",
        code: `let ${name} = ...`,
        stabilityScore: currentStability + 15,
        improvements: [
          "No mutation risk",
          "Thread-safe by default",
          "Easier to reason about",
        ],
      },
    ]

  | _ => []
  }
}

// ============================================
// Forensic Analysis (Deep Dive)
// ============================================

type forensicTrace = {
  target: string,
  targetLocation: location,
  instabilityFactors: array<(string, int, string)>,  // (description, impact, reason)
  probabilityMap: array<(string, int)>,  // (scenario, probability %)
  suggestedFix: string,
}

let forensicAnalysis = (
  target: string,
  targetLoc: location,
  analysis: analysisResult,
): forensicTrace => {
  // Find all factors affecting this target
  let factors = analysis.stabilityReport.factors
    ->Array.map(factor => {
      let impact = stabilityImpact(factor)
      let description = switch factor {
      | MutableState(_) => "Mutable state detected"
      | TypeInstability(_) => "Type changed dynamically"
      | _ => "Unknown factor"
      }
      (description, impact, "See stability report")
    })

  // Generate probability map for different behaviors
  let probabilityMap = [
    ("Standard behavior", 40),
    ("Positional override", 30),
    ("Context collapse", 20),
    ("Temporal corruption", 10),
  ]

  {
    target: target,
    targetLocation: targetLoc,
    instabilityFactors: factors,
    probabilityMap: probabilityMap,
    suggestedFix: "Move to different position or add explicit type",
  }
}

// ============================================
// Visualization Formatters
// ============================================

let formatParadox = (paradox: paradox): string =>
  switch paradox {
  | ContextCollapseKeyword({keyword, line, reason, _}) =>
    `⚡ QUANTUM KEYWORD at line ${Int.toString(line)}\n` ++
    `   '${keyword}' is both keyword AND identifier\n` ++
    `   Reason: ${reason}`

  | PositionalOperator({operator, line, column, behavior, alternatives}) =>
    `🎲 POSITIONAL OPERATOR at line ${Int.toString(line)}:${Int.toString(column)}\n` ++
    `   '${operator}' behaves as: ${behavior}\n` ++
    `   Alternative positions:\n` ++
    alternatives
      ->Array.map(((col, beh)) => `     Col ${Int.toString(col)}: ${beh}`)
      ->Array.joinWith("\n")

  | TypeSuperposition({variable, possibleTypes, collapseTarget, _}) =>
    `🌀 TYPE SUPERPOSITION\n` ++
    `   Variable '${variable}' exists as: ${Array.joinWith(possibleTypes, " | ")}\n` ++
    `   Will collapse to: ${collapseTarget}`

  | ScopeLeakage({variable, declaredLine, accessLine, leakReason, _}) =>
    `🕐 SCOPE LEAKAGE\n` ++
    `   Variable '${variable}' declared at line ${Int.toString(declaredLine)}\n` ++
    `   Accessible at line ${Int.toString(accessLine)}\n` ++
    `   Reason: ${leakReason}`

  | TemporalCorruption({variable, mechanism, _}) =>
    `⏰ TEMPORAL CORRUPTION\n` ++
    `   Variable '${variable}' affected by historical state\n` ++
    `   Mechanism: ${mechanism}`
  }

let formatStabilityReport = (report: stabilityReport): string => {
  let bar = stabilityBar(report.score)

  let factorList = report.factors
    ->Array.map(factor => {
      let emoji = consequenceEmoji(factor)
      let impact = stabilityImpact(factor)
      let desc = switch factor {
      | MutableState({mutations, readers}) =>
        `Mutable state (${Int.toString(mutations)} mutations, ${Int.toString(readers)} readers)`
      | TypeInstability({reassignments}) =>
        `Type instability (${Int.toString(reassignments)} reassignments)`
      | NullPropagation({depth}) =>
        `Null propagation (depth ${Int.toString(depth)})`
      | _ => "Other factor"
      }
      `${emoji} ${desc}: ${Int.toString(impact)} points`
    })
    ->Array.joinWith("\n   ")

  `
🎯 STABILITY REPORT
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

${bar}

Breakdown:
   ${factorList}

💡 Top Recommendations:
${report.recommendations->Array.slice(~start=0, ~end=3)->Array.mapWithIndex((rec, i) =>
  `   ${Int.toString(i + 1)}. ${rec}`
)->Array.joinWith("\n")}
`
}
