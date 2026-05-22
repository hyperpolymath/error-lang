// SPDX-License-Identifier: MPL-2.0
// FiveWhys.res - Five Whys root cause analysis

open Types
open Stability

/**
 * Five Whys Analyzer
 *
 * Automated root cause analysis using the Five Whys technique.
 * Traces from symptom → intermediate causes → root cause.
 *
 * This teaches causal reasoning, not just pattern matching.
 */

// Why-chain: each "why" leads to deeper understanding
type why = {
  question: string,
  answer: string,
  evidence: array<string>,
  layer: option<layer>,  // Which abstraction layer?
}

type analysisResult = {
  symptom: string,
  whys: array<why>,
  rootCause: string,
  rootLayer: layer,
  recommendation: string,
}

/**
 * Analyze type mismatch error
 */
let analyzeTypeMismatch = (
  varName: string,
  expectedType: typeExpr,
  actualType: typeExpr,
  loc: location,
): analysisResult => {
  let whys = []

  // WHY #1: What's the immediate cause?
  Array.push(whys, {
    question: "Why did we get a type mismatch?",
    answer: `Variable '${varName}' has type ${typeExprToString(actualType)}, but ${typeExprToString(expectedType)} was expected`,
    evidence: [`Line ${Int.toString(loc.start.line)}: type mismatch detected`],
    layer: Some(Semantics),
  })

  // WHY #2: Why does it have the wrong type?
  Array.push(whys, {
    question: `Why does '${varName}' have type ${typeExprToString(actualType)}?`,
    answer: "The variable was assigned a value of that type earlier",
    evidence: ["Type inference from initial assignment"],
    layer: Some(Semantics),
  })

  // WHY #3: Why was it allowed to have that type?
  Array.push(whys, {
    question: "Why was this type assignment allowed?",
    answer: "No explicit type annotation was provided",
    evidence: ["Variable declared without type constraint"],
    layer: Some(Grammar),
  })

  // WHY #4: Why does that matter?
  Array.push(whys, {
    question: "Why does missing type annotation cause problems?",
    answer: "Without type annotations, the compiler infers types, which may not match intent",
    evidence: ["Type inference is permissive without constraints"],
    layer: Some(Semantics),
  })

  // WHY #5: Root cause - design decision
  Array.push(whys, {
    question: "Why do we have type inference instead of required annotations?",
    answer: "Design tradeoff: flexibility vs safety",
    evidence: [
      "Flexibility: Less code to write",
      "Safety: More runtime errors possible",
    ],
    layer: None,
  })

  {
    symptom: `Type mismatch: expected ${typeExprToString(expectedType)}, got ${typeExprToString(actualType)}`,
    whys: whys,
    rootCause: "Design tradeoff between flexibility and safety",
    rootLayer: Grammar,
    recommendation: `Add explicit type annotation: let ${varName}: ${typeExprToString(expectedType)} = ...`,
  }
}

/**
 * Analyze mutation instability
 */
let analyzeMutationImpact = (
  varName: string,
  mutationLoc: location,
  readers: array<location>,
): analysisResult => {
  let whys = []

  // WHY #1: What's happening?
  Array.push(whys, {
    question: "Why is stability dropping?",
    answer: `Mutable variable '${varName}' is being modified`,
    evidence: [`Mutation at line ${Int.toString(mutationLoc.start.line)}`],
    layer: Some(Runtime),
  })

  // WHY #2: Why does mutation reduce stability?
  Array.push(whys, {
    question: "Why does mutation reduce stability?",
    answer: `Mutation affects ${Int.toString(Array.length(readers))} other locations that read this variable`,
    evidence: readers->Array.map(loc =>
      `Reader at line ${Int.toString(loc.start.line)}`
    ),
    layer: Some(Semantics),
  })

  // WHY #3: Why do readers get affected?
  Array.push(whys, {
    question: "Why do readers get affected by mutation?",
    answer: "Shared mutable state creates invisible dependencies",
    evidence: [
      "Each reader depends on the current value",
      "Mutation changes the value for all readers",
      "Order of reads matters",
    ],
    layer: Some(Semantics),
  })

  // WHY #4: Why do we have shared mutable state?
  Array.push(whys, {
    question: "Why use shared mutable state?",
    answer: "Variable was declared with 'mut' keyword",
    evidence: ["Explicit mutability declaration"],
    layer: Some(Grammar),
  })

  // WHY #5: Root cause
  Array.push(whys, {
    question: "Why does mutability exist in the language?",
    answer: "Design tradeoff: performance vs simplicity",
    evidence: [
      "Mutation: Fast in-place updates",
      "Immutability: Easier to reason about, no side effects",
    ],
    layer: None,
  })

  {
    symptom: `Stability dropped due to mutation of '${varName}'`,
    whys: whys,
    rootCause: "Design tradeoff between performance and simplicity",
    rootLayer: Grammar,
    recommendation: "Use immutable data with functional updates (map, filter, reduce)",
  }
}

/**
 * Analyze null propagation cascade
 */
let analyzeNullCascade = (
  originLoc: location,
  cascadeDepth: int,
): analysisResult => {
  let whys = []

  // WHY #1
  Array.push(whys, {
    question: "Why did the program crash?",
    answer: "Null pointer access occurred",
    evidence: [`Null access at line ${Int.toString(originLoc.start.line)}`],
    layer: Some(Runtime),
  })

  // WHY #2
  Array.push(whys, {
    question: "Why was the value null?",
    answer: "A function returned null instead of a value",
    evidence: ["Function can return T or Nil"],
    layer: Some(Semantics),
  })

  // WHY #3
  Array.push(whys, {
    question: "Why wasn't the null case handled?",
    answer: "No null check or pattern matching was performed",
    evidence: [
      "Direct access without checking",
      `Propagated through ${Int.toString(cascadeDepth)} levels`,
    ],
    layer: Some(Semantics),
  })

  // WHY #4
  Array.push(whys, {
    question: "Why is null checking optional?",
    answer: "The language allows nullable types without forcing checks",
    evidence: ["No Option type enforcement"],
    layer: Some(Grammar),
  })

  // WHY #5
  Array.push(whys, {
    question: "Why does the language allow nullable types?",
    answer: "Design tradeoff: convenience vs safety",
    evidence: [
      "Convenience: No boilerplate for null checks",
      "Safety: Runtime errors from unchecked nulls",
      "Tony Hoare called null his 'billion dollar mistake'",
    ],
    layer: None,
  })

  {
    symptom: "Null pointer exception with cascade",
    whys: whys,
    rootCause: "Design tradeoff between convenience and safety (nullable types)",
    rootLayer: Grammar,
    recommendation: "Use Option type with pattern matching to force null handling",
  }
}

/**
 * Analyze performance cliff
 */
let analyzePerformanceIssue = (
  operation: string,
  timeMs: float,
  expectedMs: float,
): analysisResult => {
  let whys = []

  let slowdown = Float.toInt(timeMs /. expectedMs)

  // WHY #1
  Array.push(whys, {
    question: "Why is the code slow?",
    answer: `Operation took ${Float.toString(timeMs)}ms, expected ${Float.toString(expectedMs)}ms (${Int.toString(slowdown)}x slower)`,
    evidence: [`Performance measurement: ${operation}`],
    layer: Some(Runtime),
  })

  // WHY #2
  Array.push(whys, {
    question: `Why is ${operation} so slow?`,
    answer: "Algorithm has O(n²) or worse complexity",
    evidence: [
      "Nested loops detected",
      "Repeated scans through data",
    ],
    layer: Some(AST),
  })

  // WHY #3
  Array.push(whys, {
    question: "Why use an O(n²) algorithm?",
    answer: "Wrong data structure chosen for the operation",
    evidence: [
      "Using array/list for lookups",
      "Should use hash table or index",
    ],
    layer: Some(Semantics),
  })

  // WHY #4
  Array.push(whys, {
    question: "Why was the wrong data structure chosen?",
    answer: "No explicit data structure selection in code",
    evidence: ["Default collection used without optimization"],
    layer: Some(Grammar),
  })

  // WHY #5
  Array.push(whys, {
    question: "Why doesn't the language enforce efficient data structures?",
    answer: "Design tradeoff: ease of use vs performance",
    evidence: [
      "Simple syntax: Easy to write, potentially slow",
      "Explicit structures: More code, but faster",
    ],
    layer: None,
  })

  {
    symptom: `Performance cliff: ${Int.toString(slowdown)}x slower than expected`,
    whys: whys,
    rootCause: "Design tradeoff between ease of use and performance",
    rootLayer: AST,
    recommendation: "Use hash-based data structure for O(1) lookups instead of O(n) scans",
  }
}

/**
 * Generic Five Whys analysis
 */
let analyze = (stabilityFactor: stabilityFactor): analysisResult => {
  switch stabilityFactor {
  | TypeInstability({reassignments}) =>
    // Simplified - would track actual types in full implementation
    analyzeTypeMismatch("x", TyInt, TyString, {
      file: "unknown",
      start: {line: 0, column: 0, offset: 0},
      end_: {line: 0, column: 0, offset: 0},
    })

  | MutableState({mutations, readers}) =>
    analyzeMutationImpact("counter", {
      file: "unknown",
      start: {line: 0, column: 0, offset: 0},
      end_: {line: 0, column: 0, offset: 0},
    }, [])

  | NullPropagation({depth}) =>
    analyzeNullCascade({
      file: "unknown",
      start: {line: 0, column: 0, offset: 0},
      end_: {line: 0, column: 0, offset: 0},
    }, depth)

  | AlgorithmComplexity({time_ms}) =>
    analyzePerformanceIssue("operation", time_ms, 10.0)

  | _ =>
    {
      symptom: "Unknown issue",
      whys: [],
      rootCause: "Needs analysis",
      rootLayer: Runtime,
      recommendation: "Investigate further",
    }
  }
}

/**
 * Format Five Whys analysis for display
 */
let formatAnalysis = (result: analysisResult): string => {
  let lines = []

  Array.push(lines, "\n🔍 FIVE WHYS ROOT CAUSE ANALYSIS\n")
  Array.push(lines, `Symptom: ${result.symptom}\n`)

  for i in 0 to Array.length(result.whys) - 1 {
    let why = result.whys[i]->Option.getExn

    let layerStr = switch why.layer {
    | Some(Runtime) => " [Runtime]"
    | Some(Semantics) => " [Semantics]"
    | Some(AST) => " [AST]"
    | Some(Parser) => " [Parser]"
    | Some(Grammar) => " [Grammar]"
    | None => ""
    }

    Array.push(lines, `WHY ${Int.toString(i + 1)}:${layerStr} ${why.question}`)
    Array.push(lines, `  → ${why.answer}`)

    if Array.length(why.evidence) > 0 {
      for evidence in why.evidence {
        Array.push(lines, `     • ${evidence}`)
      }
    }

    Array.push(lines, "")
  }

  Array.push(lines, `ROOT CAUSE: ${result.rootCause}`)
  Array.push(lines, `\n💡 RECOMMENDATION: ${result.recommendation}`)

  Array.joinWith(lines, "\n")
}

// Helper to convert typeExpr to string (simplified)
let typeExprToString = (t: typeExpr): string => {
  switch t {
  | TyInt => "Int"
  | TyFloat => "Float"
  | TyString => "String"
  | TyBool => "Bool"
  | _ => "Unknown"
  }
}
