// SPDX-License-Identifier: MPL-2.0
// Stability.res - Stability analysis and consequence amplification

open Types

// ============================================
// Paradox Detection (Language Archaeology)
// ============================================

type paradox =
  | ContextCollapseKeyword({
      keyword: string,
      line: int,
      depth: int,
      isKeyword: bool,
      reason: string,
    })
  | PositionalOperator({
      operator: string,
      line: int,
      column: int,
      behavior: string,
      alternatives: array<(int, string)>,
    })
  | TypeSuperposition({
      variable: string,
      line: int,
      possibleTypes: array<string>,
      collapseTarget: string,
    })
  | ScopeLeakage({
      variable: string,
      declaredLine: int,
      accessLine: int,
      runNumber: int,
      leakReason: string,
    })
  | TemporalCorruption({
      variable: string,
      line: int,
      affectedByRun: int,
      mechanism: string,
    })

// Check if keyword collapses to identifier based on context
let isContextCollapse = (word: string, depth: int, line: int, column: int): bool => {
  let keywordHash = (word, depth, column)

  switch word {
  | "end" => depth mod 3 != 0    // 'end' is identifier at depth 1, 2, 4, 5...
  | "let" => column mod 5 != 2   // 'let' is identifier at certain columns
  | "if" => depth >= 10          // Deep nesting makes 'if' an identifier
  | "function" => line mod 7 == 0 // Line divisible by 7 makes it identifier
  | _ => false
  }
}

// Determine operator behavior based on position
let operatorBehaviorFromPosition = (op: string, line: int, column: int): string => {
  let hash = (line * 31 + column) mod 4

  switch (op, hash) {
  | ("+", 0) => "addition"
  | ("+", 1) => "concatenation"
  | ("+", 2) => "subtraction"
  | ("+", 3) => "xor"
  | ("=>", 0) => "lambda"
  | ("=>", 1) => "comparison"
  | (".", 0) => "member-access"
  | (".", 1) => "range"
  | _ => "unknown"
  }
}

// Get alternative operator behaviors at different columns
let alternativeOperatorBehaviors = (op: string, line: int, currentCol: int): array<(int, string)> => {
  [
    (currentCol - 1, operatorBehaviorFromPosition(op, line, currentCol - 1)),
    (currentCol + 1, operatorBehaviorFromPosition(op, line, currentCol + 1)),
    (currentCol + 2, operatorBehaviorFromPosition(op, line, currentCol + 2)),
  ]
}

// Check if scope should leak on this run
let isPrime = (n: int): bool => {
  if n < 2 { false }
  else if n == 2 { true }
  else if n mod 2 == 0 { false }
  else {
    let rec check = (i: int): bool =>
      if i * i > n { true }
      else if n mod i == 0 { false }
      else { check(i + 2) }
    check(3)
  }
}

let isFibonacci = (n: int): bool => {
  // Quick Fibonacci check using golden ratio property
  let isPerfectSquare = (x: int): bool => {
    let sqrt = Float.sqrt(Int.toFloat(x))->Float.toInt
    sqrt * sqrt == x
  }

  isPerfectSquare(5 * n * n + 4) || isPerfectSquare(5 * n * n - 4)
}

let shouldScopeLeak = (varName: string, line: int, runNumber: int): bool => {
  let isPalindrome = (s: string): bool => {
    let chars = String.split(s, "")
    chars == Array.reverse(chars)
  }

  isPrime(runNumber) || isPalindrome(varName) || isFibonacci(line)
}

// ============================================
// Consequence Amplification
// ============================================

type consequence =
  | MutationRipple({
      variable: string,
      mutationLine: int,
      affectedLocations: array<int>,
      stabilityPenalty: int,
    })
  | TypeCascade({
      variable: string,
      typeLine: int,
      cascadeDepth: int,
      conflicts: array<(int, string)>,
    })
  | NullPoison({
      origin: int,
      poisonedPath: array<(int, string)>,
      finalStability: int,
    })
  | GlobalEarthquake({
      globalName: string,
      mutationLine: int,
      destabilizedFunctions: array<string>,
      stabilityLoss: int,
    })

// Analyze mutation ripple effect
let analyzeMutationRipple = (
  varName: string,
  mutationLoc: location,
  readers: array<location>,
): consequence => {
  let affectedLines = readers->Array.map(loc => loc.start.line)
  let penalty = 10 + Array.length(readers) * 5

  MutationRipple({
    variable: varName,
    mutationLine: mutationLoc.start.line,
    affectedLocations: affectedLines,
    stabilityPenalty: penalty,
  })
}

// Track type instability cascade
let analyzeTypeCascade = (
  varName: string,
  reassignments: array<(location, typeExpr)>,
): consequence => {
  let conflicts = reassignments->Array.map(((loc, typ)) => {
    (loc.start.line, "type-conflict")
  })

  TypeCascade({
    variable: varName,
    typeLine: reassignments[0]->Option.map(((loc, _)) => loc.start.line)->Option.getOr(0),
    cascadeDepth: Array.length(reassignments),
    conflicts: conflicts,
  })
}

// ============================================
// Stability Recommendations
// ============================================

let recommendStabilization = (factor: stabilityFactor): array<string> =>
  switch factor {
  | MutableState({mutations, readers}) => [
      "Consider using immutable data structures",
      `This mutation affects ${Int.toString(readers)} other locations`,
      "Alternative: Use functional updates (map, filter, reduce)",
    ]

  | TypeInstability({reassignments}) => [
      "Add explicit type annotation to prevent reassignment",
      `Type changed ${Int.toString(reassignments)} times`,
      "Alternative: Use different variable names for different types",
    ]

  | NullPropagation({depth}) => [
      "Use pattern matching to handle null cases explicitly",
      `Null propagated through ${Int.toString(depth)} levels`,
      "Alternative: Use Option type with match expression",
    ]

  | GlobalState({mutations, dependencies}) => [
      "Pass state as function parameters instead of using globals",
      `${Int.toString(dependencies)} functions depend on this global`,
      "Alternative: Use a state struct passed explicitly",
    ]

  | UnhandledError({paths}) => [
      "Add error handling with Result type",
      `${Int.toString(paths)} error paths unhandled`,
      "Alternative: Propagate errors explicitly with match",
    ]

  | AlgorithmComplexity({time_ms}) => [
      "Consider more efficient algorithm",
      `Execution took ${Float.toString(time_ms)}ms (amplified)`,
      "Alternative: Use hash-based lookup or better data structure",
    ]

  | MemoryLeak({bytes}) => [
      "Resources must be freed explicitly",
      `Leaked ${Int.toString(bytes)} bytes`,
      "Alternative: Use 'with' statement for automatic cleanup",
    ]

  | RaceCondition({conflicts}) => [
      "Synchronize access to shared state",
      `${Int.toString(conflicts)} race conditions detected`,
      "Alternative: Use Mutex or atomic operations",
    ]
  }

// Generate stability report
let generateReport = (state: runtimeState): stabilityReport => {
  let score = calculateStability(state.stabilityFactors)

  let breakdown = Dict.make()
  state.stabilityFactors->Array.forEach(factor => {
    let impact = stabilityImpact(factor)
    let category = switch factor {
    | MutableState(_) => "mutability"
    | TypeInstability(_) => "types"
    | NullPropagation(_) => "null-handling"
    | GlobalState(_) => "global-state"
    | UnhandledError(_) => "error-handling"
    | AlgorithmComplexity(_) => "performance"
    | MemoryLeak(_) => "memory"
    | RaceCondition(_) => "concurrency"
    }
    Dict.set(breakdown, category, impact)
  })

  let recommendations = state.stabilityFactors
    ->Array.map(recommendStabilization)
    ->Array.flat

  {
    score: score,
    factors: state.stabilityFactors,
    breakdown: breakdown,
    recommendations: recommendations,
  }
}

// ============================================
// Discovery System (Achievement Tracking)
// ============================================

type discovery =
  | FirstStabilization
  | CacheDetective
  | PureFunctionalConvert
  | MemoryArchaeologist
  | AsyncEnlightenment
  | ParadoxDiscovered(string)
  | RuleUnlocked(string)

let checkDiscovery = (state: runtimeState, report: stabilityReport): option<discovery> => {
  // First time reaching 100 stability
  if report.score >= 100 && !Array.includes(state.historicalRuns, 100) {
    Some(FirstStabilization)
  }
  // Discovered a paradox
  else if Array.length(state.discoveredRules) > Array.length(state.historicalRuns) {
    Some(RuleUnlocked(state.discoveredRules[Array.length(state.discoveredRules) - 1]->Option.getOr("unknown")))
  }
  else {
    None
  }
}

// ============================================
// Visualization Helpers
// ============================================

let stabilityBar = (score: int): string => {
  let filled = score / 5  // 20 blocks for 100 score
  let empty = 20 - filled
  let bar = String.repeat("█", filled) ++ String.repeat("░", empty)
  `[${bar}] ${Int.toString(score)}/100`
}

let consequenceEmoji = (factor: stabilityFactor): string =>
  switch factor {
  | MutableState(_) => "🔴"
  | TypeInstability(_) => "⚠️"
  | NullPropagation(_) => "☠️"
  | GlobalState(_) => "🌍"
  | UnhandledError(_) => "💥"
  | AlgorithmComplexity(_) => "🐌"
  | MemoryLeak(_) => "💧"
  | RaceCondition(_) => "⚡"
  }
