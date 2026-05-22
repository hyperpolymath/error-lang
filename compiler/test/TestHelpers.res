// SPDX-License-Identifier: MPL-2.0
// TestHelpers.res - Shared test utilities for Error-Lang compiler tests

open Types

// ============================================
// Assertion Helpers
// ============================================

/// Total test counter (mutable for reporting)
let totalTests = ref(0)
let passedTests = ref(0)
let failedTests = ref(0)
let currentSuite = ref("")

/// Begin a test suite (prints header)
let suite = (name: string): unit => {
  currentSuite := name
  Console.log(`\n=== ${name} ===`)
}

/// Assert that two values are equal
let assertEqual = (label: string, actual: 'a, expected: 'a): unit => {
  totalTests := totalTests.contents + 1
  if actual == expected {
    passedTests := passedTests.contents + 1
    Console.log(`  PASS: ${label}`)
  } else {
    failedTests := failedTests.contents + 1
    Console.error(`  FAIL: ${label}`)
  }
}

/// Assert a boolean condition is true
let assertTrue = (label: string, condition: bool): unit => {
  totalTests := totalTests.contents + 1
  if condition {
    passedTests := passedTests.contents + 1
    Console.log(`  PASS: ${label}`)
  } else {
    failedTests := failedTests.contents + 1
    Console.error(`  FAIL: ${label}`)
  }
}

/// Assert a boolean condition is false
let assertFalse = (label: string, condition: bool): unit =>
  assertTrue(label, !condition)

/// Print final summary and exit with appropriate code
let summarize = (): unit => {
  Console.log(`\n--- Results ---`)
  Console.log(`Total:  ${totalTests.contents->Int.toString}`)
  Console.log(`Passed: ${passedTests.contents->Int.toString}`)
  Console.log(`Failed: ${failedTests.contents->Int.toString}`)
  if failedTests.contents > 0 {
    Console.error(`\nSOME TESTS FAILED`)
  } else {
    Console.log(`\nALL TESTS PASSED`)
  }
}

// ============================================
// Lexer Helpers
// ============================================

/// Lex source code and return tokens (without EOF)
let lexSource = (source: string): array<token> => {
  let (tokens, _diagnostics) = Lexer.lex(source, "<test>", 1)
  tokens
}

/// Lex source code and return both tokens and diagnostics
let lexWithDiagnostics = (source: string): (array<token>, array<diagnostic>) => {
  Lexer.lex(source, "<test>", 1)
}

/// Get just the token types from a token array (excluding Newline and EOF)
let tokenTypes = (tokens: array<token>): array<tokenType> => {
  tokens
  ->Array.filter(t =>
    switch t.type_ {
    | Newline | EOF => false
    | _ => true
    }
  )
  ->Array.map(t => t.type_)
}

/// Get token types including newlines (but not EOF)
let tokenTypesWithNewlines = (tokens: array<token>): array<tokenType> => {
  tokens
  ->Array.filter(t =>
    switch t.type_ {
    | EOF => false
    | _ => true
    }
  )
  ->Array.map(t => t.type_)
}

/// Get lexemes from a token array (excluding Newline and EOF)
let tokenLexemes = (tokens: array<token>): array<string> => {
  tokens
  ->Array.filter(t =>
    switch t.type_ {
    | Newline | EOF => false
    | _ => true
    }
  )
  ->Array.map(t => t.lexeme)
}

// ============================================
// Parser Helpers
// ============================================

/// Parse source code and return the program AST
let parseSource = (source: string): program => {
  let (tokens, _diag) = Lexer.lex(source, "<test>", 1)
  let (prog, _diag) = Parser.parse(tokens, "<test>", 1)
  prog
}

/// Parse source code and return both program and diagnostics
let parseWithDiagnostics = (source: string): (program, array<diagnostic>) => {
  let (tokens, lexDiag) = Lexer.lex(source, "<test>", 1)
  let (prog, parseDiag) = Parser.parse(tokens, "<test>", 1)
  (prog, Array.concat(lexDiag, parseDiag))
}

/// Get first declaration from parsed source
let firstDecl = (source: string): option<decl> => {
  let prog = parseSource(source)
  prog.declarations->Array.get(0)
}

/// Get declarations count from parsed source
let declCount = (source: string): int => {
  let prog = parseSource(source)
  Array.length(prog.declarations)
}
