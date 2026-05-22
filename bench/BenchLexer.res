// SPDX-License-Identifier: MPL-2.0
// BenchLexer.res -- Lexer performance benchmark for Error-Lang
//
// Measures:
//   - Tokens per second on synthetic source (10K+ tokens)
//   - Time to lex an empty file vs a large file
//
// Run:
//   deno task res:build && node bench/BenchLexer.res.mjs
//   (or adjust to match your build pipeline)

// External binding for high-resolution timing
@val external performanceNow: unit => float = "performance.now"

// Generate a realistic Error-Lang source string.
// Uses Error-Lang's error-handling and run-based keywords.
let generateSource = (numStatements: int): string => {
  let buf = []
  let keywords = [
    "try", "catch", "throw", "error", "warn", "info", "debug",
    "fn", "let", "if", "else", "for", "while", "return", "match",
    "type", "struct", "enum", "import", "export", "const", "mut",
    "true", "false", "break", "continue",
  ]
  let operators = [
    "->", "=>", "==", "!=", "<=", ">=", "&&", "||",
    "+", "-", "*", "/", "%", "=", "<", ">", "!", "|",
  ]
  for i in 0 to numStatements - 1 {
    let kw = keywords[mod(i, Array.length(keywords))]->Option.getOr("let")
    let op = operators[mod(i, Array.length(operators))]->Option.getOr("+")
    buf->Array.push(`${kw} x_${Int.toString(i)} ${op} ${Int.toString(i * 7)};\n`)->ignore
    if mod(i, 10) == 0 {
      buf->Array.push("# comment line\n")->ignore
      buf->Array.push(`"string_${Int.toString(i)}" `)->ignore
      buf->Array.push("{ [ ( ) ] } , ; : . \n")->ignore
    }
  }
  Array.join(buf, "")
}

// Count tokens from the Error-Lang lexer
let countTokens = (source: string): int => {
  let (tokens, _diagnostics) = Lexer.lex(source, "<bench>", 1)
  Array.length(tokens)
}

// Time a function, returning elapsed milliseconds
let timeIt = (f: unit => 'a): ('a, float) => {
  let t0 = performanceNow()
  let result = f()
  let t1 = performanceNow()
  (result, t1 -. t0)
}

let iterations = 100

// --- Benchmark 1: Empty file ---
let (_, emptyMs) = timeIt(() => {
  for _ in 1 to iterations {
    countTokens("")->ignore
  }
})

Console.log("=== Error-Lang Lexer Benchmark ===\n")
Console.log("Empty file:")
Console.log(
  `  ${Int.toString(iterations)} iterations in ${Float.toFixedWithPrecision(emptyMs /. 1000.0, ~digits=4)} s (${Float.toFixedWithPrecision(emptyMs /. Int.toFloat(iterations) *. 1000.0, ~digits=2)} us/iter)`,
)

// --- Generate large source ---
let source = generateSource(2000)
let sourceBytes = String.length(source)
let tokenCount = countTokens(source)
Console.log(
  `\nLarge file (${Int.toString(sourceBytes)} bytes, ${Int.toString(tokenCount)} tokens):`,
)

// --- Benchmark 2: Tokens/sec on large file ---
let (_, largeMs) = timeIt(() => {
  for _ in 1 to iterations {
    countTokens(source)->ignore
  }
})

let largeSec = largeMs /. 1000.0
let totalTokens = Int.toFloat(tokenCount * iterations)
let tokensPerSec = totalTokens /. largeSec

Console.log(`  ${Int.toString(iterations)} iterations in ${Float.toFixedWithPrecision(largeSec, ~digits=4)} s`)
Console.log(`  ${Float.toFixedWithPrecision(tokensPerSec, ~digits=2)} tokens/sec`)
Console.log(
  `  ${Float.toFixedWithPrecision(largeSec /. totalTokens *. 1e6, ~digits=2)} us/token`,
)
Console.log(
  `  ${Float.toFixedWithPrecision(Int.toFloat(sourceBytes * iterations) /. largeSec /. 1e6, ~digits=2)} MB/sec`,
)

// --- Note on memory ---
Console.log("\nMemory allocation:")
Console.log("  (JavaScript GC prevents precise per-token measurement)")
Console.log(`  ${Int.toString(tokenCount)} tokens produced from ${Int.toString(sourceBytes)} bytes`)

Console.log("\nDone.")
