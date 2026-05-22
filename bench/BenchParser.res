// SPDX-License-Identifier: MPL-2.0
// BenchParser.res -- Parser benchmark harness for Error-Lang
//
// Generates a large synthetic Error-Lang program and measures
// parse throughput: LOC/sec, total parse time, AST node count.
//
// Error-Lang syntax: function/end, let/let mut, if/elseif/else/end,
// while/end, for/in/end, return, break, continue, println(), gutter/end.
//
// Usage:  node compiler/src/BenchParser.mjs  (after rescript build)

open Types

// Generate a synthetic Error-Lang program with `numFns` function declarations.
let generateProgram = (numFns: int): string => {
  let buf = ref("")

  // Struct declarations
  for i in 0 to numFns / 10 - 1 {
    buf :=
      buf.contents ++
      `struct Data${Int.toString(i)}\n` ++
      `  value: int\n` ++
      `  label: string\n` ++
      `end\n\n`
  }

  // Function definitions
  for i in 0 to numFns - 1 {
    let iStr = Int.toString(i)
    let factor = Int.toString(i + 1)
    let delta = Int.toString(mod(i, 7) + 1)

    buf :=
      buf.contents ++
      `function compute_${iStr}(a, b)\n` ++
      `  let x = a + b * ${factor}\n` ++
      `  let y = x - ${delta}\n` ++
      `  if x == y\n` ++
      `    println(x)\n` ++
      `    return x\n` ++
      `  elseif x > y\n` ++
      `    let z = x + y\n` ++
      `    return z\n` ++
      `  else\n` ++
      `    let mut w = x * y\n` ++
      `    return w\n` ++
      `  end\n` ++
      `end\n\n`

    // For-loop every 8th function
    if mod(i, 8) == 0 {
      buf :=
        buf.contents ++
        `function loop_${iStr}(n)\n` ++
        `  let mut total = 0\n` ++
        `  for i in n\n` ++
        `    let total = total + i\n` ++
        `  end\n` ++
        `  return total\n` ++
        `end\n\n`
    }

    // While-loop every 10th function
    if mod(i, 10) == 0 {
      buf :=
        buf.contents ++
        `function countdown_${iStr}(n)\n` ++
        `  let mut count = n\n` ++
        `  while count > 0\n` ++
        `    let count = count - 1\n` ++
        `  end\n` ++
        `  return count\n` ++
        `end\n\n`
    }
  }

  // Main block
  buf := buf.contents ++ `main\n` ++ `  println("benchmark")\n` ++ `end\n`

  buf.contents
}

// Count lines in a string.
let countLines = (s: string): int => {
  let lines = String.split(s, "\n")
  Array.length(lines)
}

// Main benchmark entry point.
let run = () => {
  let numFns = 55
  let iterations = 50
  let source = generateProgram(numFns)
  let loc = countLines(source)

  Js.log("=== Error-Lang Parser Benchmark ===")
  Js.log(`Source: ${Int.toString(loc)} LOC, ${Int.toString(String.length(source))} bytes`)
  Js.log(`Iterations: ${Int.toString(iterations)}`)
  Js.log("")

  // Warm up: lex then parse
  let (warmTokens, _warmLexDiags) = Lexer.lex(source, "<bench>", 0)
  let (warmAst, _warmDiags) = Parser.parse(warmTokens, "<bench>", 0)
  Js.log(`AST nodes (decls): ${Int.toString(Array.length(warmAst.declarations))}`)

  // Timed run
  let startTime = Js.Date.now()
  for _ in 1 to iterations {
    let (tokens, _lexDiags) = Lexer.lex(source, "<bench>", 0)
    let (ast, _diags) = Parser.parse(tokens, "<bench>", 0)
    ignore(ast)
  }
  let endTime = Js.Date.now()

  let totalMs = endTime -. startTime
  let totalSec = totalMs /. 1000.0
  let perIter = totalSec /. Int.toFloat(iterations)
  let locPerSec = Int.toFloat(loc * iterations) /. totalSec

  Js.log(`Total parse time : ${Float.toFixedWithPrecision(totalSec, ~digits=4)} s`)
  Js.log(`Time per parse   : ${Float.toFixedWithPrecision(perIter, ~digits=6)} s`)
  Js.log(`LOC/sec          : ${Float.toFixedWithPrecision(locPerSec, ~digits=0)}`)
  Js.log(
    `Bytes/sec        : ${Float.toFixedWithPrecision(
        Int.toFloat(String.length(source) * iterations) /. totalSec,
        ~digits=0,
      )}`,
  )
}

run()
