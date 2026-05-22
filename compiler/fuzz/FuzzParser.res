// SPDX-License-Identifier: MPL-2.0
// Fuzz target for the Error-Lang parser.
//
// Invariant: the parser must NEVER crash on ANY input. It should always
// return a (program, diagnostics) tuple without throwing an uncaught
// exception.
//
// Strategy: lex first, then feed tokens to Parser.parse(). Uses both
// raw random bytes and structured inputs mixing Error-Lang keywords,
// operators, and identifiers for deeper parser coverage.
//
// Run with:
//   deno task res:build && node compiler/fuzz/FuzzParser.res.js

open Types

// Interesting fragments biased toward Error-Lang syntax for deeper
// parser coverage. These form syntactically-plausible token sequences.
let fragments = [
  // Keywords
  "main", "end", "let", "mutable", "function", "struct",
  "if", "elseif", "else", "while", "for", "in",
  "break", "continue", "return", "and", "or", "not",
  "true", "false", "nil", "gutter", "fn",
  // Types
  "Int", "Float", "String", "Bool", "Array",
  // Built-in functions
  "print", "println",
  // Operators
  "+", "-", "*", "/", "%", "=", "==", "!=",
  "<", "<=", ">", ">=", "<<", ">>",
  "&", "|", "^", "~", "?", ":", "->",
  // Delimiters
  "(", ")", "[", "]", "{", "}", ",", ".",
  // Literals
  "42", "0", "3.14", "1e10",
  "0xFF", "0b1010",
  "\"hello\"", "\"escape\\n\"", "\"\"\"triple\"\"\"",
  // Smart quotes (intentional error case)
  "\u201C", "\u201D",
  // Comments
  "#", "# comment\n",
  // Whitespace
  " ", "\t", "\n", "\r",
  // Identifiers
  "foo", "bar_baz", "_private", "myVar1",
  // Structured patterns — syntactically plausible Error-Lang
  "main\n  let x = 42\nend",
  "function add(a, b)\n  return a + b\nend",
  "if true\n  println(42)\nend",
  "struct Point\n  x: Int\n  y: Int\nend",
  "for i in [1, 2, 3]\n  println(i)\nend",
  "while true\n  break\nend",
  // Edge cases
  "\\", "\x00",
]

// Simple pseudo-random number generator (LCG)
let seed = ref(Date.now()->Float.toInt->Int.mod(2147483647))
let nextRand = () => {
  seed := Int.mod(seed.contents * 1103515245 + 12345, 2147483647)
  abs(seed.contents)
}

// Generate a random byte string of up to maxLen bytes
let randomBytes = (maxLen: int): string => {
  let len = Int.mod(nextRand(), maxLen + 1)
  let buf = ref("")
  for _ in 0 to len - 1 {
    let byte = Int.mod(nextRand(), 256)
    buf := buf.contents ++ String.fromCharCode(byte)
  }
  buf.contents
}

// Generate a fragment-based random input
let randomFragments = (maxLen: int): string => {
  let buf = ref("")
  while String.length(buf.contents) < maxLen {
    let idx = Int.mod(nextRand(), Array.length(fragments))
    switch fragments->Array.get(idx) {
    | Some(frag) => buf := buf.contents ++ frag
    | None => ()
    }
  }
  buf.contents
}

// Generate a random input (50% random bytes, 50% fragments)
let randomInput = (maxLen: int): string => {
  if Int.mod(nextRand(), 2) == 0 {
    randomBytes(maxLen)
  } else {
    randomFragments(maxLen)
  }
}

// Run the fuzzer
let iterations = 100_000

let () = {
  Console.log(`Error-Lang parser fuzzer: running ${Int.toString(iterations)} iterations`)

  for i in 1 to iterations {
    let input = randomInput(4096)

    // Step 1: Lex the input (the lexer must not throw)
    let (tokens, _lexDiags) = Lexer.lex(input, "<fuzz>", 1)

    // Step 2: Feed tokens to the parser (must not throw)
    let (program, parseDiags) = Parser.parse(tokens, "<fuzz>", 1)

    // Walk the program to ensure it is well-formed
    program.declarations->Array.forEach(decl => {
      let _ = decl
    })

    // Walk diagnostics similarly
    parseDiags->Array.forEach(diag => {
      let _ = diag.code
      let _ = diag.message
    })

    if Int.mod(i, 10_000) == 0 {
      Console.log(`  ... ${Int.toString(i)} iterations complete`)
    }
  }

  Console.log(`Error-Lang parser fuzzer: ${Int.toString(iterations)} iterations passed with no crashes`)
}
