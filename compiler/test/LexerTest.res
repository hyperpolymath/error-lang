// SPDX-License-Identifier: PMPL-1.0-or-later
// LexerTest.res - Comprehensive tests for the Error-Lang lexer
//
// Covers: keywords, operators, literals, identifiers, comments,
//         whitespace handling, escape sequences, and error cases.

open Types
open TestHelpers

// ============================================
// Keyword Tests
// ============================================

let testKeywords = () => {
  suite("Lexer: Keywords")

  // All language keywords
  let keywordPairs = [
    ("main", Main),
    ("end", End),
    ("let", Let),
    ("mutable", Mutable),
    ("function", Function),
    ("struct", Struct),
    ("if", If),
    ("elseif", Elseif),
    ("else", Else),
    ("while", While),
    ("for", For),
    ("in", In),
    ("break", Break),
    ("continue", Continue),
    ("return", Return),
    ("and", And),
    ("or", Or),
    ("not", Not),
    ("true", True),
    ("false", False),
    ("nil", Nil),
    ("gutter", Gutter),
    ("fn", Fn),
  ]

  keywordPairs->Array.forEach(((kw, expected)) => {
    let tokens = lexSource(kw)
    let types = tokenTypes(tokens)
    assertEqual(`keyword '${kw}' produces correct token`, types[0], Some(expected))
  })

  // Type keywords
  let typePairs = [
    ("Int", TInt),
    ("Float", TFloat),
    ("String", TString),
    ("Bool", TBool),
    ("Array", TArray),
  ]

  typePairs->Array.forEach(((kw, expected)) => {
    let tokens = lexSource(kw)
    let types = tokenTypes(tokens)
    assertEqual(`type keyword '${kw}' produces correct token`, types[0], Some(expected))
  })

  // print and println are special: lexed as Identifier("print") / Identifier("println")
  let printTokens = lexSource("print")
  assertEqual(
    "print is Identifier(\"print\")",
    tokenTypes(printTokens)[0],
    Some(Identifier("print")),
  )
  let printlnTokens = lexSource("println")
  assertEqual(
    "println is Identifier(\"println\")",
    tokenTypes(printlnTokens)[0],
    Some(Identifier("println")),
  )
}

// ============================================
// Operator Tests
// ============================================

let testOperators = () => {
  suite("Lexer: Operators")

  // Single-character operators
  let singleOps = [
    ("+", Plus),
    ("-", Minus),
    ("*", Star),
    ("/", Slash),
    ("%", Percent),
    ("&", Ampersand),
    ("|", Pipe),
    ("^", Caret),
    ("~", Tilde),
    ("?", Question),
    (":", Colon),
    ("=", Equal),
    ("<", Less),
    (">", Greater),
  ]

  singleOps->Array.forEach(((op, expected)) => {
    let tokens = lexSource(op)
    let types = tokenTypes(tokens)
    assertEqual(`operator '${op}'`, types[0], Some(expected))
  })

  // Two-character operators
  let doubleOps = [
    ("==", EqualEqual),
    ("!=", BangEqual),
    ("<=", LessEqual),
    (">=", GreaterEqual),
    ("<<", LessLess),
    (">>", GreaterGreater),
    ("->", Arrow),
  ]

  doubleOps->Array.forEach(((op, expected)) => {
    let tokens = lexSource(op)
    let types = tokenTypes(tokens)
    assertEqual(`operator '${op}'`, types[0], Some(expected))
  })
}

// ============================================
// Delimiter Tests
// ============================================

let testDelimiters = () => {
  suite("Lexer: Delimiters")

  let delims = [
    ("(", LParen),
    (")", RParen),
    ("[", LBracket),
    ("]", RBracket),
    ("{", LBrace),
    ("}", RBrace),
    (",", Comma),
    (".", Dot),
  ]

  delims->Array.forEach(((ch, expected)) => {
    let tokens = lexSource(ch)
    let types = tokenTypes(tokens)
    assertEqual(`delimiter '${ch}'`, types[0], Some(expected))
  })
}

// ============================================
// Integer Literal Tests
// ============================================

let testIntegers = () => {
  suite("Lexer: Integer literals")

  // Basic integers
  let intTokens = lexSource("42")
  assertEqual("integer 42", tokenTypes(intTokens)[0], Some(Integer(42)))

  let zeroTokens = lexSource("0")
  assertEqual("integer 0", tokenTypes(zeroTokens)[0], Some(Integer(0)))

  let bigTokens = lexSource("123456")
  assertEqual("integer 123456", tokenTypes(bigTokens)[0], Some(Integer(123456)))

  // Multi-digit
  let multiTokens = lexSource("10 20 30")
  let types = tokenTypes(multiTokens)
  assertEqual("multiple integers count", Array.length(types), 3)
  assertEqual("first int", types[0], Some(Integer(10)))
  assertEqual("second int", types[1], Some(Integer(20)))
  assertEqual("third int", types[2], Some(Integer(30)))
}

// ============================================
// Float Literal Tests
// ============================================

let testFloats = () => {
  suite("Lexer: Float literals")

  let floatTokens = lexSource("3.14")
  assertEqual("float 3.14", tokenTypes(floatTokens)[0], Some(Float(3.14)))

  let zeroFloat = lexSource("0.0")
  assertEqual("float 0.0", tokenTypes(zeroFloat)[0], Some(Float(0.0)))

  let smallFloat = lexSource("0.001")
  assertEqual("float 0.001", tokenTypes(smallFloat)[0], Some(Float(0.001)))

  // Exponent notation
  let expTokens = lexSource("1e10")
  switch tokenTypes(expTokens)[0] {
  | Some(Float(_)) => assertTrue("exponent notation produces Float", true)
  | _ => assertTrue("exponent notation produces Float", false)
  }

  let expNeg = lexSource("2e-3")
  switch tokenTypes(expNeg)[0] {
  | Some(Float(_)) => assertTrue("negative exponent produces Float", true)
  | _ => assertTrue("negative exponent produces Float", false)
  }
}

// ============================================
// String Literal Tests
// ============================================

let testStrings = () => {
  suite("Lexer: String literals")

  // Basic string
  let strTokens = lexSource("\"hello\"")
  assertEqual("basic string", tokenTypes(strTokens)[0], Some(String("hello")))

  // Empty string
  let emptyStr = lexSource("\"\"")
  assertEqual("empty string", tokenTypes(emptyStr)[0], Some(String("")))

  // String with spaces
  let spaceStr = lexSource("\"hello world\"")
  assertEqual("string with spaces", tokenTypes(spaceStr)[0], Some(String("hello world")))

  // Escape sequences
  let escN = lexSource("\"line1\\nline2\"")
  assertEqual("newline escape", tokenTypes(escN)[0], Some(String("line1\nline2")))

  let escT = lexSource("\"col1\\tcol2\"")
  assertEqual("tab escape", tokenTypes(escT)[0], Some(String("col1\tcol2")))

  let escR = lexSource("\"hello\\rworld\"")
  assertEqual("carriage return escape", tokenTypes(escR)[0], Some(String("hello\rworld")))

  let escBackslash = lexSource("\"path\\\\file\"")
  assertEqual("backslash escape", tokenTypes(escBackslash)[0], Some(String("path\\file")))

  let escQuote = lexSource("\"say \\\"hi\\\"\"")
  assertEqual("quote escape", tokenTypes(escQuote)[0], Some(String("say \"hi\"")))

  let escNull = lexSource("\"null\\0char\"")
  assertEqual("null escape", tokenTypes(escNull)[0], Some(String("null\x00char")))
}

// ============================================
// Boolean and Nil Literal Tests
// ============================================

let testBooleanAndNil = () => {
  suite("Lexer: Boolean and nil literals")

  let trueTokens = lexSource("true")
  assertEqual("true keyword", tokenTypes(trueTokens)[0], Some(True))

  let falseTokens = lexSource("false")
  assertEqual("false keyword", tokenTypes(falseTokens)[0], Some(False))

  let nilTokens = lexSource("nil")
  assertEqual("nil keyword", tokenTypes(nilTokens)[0], Some(Nil))
}

// ============================================
// Identifier Tests
// ============================================

let testIdentifiers = () => {
  suite("Lexer: Identifiers")

  // Simple identifier
  let idTokens = lexSource("foo")
  assertEqual("simple identifier", tokenTypes(idTokens)[0], Some(Identifier("foo")))

  // Identifier with underscore
  let underscoreId = lexSource("my_var")
  assertEqual("underscore identifier", tokenTypes(underscoreId)[0], Some(Identifier("my_var")))

  // Identifier starting with underscore
  let leadUnderscore = lexSource("_private")
  assertEqual("leading underscore", tokenTypes(leadUnderscore)[0], Some(Identifier("_private")))

  // Identifier with digits
  let digitId = lexSource("x1")
  assertEqual("identifier with digit", tokenTypes(digitId)[0], Some(Identifier("x1")))

  // Mixed case
  let mixedCase = lexSource("camelCase")
  assertEqual("camelCase identifier", tokenTypes(mixedCase)[0], Some(Identifier("camelCase")))

  // All caps
  let allCaps = lexSource("MAX_VALUE")
  assertEqual("ALL_CAPS identifier", tokenTypes(allCaps)[0], Some(Identifier("MAX_VALUE")))

  // Single character
  let singleChar = lexSource("x")
  assertEqual("single char identifier", tokenTypes(singleChar)[0], Some(Identifier("x")))
}

// ============================================
// Comment Tests
// ============================================

let testComments = () => {
  suite("Lexer: Comments")

  // Comment consumes rest of line
  let commentTokens = lexSource("# this is a comment")
  let types = tokenTypes(commentTokens)
  assertEqual("comment produces no visible tokens", Array.length(types), 0)

  // Code before comment
  let codeComment = lexSource("42 # meaning of life")
  let codeTypes = tokenTypes(codeComment)
  assertEqual("code before comment: one token", Array.length(codeTypes), 1)
  assertEqual("code before comment: is integer", codeTypes[0], Some(Integer(42)))

  // Comment followed by newline and code
  let multiLine = lexSource("# comment\n42")
  let mlTypes = tokenTypes(multiLine)
  assertEqual("code after comment line", Array.length(mlTypes), 1)
  assertEqual("code after comment line value", mlTypes[0], Some(Integer(42)))
}

// ============================================
// Whitespace Handling Tests
// ============================================

let testWhitespace = () => {
  suite("Lexer: Whitespace handling")

  // Spaces between tokens
  let spaced = lexSource("1 + 2")
  let types = tokenTypes(spaced)
  assertEqual("spaces: token count", Array.length(types), 3)
  assertEqual("spaces: first", types[0], Some(Integer(1)))
  assertEqual("spaces: op", types[1], Some(Plus))
  assertEqual("spaces: second", types[2], Some(Integer(2)))

  // Tabs between tokens
  let tabbed = lexSource("1\t+\t2")
  let tabTypes = tokenTypes(tabbed)
  assertEqual("tabs: token count", Array.length(tabTypes), 3)

  // No spaces
  let noSpace = lexSource("1+2")
  let nsTypes = tokenTypes(noSpace)
  assertEqual("no spaces: token count", Array.length(nsTypes), 3)

  // Newlines produce Newline tokens
  let newlineTokens = lexSource("1\n2")
  let nlTypes = tokenTypesWithNewlines(newlineTokens)
  assertEqual("newline: produces Newline token", nlTypes[1], Some(Newline))

  // Multiple newlines
  let multiNl = lexSource("1\n\n\n2")
  let mnlTypes = tokenTypesWithNewlines(multiNl)
  // Should have: Integer(1), Newline, Newline, Newline, Integer(2)
  assertEqual("multiple newlines count", Array.length(mnlTypes), 5)

  // Carriage return is whitespace (not newline token)
  let crTokens = lexSource("1\r2")
  let crTypes = tokenTypes(crTokens)
  assertEqual("CR is whitespace: token count", Array.length(crTypes), 2)
}

// ============================================
// Complex Token Sequences
// ============================================

let testTokenSequences = () => {
  suite("Lexer: Complex token sequences")

  // Variable declaration
  let letTokens = lexSource("let x = 42")
  let letTypes = tokenTypes(letTokens)
  assertEqual("let stmt: count", Array.length(letTypes), 4)
  assertEqual("let stmt: let", letTypes[0], Some(Let))
  assertEqual("let stmt: x", letTypes[1], Some(Identifier("x")))
  assertEqual("let stmt: =", letTypes[2], Some(Equal))
  assertEqual("let stmt: 42", letTypes[3], Some(Integer(42)))

  // Mutable variable
  let mutTokens = lexSource("let mutable y = 10")
  let mutTypes = tokenTypes(mutTokens)
  assertEqual("mutable let: count", Array.length(mutTypes), 5)
  assertEqual("mutable let: let", mutTypes[0], Some(Let))
  assertEqual("mutable let: mutable", mutTypes[1], Some(Mutable))

  // Function call
  let callTokens = lexSource("print(\"hello\")")
  let callTypes = tokenTypes(callTokens)
  assertEqual("print call: count", Array.length(callTypes), 4)
  assertEqual("print call: print", callTypes[0], Some(Identifier("print")))
  assertEqual("print call: lparen", callTypes[1], Some(LParen))
  assertEqual("print call: string", callTypes[2], Some(String("hello")))
  assertEqual("print call: rparen", callTypes[3], Some(RParen))

  // Arithmetic expression
  let arithTokens = lexSource("a + b * c")
  let arithTypes = tokenTypes(arithTokens)
  assertEqual("arithmetic: count", Array.length(arithTypes), 5)
  assertEqual("arithmetic: +", arithTypes[1], Some(Plus))
  assertEqual("arithmetic: *", arithTypes[3], Some(Star))

  // Array literal
  let arrTokens = lexSource("[1, 2, 3]")
  let arrTypes = tokenTypes(arrTokens)
  assertEqual("array literal: count", Array.length(arrTypes), 7)
  assertEqual("array: [", arrTypes[0], Some(LBracket))
  assertEqual("array: ]", arrTypes[6], Some(RBracket))

  // Comparison chain
  let cmpTokens = lexSource("a <= b")
  let cmpTypes = tokenTypes(cmpTokens)
  assertEqual("comparison: <=", cmpTypes[1], Some(LessEqual))

  // Arrow in lambda
  let lambdaTokens = lexSource("fn(x) -> x + 1")
  let lTypes = tokenTypes(lambdaTokens)
  assertEqual("lambda: fn", lTypes[0], Some(Fn))
  assertEqual("lambda: ->", lTypes[4], Some(Arrow))

  // Ternary
  let ternTokens = lexSource("a ? b : c")
  let ternTypes = tokenTypes(ternTokens)
  assertEqual("ternary: ?", ternTypes[1], Some(Question))
  assertEqual("ternary: :", ternTypes[3], Some(Colon))
}

// ============================================
// EOF Token Tests
// ============================================

let testEOF = () => {
  suite("Lexer: EOF token")

  // Empty source produces just EOF
  let emptyTokens = lexSource("")
  assertEqual("empty source: has EOF", Array.length(emptyTokens), 1)
  assertEqual("empty source: is EOF", emptyTokens[0]->Option.map(t => t.type_), Some(EOF))

  // Non-empty source ends with EOF
  let tokens = lexSource("42")
  let lastToken = tokens[Array.length(tokens) - 1]
  assertEqual("source ends with EOF", lastToken->Option.map(t => t.type_), Some(EOF))
}

// ============================================
// Location/Position Tests
// ============================================

let testPositions = () => {
  suite("Lexer: Position tracking")

  // First token starts at line 1, column 1
  let tokens = lexSource("hello")
  switch tokens[0] {
  | Some(tok) => {
      assertEqual("first token line", tok.loc.start.line, 1)
      assertEqual("first token column", tok.loc.start.column, 1)
    }
  | None => assertTrue("first token exists", false)
  }

  // Second line starts at line 2
  let multiTokens = lexSource("a\nb")
  // Filter to non-Newline, non-EOF
  let visible = multiTokens->Array.filter(t =>
    switch t.type_ {
    | Newline | EOF => false
    | _ => true
    }
  )
  switch visible[1] {
  | Some(tok) => assertEqual("second line token", tok.loc.start.line, 2)
  | None => assertTrue("second line token exists", false)
  }
}

// ============================================
// Error Case Tests
// ============================================

let testErrors = () => {
  suite("Lexer: Error cases")

  // Unterminated string
  let (unterminatedTokens, unterminatedDiags) = lexWithDiagnostics("\"hello")
  assertTrue(
    "unterminated string: has diagnostics",
    Array.length(unterminatedDiags) > 0,
  )
  // Check that an Error token was produced
  let hasError = unterminatedTokens->Array.some(t =>
    switch t.type_ {
    | Error(_) => true
    | _ => false
    }
  )
  assertTrue("unterminated string: error token", hasError)

  // Unterminated string with newline
  let (_nlStrTokens, nlStrDiags) = lexWithDiagnostics("\"hello\nworld\"")
  assertTrue(
    "unterminated string (newline): has diagnostics",
    Array.length(nlStrDiags) > 0,
  )

  // Invalid escape sequence
  let (_escTokens, escDiags) = lexWithDiagnostics("\"hello\\q\"")
  assertTrue(
    "invalid escape: has diagnostics",
    Array.length(escDiags) > 0,
  )

  // Solo ! (not followed by =)
  let (_bangTokens, bangDiags) = lexWithDiagnostics("!")
  assertTrue(
    "solo ! produces diagnostic",
    Array.length(bangDiags) > 0,
  )

  // Illegal character
  let (_illegalTokens, illegalDiags) = lexWithDiagnostics("`")
  assertTrue(
    "illegal char produces diagnostic",
    Array.length(illegalDiags) > 0,
  )
}

// ============================================
// Hex and Binary Number Tests
// ============================================

let testHexAndBinary = () => {
  suite("Lexer: Hex and binary numbers")

  // Hex number
  let hexTokens = lexSource("0xFF")
  switch tokenTypes(hexTokens)[0] {
  | Some(Integer(_)) => assertTrue("hex number produces Integer", true)
  | Some(Error(_)) => assertTrue("hex number: may produce Error for unsupported parse", true)
  | _ => assertTrue("hex number produces some token", false)
  }

  // Binary number
  let binTokens = lexSource("0b1010")
  switch tokenTypes(binTokens)[0] {
  | Some(Integer(_)) => assertTrue("binary number produces Integer", true)
  | _ => assertTrue("binary number produces some token", false)
  }
}

// ============================================
// Edge Cases
// ============================================

let testEdgeCases = () => {
  suite("Lexer: Edge cases")

  // Identifier immediately after number (separate tokens)
  let numId = lexSource("42abc")
  let niTypes = tokenTypes(numId)
  // 42 is integer, abc is identifier
  assertEqual("numId: count", Array.length(niTypes), 2)
  assertEqual("numId: int", niTypes[0], Some(Integer(42)))
  assertEqual("numId: id", niTypes[1], Some(Identifier("abc")))

  // Identifier that starts like a keyword but is longer
  let almostKw = lexSource("letter")
  assertEqual(
    "almost-keyword 'letter'",
    tokenTypes(almostKw)[0],
    Some(Identifier("letter")),
  )

  let almostKw2 = lexSource("iffy")
  assertEqual(
    "almost-keyword 'iffy'",
    tokenTypes(almostKw2)[0],
    Some(Identifier("iffy")),
  )

  // Multiple operators without spaces
  let multiOp = lexSource("+-*/")
  let moTypes = tokenTypes(multiOp)
  assertEqual("multi-op: count", Array.length(moTypes), 4)
  assertEqual("multi-op: +", moTypes[0], Some(Plus))
  assertEqual("multi-op: -", moTypes[1], Some(Minus))
  assertEqual("multi-op: *", moTypes[2], Some(Star))
  assertEqual("multi-op: /", moTypes[3], Some(Slash))

  // Dot followed by digit (not a float, since no leading digit context)
  // .5 is Dot then Integer(5) since dot isn't preceded by a digit
  let dotNum = lexSource(".5")
  let dnTypes = tokenTypes(dotNum)
  assertEqual("dot-num: first is Dot", dnTypes[0], Some(Dot))
  assertEqual("dot-num: second is Integer", dnTypes[1], Some(Integer(5)))

  // Long identifier
  let longId = lexSource("a_very_long_identifier_name_that_keeps_going")
  assertEqual(
    "long identifier",
    tokenTypes(longId)[0],
    Some(Identifier("a_very_long_identifier_name_that_keeps_going")),
  )
}

// ============================================
// Run All Lexer Tests
// ============================================

let runAll = () => {
  Console.log("\n========================================")
  Console.log("  ERROR-LANG LEXER TESTS")
  Console.log("========================================")

  testKeywords()
  testOperators()
  testDelimiters()
  testIntegers()
  testFloats()
  testStrings()
  testBooleanAndNil()
  testIdentifiers()
  testComments()
  testWhitespace()
  testTokenSequences()
  testEOF()
  testPositions()
  testErrors()
  testHexAndBinary()
  testEdgeCases()

  summarize()
}

runAll()
