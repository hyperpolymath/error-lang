// SPDX-License-Identifier: PMPL-1.0-or-later
// Cst.res - Concrete Syntax Tree for Error-Lang
//
// A CST preserves ALL source information including whitespace, comments,
// and exact token text. It is lossless and can perfectly reconstruct the
// original source. This is needed for formatters, refactoring tools, and
// IDE support.
//
// This module wraps/extends the existing lexer infrastructure without
// modifying it.

open Types

// -------------------------------------------------------------------
// Trivia
// -------------------------------------------------------------------

/// The kind of trivia (non-semantic source content attached to tokens).
type triviaKind =
  | Whitespace     // Contiguous whitespace (spaces, tabs).
  | LineComment    // A line comment (# ...).
  | Newline        // A newline character.

/// A single piece of trivia attached to a token.
type trivia = {
  kind: triviaKind,
  text: string,
  loc: location,
}

// -------------------------------------------------------------------
// CST Token (leaf node)
// -------------------------------------------------------------------

/// A CST token: the leaf node of the concrete syntax tree.
type cstToken = {
  tokenKind: tokenType,
  text: string,
  leadingTrivia: array<trivia>,
  trailingTrivia: array<trivia>,
  loc: location,
}

// -------------------------------------------------------------------
// CST Node Kind
// -------------------------------------------------------------------

/// The kind of a CST tree node, matching Error-Lang grammar productions.
type cstNodeKind =
  | SourceFile       // The root node representing an entire source file.
  | MainBlock        // A main block.
  | FunctionDecl     // A function declaration.
  | StructDecl       // A struct declaration.
  | LetStmt          // A let binding statement.
  | IfStmt           // An if statement.
  | WhileStmt        // A while loop.
  | ForStmt          // A for loop.
  | ReturnStmt       // A return statement.
  | BreakStmt        // A break statement.
  | ContinueStmt     // A continue statement.
  | PrintStmt        // A print/println statement.
  | GutterBlock      // A gutter block (error injection zone).
  | ExprStmt         // An expression statement.
  | BinaryExpr       // A binary operator expression.
  | UnaryExpr        // A unary operator expression.
  | CallExpr         // A function call expression.
  | IndexExpr        // An index expression.
  | MemberExpr       // A member access expression.
  | TernaryExpr      // A ternary expression.
  | LambdaExpr       // A lambda expression.
  | ArrayLitExpr     // An array literal.
  | ParamList        // A parameter list.
  | ArgList          // An argument list.
  | ErrorNode        // An error recovery node.

// -------------------------------------------------------------------
// CST Node
// -------------------------------------------------------------------

/// A node in the concrete syntax tree: either a token (leaf) or a tree (branch).
type rec cstNode =
  | CstToken(cstToken)
  | CstTree(cstTree)

/// A branch node in the CST.
and cstTree = {
  kind: cstNodeKind,
  children: array<cstNode>,
  loc: location,
}

// -------------------------------------------------------------------
// Source reconstruction
// -------------------------------------------------------------------

/// Reconstruct exact source text from a CST node (including trivia).
let rec toSource = (node: cstNode): string => {
  switch node {
  | CstToken(tok) => {
      let leading = tok.leadingTrivia->Array.map(t => t.text)->Array.join("")
      let trailing = tok.trailingTrivia->Array.map(t => t.text)->Array.join("")
      leading ++ tok.text ++ trailing
    }
  | CstTree(tree) => treeToSource(tree)
  }
}

/// Reconstruct source from a tree node.
and treeToSource = (tree: cstTree): string => {
  tree.children->Array.map(toSource)->Array.join("")
}

// -------------------------------------------------------------------
// Token collection
// -------------------------------------------------------------------

/// Collect all tokens in document order from a CST tree.
let rec tokens = (tree: cstTree): array<cstToken> => {
  tree.children->Array.flatMap(child =>
    switch child {
    | CstToken(tok) => [tok]
    | CstTree(subtree) => tokens(subtree)
    }
  )
}

// -------------------------------------------------------------------
// Node lookup by position
// -------------------------------------------------------------------

/// Find the deepest node at the given byte offset.
let rec nodeAt = (tree: cstTree, offset: int): option<cstNode> => {
  tree.children->Array.findMap(child => {
    let loc = switch child {
    | CstToken(tok) => tok.loc
    | CstTree(t) => t.loc
    }

    if loc.start.offset <= offset && offset < loc.end_.offset {
      switch child {
      | CstToken(_) => Some(child)
      | CstTree(subtree) =>
        switch nodeAt(subtree, offset) {
        | Some(deeper) => Some(deeper)
        | None => Some(child)
        }
      }
    } else {
      None
    }
  })
}

// -------------------------------------------------------------------
// Trivia-aware lexing
// -------------------------------------------------------------------

/// Check if a character is whitespace (not newline).
let isWs = (c: string): bool => c == " " || c == "\t" || c == "\r"

/// Classify a gap of source text into trivia items.
let classifyTrivia = (gap: string, file: string, baseOffset: int): array<trivia> => {
  let result = ref([])
  let i = ref(0)
  let len = String.length(gap)

  while i.contents < len {
    let c = String.charAt(gap, i.contents)

    // Line comment: # to end of line
    if c == "#" {
      let start = i.contents
      while i.contents < len && String.charAt(gap, i.contents) != "\n" {
        i := i.contents + 1
      }
      let text = String.substring(gap, ~start, ~end=i.contents)
      let loc = {
        start: {line: 0, column: 0, offset: baseOffset + start},
        end_: {line: 0, column: 0, offset: baseOffset + i.contents},
        file,
      }
      result := Array.concat(result.contents, [{kind: LineComment, text, loc}])
    // Newline
    } else if c == "\n" {
      let loc = {
        start: {line: 0, column: 0, offset: baseOffset + i.contents},
        end_: {line: 0, column: 0, offset: baseOffset + i.contents + 1},
        file,
      }
      result := Array.concat(result.contents, [{kind: Newline, text: "\n", loc}])
      i := i.contents + 1
    // Whitespace
    } else if isWs(c) {
      let start = i.contents
      while i.contents < len && isWs(String.charAt(gap, i.contents)) {
        i := i.contents + 1
      }
      let text = String.substring(gap, ~start, ~end=i.contents)
      let loc = {
        start: {line: 0, column: 0, offset: baseOffset + start},
        end_: {line: 0, column: 0, offset: baseOffset + i.contents},
        file,
      }
      result := Array.concat(result.contents, [{kind: Whitespace, text, loc}])
    // Anything else (shouldn't happen in gaps)
    } else {
      let loc = {
        start: {line: 0, column: 0, offset: baseOffset + i.contents},
        end_: {line: 0, column: 0, offset: baseOffset + i.contents + 1},
        file,
      }
      result := Array.concat(result.contents, [{kind: Whitespace, text: c, loc}])
      i := i.contents + 1
    }
  }

  result.contents
}

/// Convert a token type back to its source text.
let tokenToText = (tok: token): string => tok.lexeme

/// Lex source into CST tokens with trivia attached.
///
/// Uses the existing lexer but captures the gaps (whitespace, comments, newlines)
/// that the normal lexer produces as separate tokens or skips.
let lexWithTrivia = (source: string, file: string, runNumber: int): array<cstToken> => {
  let (rawTokens, _diagnostics) = Lexer.lex(source, file, runNumber)

  let result = ref([])
  let prevEnd = ref(0)

  rawTokens->Array.forEach(tok => {
    let tokStart = tok.loc.start.offset
    let tokEnd = tok.loc.end_.offset
    let tokText = tokenToText(tok)

    // Collect gap before this token as leading trivia
    let leading = if tokStart > prevEnd.contents {
      let gap = String.substring(source, ~start=prevEnd.contents, ~end=tokStart)
      classifyTrivia(gap, file, prevEnd.contents)
    } else {
      []
    }

    // For Newline tokens: convert them to trivia on the previous token
    switch tok.type_ {
    | Newline => {
        // Attach as trailing trivia to previous CST token
        let newlineTrivia = {
          kind: Newline,
          text: tokText,
          loc: tok.loc,
        }
        switch result.contents {
        | [] =>
          // No previous token; create a synthetic token with trivia
          result := [{
            tokenKind: tok.type_,
            text: tokText,
            leadingTrivia: leading,
            trailingTrivia: [],
            loc: tok.loc,
          }]
        | _ => {
            let lastIdx = Array.length(result.contents) - 1
            let last = result.contents[lastIdx]
            let updated = {
              ...last,
              trailingTrivia: Array.concat(last.trailingTrivia, Array.concat(leading, [newlineTrivia])),
            }
            result.contents[lastIdx] = updated
          }
        }
      }
    | _ => {
        result := Array.concat(result.contents, [{
          tokenKind: tok.type_,
          text: tokText,
          leadingTrivia: leading,
          trailingTrivia: [],
          loc: tok.loc,
        }])
      }
    }

    prevEnd := tokEnd
  })

  // Handle trailing source after last token
  let totalLen = String.length(source)
  if prevEnd.contents < totalLen {
    let gap = String.substring(source, ~start=prevEnd.contents, ~end=totalLen)
    let trailing = classifyTrivia(gap, file, prevEnd.contents)
    let len = Array.length(result.contents)
    if len > 0 {
      let last = result.contents[len - 1]
      result.contents[len - 1] = {
        ...last,
        trailingTrivia: Array.concat(last.trailingTrivia, trailing),
      }
    }
  }

  result.contents
}

// -------------------------------------------------------------------
// Public API
// -------------------------------------------------------------------

/// Parse source code into a Concrete Syntax Tree.
///
/// Satisfies the round-trip property:
///
///   treeToSource(parseToCst(source, file, runNumber)) == source
///
/// Returns a cstTree rooted at SourceFile.
let parseToCst = (source: string, file: string, runNumber: int): cstTree => {
  let cstTokens = lexWithTrivia(source, file, runNumber)
  let children = cstTokens->Array.map(tok => CstToken(tok))

  let loc = switch cstTokens {
  | [] => {
      start: {line: 1, column: 1, offset: 0},
      end_: {line: 1, column: 1, offset: 0},
      file,
    }
  | _ => {
      let first = cstTokens[0]
      let last = cstTokens[Array.length(cstTokens) - 1]
      {
        start: first.loc.start,
        end_: last.loc.end_,
        file,
      }
    }
  }

  {kind: SourceFile, children, loc}
}

// -------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------

let runTests = () => {
  let pass = ref(0)
  let fail = ref(0)

  let assert_ = (name: string, actual: bool) => {
    if actual {
      pass := pass.contents + 1
    } else {
      Console.error(`FAIL: ${name}`)
      fail := fail.contents + 1
    }
  }

  // Round-trip: simple
  {
    let source = "CONST x = 42"
    let cst = parseToCst(source, "test.el", 1)
    assert_("round_trip_simple", treeToSource(cst) == source)
  }

  // Round-trip: with comment
  {
    let source = "# comment\nCONST x = 42"
    let cst = parseToCst(source, "test.el", 1)
    assert_("round_trip_comment", treeToSource(cst) == source)
  }

  // Round-trip: whitespace
  {
    let source = "  CONST  x  =  42  "
    let cst = parseToCst(source, "test.el", 1)
    assert_("round_trip_whitespace", treeToSource(cst) == source)
  }

  // Round-trip: multiline
  {
    let source = "# Header\n\nCONST x = 1\nCONST y = 2\n"
    let cst = parseToCst(source, "test.el", 1)
    assert_("round_trip_multiline", treeToSource(cst) == source)
  }

  // Empty source
  {
    let cst = parseToCst("", "test.el", 1)
    assert_("empty_source", treeToSource(cst) == "")
  }

  // Tokens in order
  {
    let source = "CONST x = 1"
    let cst = parseToCst(source, "test.el", 1)
    let toks = tokens(cst)
    let texts = toks->Array.map(t => t.text)
    assert_("tokens_in_order", texts == ["CONST", "x", "=", "1"])
  }

  // Trivia preserved
  {
    let source = "# comment\nCONST x = 1"
    let cst = parseToCst(source, "test.el", 1)
    let toks = tokens(cst)
    let firstTok = toks[0]
    let hasComment = firstTok.leadingTrivia->Array.some(t => t.kind == LineComment)
    assert_("trivia_preserved", hasComment)
  }

  Console.log(`CST tests: ${pass.contents->Int.toString} passed, ${fail.contents->Int.toString} failed`)
}
