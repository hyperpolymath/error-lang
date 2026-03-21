// @taxonomy: compiler/parser
// SPDX-License-Identifier: PMPL-1.0-or-later
// Error-Lang Playground - Parser
// Error-tolerant recursive descent parser for Error-Lang

open Types

// Parser state
type parserState = {
  tokens: array<token>,
  mutable pos: int,
  mutable diagnostics: array<diagnostic>,
  mutable inGutter: bool,
}

// Create parser
let make = (tokens: array<token>): parserState => {
  {tokens, pos: 0, diagnostics: [], inGutter: false}
}

// Check if at end
let isAtEnd = (state: parserState): bool => {
  state.pos >= Array.length(state.tokens) ||
  state.tokens[state.pos].kind == Eof
}

// Peek current token
let peek = (state: parserState): token => {
  if state.pos >= Array.length(state.tokens) {
    // Return synthetic EOF
    {
      kind: Eof,
      lexeme: "",
      span: {
        start: {line: 1, column: 1, offset: 0},
        end_: {line: 1, column: 1, offset: 0},
        source: "",
      },
    }
  } else {
    state.tokens[state.pos]
  }
}

// Peek next token
let peekNext = (state: parserState): token => {
  if state.pos + 1 >= Array.length(state.tokens) {
    peek(state)
  } else {
    state.tokens[state.pos + 1]
  }
}

// Advance and return current token
let advance = (state: parserState): token => {
  let current = peek(state)
  if !isAtEnd(state) {
    state.pos = state.pos + 1
  }
  current
}

// Check if current token matches
let check = (state: parserState, kind: tokenKind): bool => {
  peek(state).kind == kind
}

// Match and consume if matches
let match_ = (state: parserState, kind: tokenKind): bool => {
  if check(state, kind) {
    let _ = advance(state)
    true
  } else {
    false
  }
}

// Add diagnostic
let addDiagnostic = (
  state: parserState,
  code: errorCode,
  message: string,
  span: span,
): unit => {
  let diag: diagnostic = {
    code,
    severity: Error,
    message,
    span,
    learningObjective: "Understand parser error recovery",
    recoveryHint: "Check the syntax around this location",
    curriculumLesson: None,
  }
  state.diagnostics = Array.concat(state.diagnostics, [diag])
}

// Expect a specific token
let expect = (state: parserState, kind: tokenKind, message: string): option<token> => {
  if check(state, kind) {
    Some(advance(state))
  } else {
    let current = peek(state)
    addDiagnostic(state, E0001, message, current.span)
    None
  }
}

// Synchronize after error (skip to next statement boundary)
let synchronize = (state: parserState): unit => {
  while !isAtEnd(state) {
    switch peek(state).kind {
    | Main | Function | Struct | Let | If | While | For | Return | Gutter | End => ()
    | _ => let _ = advance(state)
    }
  }
}

// Forward declarations for mutual recursion
let parseExpr: ref<parserState => option<expr>> = ref(_ => None)
let parseStmt: ref<parserState => option<stmt>> = ref(_ => None)
let parseBlock: ref<parserState => array<stmt>> = ref(_ => [])

// Parse primary expression
let parsePrimary = (state: parserState): option<expr> => {
  let token = peek(state)
  let span = token.span

  switch token.kind {
  | IntLiteral(n) =>
    let _ = advance(state)
    Some(IntExpr(n, span))
  | FloatLiteral(f) =>
    let _ = advance(state)
    Some(FloatExpr(f, span))
  | StringLiteral(s) =>
    let _ = advance(state)
    Some(StringExpr(s, span))
  | True =>
    let _ = advance(state)
    Some(BoolExpr(true, span))
  | False =>
    let _ = advance(state)
    Some(BoolExpr(false, span))
  | Nil =>
    let _ = advance(state)
    Some(NilExpr(span))
  | Identifier(name) =>
    let _ = advance(state)
    Some(IdentExpr(name, span))
  | LParen =>
    let _ = advance(state)
    switch parseExpr.contents(state) {
    | Some(expr) =>
      if match_(state, RParen) {
        Some(GroupExpr(expr, span))
      } else {
        addDiagnostic(state, E0006, "Expected ')' after expression", peek(state).span)
        Some(expr)
      }
    | None =>
      addDiagnostic(state, E0001, "Expected expression after '('", span)
      None
    }
  | LBracket =>
    let _ = advance(state)
    let elements = ref([])
    if !check(state, RBracket) {
      switch parseExpr.contents(state) {
      | Some(first) =>
        elements := [first]
        while match_(state, Comma) {
          switch parseExpr.contents(state) {
          | Some(elem) => elements := Array.concat(elements.contents, [elem])
          | None => ()
          }
        }
      | None => ()
      }
    }
    let _ = expect(state, RBracket, "Expected ']' after array elements")
    Some(ArrayExpr(elements.contents, span))
  | _ =>
    addDiagnostic(state, E0001, `Unexpected token: ${token.lexeme}`, span)
    None
  }
}

// Parse unary expression
let parseUnary = (state: parserState): option<expr> => {
  let token = peek(state)
  switch token.kind {
  | Minus =>
    let _ = advance(state)
    switch parseUnary(state) {
    | Some(expr) => Some(UnaryExpr(Neg, expr, token.span))
    | None => None
    }
  | Not =>
    let _ = advance(state)
    switch parseUnary(state) {
    | Some(expr) => Some(UnaryExpr(Not_, expr, token.span))
    | None => None
    }
  | Tilde =>
    let _ = advance(state)
    switch parseUnary(state) {
    | Some(expr) => Some(UnaryExpr(BitNot, expr, token.span))
    | None => None
    }
  | _ => parsePrimary(state)
  }
}

// Parse binary expression with precedence climbing
let parseBinaryExpr = (state: parserState, minPrec: int): option<expr> => {
  let left = ref(parseUnary(state))

  while left.contents != None {
    let token = peek(state)
    let (op, prec) = switch token.kind {
    | Or => (Some(Or_), 1)
    | And => (Some(And_), 2)
    | EqEq => (Some(Eq_), 3)
    | BangEq => (Some(Neq), 3)
    | Lt => (Some(Lt_), 4)
    | Gt => (Some(Gt_), 4)
    | LtEq => (Some(Lte), 4)
    | GtEq => (Some(Gte), 4)
    | Pipe => (Some(BitOr), 5)
    | Caret => (Some(BitXor), 6)
    | Amp => (Some(BitAnd), 7)
    | LtLt => (Some(Shl), 8)
    | GtGt => (Some(Shr), 8)
    | Plus => (Some(Add), 9)
    | Minus => (Some(Sub), 9)
    | Star => (Some(Mul), 10)
    | Slash => (Some(Div), 10)
    | Percent => (Some(Mod), 10)
    | _ => (None, 0)
    }

    switch op {
    | Some(binOp) if prec >= minPrec =>
      let _ = advance(state)
      switch parseBinaryExpr(state, prec + 1) {
      | Some(right) =>
        switch left.contents {
        | Some(l) => left := Some(BinaryExpr(binOp, l, right, token.span))
        | None => ()
        }
      | None => ()
      }
    | _ => ()  // Break loop
    }

    // Check if we should continue
    switch peek(state).kind {
    | Or | And | EqEq | BangEq | Lt | Gt | LtEq | GtEq | Pipe | Caret | Amp | LtLt | GtGt | Plus | Minus | Star | Slash | Percent => ()
    | _ => left := None  // Force exit
    }
  }

  left.contents
}

// Initialize parseExpr
let _ = parseExpr := (state => parseBinaryExpr(state, 0))

// Parse let statement
let parseLetStmt = (state: parserState): option<stmt> => {
  let startToken = advance(state)  // consume 'let'
  let isMutable = match_(state, Mutable)

  switch peek(state).kind {
  | Identifier(name) =>
    let _ = advance(state)
    let typeAnnotation = if match_(state, Colon) {
      // Parse type
      switch peek(state).kind {
      | TInt => let _ = advance(state); Some(IntType)
      | TFloat => let _ = advance(state); Some(FloatType)
      | TString => let _ = advance(state); Some(StringType)
      | TBool => let _ = advance(state); Some(BoolType)
      | Identifier(typeName) => let _ = advance(state); Some(CustomType(typeName))
      | _ => None
      }
    } else {
      None
    }
    if match_(state, Eq) {
      switch parseExpr.contents(state) {
      | Some(value) => Some(LetStmt(name, isMutable, typeAnnotation, value, startToken.span))
      | None =>
        addDiagnostic(state, E0001, "Expected expression after '='", peek(state).span)
        None
      }
    } else {
      addDiagnostic(state, E0001, "Expected '=' after variable name", peek(state).span)
      None
    }
  | _ =>
    addDiagnostic(state, E0008, "Expected identifier after 'let'", peek(state).span)
    None
  }
}

// Parse print statement
let parsePrintStmt = (state: parserState, withNewline: bool): option<stmt> => {
  let startToken = advance(state)  // consume 'print' or 'println'
  let _ = expect(state, LParen, "Expected '(' after print")
  let args = ref([])

  if !check(state, RParen) {
    switch parseExpr.contents(state) {
    | Some(first) =>
      args := [first]
      while match_(state, Comma) {
        switch parseExpr.contents(state) {
        | Some(arg) => args := Array.concat(args.contents, [arg])
        | None => ()
        }
      }
    | None => ()
    }
  }

  let _ = expect(state, RParen, "Expected ')' after arguments")
  Some(PrintStmt(withNewline, args.contents, startToken.span))
}

// Parse gutter block (error injection zone)
let parseGutterBlock = (state: parserState): option<stmt> => {
  let startToken = advance(state)  // consume 'gutter'
  state.inGutter = true

  // Collect all tokens until 'end'
  let gutterTokens = ref([])
  while !isAtEnd(state) && !check(state, End) {
    gutterTokens := Array.concat(gutterTokens.contents, [advance(state)])
  }

  if match_(state, End) {
    state.inGutter = false
    Some(GutterStmt(gutterTokens.contents, startToken.span))
  } else {
    addDiagnostic(state, E0005, "Expected 'end' to close gutter block", peek(state).span)
    state.inGutter = false
    Some(GutterStmt(gutterTokens.contents, startToken.span))
  }
}

// Initialize parseStmt
let _ = parseStmt := (state => {
  let token = peek(state)
  switch token.kind {
  | Let => parseLetStmt(state)
  | Print => parsePrintStmt(state, false)
  | Println => parsePrintStmt(state, true)
  | Gutter => parseGutterBlock(state)
  | _ =>
    // Try expression statement
    switch parseExpr.contents(state) {
    | Some(expr) => Some(ExprStmt(expr, token.span))
    | None =>
      synchronize(state)
      None
    }
  }
})

// Initialize parseBlock
let _ = parseBlock := (state => {
  let stmts = ref([])
  while !isAtEnd(state) && !check(state, End) && !check(state, Else) && !check(state, Elseif) {
    switch parseStmt.contents(state) {
    | Some(stmt) => stmts := Array.concat(stmts.contents, [stmt])
    | None => ()
    }
  }
  stmts.contents
})

// Parse main block
let parseMainBlock = (state: parserState): option<decl> => {
  let startToken = advance(state)  // consume 'main'
  let body = parseBlock.contents(state)

  if match_(state, End) {
    Some(MainBlock(body, startToken.span))
  } else {
    addDiagnostic(state, E0005, "Expected 'end' to close main block", peek(state).span)
    Some(MainBlock(body, startToken.span))
  }
}

// Parse program
let parseProgram = (tokens: array<token>): program => {
  let state = make(tokens)
  let declarations = ref([])

  while !isAtEnd(state) {
    switch peek(state).kind {
    | Main =>
      switch parseMainBlock(state) {
      | Some(decl) => declarations := Array.concat(declarations.contents, [decl])
      | None => ()
      }
    | _ =>
      // Skip unknown tokens at top level
      let _ = advance(state)
    }
  }

  {
    declarations: declarations.contents,
    diagnostics: state.diagnostics,
    stabilityScore: 100,
  }
}

// Parse source string
let parse = (source: string): program => {
  let (tokens, lexDiags) = Lexer.tokenizeForParsing(source)
  let program = parseProgram(tokens)
  {
    ...program,
    diagnostics: Array.concat(lexDiags, program.diagnostics),
  }
}
