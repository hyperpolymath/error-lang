// @taxonomy: compiler/lexer
// SPDX-License-Identifier: MPL-2.0
// Error-Lang Playground - Lexer
// Tokenizes Error-Lang source code with position tracking and error recovery

open Types

// Lexer state
type lexerState = {
  mutable source: string,
  mutable pos: int,
  mutable line: int,
  mutable column: int,
  mutable tokens: array<token>,
  mutable diagnostics: array<diagnostic>,
}

// Create a new lexer
let make = (source: string): lexerState => {
  source,
  pos: 0,
  line: 1,
  column: 1,
  tokens: [],
  diagnostics: [],
}

// Check if at end of source
let isAtEnd = (state: lexerState): bool => {
  state.pos >= String.length(state.source)
}

// Peek current character
let peek = (state: lexerState): option<string> => {
  if isAtEnd(state) {
    None
  } else {
    Some(String.charAt(state.source, state.pos))
  }
}

// Peek next character
let peekNext = (state: lexerState): option<string> => {
  if state.pos + 1 >= String.length(state.source) {
    None
  } else {
    Some(String.charAt(state.source, state.pos + 1))
  }
}

// Advance and return current character
let advance = (state: lexerState): option<string> => {
  let ch = peek(state)
  switch ch {
  | Some(c) =>
    state.pos = state.pos + 1
    if c == "\n" {
      state.line = state.line + 1
      state.column = 1
    } else {
      state.column = state.column + 1
    }
  | None => ()
  }
  ch
}

// Create position from current state
let currentPosition = (state: lexerState): position => {
  {line: state.line, column: state.column, offset: state.pos}
}

// Create span from start to current position
let makeSpan = (state: lexerState, startPos: position): span => {
  {start: startPos, end_: currentPosition(state), source: state.source}
}

// Add a token
let addToken = (state: lexerState, kind: tokenKind, lexeme: string, startPos: position): unit => {
  let token = {kind, lexeme, span: makeSpan(state, startPos)}
  state.tokens = Array.concat(state.tokens, [token])
}

// Add a diagnostic
let addDiagnostic = (
  state: lexerState,
  code: errorCode,
  severity: errorSeverity,
  message: string,
  span: span,
  learningObjective: string,
  recoveryHint: string,
  lesson: option<int>,
): unit => {
  let diag = {
    code,
    severity,
    message,
    span,
    learningObjective,
    recoveryHint,
    curriculumLesson: lesson,
  }
  state.diagnostics = Array.concat(state.diagnostics, [diag])
}

// Check if character is digit
let isDigit = (c: string): bool => {
  c >= "0" && c <= "9"
}

// Check if character is letter or underscore
let isAlpha = (c: string): bool => {
  (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "_"
}

// Check if character is alphanumeric
let isAlphaNumeric = (c: string): bool => {
  isAlpha(c) || isDigit(c)
}

// Check for smart quotes (common mistake)
let isSmartQuote = (c: string): bool => {
  c == "\u{201C}" || c == "\u{201D}" || c == "\u{2018}" || c == "\u{2019}"
}

// Scan a string literal
let scanString = (state: lexerState, startPos: position): unit => {
  let buf = ref("")
  let terminated = ref(false)

  while !isAtEnd(state) && !terminated.contents {
    switch peek(state) {
    | Some("\"") =>
      let _ = advance(state)
      terminated := true
    | Some("\\") =>
      let _ = advance(state)
      switch advance(state) {
      | Some("n") => buf := buf.contents ++ "\n"
      | Some("r") => buf := buf.contents ++ "\r"
      | Some("t") => buf := buf.contents ++ "\t"
      | Some("\\") => buf := buf.contents ++ "\\"
      | Some("\"") => buf := buf.contents ++ "\""
      | Some("0") => buf := buf.contents ++ "\x00"
      | Some(c) =>
        addDiagnostic(
          state,
          E0003,
          Error,
          `Invalid escape sequence: \\${c}`,
          makeSpan(state, startPos),
          "Learn valid escape sequences in strings",
          `Use \\n, \\r, \\t, \\\\, or \\"`,
          Some(3),
        )
        buf := buf.contents ++ c
      | None => ()
      }
    | Some("\n") =>
      // Unterminated string at newline
      terminated := true
      addDiagnostic(
        state,
        E0002,
        Error,
        "Unterminated string literal",
        makeSpan(state, startPos),
        "Understand string literal boundaries",
        "Close the string with a double quote (\") before the newline",
        Some(2),
      )
    | Some(c) =>
      buf := buf.contents ++ c
      let _ = advance(state)
    | None => ()
    }
  }

  if !terminated.contents {
    addDiagnostic(
      state,
      E0002,
      Error,
      "Unterminated string literal at end of file",
      makeSpan(state, startPos),
      "Understand string literal boundaries",
      "Close the string with a double quote (\")",
      Some(2),
    )
  }

  addToken(state, StringLiteral(buf.contents), `"${buf.contents}"`, startPos)
}

// Scan a number literal
let scanNumber = (state: lexerState, startPos: position, firstChar: string): unit => {
  let buf = ref(firstChar)
  let isFloat = ref(false)

  while !isAtEnd(state) {
    switch peek(state) {
    | Some(c) if isDigit(c) =>
      buf := buf.contents ++ c
      let _ = advance(state)
    | Some(".") if !isFloat.contents =>
      switch peekNext(state) {
      | Some(c) if isDigit(c) =>
        isFloat := true
        buf := buf.contents ++ "."
        let _ = advance(state)
      | _ => ()  // Not a decimal point, exit
      }
    | _ => ()  // Break out of loop
    }

    // Exit condition
    switch peek(state) {
    | Some(c) if !isDigit(c) && c != "." => ()
    | None => ()
    | _ => ()
    }
  }

  if isFloat.contents {
    switch Float.fromString(buf.contents) {
    | Some(f) => addToken(state, FloatLiteral(f), buf.contents, startPos)
    | None => addToken(state, Error("Invalid float"), buf.contents, startPos)
    }
  } else {
    switch Int.fromString(buf.contents) {
    | Some(i) => addToken(state, IntLiteral(i), buf.contents, startPos)
    | None => addToken(state, Error("Invalid integer"), buf.contents, startPos)
    }
  }
}

// Scan an identifier or keyword
let scanIdentifier = (state: lexerState, startPos: position, firstChar: string): unit => {
  let buf = ref(firstChar)

  while !isAtEnd(state) {
    switch peek(state) {
    | Some(c) if isAlphaNumeric(c) =>
      buf := buf.contents ++ c
      let _ = advance(state)
    | _ => ()
    }
  }

  let lexeme = buf.contents

  // Check for keywords
  let kind = switch lexeme {
  | "main" => Main
  | "end" => End
  | "let" => Let
  | "mutable" => Mutable
  | "function" => Function
  | "struct" => Struct
  | "if" => If
  | "elseif" => Elseif
  | "else" => Else
  | "while" => While
  | "for" => For
  | "in" => In
  | "return" => Return
  | "break" => Break
  | "continue" => Continue
  | "and" => And
  | "or" => Or
  | "not" => Not
  | "true" => True
  | "false" => False
  | "nil" => Nil
  | "gutter" => Gutter
  | "fn" => Fn
  | "print" => Print
  | "println" => Println
  | "Int" => TInt
  | "Float" => TFloat
  | "String" => TString
  | "Bool" => TBool
  | "Array" => TArray
  | _ => Identifier(lexeme)
  }

  addToken(state, kind, lexeme, startPos)
}

// Scan a single token
let scanToken = (state: lexerState): unit => {
  let startPos = currentPosition(state)

  switch advance(state) {
  | None => ()
  | Some(c) =>
    switch c {
    // Single-character tokens
    | "(" => addToken(state, LParen, "(", startPos)
    | ")" => addToken(state, RParen, ")", startPos)
    | "[" => addToken(state, LBracket, "[", startPos)
    | "]" => addToken(state, RBracket, "]", startPos)
    | "{" => addToken(state, LBrace, "{", startPos)
    | "}" => addToken(state, RBrace, "}", startPos)
    | "," => addToken(state, Comma, ",", startPos)
    | "." => addToken(state, Dot, ".", startPos)
    | ";" => addToken(state, Semicolon, ";", startPos)
    | "+" => addToken(state, Plus, "+", startPos)
    | "*" => addToken(state, Star, "*", startPos)
    | "/" => addToken(state, Slash, "/", startPos)
    | "%" => addToken(state, Percent, "%", startPos)
    | "^" => addToken(state, Caret, "^", startPos)
    | "~" => addToken(state, Tilde, "~", startPos)
    | "?" => addToken(state, Question, "?", startPos)
    | ":" => addToken(state, Colon, ":", startPos)

    // Two-character tokens
    | "-" =>
      switch peek(state) {
      | Some(">") =>
        let _ = advance(state)
        addToken(state, Arrow, "->", startPos)
      | _ => addToken(state, Minus, "-", startPos)
      }
    | "=" =>
      switch peek(state) {
      | Some("=") =>
        let _ = advance(state)
        addToken(state, EqEq, "==", startPos)
      | _ => addToken(state, Eq, "=", startPos)
      }
    | "!" =>
      switch peek(state) {
      | Some("=") =>
        let _ = advance(state)
        addToken(state, BangEq, "!=", startPos)
      | _ =>
        addDiagnostic(
          state,
          E0004,
          Error,
          "Unexpected character: !",
          makeSpan(state, startPos),
          "Learn Error-Lang operators",
          "Use 'not' for logical negation instead of !",
          Some(4),
        )
      }
    | "<" =>
      switch peek(state) {
      | Some("=") =>
        let _ = advance(state)
        addToken(state, LtEq, "<=", startPos)
      | Some("<") =>
        let _ = advance(state)
        addToken(state, LtLt, "<<", startPos)
      | _ => addToken(state, Lt, "<", startPos)
      }
    | ">" =>
      switch peek(state) {
      | Some("=") =>
        let _ = advance(state)
        addToken(state, GtEq, ">=", startPos)
      | Some(">") =>
        let _ = advance(state)
        addToken(state, GtGt, ">>", startPos)
      | _ => addToken(state, Gt, ">", startPos)
      }
    | "&" =>
      switch peek(state) {
      | Some("&") =>
        let _ = advance(state)
        addDiagnostic(
          state,
          E0004,
          Warning,
          "&& is not valid - use 'and' keyword",
          makeSpan(state, startPos),
          "Learn Error-Lang boolean operators",
          "Replace && with the 'and' keyword",
          Some(4),
        )
        addToken(state, And, "&&", startPos)
      | _ => addToken(state, Amp, "&", startPos)
      }
    | "|" =>
      switch peek(state) {
      | Some("|") =>
        let _ = advance(state)
        addDiagnostic(
          state,
          E0004,
          Warning,
          "|| is not valid - use 'or' keyword",
          makeSpan(state, startPos),
          "Learn Error-Lang boolean operators",
          "Replace || with the 'or' keyword",
          Some(4),
        )
        addToken(state, Or, "||", startPos)
      | _ => addToken(state, Pipe, "|", startPos)
      }

    // Comments
    | "#" =>
      let commentStart = state.pos
      while !isAtEnd(state) && peek(state) != Some("\n") {
        let _ = advance(state)
      }
      let comment = String.slice(state.source, ~start=commentStart, ~end=state.pos)
      addToken(state, Comment(comment), "#" ++ comment, startPos)

    // Whitespace
    | " " | "\t" | "\r" => addToken(state, Whitespace, c, startPos)
    | "\n" => addToken(state, Newline, c, startPos)

    // String literals
    | "\"" => scanString(state, startPos)

    // Smart quotes (common mistake)
    | c if isSmartQuote(c) =>
      addDiagnostic(
        state,
        E0007,
        Error,
        "Smart quote detected - use straight quotes",
        makeSpan(state, startPos),
        "Understand Unicode vs ASCII in source code",
        "Replace curly/smart quotes with straight quotes (\")",
        Some(1),
      )
      // Try to recover by scanning as string
      scanString(state, startPos)

    // Numbers
    | c if isDigit(c) => scanNumber(state, startPos, c)

    // Identifiers and keywords
    | c if isAlpha(c) => scanIdentifier(state, startPos, c)

    // Unknown character
    | _ =>
      addDiagnostic(
        state,
        E0004,
        Error,
        `Illegal character: ${c}`,
        makeSpan(state, startPos),
        "Learn valid Error-Lang characters",
        "Remove or replace this character",
        Some(4),
      )
      addToken(state, Error(c), c, startPos)
    }
  }
}

// Tokenize entire source
let tokenize = (source: string): (array<token>, array<diagnostic>) => {
  let state = make(source)

  while !isAtEnd(state) {
    scanToken(state)
  }

  // Add EOF token
  let eofPos = currentPosition(state)
  addToken(state, Eof, "", eofPos)

  (state.tokens, state.diagnostics)
}

// Filter out whitespace and comments for parsing
let tokenizeForParsing = (source: string): (array<token>, array<diagnostic>) => {
  let (tokens, diagnostics) = tokenize(source)
  let filtered = tokens->Array.filter(t => {
    switch t.kind {
    | Whitespace | Newline | Comment(_) => false
    | _ => true
    }
  })
  (filtered, diagnostics)
}
