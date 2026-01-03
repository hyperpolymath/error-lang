// SPDX-License-Identifier: AGPL-3.0-or-later
// Lexer.res - Tokenizer for Error-Lang

open Types

type lexerState = {
  source: string,
  file: string,
  mutable pos: int,
  mutable line: int,
  mutable column: int,
  mutable tokens: array<token>,
  mutable diagnostics: array<diagnostic>,
  runNumber: int,
}

let make = (source: string, file: string, runNumber: int): lexerState => {
  source,
  file,
  pos: 0,
  line: 1,
  column: 1,
  tokens: [],
  diagnostics: [],
  runNumber,
}

// Peek character at offset
let peek = (state: lexerState, offset: int): option<string> => {
  let idx = state.pos + offset
  if idx < String.length(state.source) {
    Some(String.charAt(state.source, idx))
  } else {
    None
  }
}

let current = (state: lexerState): option<string> => peek(state, 0)
let next = (state: lexerState): option<string> => peek(state, 1)

// Advance position
let advance = (state: lexerState): unit => {
  switch current(state) {
  | Some("\n") => {
      state.line = state.line + 1
      state.column = 1
    }
  | Some(_) => state.column = state.column + 1
  | None => ()
  }
  state.pos = state.pos + 1
}

// Get current position
let getPosition = (state: lexerState): position => {
  line: state.line,
  column: state.column,
  offset: state.pos,
}

// Create location from start to current
let makeLocation = (state: lexerState, start: position): location => {
  start,
  end_: getPosition(state),
  file: state.file,
}

// Create token
let addToken = (state: lexerState, type_: tokenType, lexeme: string, loc: location): unit => {
  let token = {type_, lexeme, loc}
  state.tokens = Array.concat(state.tokens, [token])
}

// Add diagnostic
let addDiagnostic = (state: lexerState, code: errorCode, message: string, loc: location): unit => {
  let diag = {
    code,
    message,
    loc,
    runNumber: state.runNumber,
    hint: None,
  }
  state.diagnostics = Array.concat(state.diagnostics, [diag])
}

// Check if character is letter or underscore
let isAlpha = (c: string): bool => {
  let code = String.charCodeAt(c, 0)
  (code >= 65.0 && code <= 90.0) ||  // A-Z
  (code >= 97.0 && code <= 122.0) || // a-z
  c == "_"
}

// Check if character is digit
let isDigit = (c: string): bool => {
  let code = String.charCodeAt(c, 0)
  code >= 48.0 && code <= 57.0
}

// Check if character is alphanumeric
let isAlphaNumeric = (c: string): bool => isAlpha(c) || isDigit(c)

// Check if whitespace (excluding newline)
let isWhitespace = (c: string): bool => c == " " || c == "\t" || c == "\r"

// Keywords map
let keywords: Dict.t<tokenType> = Dict.fromArray([
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
  ("Int", TInt),
  ("Float", TFloat),
  ("String", TString),
  ("Bool", TBool),
  ("Array", TArray),
  ("print", Identifier("print")),
  ("println", Identifier("println")),
])

// Scan identifier or keyword
let scanIdentifier = (state: lexerState): unit => {
  let start = getPosition(state)
  let startPos = state.pos

  while {
    switch current(state) {
    | Some(c) => isAlphaNumeric(c)
    | None => false
    }
  } {
    advance(state)
  }

  let lexeme = String.substring(state.source, ~start=startPos, ~end=state.pos)
  let loc = makeLocation(state, start)

  let type_ = switch Dict.get(keywords, lexeme) {
  | Some(kw) => kw
  | None => Identifier(lexeme)
  }

  addToken(state, type_, lexeme, loc)
}

// Scan number (integer or float)
let scanNumber = (state: lexerState): unit => {
  let start = getPosition(state)
  let startPos = state.pos

  // Handle hex/binary prefixes
  switch (current(state), next(state)) {
  | (Some("0"), Some("x")) | (Some("0"), Some("X")) => {
      advance(state) // 0
      advance(state) // x
      while {
        switch current(state) {
        | Some(c) => isDigit(c) || (c >= "a" && c <= "f") || (c >= "A" && c <= "F")
        | None => false
        }
      } {
        advance(state)
      }
      let lexeme = String.substring(state.source, ~start=startPos, ~end=state.pos)
      let loc = makeLocation(state, start)
      // Parse hex
      switch Int.fromString(lexeme) {
      | Some(n) => addToken(state, Integer(n), lexeme, loc)
      | None => addToken(state, Error("Invalid hex number"), lexeme, loc)
      }
      return
    }
  | (Some("0"), Some("b")) | (Some("0"), Some("B")) => {
      advance(state) // 0
      advance(state) // b
      while {
        switch current(state) {
        | Some("0") | Some("1") => true
        | _ => false
        }
      } {
        advance(state)
      }
      let lexeme = String.substring(state.source, ~start=startPos, ~end=state.pos)
      let loc = makeLocation(state, start)
      // TODO: Parse binary
      addToken(state, Integer(0), lexeme, loc)
      return
    }
  | _ => ()
  }

  // Regular integer/float
  while {
    switch current(state) {
    | Some(c) => isDigit(c)
    | None => false
    }
  } {
    advance(state)
  }

  // Check for decimal point
  let isFloat = switch (current(state), next(state)) {
  | (Some("."), Some(c)) if isDigit(c) => {
      advance(state) // .
      while {
        switch current(state) {
        | Some(c) => isDigit(c)
        | None => false
        }
      } {
        advance(state)
      }
      true
    }
  | _ => false
  }

  // Check for exponent
  let hasExponent = switch current(state) {
  | Some("e") | Some("E") => {
      advance(state)
      switch current(state) {
      | Some("+") | Some("-") => advance(state)
      | _ => ()
      }
      while {
        switch current(state) {
        | Some(c) => isDigit(c)
        | None => false
        }
      } {
        advance(state)
      }
      true
    }
  | _ => false
  }

  let lexeme = String.substring(state.source, ~start=startPos, ~end=state.pos)
  let loc = makeLocation(state, start)

  if isFloat || hasExponent {
    switch Float.fromString(lexeme) {
    | Some(f) => addToken(state, Float(f), lexeme, loc)
    | None => addToken(state, Error("Invalid float"), lexeme, loc)
    }
  } else {
    switch Int.fromString(lexeme) {
    | Some(n) => addToken(state, Integer(n), lexeme, loc)
    | None => addToken(state, Error("Invalid integer"), lexeme, loc)
    }
  }
}

// Scan string literal
let scanString = (state: lexerState): unit => {
  let start = getPosition(state)
  let quote = current(state)
  advance(state) // Opening quote

  // Check for triple-quote
  let isTriple = switch (current(state), next(state)) {
  | (Some("\""), Some("\"")) if quote == Some("\"") => {
      advance(state)
      advance(state)
      true
    }
  | _ => false
  }

  let buf = ref("")

  let rec loop = () => {
    switch current(state) {
    | None => {
        let loc = makeLocation(state, start)
        addDiagnostic(state, E0002, "Unterminated string literal", loc)
        addToken(state, Error("Unterminated string"), buf.contents, loc)
      }
    | Some("\"") if !isTriple => {
        advance(state)
        let loc = makeLocation(state, start)
        addToken(state, String(buf.contents), `"${buf.contents}"`, loc)
      }
    | Some("\"") if isTriple => {
        // Check for closing triple quote
        switch (next(state), peek(state, 2)) {
        | (Some("\""), Some("\"")) => {
            advance(state)
            advance(state)
            advance(state)
            let loc = makeLocation(state, start)
            addToken(state, String(buf.contents), `"""${buf.contents}"""`, loc)
          }
        | _ => {
            buf := buf.contents ++ "\""
            advance(state)
            loop()
          }
        }
      }
    | Some("\\") => {
        advance(state)
        switch current(state) {
        | Some("n") => {
            buf := buf.contents ++ "\n"
            advance(state)
          }
        | Some("r") => {
            buf := buf.contents ++ "\r"
            advance(state)
          }
        | Some("t") => {
            buf := buf.contents ++ "\t"
            advance(state)
          }
        | Some("\\") => {
            buf := buf.contents ++ "\\"
            advance(state)
          }
        | Some("\"") => {
            buf := buf.contents ++ "\""
            advance(state)
          }
        | Some("0") => {
            buf := buf.contents ++ "\x00"
            advance(state)
          }
        | Some(c) => {
            let loc = makeLocation(state, start)
            addDiagnostic(state, E0003, `Invalid escape sequence: \\${c}`, loc)
            advance(state)
          }
        | None => ()
        }
        loop()
      }
    | Some("\n") if !isTriple => {
        let loc = makeLocation(state, start)
        addDiagnostic(state, E0002, "Unterminated string literal (newline in string)", loc)
        addToken(state, Error("Unterminated string"), buf.contents, loc)
      }
    | Some(c) => {
        buf := buf.contents ++ c
        advance(state)
        loop()
      }
    }
  }

  loop()
}

// Scan comment (skip to end of line)
let scanComment = (state: lexerState): unit => {
  while {
    switch current(state) {
    | Some("\n") | None => false
    | Some(_) => true
    }
  } {
    advance(state)
  }
}

// Main tokenize function
let tokenize = (state: lexerState): unit => {
  while state.pos < String.length(state.source) {
    switch current(state) {
    // Skip whitespace (except newline)
    | Some(" ") | Some("\t") | Some("\r") => advance(state)

    // Newline
    | Some("\n") => {
        let start = getPosition(state)
        advance(state)
        let loc = makeLocation(state, start)
        addToken(state, Newline, "\\n", loc)
      }

    // Comment
    | Some("#") => scanComment(state)

    // Operators and delimiters
    | Some("+") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Plus, "+", makeLocation(state, start))
      }
    | Some("-") => {
        let start = getPosition(state)
        advance(state)
        switch current(state) {
        | Some(">") => {
            advance(state)
            addToken(state, Arrow, "->", makeLocation(state, start))
          }
        | _ => addToken(state, Minus, "-", makeLocation(state, start))
        }
      }
    | Some("*") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Star, "*", makeLocation(state, start))
      }
    | Some("/") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Slash, "/", makeLocation(state, start))
      }
    | Some("%") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Percent, "%", makeLocation(state, start))
      }
    | Some("=") => {
        let start = getPosition(state)
        advance(state)
        switch current(state) {
        | Some("=") => {
            advance(state)
            addToken(state, EqualEqual, "==", makeLocation(state, start))
          }
        | _ => addToken(state, Equal, "=", makeLocation(state, start))
        }
      }
    | Some("!") => {
        let start = getPosition(state)
        advance(state)
        switch current(state) {
        | Some("=") => {
            advance(state)
            addToken(state, BangEqual, "!=", makeLocation(state, start))
          }
        | _ => {
            addDiagnostic(state, E0001, "Unexpected character '!'", makeLocation(state, start))
          }
        }
      }
    | Some("<") => {
        let start = getPosition(state)
        advance(state)
        switch current(state) {
        | Some("=") => {
            advance(state)
            addToken(state, LessEqual, "<=", makeLocation(state, start))
          }
        | Some("<") => {
            advance(state)
            addToken(state, LessLess, "<<", makeLocation(state, start))
          }
        | _ => addToken(state, Less, "<", makeLocation(state, start))
        }
      }
    | Some(">") => {
        let start = getPosition(state)
        advance(state)
        switch current(state) {
        | Some("=") => {
            advance(state)
            addToken(state, GreaterEqual, ">=", makeLocation(state, start))
          }
        | Some(">") => {
            advance(state)
            addToken(state, GreaterGreater, ">>", makeLocation(state, start))
          }
        | _ => addToken(state, Greater, ">", makeLocation(state, start))
        }
      }
    | Some("&") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Ampersand, "&", makeLocation(state, start))
      }
    | Some("|") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Pipe, "|", makeLocation(state, start))
      }
    | Some("^") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Caret, "^", makeLocation(state, start))
      }
    | Some("~") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Tilde, "~", makeLocation(state, start))
      }
    | Some("?") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Question, "?", makeLocation(state, start))
      }
    | Some(":") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Colon, ":", makeLocation(state, start))
      }
    | Some("(") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, LParen, "(", makeLocation(state, start))
      }
    | Some(")") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, RParen, ")", makeLocation(state, start))
      }
    | Some("[") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, LBracket, "[", makeLocation(state, start))
      }
    | Some("]") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, RBracket, "]", makeLocation(state, start))
      }
    | Some("{") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, LBrace, "{", makeLocation(state, start))
      }
    | Some("}") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, RBrace, "}", makeLocation(state, start))
      }
    | Some(",") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Comma, ",", makeLocation(state, start))
      }
    | Some(".") => {
        let start = getPosition(state)
        advance(state)
        addToken(state, Dot, ".", makeLocation(state, start))
      }

    // String
    | Some("\"") => scanString(state)

    // Smart quotes (common error!)
    | Some("\u201C") | Some("\u201D") => {
        // " or "
        let start = getPosition(state)
        let c = current(state)->Option.getOr("")
        advance(state)
        let loc = makeLocation(state, start)
        addDiagnostic(state, E0007, `Smart quote '${c}' found - use straight quote '"' instead`, loc)
        addToken(state, Error("Smart quote"), c, loc)
      }

    // Number
    | Some(c) if isDigit(c) => scanNumber(state)

    // Identifier/keyword
    | Some(c) if isAlpha(c) => scanIdentifier(state)

    // Unknown character
    | Some(c) => {
        let start = getPosition(state)
        advance(state)
        let loc = makeLocation(state, start)
        addDiagnostic(state, E0004, `Illegal character '${c}'`, loc)
        addToken(state, Error(`Illegal: ${c}`), c, loc)
      }

    | None => ()
    }
  }

  // Add EOF token
  let eofPos = getPosition(state)
  addToken(state, EOF, "", {start: eofPos, end_: eofPos, file: state.file})
}

// Main entry point
let lex = (source: string, file: string, runNumber: int): (array<token>, array<diagnostic>) => {
  let state = make(source, file, runNumber)
  tokenize(state)
  (state.tokens, state.diagnostics)
}
