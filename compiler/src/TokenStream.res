// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

/// Token Stream API for macro systems.
///
/// This module provides the foundational token stream abstraction used by
/// procedural and declarative macro systems. Macros receive a token stream
/// as input, manipulate it, and produce a token stream as output.
///
/// The design follows Rust's `proc_macro::TokenStream` as a reference but
/// is adapted for ReScript-based nextgen languages (Error-Lang).
///
/// ## Architecture
///
/// A token stream is an array of token tree values. Each token tree is one
/// of four variants:
///
/// - `Ident` — an identifier or keyword
/// - `Punct` — a single punctuation character with spacing information
/// - `Literal` — a numeric, string, char, or boolean literal
/// - `Group` — a delimited group of tokens (parentheses, brackets, braces)
///
/// Keywords are treated as identifiers at this level. Multi-character
/// operators (e.g., `!=`, `->`) are represented as multiple `Punct` nodes
/// with `Joint` spacing on all but the last.

open Types

/// Spacing information for punctuation tokens.
///
/// Determines whether adjacent punctuation characters form a single
/// multi-character operator or are separate tokens.
type spacing =
  /// Followed by whitespace or a non-punctuation token.
  | Alone
  /// Immediately followed by another punctuation character.
  | Joint

/// The kind of a literal value.
///
/// Discriminates between different literal categories without requiring
/// the token stream to parse the literal value.
type literalKind =
  /// Integer literal: `42`, `0xFF`, `0b1010`, `0o77`.
  | LitInteger
  /// Floating-point literal: `3.14`, `1.5e10`.
  | LitFloat
  /// String literal: `"hello"`.
  | LitString
  /// Character literal: `'a'`, `'\n'`.
  | LitChar
  /// Boolean literal: `true`, `false`.
  | LitBool

/// Delimiter kind for groups.
///
/// Determines which matched pair of characters surrounds the group.
type delimiter =
  /// Round parentheses: `( ... )`.
  | Paren
  /// Square brackets: `[ ... ]`.
  | Bracket
  /// Curly braces: `{ ... }`.
  | Brace
  /// No visible delimiter (implicit grouping from macro expansion).
  | NoneDelim

/// A span in the source code for token stream nodes.
///
/// Uses byte offsets rather than the full `location` type from `Types`
/// to keep the token stream lightweight.
type tsSpan = {
  start: int,
  end_: int,
  file: string,
}

/// A single token tree — the fundamental unit of the token stream.
///
/// Every token in source code maps to exactly one token tree, except
/// that delimiter pairs and their contents are collapsed into a single
/// `Group` node.
type rec tokenTree =
  /// An identifier: variable name, keyword, type name, etc.
  /// Keywords are not distinguished from identifiers at this level.
  | TtIdent({sym: string, span: tsSpan})
  /// A single punctuation character with spacing information.
  /// Multi-character operators are multiple `TtPunct` nodes with
  /// `Joint` spacing on all but the last.
  | TtPunct({ch: string, spacing: spacing, span: tsSpan})
  /// A literal value: integer, float, string, char, or boolean.
  /// Preserves the original source text for round-tripping.
  | TtLiteral({kind: literalKind, text: string, span: tsSpan})
  /// A delimited group of tokens: `(...)`, `[...]`, `{...}`.
  /// The delimiter tokens themselves are captured by the delimiter field.
  | TtGroup({delimiter: delimiter, stream: tokenStream, span: tsSpan})

/// A sequence of token trees, the fundamental input/output type for macros.
and tokenStream = array<tokenTree>

/// Parse error from lexing source into a token stream.
type parseError = {
  message: string,
  offset: int,
}

/// A dummy span for programmatically constructed tokens.
let dummySpan: tsSpan = {start: 0, end_: 0, file: "<generated>"}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

/// Creates an empty token stream.
let empty = (): tokenStream => []

/// Creates an identifier token tree.
let ident = (sym: string, span: tsSpan): tokenTree => TtIdent({sym, span})

/// Creates a punctuation token tree.
let punct = (ch: string, spacing: spacing, span: tsSpan): tokenTree =>
  TtPunct({ch, spacing, span})

/// Creates a literal token tree.
let literal = (kind: literalKind, text: string, span: tsSpan): tokenTree =>
  TtLiteral({kind, text, span})

/// Creates a delimited group token tree.
let group = (delimiter: delimiter, stream: tokenStream, span: tsSpan): tokenTree =>
  TtGroup({delimiter, stream, span})

// ---------------------------------------------------------------------------
// Accessors
// ---------------------------------------------------------------------------

/// Returns the span of a token tree.
let spanOf = (tree: tokenTree): tsSpan =>
  switch tree {
  | TtIdent({span, _}) => span
  | TtPunct({span, _}) => span
  | TtLiteral({span, _}) => span
  | TtGroup({span, _}) => span
  }

/// Returns `true` if the stream is empty.
let isEmpty = (stream: tokenStream): bool => Array.length(stream) == 0

/// Returns the number of top-level token trees.
let length = (stream: tokenStream): int => Array.length(stream)

/// Returns the span covering the entire stream, or a dummy span if empty.
let streamSpan = (stream: tokenStream): tsSpan => {
  let len = Array.length(stream)
  if len == 0 {
    dummySpan
  } else {
    let first = spanOf(Array.getUnsafe(stream, 0))
    let last = spanOf(Array.getUnsafe(stream, len - 1))
    {start: first.start, end_: last.end_, file: first.file}
  }
}

// ---------------------------------------------------------------------------
// Stream operations
// ---------------------------------------------------------------------------

/// Concatenates two token streams.
let concat = (a: tokenStream, b: tokenStream): tokenStream =>
  Array.concat(a, b)

/// Appends a token tree to the end of a stream.
let push = (stream: tokenStream, tree: tokenTree): tokenStream =>
  Array.concat(stream, [tree])

/// Flattens a list of token streams into one.
let flatten = (streams: array<tokenStream>): tokenStream =>
  Array.flatMap(streams, x => x)

// ---------------------------------------------------------------------------
// Pretty-printing — reconstruct source from token stream
// ---------------------------------------------------------------------------

/// Converts a single token tree to its string representation.
let rec treeToString = (tree: tokenTree): string =>
  switch tree {
  | TtIdent({sym, _}) => sym
  | TtPunct({ch, _}) => ch
  | TtLiteral({text, _}) => text
  | TtGroup({delimiter, stream, _}) =>
    let inner = toSource(stream)
    switch delimiter {
    | Paren => "(" ++ inner ++ ")"
    | Bracket => "[" ++ inner ++ "]"
    | Brace => "{" ++ inner ++ "}"
    | NoneDelim => inner
    }
  }

/// Reconstructs source text from a token stream.
///
/// Inserts a single space between tokens, respecting `Joint` spacing
/// for multi-character operators. The output, when re-lexed, yields a
/// structurally equivalent token stream.
and toSource = (stream: tokenStream): string => {
  let buf = ref("")
  let first = ref(true)
  Array.forEach(stream, tree => {
    if !first.contents {
      let prefix = switch tree {
      | TtPunct({spacing: Joint, _}) => ""
      | _ => " "
      }
      buf := buf.contents ++ prefix
    }
    buf := buf.contents ++ treeToString(tree)
    first := false
  })
  buf.contents
}

// ---------------------------------------------------------------------------
// Parsing — lex source text into a token stream
// ---------------------------------------------------------------------------

/// Returns `true` if the character is a punctuation character recognised
/// by the token stream lexer. Delimiters are excluded.
let isPunctChar = (ch: string): bool =>
  switch ch {
  | "+" | "-" | "*" | "/" | "%" | "=" | "!" | "<" | ">" | "&" | "|"
  | "^" | "~" | "." | "," | ";" | ":" | "@" | "#" | "?" | "\\" => true
  | _ => false
  }

let isAlpha = (ch: string): bool => {
  let c = String.charCodeAt(ch, 0)
  (c >= 65.0 && c <= 90.0) || (c >= 97.0 && c <= 122.0) || ch == "_"
}

let isDigit = (ch: string): bool => {
  let c = String.charCodeAt(ch, 0)
  c >= 48.0 && c <= 57.0
}

let isAlnum = (ch: string): bool => isAlpha(ch) || isDigit(ch)

let isWhitespace = (ch: string): bool =>
  ch == " " || ch == "\t" || ch == "\r" || ch == "\n"

/// Lexes source text into a token stream.
///
/// Handles identifiers, keywords (as identifiers), numeric literals
/// (decimal, hex, binary, octal, float), string and character literals,
/// boolean literals, punctuation with correct spacing, delimiter grouping,
/// and line/block comments.
///
/// Returns `Ok(stream)` on success or `Error(parseError)` on failure.
let ofString = (source: string, ~file: string="<input>"): result<tokenStream, parseError> => {
  let len = String.length(source)
  let pos = ref(0)

  // Stack for nested groups: (delimiter, startPos, parentTrees)
  let stack: ref<list<(delimiter, int, array<tokenTree>)>> = ref(list{})
  let trees: ref<array<tokenTree>> = ref([])
  let error: ref<option<parseError>> = ref(None)

  let peekAt = (i: int): option<string> =>
    if i < len {
      Some(String.charAt(source, i))
    } else {
      None
    }

  let makeSpan = (start: int): tsSpan => {
    start,
    end_: pos.contents,
    file,
  }

  while pos.contents < len && error.contents == None {
    let ch = String.charAt(source, pos.contents)

    // Whitespace
    if isWhitespace(ch) {
      pos := pos.contents + 1
    }
    // Line comment
    else if ch == "/" && peekAt(pos.contents + 1) == Some("/") {
      pos := pos.contents + 2
      while pos.contents < len && String.charAt(source, pos.contents) != "\n" {
        pos := pos.contents + 1
      }
    }
    // Block comment
    else if ch == "/" && peekAt(pos.contents + 1) == Some("*") {
      let start = pos.contents
      pos := pos.contents + 2
      let depth = ref(1)
      while pos.contents + 1 < len && depth.contents > 0 {
        let c1 = String.charAt(source, pos.contents)
        let c2 = String.charAt(source, pos.contents + 1)
        if c1 == "/" && c2 == "*" {
          depth := depth.contents + 1
          pos := pos.contents + 2
        } else if c1 == "*" && c2 == "/" {
          depth := depth.contents - 1
          pos := pos.contents + 2
        } else {
          pos := pos.contents + 1
        }
      }
      if depth.contents > 0 {
        error := Some({message: "unterminated block comment", offset: start})
      }
    }
    // Opening delimiters
    else if ch == "(" || ch == "[" || ch == "{" {
      let delim = switch ch {
      | "(" => Paren
      | "[" => Bracket
      | "{" => Brace
      | _ => Paren // unreachable
      }
      stack := list{(delim, pos.contents, trees.contents), ...stack.contents}
      trees := []
      pos := pos.contents + 1
    }
    // Closing delimiters
    else if ch == ")" || ch == "]" || ch == "}" {
      let expected = switch ch {
      | ")" => Paren
      | "]" => Bracket
      | "}" => Brace
      | _ => Paren // unreachable
      }
      switch stack.contents {
      | list{(delim, start, parentTrees), ...rest} if delim == expected =>
        pos := pos.contents + 1
        let grp = TtGroup({
          delimiter: delim,
          stream: trees.contents,
          span: {start, end_: pos.contents, file},
        })
        stack := rest
        trees := Array.concat(parentTrees, [grp])
      | list{(delim, _, _), ..._} =>
        let expectedChar = switch delim {
        | Paren => ")"
        | Bracket => "]"
        | Brace => "}"
        | NoneDelim => "?"
        }
        error := Some({
          message: "mismatched delimiter: expected '" ++
            expectedChar ++ "', found '" ++ ch ++ "'",
          offset: pos.contents,
        })
      | list{} =>
        error := Some({
          message: "unexpected closing delimiter '" ++ ch ++ "'",
          offset: pos.contents,
        })
      }
    }
    // String literal
    else if ch == "\"" {
      let start = pos.contents
      pos := pos.contents + 1
      let done = ref(false)
      while pos.contents < len && !done.contents {
        let c = String.charAt(source, pos.contents)
        if c == "\\" {
          pos := pos.contents + 2
        } else if c == "\"" {
          pos := pos.contents + 1
          done := true
        } else {
          pos := pos.contents + 1
        }
      }
      if !done.contents {
        error := Some({message: "unterminated string literal", offset: start})
      } else {
        let text = String.slice(source, ~start, ~end=pos.contents)
        trees := Array.concat(trees.contents, [
          TtLiteral({kind: LitString, text, span: makeSpan(start)}),
        ])
      }
    }
    // Character literal
    else if ch == "'" {
      let start = pos.contents
      pos := pos.contents + 1
      if pos.contents < len && String.charAt(source, pos.contents) == "\\" {
        pos := pos.contents + 2 // escaped char
      } else if pos.contents < len {
        pos := pos.contents + 1
      }
      if pos.contents >= len || String.charAt(source, pos.contents) != "'" {
        error := Some({message: "unterminated character literal", offset: start})
      } else {
        pos := pos.contents + 1 // closing quote
        let text = String.slice(source, ~start, ~end=pos.contents)
        trees := Array.concat(trees.contents, [
          TtLiteral({kind: LitChar, text, span: makeSpan(start)}),
        ])
      }
    }
    // Numeric literal
    else if isDigit(ch) {
      let start = pos.contents
      let isFloat = ref(false)

      // Check base prefix
      if ch == "0" && pos.contents + 1 < len {
        let next = String.charAt(source, pos.contents + 1)
        if next == "x" || next == "X" || next == "b" || next == "B" ||
           next == "o" || next == "O" {
          pos := pos.contents + 2
          while pos.contents < len && isAlnum(String.charAt(source, pos.contents)) {
            pos := pos.contents + 1
          }
          let text = String.slice(source, ~start, ~end=pos.contents)
          trees := Array.concat(trees.contents, [
            TtLiteral({kind: LitInteger, text, span: makeSpan(start)}),
          ])
        } else {
          // Regular decimal starting with 0
          while pos.contents < len && (isDigit(String.charAt(source, pos.contents)) ||
            String.charAt(source, pos.contents) == "_") {
            pos := pos.contents + 1
          }
          // Decimal point
          if pos.contents < len && String.charAt(source, pos.contents) == "." &&
             pos.contents + 1 < len && isDigit(String.charAt(source, pos.contents + 1)) {
            isFloat := true
            pos := pos.contents + 1
            while pos.contents < len && (isDigit(String.charAt(source, pos.contents)) ||
              String.charAt(source, pos.contents) == "_") {
              pos := pos.contents + 1
            }
          }
          // Exponent
          if pos.contents < len && (String.charAt(source, pos.contents) == "e" ||
             String.charAt(source, pos.contents) == "E") {
            isFloat := true
            pos := pos.contents + 1
            if pos.contents < len && (String.charAt(source, pos.contents) == "+" ||
               String.charAt(source, pos.contents) == "-") {
              pos := pos.contents + 1
            }
            while pos.contents < len && isDigit(String.charAt(source, pos.contents)) {
              pos := pos.contents + 1
            }
          }
          let text = String.slice(source, ~start, ~end=pos.contents)
          let kind = if isFloat.contents { LitFloat } else { LitInteger }
          trees := Array.concat(trees.contents, [
            TtLiteral({kind, text, span: makeSpan(start)}),
          ])
        }
      } else {
        // Regular decimal
        while pos.contents < len && (isDigit(String.charAt(source, pos.contents)) ||
          String.charAt(source, pos.contents) == "_") {
          pos := pos.contents + 1
        }
        if pos.contents < len && String.charAt(source, pos.contents) == "." &&
           pos.contents + 1 < len && isDigit(String.charAt(source, pos.contents + 1)) {
          isFloat := true
          pos := pos.contents + 1
          while pos.contents < len && (isDigit(String.charAt(source, pos.contents)) ||
            String.charAt(source, pos.contents) == "_") {
            pos := pos.contents + 1
          }
        }
        if pos.contents < len && (String.charAt(source, pos.contents) == "e" ||
           String.charAt(source, pos.contents) == "E") {
          isFloat := true
          pos := pos.contents + 1
          if pos.contents < len && (String.charAt(source, pos.contents) == "+" ||
             String.charAt(source, pos.contents) == "-") {
            pos := pos.contents + 1
          }
          while pos.contents < len && isDigit(String.charAt(source, pos.contents)) {
            pos := pos.contents + 1
          }
        }
        let text = String.slice(source, ~start, ~end=pos.contents)
        let kind = if isFloat.contents { LitFloat } else { LitInteger }
        trees := Array.concat(trees.contents, [
          TtLiteral({kind, text, span: makeSpan(start)}),
        ])
      }
    }
    // Identifier or keyword (including true/false)
    else if isAlpha(ch) {
      let start = pos.contents
      while pos.contents < len && isAlnum(String.charAt(source, pos.contents)) {
        pos := pos.contents + 1
      }
      let text = String.slice(source, ~start, ~end=pos.contents)
      let tree = if text == "true" || text == "false" {
        TtLiteral({kind: LitBool, text, span: makeSpan(start)})
      } else {
        TtIdent({sym: text, span: makeSpan(start)})
      }
      trees := Array.concat(trees.contents, [tree])
    }
    // Punctuation
    else if isPunctChar(ch) {
      let start = pos.contents
      pos := pos.contents + 1
      let spacing = if pos.contents < len && isPunctChar(String.charAt(source, pos.contents)) {
        Joint
      } else {
        Alone
      }
      trees := Array.concat(trees.contents, [
        TtPunct({ch, spacing, span: makeSpan(start)}),
      ])
    }
    // Unknown character
    else {
      error := Some({
        message: "unexpected character: '" ++ ch ++ "'",
        offset: pos.contents,
      })
    }
  }

  switch error.contents {
  | Some(err) => Error(err)
  | None =>
    switch stack.contents {
    | list{(delim, start, _), ..._} =>
      let ch = switch delim {
      | Paren => "("
      | Bracket => "["
      | Brace => "{"
      | NoneDelim => "?"
      }
      Error({message: "unclosed delimiter '" ++ ch ++ "'", offset: start})
    | list{} => Ok(trees.contents)
    }
  }
}
