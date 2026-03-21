// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Incremental lexing support for Error-Lang.
//
// This module wraps the existing Error-Lang lexer to provide incremental
// re-lexing. When an edit occurs, only the affected token range is re-lexed,
// and the resulting tokens are spliced back into the cached token list.
//
// The design follows tree-sitter's approach:
// 1. Store the previous token list with byte offsets
// 2. On edit, receive an edit delta (start, old_end, new_text)
// 3. Find the affected token range
// 4. Re-lex only that range plus a small buffer
// 5. Splice the new tokens into the cached list

open Types

/// An edit to a source file. Replaces bytes [start, old_end) with new_text.
type edit = {
  start: int,
  oldEnd: int,
  newText: string,
}

/// A cached token with byte offsets into the source.
type cachedToken = {
  token: token,
  startOffset: int,
  endOffset: int,
}

/// The incremental lexer state.
type t = {
  source: string,
  tokens: array<cachedToken>,
}

/// Number of extra tokens to include before/after the edit region for
/// correct re-synchronisation.
let resyncBuffer = 2

/// Perform a full lex of the given source and return cached tokens.
let fullLex = (source: string): array<cachedToken> => {
  let (tokens, _diagnostics) = Lexer.lex(source, "<incremental>", 0)
  tokens->Array.map(tok => {
    token: tok,
    startOffset: tok.loc.start.offset,
    endOffset: tok.loc.end_.offset,
  })
}

/// Create a new incremental lexer from the given source.
let create = (source: string): t => {
  let tokens = fullLex(source)
  {source, tokens}
}

/// Get all current tokens.
let tokens = (state: t): array<cachedToken> => state.tokens

/// Get the current source text.
let source = (state: t): string => state.source

/// Get token types (excluding EOF) for comparison.
let tokenTypes = (state: t): array<tokenType> => {
  state.tokens
  ->Array.filter(ct => ct.token.type_ != EOF)
  ->Array.map(ct => ct.token.type_)
}

/// Binary search: find the first token index whose endOffset > target.
let findFirstAfter = (tokens: array<cachedToken>, target: int): int => {
  let lo = ref(0)
  let hi = ref(Array.length(tokens))
  while lo.contents < hi.contents {
    let mid = lo.contents + (hi.contents - lo.contents) / 2
    switch tokens->Array.get(mid) {
    | Some(ct) if ct.endOffset <= target => lo := mid + 1
    | _ => hi := mid
    }
  }
  lo.contents
}

/// Find the index of the first token starting at or past oldEnd.
let findLastAffected = (tokens: array<cachedToken>, oldEnd: int, from: int): int => {
  let n = Array.length(tokens)
  let result = ref(n)
  let i = ref(from)
  let found = ref(false)
  while i.contents < n && !found.contents {
    switch tokens->Array.get(i.contents) {
    | Some(ct) if ct.startOffset >= oldEnd => {
        result := i.contents
        found := true
      }
    | _ => i := i.contents + 1
    }
  }
  result.contents
}

/// Apply an edit and re-lex only the affected region.
/// Returns a new state with the updated source and tokens.
let edit = (state: t, e: edit): t => {
  let oldSource = state.source
  let oldLen = String.length(oldSource)

  // Apply the text edit to the source string.
  let prefix = String.substring(oldSource, ~start=0, ~end=e.start)
  let suffix = String.substring(oldSource, ~start=e.oldEnd, ~end=oldLen)
  let newSource = prefix ++ e.newText ++ suffix

  let newEnd = e.start + String.length(e.newText)
  let delta = newEnd - e.oldEnd
  let n = Array.length(state.tokens)

  // Find the first affected token index.
  let firstRaw = findFirstAfter(state.tokens, e.start)
  let firstAffected = Int.max(0, firstRaw - resyncBuffer)

  // Find the last affected token.
  let lastRaw = findLastAffected(state.tokens, e.oldEnd, firstAffected)
  let lastAffected = Int.min(n, lastRaw + resyncBuffer)

  // Determine byte range to re-lex in the new source.
  let relexStart = switch state.tokens->Array.get(firstAffected) {
  | Some(ct) => Int.min(ct.startOffset, e.start)
  | None => e.start
  }

  let relexEndOld = switch state.tokens->Array.get(lastAffected - 1) {
  | Some(ct) => Int.max(ct.endOffset, e.oldEnd)
  | None => e.oldEnd
  }
  let relexEnd = Int.min(String.length(newSource), relexEndOld + delta)

  // Re-lex the affected region.
  let region = String.substring(newSource, ~start=relexStart, ~end=relexEnd)
  let newTokensRaw = fullLex(region)

  // Offset new tokens and filter out EOF.
  let newTokens =
    newTokensRaw
    ->Array.filter(ct => ct.token.type_ != EOF)
    ->Array.map(ct => {
      token: {
        ...ct.token,
        loc: {
          ...ct.token.loc,
          start: {...ct.token.loc.start, offset: ct.token.loc.start.offset + relexStart},
          end_: {...ct.token.loc.end_, offset: ct.token.loc.end_.offset + relexStart},
        },
      },
      startOffset: ct.startOffset + relexStart,
      endOffset: ct.endOffset + relexStart,
    })

  // Build head: tokens before the affected region.
  let head = state.tokens->Array.slice(~start=0, ~end=firstAffected)

  // Build tail: tokens after the affected region with adjusted offsets.
  let tail =
    state.tokens
    ->Array.slice(~start=lastAffected, ~end=n)
    ->Array.map(ct => {
      token: {
        ...ct.token,
        loc: {
          ...ct.token.loc,
          start: {...ct.token.loc.start, offset: ct.token.loc.start.offset + delta},
          end_: {...ct.token.loc.end_, offset: ct.token.loc.end_.offset + delta},
        },
      },
      startOffset: ct.startOffset + delta,
      endOffset: ct.endOffset + delta,
    })

  // Combine head + new + tail.
  let combined = Array.concat(Array.concat(head, newTokens), tail)

  // Ensure EOF is present.
  let hasEof = combined->Array.some(ct => ct.token.type_ == EOF)
  let result = if hasEof {
    combined
  } else {
    let eofPos = String.length(newSource)
    let eofLoc = {
      start: {line: 1, column: 1, offset: eofPos},
      end_: {line: 1, column: 1, offset: eofPos},
      file: "<incremental>",
    }
    Array.concat(combined, [{
      token: {type_: EOF, lexeme: "", loc: eofLoc},
      startOffset: eofPos,
      endOffset: eofPos,
    }])
  }

  {source: newSource, tokens: result}
}

// ===========================================================================
// Tests
// ===========================================================================

/// Helper: compare token types from incremental vs fresh full lex.
let assertMatchesFullLex = (state: t): unit => {
  let incTypes = tokenTypes(state)
  let freshState = create(state.source)
  let fullTypes = tokenTypes(freshState)
  let n = Array.length(incTypes)
  let m = Array.length(fullTypes)
  if n != m {
    Console.error(`Token count mismatch: incremental=${n->Int.toString}, full=${m->Int.toString}`)
    assert(false)
  }
  for i in 0 to n - 1 {
    let inc = incTypes->Array.getUnsafe(i)
    let full = fullTypes->Array.getUnsafe(i)
    if inc != full {
      Console.error(`Token mismatch at index ${i->Int.toString}`)
      assert(false)
    }
  }
}

// Test: edit in middle of file
let testEditInMiddle = () => {
  let state = create("let x = 10 + 20")
  let state = edit(state, {start: 8, oldEnd: 10, newText: "42"})
  assert(source(state) == "let x = 42 + 20")
  assertMatchesFullLex(state)
}

// Test: edit at start
let testEditAtStart = () => {
  let state = create("let x = 5")
  let state = edit(state, {start: 0, oldEnd: 3, newText: "function"})
  assert(source(state) == "function x = 5")
  assertMatchesFullLex(state)
}

// Test: edit at end
let testEditAtEnd = () => {
  let state = create("let x = 5")
  let len = String.length(source(state))
  let state = edit(state, {start: len, oldEnd: len, newText: " + 1"})
  assert(source(state) == "let x = 5 + 1")
  assertMatchesFullLex(state)
}

// Test: insert new text
let testInsertText = () => {
  let state = create("let x")
  let state = edit(state, {start: 5, oldEnd: 5, newText: " = 42"})
  assert(source(state) == "let x = 42")
  assertMatchesFullLex(state)
}

// Test: delete text
let testDeleteText = () => {
  let state = create("let x = 10 + 20")
  let state = edit(state, {start: 11, oldEnd: 15, newText: ""})
  assert(source(state) == "let x = 10 ")
  assertMatchesFullLex(state)
}

// Test: replace text
let testReplaceText = () => {
  let state = create("let x = 10")
  let state = edit(state, {start: 4, oldEnd: 10, newText: "name = 42"})
  assert(source(state) == "let name = 42")
  assertMatchesFullLex(state)
}

// Test: token boundary change (== to =)
let testTokenBoundaryChange = () => {
  let state = create("x == y")
  let state = edit(state, {start: 2, oldEnd: 4, newText: "="})
  assert(source(state) == "x = y")
  assertMatchesFullLex(state)
}

// Test: edit inside a string literal
let testEditInsideString = () => {
  let state = create("let s = \"hello world\"")
  let state = edit(state, {start: 9, oldEnd: 14, newText: "goodbye"})
  assert(source(state) == "let s = \"goodbye world\"")
  assertMatchesFullLex(state)
}

// Test: edit inside a comment
let testEditInsideComment = () => {
  let state = create("# old comment\nlet x = 1")
  let state = edit(state, {start: 2, oldEnd: 5, newText: "new"})
  assert(source(state) == "# new comment\nlet x = 1")
  assertMatchesFullLex(state)
}

// Test: empty source then insert
let testEmptySource = () => {
  let state = create("")
  let state = edit(state, {start: 0, oldEnd: 0, newText: "let x = 1"})
  assert(source(state) == "let x = 1")
  assertMatchesFullLex(state)
}

// Test: delete all
let testDeleteAll = () => {
  let state = create("let x = 1")
  let len = String.length(source(state))
  let state = edit(state, {start: 0, oldEnd: len, newText: ""})
  assert(source(state) == "")
  assertMatchesFullLex(state)
}

// Test: multiple sequential edits
let testMultipleEdits = () => {
  let state = create("let a = 1\nlet b = 2")
  let state = edit(state, {start: 4, oldEnd: 5, newText: "x"})
  assertMatchesFullLex(state)
  let state = edit(state, {start: 14, oldEnd: 15, newText: "y"})
  assert(source(state) == "let x = 1\nlet y = 2")
  assertMatchesFullLex(state)
}

// Run all tests when this module is loaded.
let runTests = () => {
  testEditInMiddle()
  testEditAtStart()
  testEditAtEnd()
  testInsertText()
  testDeleteText()
  testReplaceText()
  testTokenBoundaryChange()
  testEditInsideString()
  testEditInsideComment()
  testEmptySource()
  testDeleteAll()
  testMultipleEdits()
}
