// SPDX-License-Identifier: MPL-2.0
// IncrementalParser.res - Incremental parsing wrapper for Error-Lang
//
// Wraps the existing Parser to provide incremental re-parsing.
// Instead of re-parsing the entire file on every edit, we cache
// top-level declarations with byte ranges and re-parse only the
// affected declarations when an edit occurs.
//
// Strategy:
// 1. Cache the full source and a list of CachedDecl records with byte ranges.
// 2. On edit, apply the text change, find overlapping declarations.
// 3. Re-parse only those declarations via Parser.parse.
// 4. Splice the new declarations into the cache, adjusting byte offsets.

open Types

// ============================================
// Types
// ============================================

/// Describes a text edit applied to the source.
type edit = {
  start: int,
  oldEnd: int,
  newText: string,
}

/// A cached top-level declaration with its byte range.
type cachedDecl = {
  kind: string,
  start: int,
  end_: int,
  decl: decl,
}

/// Diagnostic from incremental parsing.
type parseDiagnostic = {
  message: string,
  offset: int,
}

/// The incremental parser state.
type t = {
  mutable source: string,
  mutable items: array<cachedDecl>,
  mutable errors: array<diagnostic>,
}

// ============================================
// Helpers
// ============================================

/// Classify a declaration to a kind string.
let declKind = (d: decl): string =>
  switch d {
  | FunctionDecl(_) => "Function"
  | StructDecl(_) => "Struct"
  | MainBlock(_) => "Main"
  | StmtDecl(_) => "Statement"
  }

/// Top-level keywords that can start a declaration in Error-Lang.
let topKeywords = ["function", "main", "struct", "let", "if", "while", "for", "gutter"]

/// Check if a substring at the given position starts with a top-level keyword.
let startsWithKeyword = (src: string, pos: int): bool => {
  Array.some(topKeywords->Array.map(kw => {
    let kwLen = String.length(kw)
    if pos + kwLen <= String.length(src) {
      let sub = String.substring(src, ~start=pos, ~end=pos + kwLen)
      if sub == kw {
        // Check it is a whole word
        if pos + kwLen >= String.length(src) {
          true
        } else {
          let c = String.charCodeAt(src, pos + kwLen)
          Float.isNaN(c) || (!(c >= 97.0 && c <= 122.0) && !(c >= 65.0 && c <= 90.0) && !(c >= 48.0 && c <= 57.0) && c != 95.0)
        }
      } else {
        false
      }
    } else {
      false
    }
  }), x => x)
}

/// Find byte offsets of top-level declaration boundaries.
let findDeclBoundaries = (src: string): array<int> => {
  let boundaries = ref([])
  let lines = String.split(src, "\n")
  let offset = ref(0)

  Array.forEach(lines, line => {
    let trimmed = String.trimStart(line)
    let trimOffset = offset.contents + (String.length(line) - String.length(trimmed))
    if String.length(trimmed) > 0 && startsWithKeyword(src, trimOffset) {
      boundaries := Array.concat(boundaries.contents, [trimOffset])
    }
    offset := offset.contents + String.length(line) + 1 // +1 for newline
  })

  boundaries.contents
}

/// Build cached declarations from parsed program and source text.
let buildCache = (prog: program, src: string): array<cachedDecl> => {
  let boundaries = findDeclBoundaries(src)
  let decls = prog.declarations
  let srcLen = String.length(src)
  let nDecls = Array.length(decls)
  let nBounds = Array.length(boundaries)

  if nDecls == 0 {
    []
  } else if nBounds == nDecls {
    Array.mapWithIndex(decls, (decl, i) => {
      let s = boundaries[i]
      let e = if i + 1 < nBounds {
        boundaries[i + 1]
      } else {
        srcLen
      }
      {kind: declKind(decl), start: s, end_: e, decl}
    })
  } else {
    // Fallback: distribute evenly
    Array.mapWithIndex(decls, (decl, i) => {
      let s = i * srcLen / nDecls
      let e = (i + 1) * srcLen / nDecls
      {kind: declKind(decl), start: s, end_: e, decl}
    })
  }
}

/// Lex the source string into tokens.
let lexSource = (src: string, file: string): array<token> => {
  let (tokens, _diags) = Lexer.lex(src, file, 0)
  tokens
}

/// Full parse: lex and parse the source.
let fullParse = (src: string, file: string): (array<cachedDecl>, array<diagnostic>) => {
  let (tokens, lexDiags) = Lexer.lex(src, file, 0)
  let (prog, parseDiags) = Parser.parse(tokens, file, 0)
  let items = buildCache(prog, src)
  (items, Array.concat(lexDiags, parseDiags))
}

// ============================================
// Public API
// ============================================

/// Create a new incremental parser by performing a full initial parse.
let make = (source: string, file: string): t => {
  let (items, errors) = fullParse(source, file)
  {source, items, errors}
}

/// Apply a text edit and re-parse only the affected declarations.
/// Returns diagnostics from the re-parsed region.
let edit = (t: t, e: edit, file: string): array<parseDiagnostic> => {
  let editLenDiff = String.length(e.newText) - (e.oldEnd - e.start)

  // 1. Apply text edit to source
  let before = String.substring(t.source, ~start=0, ~end=e.start)
  let after = String.substring(t.source, ~start=e.oldEnd, ~end=String.length(t.source))
  t.source = before ++ e.newText ++ after

  // 2. Find affected items
  let firstAffected = ref(None)
  let lastAffected = ref(None)

  Array.forEachWithIndex(t.items, (item, i) => {
    if item.end_ > e.start && item.start < e.oldEnd {
      if Option.isNone(firstAffected.contents) {
        firstAffected := Some(i)
      }
      lastAffected := Some(i)
    }
  })

  switch (firstAffected.contents, lastAffected.contents) {
  | (None, _) | (_, None) =>
    // No overlap — full re-parse
    let (items, errors) = fullParse(t.source, file)
    t.items = items
    t.errors = errors
    Array.map(errors, e => {
      {message: formatDiagnostic(e), offset: 0}
    })

  | (Some(first), Some(last)) =>
    // 3. Determine re-parse range
    let reparseStart = t.items[first].start
    let oldReparseEnd = t.items[last].end_
    let reparseEnd = Int.min(oldReparseEnd + editLenDiff, String.length(t.source))
    let fragmentLen = Int.max(reparseEnd - reparseStart, 0)
    let fragment = String.substring(t.source, ~start=reparseStart, ~end=reparseStart + fragmentLen)

    // 4. Re-parse the fragment
    let (tokens, _lexDiags) = Lexer.lex(fragment, file, 0)
    let (prog, errors) = Parser.parse(tokens, file, 0)

    let diagnostics = Array.map(errors, e => {
      {message: formatDiagnostic(e), offset: reparseStart}
    })

    // 5. Build new cached items from fragment, offset by reparseStart
    let newCached = buildCache(prog, fragment)->Array.map(ci => {
      {...ci, start: ci.start + reparseStart, end_: ci.end_ + reparseStart}
    })

    // 6. Splice: build before/after arrays using index ranges
    let beforeItems = Array.slice(t.items, ~start=0, ~end=first)
    let afterItems = Array.sliceToEnd(t.items, ~start=last + 1)->Array.map(item => {
      {...item, start: item.start + editLenDiff, end_: item.end_ + editLenDiff}
    })

    t.items = Array.concatMany([beforeItems, newCached, afterItems])
    t.errors = errors
    diagnostics
  }
}

/// Return the current cached declarations.
let items = (t: t): array<cachedDecl> => t.items

/// Reconstruct the full AST program from cached declarations.
let fullAst = (t: t): program => {
  {
    declarations: Array.map(t.items, ci => ci.decl),
    loc: {
      start: {line: 1, column: 1, offset: 0},
      end_: {line: 0, column: 0, offset: String.length(t.source)},
      file: "<incremental>",
    },
  }
}

/// Return the current source text.
let source = (t: t): string => t.source
