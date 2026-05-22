// SPDX-License-Identifier: MPL-2.0
// MonacoBindings.res - ReScript bindings for Monaco Editor
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

/**
 * MonacoBindings - Foreign function interface for Monaco Editor
 *
 * Provides type-safe ReScript bindings to the Monaco Editor API.
 * Monaco is loaded via CDN (esm.sh) in index.html, so these bindings
 * reference the globally available `monaco` object via MonacoInterop.js.
 *
 * This module handles:
 * - Editor creation and lifecycle
 * - Custom language registration (Error-Lang syntax)
 * - Theme registration (Error-Lang dark/light themes)
 * - Event forwarding (content changes, cursor moves, etc.)
 */

// Monaco editor instance (opaque type)
type editor

// Monaco text model
type textModel

// Monaco position
type monacoPosition = {
  lineNumber: int,
  column: int,
}

// Monaco range
type monacoRange = {
  startLineNumber: int,
  startColumn: int,
  endLineNumber: int,
  endColumn: int,
}

// Decoration options
type decorationOptions = {
  isWholeLine: bool,
  className: string,
  glyphMarginClassName: string,
}

// Decoration description
type decoration = {
  range: monacoRange,
  options: decorationOptions,
}

// Editor creation options
type editorOptions = {
  value: string,
  language: string,
  theme: string,
  fontSize: int,
  minimap: {"enabled": bool},
  lineNumbers: string,
  renderWhitespace: string,
  automaticLayout: bool,
  scrollBeyondLastLine: bool,
  wordWrap: string,
  tabSize: int,
  insertSpaces: bool,
  glyphMargin: bool,
  folding: bool,
  bracketPairColorization: {"enabled": bool},
  padding: {"top": int},
}

// Content change event from Monaco
type contentChangeEvent

// Cursor position change event from Monaco
type cursorPositionChangeEvent = {
  position: monacoPosition,
}

// Marker severity level
type markerSeverity = int

// Marker data for error/warning squiggles
type markerData = {
  severity: markerSeverity,
  message: string,
  startLineNumber: int,
  startColumn: int,
  endLineNumber: int,
  endColumn: int,
  source: string,
}

// ============================================
// External bindings (FFI to MonacoInterop.js)
// ============================================

@module("./MonacoInterop.js")
external createEditor: (Dom.element, editorOptions) => editor = "createEditor"

@module("./MonacoInterop.js")
external disposeEditor: editor => unit = "disposeEditor"

@module("./MonacoInterop.js")
external setEditorValue: (editor, string) => unit = "setEditorValue"

@module("./MonacoInterop.js")
external getEditorValue: editor => string = "getEditorValue"

@module("./MonacoInterop.js")
external setEditorTheme: string => unit = "setEditorTheme"

@module("./MonacoInterop.js")
external setEditorFontSize: (editor, int) => unit = "setEditorFontSize"

@module("./MonacoInterop.js")
external onDidChangeContent: (editor, unit => unit) => unit = "onDidChangeContent"

@module("./MonacoInterop.js")
external onDidChangeCursorPosition: (editor, cursorPositionChangeEvent => unit) => unit = "onDidChangeCursorPosition"

@module("./MonacoInterop.js")
external setModelMarkers: (editor, array<markerData>) => unit = "setModelMarkers"

@module("./MonacoInterop.js")
external registerErrorLangLanguage: unit => unit = "registerErrorLangLanguage"

@module("./MonacoInterop.js")
external registerErrorLangThemes: unit => unit = "registerErrorLangThemes"

@module("./MonacoInterop.js")
external revealLineInCenter: (editor, int) => unit = "revealLineInCenter"

@module("./MonacoInterop.js")
external setPosition: (editor, monacoPosition) => unit = "setPosition"

@module("./MonacoInterop.js")
external focusEditor: editor => unit = "focusEditor"

@module("./MonacoInterop.js")
external getMarkerSeverityError: unit => markerSeverity = "getMarkerSeverityError"

@module("./MonacoInterop.js")
external getMarkerSeverityWarning: unit => markerSeverity = "getMarkerSeverityWarning"

@module("./MonacoInterop.js")
external getMarkerSeverityInfo: unit => markerSeverity = "getMarkerSeverityInfo"

// ============================================
// High-level helpers
// ============================================

// Default editor options for Error-Lang
let makeDefaultOptions = (~code: string, ~theme: string, ~fontSize: int): editorOptions => {
  value: code,
  language: "error-lang",
  theme: theme,
  fontSize: fontSize,
  minimap: {"enabled": false},
  lineNumbers: "on",
  renderWhitespace: "selection",
  automaticLayout: true,
  scrollBeyondLastLine: false,
  wordWrap: "off",
  tabSize: 4,
  insertSpaces: true,
  glyphMargin: true,
  folding: true,
  bracketPairColorization: {"enabled": true},
  padding: {"top": 8},
}

// Map IDE theme to Monaco theme ID
let themeToMonacoTheme = (theme: string): string =>
  switch theme {
  | "light" => "error-lang-light"
  | "dark" => "error-lang-dark"
  | "highcontrast" => "hc-black"
  | _ => "error-lang-dark"
  }

// Create error marker for parse errors
let makeErrorMarker = (
  ~startLine: int,
  ~startCol: int,
  ~endLine: int,
  ~endCol: int,
  ~message: string,
): markerData => {
  severity: getMarkerSeverityError(),
  message: message,
  startLineNumber: startLine,
  startColumn: startCol,
  endLineNumber: endLine,
  endColumn: endCol,
  source: "error-lang",
}

// Create warning marker for stability issues
let makeWarningMarker = (
  ~startLine: int,
  ~startCol: int,
  ~endLine: int,
  ~endCol: int,
  ~message: string,
): markerData => {
  severity: getMarkerSeverityWarning(),
  message: message,
  startLineNumber: startLine,
  startColumn: startCol,
  endLineNumber: endLine,
  endColumn: endCol,
  source: "error-lang-stability",
}
