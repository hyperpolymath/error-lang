// SPDX-License-Identifier: MPL-2.0
// monaco-setup.js - Monaco Editor initialization for Error-Lang Studio
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// This script initializes the Monaco editor once it is loaded via the
// AMD loader. It registers the Error-Lang language, themes, creates
// the editor instance, and wires up event handlers.
//
// Monaco is loaded via AMD (require.js) because that is the standard
// distribution method for Monaco Editor - it does not support ES modules
// natively. We use the CDN-hosted version to avoid npm/node dependencies.

import {
  registerErrorLangLanguage,
  registerErrorLangThemes,
  createEditor,
  getEditorValue,
  onDidChangeContent,
  onDidChangeCursorPosition,
  setEditorTheme,
  setModelMarkers,
  setEditorFontSize,
} from "./MonacoInterop.js";

// ============================================
// Global state for the Monaco editor instance
// ============================================

// The active Monaco editor instance (accessible from ReScript via window)
let editorInstance = null;

// Debounce timer for content change events
let contentChangeTimer = null;

// Current theme name
let currentTheme = "error-lang-dark";

/**
 * Initialize Monaco editor after AMD loader has loaded it.
 * Called from the require() callback in index.html.
 */
export function initMonacoEditor() {
  // Register Error-Lang language definition and themes
  registerErrorLangLanguage();
  registerErrorLangThemes();

  // Find or create the editor container
  const container = document.getElementById("monaco-editor-container");
  if (!container) {
    console.error("Monaco container element #monaco-editor-container not found");
    return null;
  }

  // Default code sample
  const defaultCode = [
    "# Write your Error-Lang code here",
    "",
    "main",
    "    let x = 42",
    "    println(\"The answer is:\", x)",
    "",
    "    # Try using mutable state - watch stability drop!",
    "    let mut counter = 0",
    "    counter = counter + 1",
    "    println(\"Counter:\", counter)",
    "",
    "    # Explore the gutter zone",
    "    gutter",
    "        # This is the error injection zone",
    "        let broken = @#$",
    "    end",
    "",
    "    println(\"Program complete!\")",
    "end",
  ].join("\n");

  // Create the editor with Error-Lang options
  editorInstance = createEditor(container, {
    value: defaultCode,
    language: "error-lang",
    theme: currentTheme,
    fontSize: 14,
    minimap: { enabled: false },
    lineNumbers: "on",
    renderWhitespace: "selection",
    automaticLayout: true,
    scrollBeyondLastLine: false,
    wordWrap: "off",
    tabSize: 4,
    insertSpaces: true,
    glyphMargin: true,
    folding: true,
    bracketPairColorization: { enabled: true },
    padding: { top: 8 },
  });

  // Wire up content change handler with debounce
  onDidChangeContent(editorInstance, () => {
    if (contentChangeTimer) {
      clearTimeout(contentChangeTimer);
    }
    contentChangeTimer = setTimeout(() => {
      const code = getEditorValue(editorInstance);
      // Dispatch custom event for the TEA architecture to pick up
      const event = new CustomEvent("error-lang-code-changed", {
        detail: { code },
      });
      document.dispatchEvent(event);
    }, 300); // 300ms debounce
  });

  // Wire up cursor position change handler
  onDidChangeCursorPosition(editorInstance, (e) => {
    const event = new CustomEvent("error-lang-cursor-moved", {
      detail: {
        line: e.position.lineNumber,
        column: e.position.column,
      },
    });
    document.dispatchEvent(event);
  });

  // Expose editor instance globally for ReScript interop
  window.__errorLangEditor = editorInstance;

  console.log("Error-Lang Studio: Monaco editor initialized");
  return editorInstance;
}

/**
 * Get the current editor instance.
 * @returns {object|null} The Monaco editor instance or null
 */
export function getEditorInstance() {
  return editorInstance;
}

/**
 * Update the editor theme.
 * @param {string} theme - "dark", "light", or "highcontrast"
 */
export function updateTheme(theme) {
  const themeMap = {
    dark: "error-lang-dark",
    light: "error-lang-light",
    highcontrast: "hc-black",
  };
  currentTheme = themeMap[theme] || "error-lang-dark";
  setEditorTheme(currentTheme);
}

/**
 * Update the editor font size.
 * @param {number} size - Font size in pixels
 */
export function updateFontSize(size) {
  if (editorInstance) {
    setEditorFontSize(editorInstance, size);
  }
}

/**
 * Set error markers on the editor from parse results.
 * @param {Array} errors - Array of { line, column, endLine, endColumn, message }
 */
export function setErrors(errors) {
  if (!editorInstance) return;

  const markers = errors.map((err) => ({
    severity: window.monaco.MarkerSeverity.Error,
    message: err.message,
    startLineNumber: err.line || 1,
    startColumn: err.column || 1,
    endLineNumber: err.endLine || err.line || 1,
    endColumn: err.endColumn || err.column || 1,
    source: "error-lang",
  }));

  setModelMarkers(editorInstance, markers);
}

/**
 * Set stability warning markers on the editor.
 * @param {Array} warnings - Array of { line, column, endLine, endColumn, message }
 */
export function setStabilityWarnings(warnings) {
  if (!editorInstance) return;

  const markers = warnings.map((warn) => ({
    severity: window.monaco.MarkerSeverity.Warning,
    message: "[Stability] " + warn.message,
    startLineNumber: warn.line || 1,
    startColumn: warn.column || 1,
    endLineNumber: warn.endLine || warn.line || 1,
    endColumn: warn.endColumn || warn.column || 1,
    source: "error-lang-stability",
  }));

  // Append to existing error markers
  const existingModel = editorInstance.getModel();
  if (existingModel) {
    const existingMarkers = window.monaco.editor.getModelMarkers({
      resource: existingModel.uri,
      owner: "error-lang",
    });
    setModelMarkers(editorInstance, [...existingMarkers, ...markers]);
  }
}

/**
 * Set the editor content programmatically (e.g., loading a file).
 * @param {string} code - The new code content
 */
export function setCode(code) {
  if (editorInstance && editorInstance.getModel()) {
    editorInstance.getModel().setValue(code);
  }
}

/**
 * Get the current editor content.
 * @returns {string} The editor content
 */
export function getCode() {
  if (editorInstance) {
    return getEditorValue(editorInstance);
  }
  return "";
}
