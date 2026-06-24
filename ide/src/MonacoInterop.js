// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// MonacoInterop.js - JavaScript interop layer for Monaco Editor
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Host-side Monaco Editor glue. The AffineScript MonacoBindings.affine externs
// are wired to these functions by the host shim (host.js); Monaco is loaded
// globally via the AMD loader in index.html.

function getMonaco() {
  if (typeof window !== "undefined" && window.monaco) {
    return window.monaco;
  }
  throw new Error("Monaco editor not loaded. Ensure AMD loader is in index.html.");
}

export function registerErrorLangLanguage() {
  const monaco = getMonaco();
  monaco.languages.register({
    id: "error-lang",
    extensions: [".err"],
    aliases: ["Error-Lang", "error-lang", "err"],
    mimetypes: ["text/x-error-lang"],
  });
  monaco.languages.setMonarchTokensProvider("error-lang", {
    defaultToken: "",
    ignoreCase: false,
    keywords: [
      "main","end","let","mutable","function","struct",
      "if","elseif","else","while","for","in",
      "break","continue","return","and","or","not",
      "true","false","nil","null","gutter","fn",
      "async","await","match","type","mut",
      "\u0433\u043b\u0430\u0432\u043d\u044b\u0439","\u043a\u043e\u043d\u0435\u0446",
      "\u043f\u0443\u0441\u0442\u044c","\u0444\u0443\u043d\u043a\u0446\u0438\u044f",
      "\u0435\u0441\u043b\u0438","\u0438\u043d\u0430\u0447\u0435",
      "\u043f\u043e\u043a\u0430","\u0434\u043b\u044f",
      "\u0432\u0435\u0440\u043d\u0443\u0442\u044c","\u043f\u0435\u0447\u0430\u0442\u044c",
      "\u043e\u0448\u0438\u0431\u043a\u0430",
    ],
    typeKeywords: ["Int","Float","String","Bool","Array","Option","Decimal"],
    builtins: ["println","print","stability","sleep","assert","typeof"],
    operators: [
      "+","-","*","/","%","==","!=","<",">",
      "<=",">=","=","->","++","&&","||","!",
      "&","|","^","~","<<",">>","?",
    ],
    symbols: /[=><!~?:&|+\-*\/\^%]+/,
    escapes: /\\(?:[abfnrtv\\"'0]|x[0-9A-Fa-f]{2}|u[0-9A-Fa-f]{4})/,
    tokenizer: {
      root: [
        [/#.*$/, "comment"],
        [/\b(gutter|\u043e\u0448\u0438\u0431\u043a\u0430)\b/,
          { token: "keyword.gutter", bracket: "@open", next: "@gutterBlock" }],
        [/\b(Int|Float|String|Bool|Array|Option|Decimal)\b/, "type.identifier"],
        [/\b(println|print|stability|sleep|assert|typeof)\b/, "support.function"],
        [/[a-zA-Z_\u0400-\u04FF][a-zA-Z0-9_\u0400-\u04FF]*/,
          { cases: {
              "@keywords": "keyword",
              "@typeKeywords": "type.identifier",
              "@builtins": "support.function",
              "@default": "identifier",
          }}],
        [/0[xX][0-9a-fA-F]+/, "number.hex"],
        [/0[bB][01]+/, "number.binary"],
        [/\d+\.\d*([eE][\-+]?\d+)?/, "number.float"],
        [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
        [/\d+[eE][\-+]?\d+/, "number.float"],
        [/\d+/, "number"],
        [/"""/, { token: "string.quote", bracket: "@open", next: "@tripleString" }],
        [/"/, { token: "string.quote", bracket: "@open", next: "@string" }],
        [/->/, "operator.arrow"],
        [/[=><!~?:&|+\-*\/\^%]+/,
          { cases: { "@operators": "operator", "@default": "" }}],
        [/[{}()\[\]]/, "@brackets"],
        [/[,.]/, "delimiter"],
      ],
      string: [
        [/[^\\"]+/, "string"],
        [/@escapes/, "string.escape"],
        [/\\./, "string.escape.invalid"],
        [/"/, { token: "string.quote", bracket: "@close", next: "@pop" }],
      ],
      tripleString: [
        [/"""/, { token: "string.quote", bracket: "@close", next: "@pop" }],
        [/[^"]+/, "string"],
        [/"/, "string"],
      ],
      gutterBlock: [
        [/#.*$/, "comment.gutter"],
        [/\b(end|\u043a\u043e\u043d\u0435\u0446)\b/,
          { token: "keyword.gutter", bracket: "@close", next: "@pop" }],
        [/"[^"]*"/, "string.gutter"],
        [/[a-zA-Z_\u0400-\u04FF][a-zA-Z0-9_\u0400-\u04FF]*/, "variable.gutter"],
        [/./, "invalid.gutter"],
      ],
    },
  });
  monaco.languages.setLanguageConfiguration("error-lang", {
    comments: { lineComment: "#" },
    brackets: [["{","}"],["[","]"],["(",")"]],
    autoClosingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: '"', close: '"', notIn: ["string"] },
    ],
    surroundingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: '"', close: '"' },
    ],
    folding: {
      markers: {
        start: /^\s*(main|function|if|while|for|gutter|struct)\b/,
        end: /^\s*(end)\b/,
      },
    },
    indentationRules: {
      increaseIndentPattern: /^\s*(main|function|if|elseif|else|while|for|gutter|struct|match)\b/,
      decreaseIndentPattern: /^\s*(end|else|elseif)\b/,
    },
    wordPattern: /(-?\d*\.\d\w*)|([^\`\~\!\@\#\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g,
  });
}

export function registerErrorLangThemes() {
  const monaco = getMonaco();
  monaco.editor.defineTheme("error-lang-dark", {
    base: "vs-dark", inherit: true,
    rules: [
      { token: "keyword", foreground: "569CD6", fontStyle: "bold" },
      { token: "keyword.gutter", foreground: "F48771", fontStyle: "bold italic" },
      { token: "keyword.stability", foreground: "DCDCAA", fontStyle: "bold" },
      { token: "type.identifier", foreground: "4EC9B0" },
      { token: "support.function", foreground: "DCDCAA" },
      { token: "identifier", foreground: "9CDCFE" },
      { token: "number", foreground: "B5CEA8" },
      { token: "number.hex", foreground: "B5CEA8" },
      { token: "number.binary", foreground: "B5CEA8" },
      { token: "number.float", foreground: "B5CEA8" },
      { token: "string", foreground: "CE9178" },
      { token: "string.quote", foreground: "CE9178" },
      { token: "string.escape", foreground: "D7BA7D" },
      { token: "string.escape.invalid", foreground: "F48771", fontStyle: "underline" },
      { token: "operator", foreground: "D4D4D4" },
      { token: "operator.arrow", foreground: "569CD6" },
      { token: "comment", foreground: "6A9955", fontStyle: "italic" },
      { token: "comment.gutter", foreground: "6A5555", fontStyle: "italic" },
      { token: "string.gutter", foreground: "9E6868" },
      { token: "variable.gutter", foreground: "7A6A6A" },
      { token: "invalid.gutter", foreground: "5A4A4A" },
      { token: "delimiter", foreground: "D4D4D4" },
    ],
    colors: {
      "editor.background": "#1E1E1E",
      "editor.foreground": "#D4D4D4",
      "editorCursor.foreground": "#AEAFAD",
      "editor.lineHighlightBackground": "#2D2D2D",
      "editorLineNumber.foreground": "#5A5A5A",
      "editorLineNumber.activeForeground": "#C6C6C6",
      "editor.selectionBackground": "#264F78",
      "editor.inactiveSelectionBackground": "#3A3D41",
      "editorIndentGuide.background": "#404040",
      "editorIndentGuide.activeBackground": "#707070",
      "editorBracketMatch.background": "#0064001A",
      "editorBracketMatch.border": "#888888",
      "editorGutter.background": "#1E1E1E",
      "editorError.foreground": "#F48771",
      "editorWarning.foreground": "#DCDCAA",
      "editorInfo.foreground": "#4EC9B0",
    },
  });
  monaco.editor.defineTheme("error-lang-light", {
    base: "vs", inherit: true,
    rules: [
      { token: "keyword", foreground: "0000FF", fontStyle: "bold" },
      { token: "keyword.gutter", foreground: "CC3333", fontStyle: "bold italic" },
      { token: "keyword.stability", foreground: "795E26", fontStyle: "bold" },
      { token: "type.identifier", foreground: "267F99" },
      { token: "support.function", foreground: "795E26" },
      { token: "identifier", foreground: "001080" },
      { token: "number", foreground: "098658" },
      { token: "number.hex", foreground: "098658" },
      { token: "number.binary", foreground: "098658" },
      { token: "number.float", foreground: "098658" },
      { token: "string", foreground: "A31515" },
      { token: "string.quote", foreground: "A31515" },
      { token: "string.escape", foreground: "EE0000" },
      { token: "string.escape.invalid", foreground: "FF0000", fontStyle: "underline" },
      { token: "operator", foreground: "000000" },
      { token: "operator.arrow", foreground: "0000FF" },
      { token: "comment", foreground: "008000", fontStyle: "italic" },
      { token: "comment.gutter", foreground: "AA8888", fontStyle: "italic" },
      { token: "string.gutter", foreground: "CC8888" },
      { token: "variable.gutter", foreground: "AA7777" },
      { token: "invalid.gutter", foreground: "CC6666" },
      { token: "delimiter", foreground: "000000" },
    ],
    colors: {
      "editor.background": "#FFFFFF",
      "editor.foreground": "#000000",
      "editorCursor.foreground": "#000000",
      "editor.lineHighlightBackground": "#F5F5F5",
      "editorLineNumber.foreground": "#999999",
      "editorLineNumber.activeForeground": "#333333",
      "editor.selectionBackground": "#ADD6FF",
      "editor.inactiveSelectionBackground": "#E5EBF1",
    },
  });
}

export function createEditor(container, options) {
  return getMonaco().editor.create(container, options);
}
export function disposeEditor(editor) { if (editor) editor.dispose(); }
export function setEditorValue(editor, value) {
  if (editor && editor.getModel()) editor.getModel().setValue(value);
}
export function getEditorValue(editor) {
  if (editor && editor.getModel()) return editor.getModel().getValue();
  return "";
}
export function setEditorTheme(themeName) { getMonaco().editor.setTheme(themeName); }
export function setEditorFontSize(editor, fontSize) {
  if (editor) editor.updateOptions({ fontSize });
}
export function onDidChangeContent(editor, callback) {
  if (editor && editor.getModel()) {
    editor.getModel().onDidChangeContent(() => callback());
  }
}
export function onDidChangeCursorPosition(editor, callback) {
  if (editor) {
    editor.onDidChangeCursorPosition((e) => {
      callback({ position: { lineNumber: e.position.lineNumber, column: e.position.column } });
    });
  }
}
export function setModelMarkers(editor, markers) {
  if (editor && editor.getModel()) {
    getMonaco().editor.setModelMarkers(editor.getModel(), "error-lang", markers);
  }
}
export function revealLineInCenter(editor, lineNumber) {
  if (editor) editor.revealLineInCenter(lineNumber);
}
export function setPosition(editor, position) {
  if (editor) editor.setPosition(position);
}
export function focusEditor(editor) { if (editor) editor.focus(); }
export function getMarkerSeverityError() { return getMonaco().MarkerSeverity.Error; }
export function getMarkerSeverityWarning() { return getMonaco().MarkerSeverity.Warning; }
export function getMarkerSeverityInfo() { return getMonaco().MarkerSeverity.Info; }
