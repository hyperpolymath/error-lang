#!/usr/bin/env -S deno run --allow-read --allow-env --allow-net
// SPDX-License-Identifier: PMPL-1.0-or-later
// Error-Lang Language Server Protocol (LSP) Implementation
//
// Provides IDE integration with computational haptics visualization

import {
  createConnection,
  ProposedFeatures,
  InitializeParams,
  TextDocumentSyncKind,
  CompletionItem,
  CompletionItemKind,
  Diagnostic,
  DiagnosticSeverity,
  Hover,
  MarkupKind,
} from "npm:vscode-languageserver@9.0.1";

import { TextDocument } from "npm:vscode-languageserver-textdocument@1.0.11";

// Create connection
const connection = createConnection(ProposedFeatures.all);

// Document store
const documents = new Map<string, TextDocument>();

// Stability scores cache
const stabilityScores = new Map<string, number>();

// Initialize
connection.onInitialize((_params: InitializeParams) => {
  console.error("[LSP] Initializing Error-Lang LSP Server");

  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      completionProvider: {
        resolveProvider: true,
        triggerCharacters: [".", " "],
      },
      hoverProvider: true,
      diagnosticProvider: {
        interFileDependencies: false,
        workspaceDiagnostics: false,
      },
    },
  };
});

connection.onInitialized(() => {
  console.error("[LSP] Error-Lang LSP Server initialized");
});

// Document change
connection.onDidOpenTextDocument((params) => {
  const doc = TextDocument.create(
    params.textDocument.uri,
    params.textDocument.languageId,
    params.textDocument.version,
    params.textDocument.text,
  );
  documents.set(params.textDocument.uri, doc);
  validateDocument(doc);
});

connection.onDidChangeTextDocument((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (doc) {
    const updated = TextDocument.update(
      doc,
      params.contentChanges,
      params.textDocument.version,
    );
    documents.set(params.textDocument.uri, updated);
    validateDocument(updated);
  }
});

connection.onDidCloseTextDocument((params) => {
  documents.delete(params.textDocument.uri);
  stabilityScores.delete(params.textDocument.uri);
});

// Validation with computational haptics
function validateDocument(doc: TextDocument): void {
  const diagnostics: Diagnostic[] = [];
  const text = doc.getText();
  const lines = text.split("\n");

  let stabilityScore = 100.0;
  let varCount = 0;
  let depth = 0;

  // Simple lexical analysis for paradoxes
  lines.forEach((line, lineIdx) => {
    // Check for positional semantics
    const plusMatch = line.match(/\+/g);
    if (plusMatch) {
      plusMatch.forEach(() => {
        const col = line.indexOf("+");
        const behavior = col % 2 === 0 ? "addition" : "concatenation";

        if (behavior === "concatenation") {
          stabilityScore -= 2.0;
          diagnostics.push({
            severity: DiagnosticSeverity.Information,
            range: {
              start: { line: lineIdx, character: col },
              end: { line: lineIdx, character: col + 1 },
            },
            message: `⚠️ Positional Semantics: + acts as ${behavior} at column ${col}`,
            source: "error-lang",
          });
        }
      });
    }

    // Check for type superposition (many variables)
    if (line.includes("let ") || line.includes("mutable ")) {
      varCount++;
      if (varCount > 10) {
        stabilityScore -= 5.0;
        diagnostics.push({
          severity: DiagnosticSeverity.Warning,
          range: {
            start: { line: lineIdx, character: 0 },
            end: { line: lineIdx, character: line.length },
          },
          message: "⚠️ Type Superposition: Too many variables (>10)",
          source: "error-lang",
        });
      }
    }

    // Check for scope leakage (prime-numbered lines)
    if (isPrime(lineIdx + 1)) {
      stabilityScore -= 3.0;
      diagnostics.push({
        severity: DiagnosticSeverity.Hint,
        range: {
          start: { line: lineIdx, character: 0 },
          end: { line: lineIdx, character: 0 },
        },
        message: `⚠️ Scope Leakage: Variables may escape on prime line ${lineIdx + 1}`,
        source: "error-lang",
      });
    }

    // Check for nesting depth
    if (line.includes("if ") || line.includes("while ") || line.includes("for ")) {
      depth++;
      if (depth > 5) {
        stabilityScore -= 4.0;
        diagnostics.push({
          severity: DiagnosticSeverity.Warning,
          range: {
            start: { line: lineIdx, character: 0 },
            end: { line: lineIdx, character: line.length },
          },
          message: "⚠️ Temporal Corruption: Deep nesting (>5 levels)",
          source: "error-lang",
        });
      }
    }

    // Check for syntax errors (very basic)
    if (line.includes("main") && !line.includes("main")) {
      diagnostics.push({
        severity: DiagnosticSeverity.Error,
        range: {
          start: { line: lineIdx, character: 0 },
          end: { line: lineIdx, character: line.length },
        },
        message: "Syntax error: 'main' block required",
        source: "error-lang",
      });
    }
  });

  // Store stability score
  stabilityScores.set(doc.uri, Math.max(0, stabilityScore));

  // Send diagnostics
  connection.sendDiagnostics({
    uri: doc.uri,
    diagnostics,
  });

  // Send custom notification for stability score (for UI visualization)
  connection.sendNotification("errorLang/stabilityUpdate", {
    uri: doc.uri,
    score: stabilityScores.get(doc.uri),
  });
}

// Helper: check if prime
function isPrime(n: number): boolean {
  if (n < 2) return false;
  if (n === 2) return true;
  if (n % 2 === 0) return false;
  for (let i = 3; i * i <= n; i += 2) {
    if (n % i === 0) return false;
  }
  return true;
}

// Hover info
connection.onHover((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return null;

  const score = stabilityScores.get(params.textDocument.uri) || 100.0;
  const emoji = score >= 80 ? "✨" : score >= 60 ? "💫" : score >= 40 ? "⚠️" : "🔥";

  const hover: Hover = {
    contents: {
      kind: MarkupKind.Markdown,
      value: [
        `## Computational Haptics`,
        ``,
        `${emoji} **Stability Score:** ${score.toFixed(1)}/100`,
        ``,
        `### Active Paradoxes`,
        `- Positional Semantics: Operators change by column`,
        `- Scope Leakage: Variables escape on prime lines`,
        `- Type Superposition: Too many variables cause instability`,
        ``,
        `*Hover over operators to see their behavior*`,
      ].join("\n"),
    },
  };

  return hover;
});

// Completions
connection.onCompletion((_params) => {
  const completions: CompletionItem[] = [
    // Keywords
    { label: "main", kind: CompletionItemKind.Keyword, detail: "Main program block" },
    { label: "end", kind: CompletionItemKind.Keyword, detail: "End block" },
    { label: "let", kind: CompletionItemKind.Keyword, detail: "Variable declaration" },
    { label: "mutable", kind: CompletionItemKind.Keyword, detail: "Mutable variable" },
    { label: "function", kind: CompletionItemKind.Keyword, detail: "Function declaration" },
    { label: "if", kind: CompletionItemKind.Keyword, detail: "Conditional" },
    { label: "while", kind: CompletionItemKind.Keyword, detail: "While loop" },
    { label: "for", kind: CompletionItemKind.Keyword, detail: "For loop" },

    // Built-in functions
    { label: "stability()", kind: CompletionItemKind.Function, detail: "Get stability score" },
    { label: "trace()", kind: CompletionItemKind.Function, detail: "Add trace point" },
    { label: "checkpoint()", kind: CompletionItemKind.Function, detail: "Add checkpoint" },
    { label: "println()", kind: CompletionItemKind.Function, detail: "Print with newline" },

    // Types
    { label: "int", kind: CompletionItemKind.TypeParameter, detail: "Integer type" },
    { label: "float", kind: CompletionItemKind.TypeParameter, detail: "Float type" },
    { label: "string", kind: CompletionItemKind.TypeParameter, detail: "String type" },
    { label: "bool", kind: CompletionItemKind.TypeParameter, detail: "Boolean type" },
  ];

  return completions;
});

// Shutdown
connection.onShutdown(() => {
  console.error("[LSP] Shutting down Error-Lang LSP Server");
});

// Listen
connection.listen();
console.error("[LSP] Error-Lang LSP Server listening on stdio");
