// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Backend implementation for the Error-Lang LSP server.
//!
//! Analyses `.err` source files to produce diagnostics, type information,
//! completions, definitions, and document symbols.  The analysis includes
//! Error-Lang's unique "computational haptics" features: positional semantics,
//! scope leakage on prime-numbered lines, and type superposition.

use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

// ---------------------------------------------------------------------------
// Language constants
// ---------------------------------------------------------------------------

/// Error-Lang keywords.
const KEYWORDS: &[&str] = &[
    "main", "end", "let", "mutable", "function", "return",
    "if", "else", "while", "for", "in",
    "gutter", "checkpoint", "trace",
    "true", "false",
];

/// Built-in function names.
const BUILTINS: &[&str] = &[
    "println", "print", "stability", "trace", "checkpoint",
    "typeof", "len", "push", "pop", "abs", "sqrt",
];

/// Error-Lang type names.
const TYPES: &[&str] = &[
    "int", "float", "string", "bool", "array", "void",
];

// ---------------------------------------------------------------------------
// Document state
// ---------------------------------------------------------------------------

/// Parsed state of one open document.
struct DocumentState {
    source: String,
    line_starts: Vec<usize>,
    /// Definitions: (name, line, col, kind_label).
    definitions: Vec<(String, u32, u32, &'static str)>,
    /// Diagnostics from the analysis pass.
    diagnostics: Vec<Diagnostic>,
    /// Computational haptics stability score (0.0-100.0).
    stability_score: f64,
}

impl DocumentState {
    fn new(source: String) -> Self {
        let line_starts = std::iter::once(0)
            .chain(source.char_indices().filter_map(|(i, c)| {
                if c == '\n' { Some(i + 1) } else { None }
            }))
            .collect();

        let mut state = Self {
            source,
            line_starts,
            definitions: Vec::new(),
            diagnostics: Vec::new(),
            stability_score: 100.0,
        };
        state.analyze();
        state
    }

    fn offset_to_position(&self, offset: usize) -> Position {
        let line = self
            .line_starts
            .binary_search(&offset)
            .unwrap_or_else(|i| i.saturating_sub(1));
        let col = offset.saturating_sub(self.line_starts[line]);
        Position::new(line as u32, col as u32)
    }

    fn word_at(&self, line: u32, col: u32) -> Option<String> {
        let line_str = self.source.lines().nth(line as usize)?;
        let c = col as usize;
        if c > line_str.len() {
            return None;
        }

        let start = line_str[..c]
            .rfind(|ch: char| !ch.is_alphanumeric() && ch != '_')
            .map(|i| i + 1)
            .unwrap_or(0);
        let end = line_str[c..]
            .find(|ch: char| !ch.is_alphanumeric() && ch != '_')
            .map(|i| c + i)
            .unwrap_or(line_str.len());

        if start < end {
            Some(line_str[start..end].to_string())
        } else {
            None
        }
    }

    // -----------------------------------------------------------------------
    // Analysis pass
    // -----------------------------------------------------------------------

    fn analyze(&mut self) {
        self.definitions.clear();
        self.diagnostics.clear();
        self.stability_score = 100.0;

        let mut var_count = 0u32;
        let mut nesting_depth: i32 = 0;
        let mut has_main = false;
        let mut main_closed = false;

        for (line_idx, line) in self.source.lines().enumerate() {
            let trimmed = line.trim();
            let ln = line_idx as u32;
            let line_number = line_idx + 1; // 1-based

            // Skip comments (lines starting with //)
            if trimmed.starts_with("//") {
                continue;
            }

            // ------ Track main block ------
            if trimmed == "main" {
                has_main = true;
                nesting_depth += 1;
                let col = line.find("main").unwrap_or(0) as u32;
                self.definitions.push(("main".into(), ln, col, "Function"));
            }
            if trimmed == "end" {
                nesting_depth -= 1;
                if nesting_depth == 0 && has_main {
                    main_closed = true;
                }
            }

            // ------ Definitions ------

            // function name(...)
            if trimmed.starts_with("function ") {
                if let Some(rest) = trimmed.strip_prefix("function ") {
                    let name = rest
                        .split(|c: char| !c.is_alphanumeric() && c != '_')
                        .next()
                        .unwrap_or("");
                    if !name.is_empty() {
                        let col = line.find(name).unwrap_or(0) as u32;
                        self.definitions.push((name.into(), ln, col, "Function"));
                    }
                }
                nesting_depth += 1;
            }

            // let name = ...
            if trimmed.starts_with("let ") {
                if let Some(rest) = trimmed.strip_prefix("let ") {
                    let name = rest
                        .split(|c: char| !c.is_alphanumeric() && c != '_')
                        .next()
                        .unwrap_or("");
                    if !name.is_empty() {
                        let col = line.find(name).unwrap_or(0) as u32;
                        self.definitions.push((name.into(), ln, col, "Variable"));
                        var_count += 1;
                    }
                }
            }

            // mutable name = ...
            if trimmed.starts_with("mutable ") {
                if let Some(rest) = trimmed.strip_prefix("mutable ") {
                    let name = rest
                        .split(|c: char| !c.is_alphanumeric() && c != '_')
                        .next()
                        .unwrap_or("");
                    if !name.is_empty() {
                        let col = line.find(name).unwrap_or(0) as u32;
                        self.definitions.push((name.into(), ln, col, "Variable"));
                        var_count += 1;
                    }
                }
            }

            // if / while / for increase nesting
            if trimmed.starts_with("if ") || trimmed.starts_with("while ") || trimmed.starts_with("for ") {
                nesting_depth += 1;
            }

            // ------ Computational haptics diagnostics ------

            // Positional semantics: + acts as concatenation at odd columns
            for (col_idx, ch) in line.char_indices() {
                if ch == '+' {
                    if col_idx % 2 != 0 {
                        self.stability_score -= 2.0;
                        self.diagnostics.push(Diagnostic {
                            range: Range {
                                start: Position::new(ln, col_idx as u32),
                                end: Position::new(ln, col_idx as u32 + 1),
                            },
                            severity: Some(DiagnosticSeverity::INFORMATION),
                            source: Some("error-lang-lsp".into()),
                            message: format!(
                                "Positional semantics: `+` acts as concatenation at column {} (odd column)",
                                col_idx
                            ),
                            ..Default::default()
                        });
                    }
                }
            }

            // Scope leakage on prime-numbered lines
            if is_prime(line_number) && (trimmed.starts_with("let ") || trimmed.starts_with("mutable ")) {
                self.stability_score -= 3.0;
                self.diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position::new(ln, 0),
                        end: Position::new(ln, line.len() as u32),
                    },
                    severity: Some(DiagnosticSeverity::HINT),
                    source: Some("error-lang-lsp".into()),
                    message: format!(
                        "Scope leakage: variable on prime line {} may escape its scope",
                        line_number
                    ),
                    ..Default::default()
                });
            }

            // Type superposition warning (too many variables)
            if var_count > 10 && (trimmed.starts_with("let ") || trimmed.starts_with("mutable ")) {
                self.stability_score -= 5.0;
                self.diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position::new(ln, 0),
                        end: Position::new(ln, line.len() as u32),
                    },
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("error-lang-lsp".into()),
                    message: format!(
                        "Type superposition: {} variables declared (>10 causes instability)",
                        var_count
                    ),
                    ..Default::default()
                });
            }

            // Deep nesting warning
            if nesting_depth > 5 {
                self.stability_score -= 4.0;
                self.diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position::new(ln, 0),
                        end: Position::new(ln, line.len() as u32),
                    },
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("error-lang-lsp".into()),
                    message: format!(
                        "Temporal corruption: nesting depth {} (>5 causes instability)",
                        nesting_depth
                    ),
                    ..Default::default()
                });
            }

            // Unclosed string on this line
            let quote_count = trimmed.chars().filter(|&c| c == '"').count();
            if quote_count % 2 != 0 {
                self.diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position::new(ln, 0),
                        end: Position::new(ln, line.len() as u32),
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("error-lang-lsp".into()),
                    message: "Unclosed string literal".into(),
                    ..Default::default()
                });
            }
        }

        // Missing main block
        if !has_main && !self.source.trim().is_empty() {
            self.diagnostics.push(Diagnostic {
                range: Range {
                    start: Position::new(0, 0),
                    end: Position::new(0, 1),
                },
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("error-lang-lsp".into()),
                message: "No `main` block found — programs should start with `main`".into(),
                ..Default::default()
            });
        }

        // Unclosed main block
        if has_main && !main_closed {
            self.diagnostics.push(Diagnostic {
                range: Range {
                    start: Position::new(0, 0),
                    end: Position::new(0, 1),
                },
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("error-lang-lsp".into()),
                message: "Unclosed `main` block — expected `end`".into(),
                ..Default::default()
            });
        }

        // Unmatched nesting
        if nesting_depth > 0 {
            let last = self.source.lines().count().saturating_sub(1) as u32;
            self.diagnostics.push(Diagnostic {
                range: Range {
                    start: Position::new(last, 0),
                    end: Position::new(last, 1),
                },
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("error-lang-lsp".into()),
                message: format!("{} unclosed block(s) — missing `end`", nesting_depth),
                ..Default::default()
            });
        }

        self.stability_score = self.stability_score.max(0.0);
    }
}

/// Check if a number is prime.
fn is_prime(n: usize) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }
    let mut i = 3;
    while i * i <= n {
        if n % i == 0 {
            return false;
        }
        i += 2;
    }
    true
}

/// Documentation for built-in keywords and functions.
fn keyword_doc(word: &str) -> Option<&'static str> {
    match word {
        "main" => Some("**main** ... **end**\n\nThe main program block. All Error-Lang programs start here."),
        "end" => Some("**end**\n\nCloses a block (`main`, `function`, `if`, `while`, `for`)."),
        "let" => Some("**let** name = value\n\nDeclare an immutable variable."),
        "mutable" => Some("**mutable** name = value\n\nDeclare a mutable variable that can be reassigned."),
        "function" => Some("**function** name(params)\n  body\n**end**\n\nDeclare a named function."),
        "return" => Some("**return** value\n\nReturn a value from a function."),
        "if" => Some("**if** condition\n  body\n[**else**\n  body]\n**end**\n\nConditional branching."),
        "while" => Some("**while** condition\n  body\n**end**\n\nLoop while condition is true."),
        "for" => Some("**for** var **in** iterable\n  body\n**end**\n\nIterate over a range or collection."),
        "gutter" => Some("**gutter**\n\nError-Lang's unique error recovery mechanism.\nThe gutter catches and transforms errors into values."),
        "println" => Some("**println**(value)\n\nPrint a value followed by a newline."),
        "stability" => Some("**stability**() : float\n\nReturns the current computational haptics stability score (0.0-100.0)."),
        "trace" => Some("**trace**(label)\n\nAdd a trace point for debugging."),
        "checkpoint" => Some("**checkpoint**(label)\n\nAdd a stability checkpoint."),
        "int" => Some("**int** — Integer type\n\nWhole numbers."),
        "float" => Some("**float** — Float type\n\nFloating-point numbers."),
        "string" => Some("**string** — String type\n\nText values (double-quoted)."),
        "bool" => Some("**bool** — Boolean type\n\n`true` or `false`."),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Backend
// ---------------------------------------------------------------------------

/// Error-Lang LSP backend.
pub struct ErrorLangBackend {
    client: Client,
    documents: Arc<DashMap<Url, DocumentState>>,
}

impl ErrorLangBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
        }
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        if let Some(doc) = self.documents.get(uri) {
            self.client
                .publish_diagnostics(uri.clone(), doc.diagnostics.clone(), None)
                .await;

            // Send stability score as custom notification
            let _ = self.client.send_notification::<StabilityNotification>(
                StabilityParams {
                    uri: uri.to_string(),
                    score: doc.stability_score,
                },
            ).await;
        }
    }
}

/// Custom notification for stability score updates.
struct StabilityNotification;

#[derive(serde::Serialize)]
struct StabilityParams {
    uri: String,
    score: f64,
}

impl tower_lsp::lsp_types::notification::Notification for StabilityNotification {
    type Params = StabilityParams;
    const METHOD: &'static str = "errorLang/stabilityUpdate";
}

#[tower_lsp::async_trait]
impl LanguageServer for ErrorLangBackend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![" ".into(), ".".into()]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "error-lang-lsp".into(),
                version: Some("0.1.0".into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Error-Lang LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Document sync
    // -----------------------------------------------------------------------

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let doc = DocumentState::new(params.text_document.text);
        self.documents.insert(uri.clone(), doc);
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.first() {
            let doc = DocumentState::new(change.text.clone());
            self.documents.insert(uri.clone(), doc);
            self.publish_diagnostics(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
    }

    // -----------------------------------------------------------------------
    // Hover
    // -----------------------------------------------------------------------

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let doc = match self.documents.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let word = match doc.word_at(pos.line, pos.character) {
            Some(w) => w,
            None => return Ok(None),
        };

        // Built-in documentation
        if let Some(doc_text) = keyword_doc(&word) {
            let score = doc.stability_score;
            let full_text = format!(
                "{}\n\n---\nStability score: {:.1}/100.0",
                doc_text, score
            );
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: full_text,
                }),
                range: None,
            }));
        }

        // User-defined names
        for (name, _ln, _col, kind) in &doc.definitions {
            if name == &word {
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("**{}** `{}`\n\nDefined in this file.", kind, name),
                    }),
                    range: None,
                }));
            }
        }

        Ok(None)
    }

    // -----------------------------------------------------------------------
    // Completion
    // -----------------------------------------------------------------------

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let doc = match self.documents.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let prefix = doc.word_at(pos.line, pos.character).unwrap_or_default();

        let mut items = Vec::new();

        // Keywords
        for kw in KEYWORDS {
            if kw.starts_with(&prefix) || prefix.is_empty() {
                items.push(CompletionItem {
                    label: kw.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("Keyword".into()),
                    sort_text: Some(format!("0_{}", kw)),
                    ..Default::default()
                });
            }
        }

        // Built-in functions
        for bi in BUILTINS {
            if bi.starts_with(&prefix) || prefix.is_empty() {
                items.push(CompletionItem {
                    label: bi.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some("Built-in".into()),
                    sort_text: Some(format!("1_{}", bi)),
                    ..Default::default()
                });
            }
        }

        // Types
        for ty in TYPES {
            if ty.starts_with(&prefix) || prefix.is_empty() {
                items.push(CompletionItem {
                    label: ty.to_string(),
                    kind: Some(CompletionItemKind::TYPE_PARAMETER),
                    detail: Some("Type".into()),
                    sort_text: Some(format!("2_{}", ty)),
                    ..Default::default()
                });
            }
        }

        // Identifiers from the document
        for (name, _ln, _col, kind) in &doc.definitions {
            if name.starts_with(&prefix) || prefix.is_empty() {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(match *kind {
                        "Function" => CompletionItemKind::FUNCTION,
                        "Variable" => CompletionItemKind::VARIABLE,
                        _ => CompletionItemKind::TEXT,
                    }),
                    detail: Some(kind.to_string()),
                    sort_text: Some(format!("3_{}", name)),
                    ..Default::default()
                });
            }
        }

        Ok(Some(CompletionResponse::Array(items)))
    }

    // -----------------------------------------------------------------------
    // Go to definition
    // -----------------------------------------------------------------------

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let doc = match self.documents.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let word = match doc.word_at(pos.line, pos.character) {
            Some(w) => w,
            None => return Ok(None),
        };

        for (name, ln, col, _kind) in &doc.definitions {
            if name == &word {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: uri.clone(),
                    range: Range {
                        start: Position::new(*ln, *col),
                        end: Position::new(*ln, *col + name.len() as u32),
                    },
                })));
            }
        }

        Ok(None)
    }

    // -----------------------------------------------------------------------
    // Document symbols
    // -----------------------------------------------------------------------

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        let doc = match self.documents.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        #[allow(deprecated)]
        let symbols: Vec<SymbolInformation> = doc
            .definitions
            .iter()
            .map(|(name, ln, col, kind)| SymbolInformation {
                name: name.clone(),
                kind: match *kind {
                    "Function" => SymbolKind::FUNCTION,
                    "Variable" => SymbolKind::VARIABLE,
                    _ => SymbolKind::KEY,
                },
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range: Range {
                        start: Position::new(*ln, *col),
                        end: Position::new(*ln, *col + name.len() as u32),
                    },
                },
                container_name: None,
            })
            .collect();

        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }
}
