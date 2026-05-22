// SPDX-License-Identifier: MPL-2.0
//! error-lang-lsp — Language Server Protocol server for Error-Lang.
//!
//! Error-Lang is an educational programming language where errors are features,
//! not bugs.  This LSP server provides diagnostics (including computational
//! haptics scoring), hover, completion, go-to-definition, and document symbols
//! for `.err` files.

#![forbid(unsafe_code)]
mod backend;

use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| backend::ErrorLangBackend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
