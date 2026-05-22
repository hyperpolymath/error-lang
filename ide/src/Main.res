// SPDX-License-Identifier: MPL-2.0
// Main.res - Application entry point
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

/**
 * Main - Bootstrap the Error-Lang Studio IDE
 *
 * Wires together Model, Update, and View into a running TEA application.
 * Also sets up Monaco editor event bridge: Monaco dispatches CustomEvents
 * on the document, and this module listens for them and converts them
 * into TEA messages.
 */

open Model
open Msg
open Update
open View

// External binding to add DOM event listeners
@val external document: Dom.document = "document"

@send external addEventListener: (Dom.document, string, Dom.event => unit) => unit = "addEventListener"

// Access CustomEvent detail fields
@get external detail: Dom.event => {..} = "detail"

// Main application program
let main = () => {
  let _app = Tea.App.beginnerProgram({
    model: Model.init(),
    update: Update.update,
    view: View.view,
  })

  // Bridge Monaco editor events into TEA message dispatch.
  // Monaco fires CustomEvents on document; we listen and forward to the app.
  // Note: In a full TEA implementation, this would use subscriptions (Tea.Sub).
  // For now, we use a simple event listener bridge.

  // Listen for code changes from Monaco
  addEventListener(document, "error-lang-code-changed", event => {
    let d = detail(event)
    let code: string = d["code"]
    // In a full implementation: dispatch MonacoContentChanged(code) to the app
    ignore(code)
  })

  // Listen for cursor position changes from Monaco
  addEventListener(document, "error-lang-cursor-moved", event => {
    let d = detail(event)
    let _line: int = d["line"]
    let _column: int = d["column"]
    // In a full implementation: dispatch MonacoCursorMoved(line, column) to the app
  })

  // Initialize Monaco editor after a short delay to ensure DOM is ready
  // The monaco-setup.js module handles the actual initialization
  ignore(
    Js.Global.setTimeout(() => {
      // Check if Monaco setup is available and the container exists
      switch %raw(`typeof window.__errorLangSetup`) {
      | "object" => {
          let setup: {..} = %raw(`window.__errorLangSetup`)
          let _editor = setup["initMonacoEditor"]()
          Js.log("Error-Lang Studio: Monaco editor initialized from Main.res")
        }
      | _ => Js.log("Error-Lang Studio: Monaco setup not yet available, AMD loader will handle it")
      }
    }, 500)
  )
}

// Start the application when DOM is ready
let () = main()
