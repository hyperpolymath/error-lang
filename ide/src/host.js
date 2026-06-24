// SPDX-License-Identifier: MPL-2.0
// host.js — browser host shim for the AffineScript Error-Lang Studio IDE.
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Backs the `extern fn` declarations in Dom.affine / MonacoBindings.affine /
// Main.affine with real DOM + Monaco implementations, over an Int handle table,
// and routes fired DOM/Monaco events back into the TEA loop (Main.bridge_*).
//
// This is the application entry (index.html loads it as a module). It imports
// the self-contained compiled Main.js (affinescript --deno-esm inlines every
// dependency, so Main.js alone is the whole app) and the existing Monaco glue
// (MonacoInterop.js). The compiled affine code calls the externs as bare global
// identifiers (`dom_create_element(...)`), which resolve to the `globalThis.*`
// definitions installed below; so the globals must exist before Main.main() runs
// — guaranteed here because the import side-effects (function definitions only,
// no extern calls at module top-level) complete before this module's body.
//
// Verified: type-checks (affinescript check) + compiles (affinescript compile
// --deno-esm) clean. Runtime behaviour (Monaco wiring, event detail fields) is
// browser-side; confirm in-browser. Touch points flagged with NOTE: below.

import * as Main from "./Main.js";
import * as MI from "./MonacoInterop.js";

// ---- handle table: Int <-> host object (0 = null / None) -------------------
// Index 0 is reserved null; index 1 is a recycled "current event" slot (events
// are consumed synchronously by Main.dispatch, so one slot suffices and the
// table does not grow per event); index >= 2 holds persistent DOM/Monaco nodes.
const H = [null, null];
function put(obj) { if (obj == null) return 0; H.push(obj); return H.length - 1; }
function get(h) { return (h > 0 && h < H.length) ? H[h] : null; }
function putEvent(e) { H[1] = e; return 1; }

// ---- the model cell (the host owns it; Main threads the model by value) ----
let MODEL = null;

// ---- event routing: a fired handler-id re-enters the TEA loop --------------
function routeEvent(handlerId, eventHandle, domValue) {
  if (MODEL == null) return;
  if (handlerId === "monaco-content-changed") {
    MODEL = Main.bridge_monaco_content(eventHandle, MODEL);
  } else if (handlerId === "monaco-cursor-moved") {
    MODEL = Main.bridge_monaco_cursor(eventHandle, MODEL);
  } else {
    MODEL = Main.bridge_handler(handlerId, MODEL, domValue == null ? "" : domValue);
  }
}
function routeTimeout(handlerId) {
  if (handlerId === "boot-monaco") Main.boot_monaco();
}

// ---- DOM externs (Dom.affine) ----------------------------------------------
globalThis.dom_query_selector  = (sel) => put(document.querySelector(sel));
globalThis.dom_create_element  = (tag) => put(document.createElement(tag));
globalThis.dom_create_text_node = (content) => put(document.createTextNode(content));
globalThis.dom_append_child = (p, c) => {
  const pn = get(p), cn = get(c);
  if (pn && cn) pn.appendChild(cn);
  return null;
};
globalThis.dom_set_attribute = (el, name, value) => {
  const n = get(el); if (n) n.setAttribute(name, value); return null;
};
globalThis.dom_clear_children = (el) => {
  const n = get(el); if (n) while (n.firstChild) n.removeChild(n.firstChild); return null;
};
// View binds UI events as (event-name, handler-id); these route through
// Main.bridge_handler, which does not read the event object — so pass eventHandle
// 0 and forward the DOM value (e.g. a <select>'s value for change-theme).
globalThis.dom_bind_event = (el, event, handlerId) => {
  const n = get(el);
  if (n) n.addEventListener(event, (e) => {
    const t = e && e.target;
    const v = (t && typeof t.value === "string") ? t.value : "";
    routeEvent(handlerId, 0, v);
  });
  return null;
};
globalThis.dom_document = () => put(document);
// Used by Main.main() for the Monaco CustomEvents ("error-lang-code-changed",
// "error-lang-cursor-moved"); these DO carry a detail payload the bridge reads.
globalThis.dom_add_event_listener = (doc, event, handlerId) => {
  const n = get(doc);
  if (n) n.addEventListener(event, (e) => routeEvent(handlerId, putEvent(e), ""));
  return null;
};

// ---- host externs (Main.affine) --------------------------------------------
// NOTE: reads CustomEvent.detail[field] first (how monaco-setup.js dispatches
// "error-lang-code-changed" {code} / "error-lang-cursor-moved" {line,column}),
// then falls back to a plain property.
globalThis.host_event_string = (evH, field) => {
  const e = get(evH); if (!e) return "";
  const d = e.detail || {};
  const v = (field in d) ? d[field] : e[field];
  return v == null ? "" : String(v);
};
globalThis.host_event_int = (evH, field) => {
  const e = get(evH); if (!e) return 0;
  const d = e.detail || {};
  const v = (field in d) ? d[field] : e[field];
  return v == null ? 0 : (Number(v) | 0);
};
globalThis.host_set_timeout = (handlerId, ms) => { setTimeout(() => routeTimeout(handlerId), ms); return null; };
globalThis.host_log = (message) => { console.log(message); return null; };
globalThis.monaco_setup_available = () => (typeof window !== "undefined" && window.__errorLangSetup != null);
globalThis.monaco_init_editor = () => {
  const setup = (typeof window !== "undefined") ? window.__errorLangSetup : null;
  const ed = (setup && setup.initMonacoEditor) ? setup.initMonacoEditor() : null;
  return put(ed);
};

// ---- Monaco externs (MonacoBindings.affine) — adapters over MonacoInterop ----
// EditorOptions is emitted as a flat object; Monaco wants a few nested fields.
function toMonacoOptions(o) {
  return {
    value: o.value, language: o.language, theme: o.theme, fontSize: o.fontSize,
    minimap: { enabled: o.minimapEnabled },
    lineNumbers: o.lineNumbers, renderWhitespace: o.renderWhitespace,
    automaticLayout: o.automaticLayout, scrollBeyondLastLine: o.scrollBeyondLastLine,
    wordWrap: o.wordWrap, tabSize: o.tabSize, insertSpaces: o.insertSpaces,
    glyphMargin: o.glyphMargin, folding: o.folding,
    bracketPairColorization: { enabled: o.bracketPairColorization },
    padding: { top: o.paddingTop },
  };
}
globalThis.monaco_create_editor = (container, options) => put(MI.createEditor(get(container), toMonacoOptions(options)));
globalThis.monaco_dispose_editor = (ed) => { MI.disposeEditor(get(ed)); return null; };
globalThis.monaco_set_editor_value = (ed, v) => { MI.setEditorValue(get(ed), v); return null; };
globalThis.monaco_get_editor_value = (ed) => MI.getEditorValue(get(ed));
globalThis.monaco_set_editor_theme = (theme) => { MI.setEditorTheme(theme); return null; };
globalThis.monaco_set_editor_font_size = (ed, size) => { MI.setEditorFontSize(get(ed), size); return null; };
// MarkerData / MonacoPosition are emitted with field names matching Monaco's, so
// the arrays/objects pass straight through.
globalThis.monaco_set_model_markers = (ed, markers) => { MI.setModelMarkers(get(ed), markers); return null; };
globalThis.monaco_set_position = (ed, position) => { MI.setPosition(get(ed), position); return null; };
globalThis.monaco_reveal_line_in_center = (ed, line) => { MI.revealLineInCenter(get(ed), line); return null; };
globalThis.monaco_focus_editor = (ed) => { MI.focusEditor(get(ed)); return null; };
globalThis.monaco_register_error_lang_language = () => { MI.registerErrorLangLanguage(); return null; };
globalThis.monaco_register_error_lang_themes = () => { MI.registerErrorLangThemes(); return null; };
globalThis.monaco_get_marker_severity_error = () => MI.getMarkerSeverityError();
globalThis.monaco_get_marker_severity_warning = () => MI.getMarkerSeverityWarning();
globalThis.monaco_get_marker_severity_info = () => MI.getMarkerSeverityInfo();
// Editor change listeners: synthesise a detail-bearing event so the same bridge
// path (host_event_*) reads the payload, then route by the registered handler-id.
globalThis.monaco_on_did_change_content = (ed, handlerId) => {
  const inst = get(ed);
  MI.onDidChangeContent(inst, () => routeEvent(handlerId, putEvent({ detail: { code: MI.getEditorValue(inst) } }), ""));
  return null;
};
globalThis.monaco_on_did_change_cursor_position = (ed, handlerId) => {
  MI.onDidChangeCursorPosition(get(ed), (e) => {
    const pos = (e && e.position) ? e.position : { lineNumber: 0, column: 0 };
    routeEvent(handlerId, putEvent({ detail: { line: pos.lineNumber, column: pos.column } }), "");
  });
  return null;
};

// ---- boot ------------------------------------------------------------------
// Seed the model cell (Model.init is pure, so this equals main()'s internal
// model), then run main() for its effects: register listeners, initial render,
// and the setTimeout-guarded Monaco bootstrap.
function boot() {
  MODEL = Main.init();
  Main.main();
}
if (typeof document !== "undefined") {
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", boot);
  else boot();
}
