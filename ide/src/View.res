// SPDX-License-Identifier: MPL-2.0
// View.res - Rendering functions

/**
 * View - Render the UI from model state
 *
 * Pure function: model -> html
 * This is the view layer of TEA - all rendering flows through here.
 */

open Model
open Msg

// Placeholder HTML type (in full implementation: use rescript-tea's Vdom)
type html = Tea.Html.html<msg>

// Main view function
let view = (model: model): html => {
  Tea.Html.div(
    ~className="error-lang-studio",
    [
      renderTopBar(model),
      renderMainContent(model),
      renderBottomPanel(model),
    ],
  )
}

// Top bar with controls
and renderTopBar = (model: model): html => {
  Tea.Html.header(
    ~className="top-bar",
    [
      Tea.Html.h1(~className="logo", [Tea.Html.text("Error-Lang Studio")]),
      renderRunControls(model),
      renderThemeSelector(model),
    ],
  )
}

// Run/Stop/Step controls
and renderRunControls = (model: model): html => {
  Tea.Html.div(
    ~className="run-controls",
    [
      if model.isRunning {
        Tea.Html.button(
          ~onClick=Tea.Html.onClick(StopProgram),
          [Tea.Html.text("⏸ Pause")],
        )
      } else {
        Tea.Html.button(
          ~onClick=Tea.Html.onClick(RunProgram),
          [Tea.Html.text("▶ Run")],
        )
      },
      Tea.Html.button(
        ~onClick=Tea.Html.onClick(StepProgram),
        ~disabled=model.isRunning,
        [Tea.Html.text("⏭ Step")],
      ),
      Tea.Html.span(
        ~className="run-number",
        [Tea.Html.text(`Run #${Int.toString(model.runNumber)}`)],
      ),
    ],
  )
}

// Theme selector dropdown
and renderThemeSelector = (model: model): html => {
  Tea.Html.select(
    ~onChange=Tea.Html.onChange(value => ChangeTheme(themeFromString(value))),
    [
      Tea.Html.option(
        ~value="dark",
        ~selected=model.theme == Dark,
        [Tea.Html.text("Dark")],
      ),
      Tea.Html.option(
        ~value="light",
        ~selected=model.theme == Light,
        [Tea.Html.text("Light")],
      ),
      Tea.Html.option(
        ~value="highcontrast",
        ~selected=model.theme == HighContrast,
        [Tea.Html.text("High Contrast")],
      ),
    ],
  )
}

// Main content area with 3-column layout
and renderMainContent = (model: model): html => {
  Tea.Html.main(
    ~className="main-content",
    [
      if model.sidebarVisible {
        renderLeftSidebar(model)
      } else {
        Tea.Html.div(~className="sidebar-collapsed", [])
      },
      renderCenterPanel(model),
      renderRightSidebar(model),
    ],
  )
}

// Left sidebar: Layer Navigator
and renderLeftSidebar = (model: model): html => {
  Tea.Html.aside(
    ~className="left-sidebar",
    [
      Tea.Html.h2([], [Tea.Html.text("Layers")]),
      Tea.Html.nav(
        ~className="layer-nav",
        [
          renderLayerButton(model, Grammar, "Grammar"),
          renderLayerButton(model, Parser, "Parser"),
          renderLayerButton(model, AST, "AST"),
          renderLayerButton(model, Semantics, "Semantics"),
          renderLayerButton(model, Runtime, "Runtime"),
        ],
      ),
      Tea.Html.div(
        ~className="layer-content",
        [renderLayerView(model)],
      ),
    ],
  )
}

// Individual layer button
and renderLayerButton = (model: model, layer: layer, label: string): html => {
  let isActive = model.currentLayer == layer
  Tea.Html.button(
    ~className=isActive ? "layer-btn active" : "layer-btn",
    ~onClick=Tea.Html.onClick(NavigateToLayer(layer)),
    [Tea.Html.text(label)],
  )
}

// Layer view content (shows current layer details)
and renderLayerView = (model: model): html => {
  let layerName = getCurrentLayerName(model)
  let content = Dict.get(model.layerViews, layerName)->Option.getOr("No content yet")

  Tea.Html.div(
    ~className="layer-display",
    [
      Tea.Html.h3([], [Tea.Html.text(layerName)]),
      Tea.Html.pre(
        ~className="layer-code",
        [Tea.Html.text(content)],
      ),
    ],
  )
}

// Center panel: Code Editor
and renderCenterPanel = (model: model): html => {
  Tea.Html.section(
    ~className="center-panel",
    [
      renderPanelTabs(model),
      if model.activePanel == EditorPanel {
        renderCodeEditor(model)
      } else if model.activePanel == DashboardPanel {
        renderDashboard(model)
      } else if model.activePanel == AnalyzerPanel {
        renderAnalyzer(model)
      } else {
        Tea.Html.div([], [Tea.Html.text("Panel not implemented")])
      },
    ],
  )
}

// Panel tabs (Editor, Dashboard, Analyzer, etc.)
and renderPanelTabs = (model: model): html => {
  Tea.Html.div(
    ~className="panel-tabs",
    [
      renderTab(model, EditorPanel, "Editor"),
      renderTab(model, DashboardPanel, "Dashboard"),
      renderTab(model, AnalyzerPanel, "Analyzer"),
      renderTab(model, NavigatorPanel, "Navigator"),
      renderTab(model, ExplorerPanel, "Explorer"),
    ],
  )
}

// Individual tab button
and renderTab = (model: model, panel: panel, label: string): html => {
  let isActive = model.activePanel == panel
  Tea.Html.button(
    ~className=isActive ? "tab active" : "tab",
    ~onClick=Tea.Html.onClick(SwitchPanel(panel)),
    [Tea.Html.text(label)],
  )
}

// Code editor - Monaco Editor integration
// Monaco is loaded via AMD in index.html and initialized by monaco-setup.js.
// The div#monaco-editor-container is the mount point for the Monaco instance.
// Content changes and cursor moves are forwarded via CustomEvents to the TEA app.
and renderCodeEditor = (model: model): html => {
  let cursorText = "Ln " ++ Int.toString(model.cursorPosition.line) ++ ", Col " ++ Int.toString(model.cursorPosition.column)
  let stabilityText = "Stability: " ++ Int.toString(model.stabilityScore) ++ "/100"

  Tea.Html.div(
    ~className="code-editor",
    [
      // Monaco editor mount point
      // The monaco-setup.js module watches for this element and initializes
      // the editor when it appears. automaticLayout: true handles resizing.
      Tea.Html.div(
        ~id="monaco-editor-container",
        ~className="monaco-editor-wrapper",
        [],
      ),
      // Status bar below editor
      Tea.Html.div(
        ~className="editor-status-bar",
        [
          Tea.Html.span(
            ~className="cursor-position",
            [Tea.Html.text(cursorText)],
          ),
          Tea.Html.span(
            ~className="editor-language",
            [Tea.Html.text("Error-Lang")],
          ),
          Tea.Html.span(
            ~className="stability-badge",
            [Tea.Html.text(stabilityText)],
          ),
        ],
      ),
      if hasErrors(model) {
        renderErrorList(model)
      } else {
        Tea.Html.div([], [])
      },
    ],
  )
}

// Error list display
and renderErrorList = (model: model): html => {
  Tea.Html.div(
    ~className="error-list",
    Array.map(model.parseErrors, error =>
      Tea.Html.div(
        ~className="error-item",
        [Tea.Html.text("⚠ " ++ error)],
      )
    )->Array.toList,
  )
}

// Dashboard: Stability metrics and history
and renderDashboard = (model: model): html => {
  Tea.Html.div(
    ~className="dashboard",
    [
      renderStabilityScore(model),
      renderStabilityHistory(model),
      renderStabilityFactors(model),
    ],
  )
}

// Stability score display (large number)
and renderStabilityScore = (model: model): html => {
  let scoreClass = if model.stabilityScore >= 90 {
    "score excellent"
  } else if model.stabilityScore >= 70 {
    "score good"
  } else if model.stabilityScore >= 50 {
    "score fair"
  } else {
    "score poor"
  }

  Tea.Html.div(
    ~className="stability-score",
    [
      Tea.Html.div(
        ~className=scoreClass,
        [Tea.Html.text(Int.toString(model.stabilityScore))],
      ),
      Tea.Html.div(
        ~className="score-label",
        [Tea.Html.text("Stability Score")],
      ),
    ],
  )
}

// Stability history sparkline
and renderStabilityHistory = (model: model): html => {
  Tea.Html.div(
    ~className="stability-history",
    [
      Tea.Html.h3([], [Tea.Html.text("History")]),
      Tea.Html.div(
        ~className="sparkline",
        Array.map(model.stabilityHistory, score =>
          Tea.Html.span(
            ~className="bar",
            ~style=`height: ${Int.toString(score)}%`,
            [],
          )
        )->Array.toList,
      ),
    ],
  )
}

// Stability factors list
and renderStabilityFactors = (model: model): html => {
  Tea.Html.div(
    ~className="stability-factors",
    [
      Tea.Html.h3([], [Tea.Html.text("Factors")]),
      if Array.length(model.stabilityFactors) == 0 {
        Tea.Html.p([], [Tea.Html.text("No stability issues detected")])
      } else {
        Tea.Html.ul(
          [],
          Array.map(model.stabilityFactors, factor =>
            Tea.Html.li(
              [],
              [
                Tea.Html.text(factor),
                Tea.Html.button(
                  ~onClick=Tea.Html.onClick(RunFiveWhys(factor)),
                  ~className="why-btn",
                  [Tea.Html.text("Why?")],
                ),
              ],
            )
          )->Array.toList,
        )
      },
    ],
  )
}

// Analyzer panel: Five Whys results
and renderAnalyzer = (model: model): html => {
  Tea.Html.div(
    ~className="analyzer",
    [
      Tea.Html.h2([], [Tea.Html.text("Five Whys Analysis")]),
      switch model.fiveWhysResult {
      | None =>
        Tea.Html.p(
          [],
          [Tea.Html.text("Select a stability factor and click 'Why?' to analyze")],
        )
      | Some(result) =>
        Tea.Html.pre(
          ~className="analysis-result",
          [Tea.Html.text(result)],
        )
      },
    ],
  )
}

// Right sidebar: Stability dashboard
and renderRightSidebar = (model: model): html => {
  Tea.Html.aside(
    ~className="right-sidebar",
    [
      renderStabilityIndicator(model),
      renderQuantumVariables(model),
      renderParadoxes(model),
    ],
  )
}

// Stability indicator (visual bar)
and renderStabilityIndicator = (model: model): html => {
  let emoji = if model.stabilityScore >= 90 {
    "✨"
  } else if model.stabilityScore >= 70 {
    "💫"
  } else if model.stabilityScore >= 50 {
    "⚠️"
  } else {
    "🔥"
  }

  let barWidth = 20
  let filled = (model.stabilityScore * barWidth) / 100
  let empty = barWidth - filled

  Tea.Html.div(
    ~className="stability-indicator",
    [
      Tea.Html.div(
        ~className="emoji",
        [Tea.Html.text(emoji)],
      ),
      Tea.Html.div(
        ~className="bar-container",
        [
          Tea.Html.div(
            ~className="bar-filled",
            ~style=`width: ${Int.toString(model.stabilityScore)}%`,
            [],
          ),
        ],
      ),
      Tea.Html.div(
        ~className="score-text",
        [Tea.Html.text(`${Int.toString(model.stabilityScore)}/100`)],
      ),
    ],
  )
}

// Quantum variables list
and renderQuantumVariables = (model: model): html => {
  Tea.Html.div(
    ~className="quantum-variables",
    [
      Tea.Html.h3([], [Tea.Html.text("Quantum Variables")]),
      if Array.length(model.quantumVariables) == 0 {
        Tea.Html.p([], [Tea.Html.text("No quantum variables")])
      } else {
        Tea.Html.ul(
          [],
          Array.map(model.quantumVariables, ((name, types)) =>
            Tea.Html.li(
              [],
              [Tea.Html.text(`${name}: ${types}`)],
            )
          )->Array.toList,
        )
      },
    ],
  )
}

// Paradoxes discovered
and renderParadoxes = (model: model): html => {
  Tea.Html.div(
    ~className="paradoxes",
    [
      Tea.Html.h3([], [Tea.Html.text("Paradoxes Discovered")]),
      if Array.length(model.paradoxes) == 0 {
        Tea.Html.p([], [Tea.Html.text("No paradoxes yet - keep exploring!")])
      } else {
        Tea.Html.ul(
          [],
          Array.map(model.paradoxes, paradox =>
            Tea.Html.li(
              [],
              [Tea.Html.text(paradox)],
            )
          )->Array.toList,
        )
      },
    ],
  )
}

// Bottom panel: Output and logs
and renderBottomPanel = (model: model): html => {
  Tea.Html.footer(
    ~className="bottom-panel",
    [
      Tea.Html.h3([], [Tea.Html.text("Output")]),
      Tea.Html.div(
        ~className="output-area",
        Array.map(model.runtimeOutput, line =>
          Tea.Html.div(
            ~className="output-line",
            [Tea.Html.text(line)],
          )
        )->Array.toList,
      ),
    ],
  )
}

// Helper: Convert string to theme
let themeFromString = (str: string): theme =>
  switch str {
  | "light" => Light
  | "dark" => Dark
  | "highcontrast" => HighContrast
  | _ => Dark
  }
