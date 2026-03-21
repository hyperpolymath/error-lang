// SPDX-License-Identifier: PMPL-1.0-or-later
// Model.res - Application state

/**
 * Model - The application state
 *
 * Everything the IDE knows about the current session:
 * - Code being edited
 * - Compilation results
 * - Stability metrics
 * - Active visualizations
 * - Analysis results
 */

// Import compiler types
// In full implementation: open Types from compiler

type position = {
  line: int,
  column: int,
}

type layer =
  | Grammar
  | Parser
  | AST
  | Semantics
  | Runtime

type panel =
  | EditorPanel
  | DashboardPanel
  | NavigatorPanel
  | AnalyzerPanel
  | ExplorerPanel

type analysisMode =
  | FiveWhys
  | Fishbone
  | SoftSystems
  | Paradoxes

type theme = Light | Dark | HighContrast

// Main application model
type model = {
  // Code state
  code: string,
  fileName: option<string>,
  cursorPosition: position,
  selection: option<{start: position, end_: position}>,

  // Compilation state
  ast: option<string>,  // Serialized AST (in full version: program type)
  parseErrors: array<string>,  // Simplified for now

  // Stability tracking
  stabilityScore: int,
  stabilityHistory: array<int>,
  stabilityFactors: array<string>,  // Simplified

  // Layer navigation
  currentLayer: layer,
  layerViews: dict<string, string>,  // layer name -> content

  // Analysis results
  fiveWhysResult: option<string>,
  paradoxes: array<string>,
  quantumVariables: array<(string, string)>,  // name, types

  // UI state
  activePanel: panel,
  theme: theme,
  fontSize: int,
  sidebarVisible: bool,

  // Execution state
  isRunning: bool,
  runtimeOutput: array<string>,
  runNumber: int,

  // Monaco editor state
  monacoReady: bool,
}

// Initial model
let init = (): model => {
  code: "# Write your Error-Lang code here\n\nmain\n    let x = 42\n    println(x)\nend",
  fileName: None,
  cursorPosition: {line: 1, column: 1},
  selection: None,

  ast: None,
  parseErrors: [],

  stabilityScore: 100,
  stabilityHistory: [100],
  stabilityFactors: [],

  currentLayer: AST,
  layerViews: Dict.make(),

  fiveWhysResult: None,
  paradoxes: [],
  quantumVariables: [],

  activePanel: EditorPanel,
  theme: Dark,
  fontSize: 14,
  sidebarVisible: true,

  isRunning: false,
  runtimeOutput: [],
  runNumber: 1,

  monacoReady: false,
}

// Selectors for derived state
let getCurrentLayerName = (model: model): string =>
  switch model.currentLayer {
  | Grammar => "Grammar"
  | Parser => "Parser"
  | AST => "AST"
  | Semantics => "Semantics"
  | Runtime => "Runtime"
  }

let hasErrors = (model: model): bool =>
  Array.length(model.parseErrors) > 0

let isStable = (model: model): bool =>
  model.stabilityScore >= 70

// Theme to string for Monaco interop
let themeToString = (theme: theme): string =>
  switch theme {
  | Light => "light"
  | Dark => "dark"
  | HighContrast => "highcontrast"
  }
