// SPDX-License-Identifier: MPL-2.0
// Update.res - State transitions

/**
 * Update - Handle messages and produce new state
 *
 * Pure function: (msg, model) -> (model, cmd)
 * This is the heart of TEA - all state changes flow through here.
 */

open Model
open Msg

type cmd = Tea.Cmd.t<msg>

// Main update function
let update = (msg: msg, model: model): (model, cmd) => {
  switch msg {
  // Code editing
  | CodeChanged(newCode) =>
    let newModel = {...model, code: newCode}
    // Trigger background compilation
    (newModel, Tea.Cmd.msg(CompileCode))

  | CursorMoved(pos) =>
    ({...model, cursorPosition: pos}, Tea.Cmd.none)

  // Monaco editor events
  | MonacoReady =>
    ({...model, monacoReady: true}, Tea.Cmd.none)

  | MonacoContentChanged(newCode) =>
    let newModel = {...model, code: newCode}
    // Trigger background compilation
    (newModel, Tea.Cmd.msg(CompileCode))

  | MonacoCursorMoved(line, col) =>
    ({...model, cursorPosition: {line, column: col}}, Tea.Cmd.none)

  // Compilation
  | CompileCode =>
    // In full implementation: call ReScript compiler
    // For now: simulate compilation
    (model, Tea.Cmd.msg(CompilationSuccess("AST placeholder")))

  | CompilationSuccess(ast) =>
    let newModel = {
      ...model,
      ast: Some(ast),
      parseErrors: [],
    }
    // Trigger stability analysis
    (newModel, Tea.Cmd.msg(AnalyzeStability))

  | CompilationFailure(errors) =>
    ({...model, parseErrors: errors, ast: None}, Tea.Cmd.none)

  // Layer navigation
  | NavigateToLayer(layer) =>
    // Switch to different abstraction layer
    let newModel = {...model, currentLayer: layer}
    // Update layer view
    (newModel, Tea.Cmd.none)

  // Stability analysis
  | AnalyzeStability =>
    // In full implementation: call Analyzer.analyzeProgram
    // For now: simulate analysis
    (model, Tea.Cmd.msg(StabilityAnalyzed({
      score: 95,
      factors: ["mutable-state"],
    })))

  | StabilityAnalyzed({score, factors}) =>
    let newHistory = Array.concat(model.stabilityHistory, [score])
    ({
      ...model,
      stabilityScore: score,
      stabilityFactors: factors,
      stabilityHistory: newHistory,
    }, Tea.Cmd.none)

  // Five Whys analysis
  | RunFiveWhys(factor) =>
    // In full implementation: call FiveWhys.analyze
    (model, Tea.Cmd.msg(FiveWhysComplete("Analysis result...")))

  | FiveWhysComplete(result) =>
    ({...model, fiveWhysResult: Some(result)}, Tea.Cmd.none)

  // Execution
  | RunProgram =>
    let newModel = {...model, isRunning: true, runtimeOutput: []}
    // In full implementation: spawn worker to execute code
    (newModel, Tea.Cmd.none)

  | RuntimeOutput(output) =>
    let newOutput = Array.concat(model.runtimeOutput, [output])
    ({...model, runtimeOutput: newOutput}, Tea.Cmd.none)

  | ExecutionComplete(finalScore) =>
    ({
      ...model,
      isRunning: false,
      stabilityScore: finalScore,
    }, Tea.Cmd.none)

  // UI
  | SwitchPanel(panel) =>
    ({...model, activePanel: panel}, Tea.Cmd.none)

  | ChangeTheme(theme) =>
    ({...model, theme: theme}, Tea.Cmd.none)

  | ToggleSidebar =>
    ({...model, sidebarVisible: !model.sidebarVisible}, Tea.Cmd.none)

  // Router integration
  | RouteChanged(path) =>
    // Handle route changes from cadre-tea-router
    let (newModel, cmd) = handleRoute(path, model)
    (newModel, cmd)

  | NavigateTo(path) =>
    // Trigger navigation (will cause RouteChanged)
    (model, Tea.Cmd.none)  // Router handles this

  | _ =>
    (model, Tea.Cmd.none)
  }
}

// Handle route changes
and handleRoute = (path: string, model: model): (model, cmd) => {
  // Parse route and update model accordingly
  // In full implementation: use cadre-tea-router

  if String.startsWith(path, "/explore/") {
    let layerName = String.sliceToEnd(path, ~start=9)
    let layer = switch layerName {
    | "grammar" => Grammar
    | "parser" => Parser
    | "ast" => AST
    | "semantics" => Semantics
    | "runtime" => Runtime
    | _ => AST
    }

    update(NavigateToLayer(layer), model)
  } else {
    (model, Tea.Cmd.none)
  }
}
