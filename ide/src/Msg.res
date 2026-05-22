// SPDX-License-Identifier: MPL-2.0
// Msg.res - Event messages

/**
 * Msg - Events that can happen in the IDE
 *
 * All user interactions and system events are messages.
 * Update function handles messages to produce new state.
 */

open Model

type msg =
  // Code editing
  | CodeChanged(string)
  | CursorMoved(position)
  | SelectionChanged({start: position, end_: position})
  | FileLoaded(string, string)  // filename, content

  // Compilation
  | CompileCode
  | CompilationSuccess(string)  // AST (serialized)
  | CompilationFailure(array<string>)  // errors

  // Execution
  | RunProgram
  | StepProgram
  | PauseProgram
  | StopProgram
  | RuntimeOutput(string)
  | ExecutionComplete(int)  // final stability score

  // Layer navigation
  | NavigateToLayer(layer)
  | SelectASTNode(string)  // node ID
  | HighlightCode({start: int, end_: int})

  // Analysis
  | AnalyzeStability
  | StabilityAnalyzed({score: int, factors: array<string>})
  | RunFiveWhys(string)  // stability factor
  | FiveWhysComplete(string)  // analysis result
  | ExploreParadox(string)

  // Visualization
  | ShowRipple({source: int, affected: array<int>})
  | ShowCascade({path: array<int>})
  | AnimateStability(int)

  // UI
  | SwitchPanel(panel)
  | ChangeTheme(theme)
  | ChangeFontSize(int)
  | ToggleSidebar

  // Persistence
  | SaveCode
  | LoadCode(string)
  | ExportProgram

  // Monaco editor
  | MonacoReady
  | MonacoContentChanged(string)  // New code from Monaco
  | MonacoCursorMoved(int, int)   // line, column

  // Router integration (cadre-tea-router)
  | RouteChanged(string)  // URL path
  | NavigateTo(string)    // Trigger navigation
