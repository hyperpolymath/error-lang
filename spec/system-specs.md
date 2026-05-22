# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

# Error-Lang System Specifications

Error-Lang is a pedagogical programming language where errors are features.
Implementation stack: ReScript compiler (compiles to JavaScript), Zig FFI for
computational haptics feedback. Designed for learning through deliberate failure.

---

## Memory Model

Error-Lang's memory model is intentionally simple, befitting its pedagogical
purpose, with a specialised FFI layer for haptics.

### JavaScript Runtime (Primary)

- ReScript compiles to JavaScript; all Error-Lang values are JS heap objects
  managed by the JavaScript engine's garbage collector.
- No manual memory management is exposed to Error-Lang users.
- Values are immutable by default (ReScript's functional core).
- Mutable state is limited to the interpreter's internal bookkeeping.

### Interpreter State

- The interpreter maintains a `StabilityState` struct containing:
  - **stability_score**: `float` — current program stability (0.0 to 1.0).
  - **paradox_level**: `int` — depth of active paradox nesting.
  - **error_history**: `array<ErrorEvent>` — log of all errors encountered.
  - **correction_attempts**: `int` — number of user fix attempts this session.
- This state persists across statements within a single REPL session or file
  execution and is reset between sessions.

### Zig FFI Layer (Computational Haptics)

- The Zig FFI module manages its own memory via `std.mem.Allocator`.
- Haptic feedback buffers are allocated per-event and freed after transmission
  to the haptic device.
- No GC interaction — Zig allocations are invisible to the JS runtime.
- Data crossing the FFI boundary is serialised to C-compatible structs defined
  in `generated/abi/haptics.h`.

### Memory Invariants

- Error-Lang programs cannot cause memory leaks in user-space (GC handles all).
- Zig FFI allocations are bounded: at most one haptic buffer active at a time.
- The stability state struct has a fixed, small memory footprint.

---

## Concurrency Model

Error-Lang is deliberately single-threaded.

### Design Rationale

- Concurrency adds complexity that conflicts with pedagogical goals.
- Learners should focus on understanding errors and stability, not race
  conditions or deadlocks.
- The single-threaded model makes program behaviour fully deterministic
  (modulo stability score thresholds).

### Execution Model

- Statements execute sequentially in source order.
- The REPL processes one input at a time, updating stability state after each.
- No async operations, no event loop, no callbacks.

### Haptic Feedback Timing

- Zig FFI calls for haptic feedback are synchronous and blocking.
- Haptic events are brief (< 50ms) so blocking is imperceptible.
- If no haptic device is connected, the FFI call returns immediately (no-op).

---

## Effect System

Error-Lang's effect system is unconventional: stability impact and paradox
state are the primary tracked effects.

### Stability as Effect

Every statement in Error-Lang has a stability impact:

| Category            | Stability Effect         | Example                          |
|---------------------|--------------------------|----------------------------------|
| Correct statement   | `+0.05` to `+0.10`      | Valid assignment, correct logic   |
| Syntax error        | `-0.15` to `-0.25`       | Missing semicolon, bad indent    |
| Type error          | `-0.10` to `-0.20`       | Wrong argument type              |
| Deliberate error    | `+0.02` (learning bonus) | Annotated with `@intentional`    |
| Error correction    | `+0.15` to `+0.20`       | Fixing a previous error          |
| Repeated error      | `-0.30` (penalty)        | Same error class within 5 stmts  |

- Stability is checked implicitly after every statement.
- When stability drops below `0.2`, the interpreter enters "crisis mode" —
  haptic feedback intensifies and hints become more explicit.
- When stability reaches `1.0`, the session is "mastered."

### Paradox State as Implicit Effect

- Certain constructs create paradoxes (self-referential errors, contradictions).
- Paradox depth is tracked as an implicit effect counter.
- Paradoxes cannot be nested beyond depth 3 (interpreter rejects deeper nesting).
- Resolving a paradox grants a significant stability bonus (`+0.25`).

### Haptic Effect

- Error events trigger haptic feedback via the Zig FFI.
- The haptic intensity is proportional to the stability drop.
- This is a side effect managed entirely by the interpreter — not visible in
  the Error-Lang type system.

### No User-Defined Effects

- Error-Lang does not expose an effect system to users.
- All effects are implicit and managed by the interpreter runtime.
- This is intentional: the language teaches through experience, not abstraction.

---

## Module System

Error-Lang has no explicit module system.

### Design Rationale

- Modules add cognitive overhead for beginners.
- Error-Lang programs are small (typically < 100 lines) and self-contained.
- The focus is on understanding individual errors, not software architecture.

### File Execution

- Each `.err` file is an independent program.
- No imports, no exports, no namespaces.
- The standard library (error constructors, stability queries) is always
  available without import.

### Built-in Functions (Always Available)

| Function            | Description                                    |
|---------------------|------------------------------------------------|
| `stability()`       | Returns current stability score                |
| `paradox_depth()`   | Returns current paradox nesting level           |
| `error_count()`     | Returns total errors in this session            |
| `hint()`            | Requests a contextual hint                     |
| `intentional(expr)` | Marks an expression as a deliberate error      |
| `history()`         | Returns the error history for this session      |

### Compiler Organisation (Internal)

- The ReScript compiler is a single package (not split into sub-packages).
- Source files: `Lexer.res`, `Parser.res`, `Interpreter.res`, `Stability.res`,
  `HapticsBridge.res`.
- The Zig FFI is a single `haptics.zig` file compiled to a shared library.
