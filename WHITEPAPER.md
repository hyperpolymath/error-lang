# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

# Error-Lang: A Pedagogical Programming Language for Systems Thinking Through Consequence Amplification

**Author:** Jonathan D.A. Jewell
**Version:** 1.0
**Date:** 2026-03-14
**Status:** Production-Ready (v1.0)

---

## Abstract

Error-Lang is a Turing-complete, production-ready programming language designed to
teach systems thinking by making the consequences of design decisions immediately
visible and quantifiable. Rather than shielding learners from complexity—the
dominant approach in pedagogical language design since Logo (1967)—Error-Lang
employs *consequence amplification*: every design choice (mutable state, unchecked
nulls, global variables, algorithm complexity) produces instant, measurable feedback
through a real-time *stability score*. The language embodies ten intentional design
paradoxes that demonstrate *why* programming languages are designed the way they are,
transforming error diagnosis from frustrating debugging into structured exploration.
This paper presents the theoretical foundations, the paradox catalogue, the quantum
type collapse model, and the computational haptics system that together constitute
a novel pedagogy for programming education.

---

## 1. Introduction

### 1.1 The Problem with Teaching Programming

Traditional approaches to programming education fall into two camps:

1. **Simplification languages** (Scratch, Logo, BASIC): Hide complexity behind
   abstractions, allowing learners to build working programs without understanding
   why they work. Students struggle to transfer skills to production languages
   because the simplified models are too far from reality.

2. **Production languages with training wheels** (Python for beginners, JavaScript
   tutorials): Use real languages but restrict the feature set. Students encounter
   the full language's complexity without preparation, leading to cargo-cult
   programming—copying patterns without understanding their purpose.

Both approaches share a fundamental flaw: they treat errors as obstacles rather
than learning opportunities. Error messages are designed to be "helpful" by
pointing to solutions, but this bypasses the most valuable part of learning—
understanding *why* a constraint exists and *what happens* when it is violated.

### 1.2 Consequence Amplification

Error-Lang introduces a third approach: **consequence amplification**. Instead of
preventing mistakes or hiding complexity, Error-Lang makes the consequences of
every design decision immediately visible:

- Mutable state? The stability score drops by 10 points per mutation.
- Type instability? −15 per reassignment to a different type.
- Global state mutation? −30 per occurrence.
- Unhandled error paths? −25 per failure path.
- O(n²) algorithm? Penalty proportional to actual execution time.

The key insight is that learners develop *intuition* for design quality when
consequences are immediate and quantified, rather than deferred and binary
(compiles/doesn't compile, passes tests/doesn't pass tests).

### 1.3 Contributions

This paper makes the following contributions:

1. **Consequence amplification** as a pedagogical framework for teaching
   programming (Section 2).
2. **Ten design paradoxes** that embody common language design tradeoffs,
   teaching by contradiction (Section 3).
3. A **quantum type collapse model** that uses physics metaphors to make type
   inference tangible (Section 4).
4. **Computational haptics**: a real-time visual and quantitative feedback
   system for code quality (Section 5).
5. A **five-layer debugging methodology** that reframes error diagnosis as
   structured exploration across compiler phases (Section 6).
6. **Formal verification** of core pedagogical invariants using Idris2 dependent
   types (Section 7).

---

## 2. Pedagogical Foundation

### 2.1 Learning from Consequences vs. Learning from Rules

Kolb's experiential learning cycle (1984) identifies four stages: concrete
experience, reflective observation, abstract conceptualisation, and active
experimentation. Traditional programming education emphasises abstract
conceptualisation (learn the rules) and active experimentation (write code),
but underserves concrete experience (see consequences) and reflective observation
(understand *why*).

Error-Lang's consequence amplification targets exactly these two underserved
stages. The stability score provides concrete, quantitative experience; the
Five Whys and Fishbone analysis tools support reflective observation.

### 2.2 The Stability Score

The stability score is a real-time metric (0–100) that quantifies the structural
quality of a program:

```
Stability = Base(100) − Σ(Decision Costs)

Decision Costs:
  Mutable state:        −10 per mutation, −5 per reader
  Type instability:     −15 per type-changing reassignment
  Null propagation:     −20 per unchecked nullable
  Global state:         −30 per mutation
  Unhandled errors:     −25 per failure path
  Algorithm complexity: −(time_ms / 10)
  Memory leaks:         −10 per KB
  Race conditions:      −40 per conflict
```

Crucially, the stability score is *not* a test suite. It is a *live metric*
that changes as the student types, providing immediate feedback without the
delay of running tests. This creates a feedback loop analogous to a musician
hearing wrong notes immediately rather than waiting for an audience review.

### 2.3 Relationship to Existing Work

Error-Lang's approach draws on several traditions:

- **Constructionism** (Papert, 1980): Learning through building artefacts, but
  with richer feedback than Logo's turtle graphics.
- **Cognitive load theory** (Sweller, 1988): The stability score externalises
  intrinsic complexity, reducing cognitive load by making quality visible.
- **Deliberate practice** (Ericsson, 1993): Immediate feedback on specific
  dimensions of quality supports targeted improvement.
- **Design patterns as forces** (Alexander, 1977): Each paradox embodies a
  design force; resolution teaches the pattern's rationale.

---

## 3. The Ten Paradoxes

Error-Lang embodies ten intentional design paradoxes. Each paradox violates a
principle that students take for granted, forcing them to articulate *why* the
principle exists.

### 3.1 Type Quantum Superposition

**Principle violated:** Variables have a single, deterministic type.

In Error-Lang, untyped variables exist in *superposition*—multiple possible types
simultaneously—until they are "observed" (used in a typed context):

```error-lang
let x = 42           # x is Int|String|Float (superposition)
print(x + 1)         # Collapses to Int: 43
print(x ++ " hello") # Would collapse to String: "42 hello"
```

**Pedagogical value:** Students learn that type inference is not magic but
contextual decision-making. The physics metaphor (wave function collapse) makes
the abstract concept concrete. The nondeterminism—seeded, so reproducible—
demonstrates that inference *could* choose differently, highlighting the role
of convention and language design choices.

### 3.2 Positional Operator Semantics

**Principle violated:** Operators have fixed semantics regardless of position.

```error-lang
let a = 1 + 2    # Column 12 (even): addition → 3
let b = 1 + 2    # Column 13 (odd): concatenation → "12"
```

**Pedagogical value:** Demonstrates that syntax is arbitrary convention. Forces
students to articulate *why* consistent semantics matter and appreciate that
mainstream languages' consistency is a deliberate design choice, not a
necessary truth.

### 3.3 Context-Collapse Keywords

**Principle violated:** Keywords are always keywords; identifiers are always identifiers.

At certain nesting depths, keywords become valid identifiers:

```error-lang
let end = 42         # At depth 1, 'end' is an identifier
```

**Pedagogical value:** Teaches the distinction between reserved words and
contextual keywords, and why language designers choose one approach over the other.

### 3.4 Scope Leakage on Primes

**Principle violated:** Lexical scoping is invariant.

Variables leak out of blocks when the run number is prime, the variable name
is a palindrome, or the declaration line is a Fibonacci number:

```error-lang
if true
    let secret = "leaked"
end
print(secret)    # Error on run #4,6,8 (non-primes)
                 # Works on run #3,5,7,11 (primes!)
```

**Pedagogical value:** Dramatises the importance of scope rules by showing what
happens when they are nondeterministic. Makes "variable lifetime" viscerally
real.

### 3.5 Temporal Corruption

**Principle violated:** Programs are referentially transparent across runs.

Previous run history affects current execution via persistent state:

**Pedagogical value:** Demonstrates the dangers of hidden state and why
functional programming emphasises purity.

### 3.6–3.10 Additional Paradoxes

The remaining five paradoxes (Reserved Word Roulette, Arithmetic Drift, Null
Propagation Cascade, Global State Entanglement, Memory Phantom) follow the
same structure: violate a principle, demonstrate consequences, guide the
student to articulate the principle's value. Full specifications are in the
language's `spec/` directory.

---

## 4. Type System: Quantum Collapse Model

### 4.1 Formal Definition

The type system models variables as quantum states:

```
τ ::= Collapsed(T)
    | Superposition({possibleTypes: [T₁, ..., Tₙ], seed: ℤ, declaredAt: Loc})
```

**Collapse rules:**

1. *Arithmetic context*: `x + y` collapses both operands to `Int` or `Float`.
2. *String context*: `x ++ y` collapses to `String`.
3. *Comparison context*: `x > y` collapses to the "widest" numeric type.
4. *Print context*: `print(x)` collapses to `String`.
5. *Type annotation*: `let x: Int = 42` prevents superposition entirely.

**Determinism guarantee:** Given the same seed and observation context, collapse
is deterministic. This means programs are reproducible within a run but may
differ across runs (different seeds), mirroring real physics experiments.

### 4.2 Implementation

The type checker is implemented in ReScript (`compiler/src/TypeSuperposition.res`)
using algebraic data types for quantum states. The seed is derived from the
variable's declaration location and the run counter, ensuring reproducibility.

### 4.3 Relationship to Gradual Typing

Error-Lang's quantum types share structural similarities with gradual typing
(Siek & Taha, 2006), but differ in intent:

- **Gradual typing:** Allows mixing typed and untyped code for practical
  flexibility. The dynamic type `?` is a convenience.
- **Quantum types:** Intentionally amplify the consequences of omitting type
  annotations. Superposition is a *pedagogical device*, not a practical feature.

---

## 5. Computational Haptics

### 5.1 Making the Invisible Visible

"Computational haptics" is our term for the real-time feedback system that
makes abstract code quality metrics tangible:

```
💫 [█████████████░░░░░░░] 65/100
Stability: FAIR

Factors:
  Positional semantics:  −12
  Type superposition:    −15
  Mutable state:         −8
  Unhandled errors:      −5
```

The system provides:

- **Animated stability bar** (0–100) with colour coding (green → red).
- **Per-factor breakdown** showing exactly which decisions cost stability.
- **Real-time updates** as the student types (via LSP integration).
- **IDE overlay** highlighting specific lines that reduce stability.
- **Paradox highlighting** with suggestions for resolution.

### 5.2 Implementation

The haptics system is implemented in Zig (`ffi/zig/`) for performance, with
ReScript bindings for the compiler and LSP server. The Zig FFI computes
stability scores in real-time, including algorithm complexity estimation via
instruction counting.

---

## 6. Five-Layer Debugging Methodology

### 6.1 Layers

Error-Lang teaches debugging as *structured exploration* across five
compiler/runtime layers:

| Layer | Name | Question |
|-------|------|----------|
| 1 | Grammar (EBNF) | Is this expression syntactically valid? |
| 2 | Parser | How does text become structure? |
| 3 | AST | How is code organised? |
| 4 | Semantics | What does structure mean? |
| 5 | Runtime | What actually happens? |

### 6.2 Root Cause Analysis Tools

- **Five Whys**: Iterative depth analysis (`Why → Why → Why → Root`).
- **Fishbone Diagram**: Causal categories (Grammar, Parser, Semantics, Types, Runtime).
- **Soft Systems Methodology**: Holistic view of the system.

These tools reframe debugging from "find and fix the bug" to "understand the
system well enough to explain why the bug exists," which is a fundamentally
different (and more durable) skill.

---

## 7. Formal Verification

### 7.1 Idris2 Proofs

Error-Lang's core pedagogical invariants are formally verified using Idris2
dependent types:

1. **Stability score determinism**: Given the same source and seed, the stability
   score is the same.
2. **Type collapse determinism**: Given the same seed and context, type collapse
   produces the same type.
3. **Scope leakage correctness**: Leakage occurs if and only if the specified
   conditions hold (primality, palindrome, Fibonacci).

These proofs ensure that the pedagogical properties are reliable—students can
trust that the language behaves as documented.

### 7.2 Zig FFI

The formal proofs are bridged to the runtime via a Zig FFI layer (`ffi/zig/`),
following the hyperpolymath Idris2 ABI / Zig FFI standard.

---

## 8. Architecture

| Component | Language | LOC | Purpose |
|-----------|----------|-----|---------|
| Lexer | ReScript | 605 | Tokenisation with position tracking |
| Parser | ReScript | 952 | CST and AST construction |
| Type Superposition | ReScript | 601 | Quantum type inference engine |
| Stability Tracker | ReScript | 315 | Real-time consequence scoring |
| Analyser | ReScript | 317 | Paradox detection |
| Five Whys Engine | ReScript | 387 | Root cause analysis |
| Layer Navigator | ReScript | 370 | Cross-layer debugging |
| Bytecode VM | ReScript | 520 | Stack-based interpreter |
| Codegen | ReScript | 425 | AST → bytecode compilation |
| LSP Server | ReScript | 310 | IDE integration |
| Computational Haptics | Zig | 450 | Real-time feedback engine |
| Formal Proofs | Idris2 | ~300 | Pedagogical invariants |

**Total:** ~5,500 LOC (compiler) + ~3,700 LOC (tooling/proofs)

---

## 9. Evaluation

### 9.1 Target Audience

Error-Lang is designed for:

- **CS education** (introductory and intermediate courses)
- **Language design courses** (compiler construction, PL theory)
- **Debugging mastery** (root cause analysis through structured exploration)
- **Code quality awareness** (developing intuition through consequence)
- **Pedagogical research** (studying learning through intentional mistakes)

### 9.2 Comparison with Existing Pedagogical Languages

| Property | Logo | Scratch | Hedy | Pyret | Error-Lang |
|----------|------|---------|------|-------|------------|
| Consequence visibility | None | None | None | Limited | Full (stability score) |
| Design tradeoff exposure | None | None | None | Some | Intentional (10 paradoxes) |
| Debugging methodology | None | None | None | None | Five Whys + Fishbone |
| Real-time feedback | Turtle | Visual | None | None | Computational haptics |
| Type system pedagogy | None | None | None | Gradual | Quantum collapse |
| Production-capable | No | No | No | Limited | Yes (Turing-complete) |

---

## 10. Conclusion

Error-Lang demonstrates that pedagogical programming languages need not choose
between simplicity and depth. By making consequences immediate and quantifiable,
Error-Lang teaches systems thinking without sacrificing the ability to build real
programs. The ten paradoxes create memorable, visceral learning experiences that
expose design principles students would otherwise accept without examination.
The quantum type model makes type inference tangible. The stability score
externalises quality. The Five Whys methodology teaches debugging as exploration.

Together, these innovations suggest a new direction for programming education:
*teach through consequence, not through rules.*

---

## References

1. Alexander, C. (1977). *A Pattern Language*. Oxford University Press.
2. Ericsson, K. A. et al. (1993). "The Role of Deliberate Practice in the
   Acquisition of Expert Performance." *Psychological Review*, 100(3), 363–406.
3. Kolb, D. A. (1984). *Experiential Learning*. Prentice Hall.
4. Papert, S. (1980). *Mindstorms: Children, Computers, and Powerful Ideas*. Basic Books.
5. Pierce, B. C. (2002). *Types and Programming Languages*. MIT Press.
6. Siek, J. G. & Taha, W. (2006). "Gradual Typing for Functional Languages."
   *Scheme and Functional Programming Workshop*, 81–92.
7. Sweller, J. (1988). "Cognitive Load During Problem Solving: Effects on
   Learning." *Cognitive Science*, 12(2), 257–285.
8. Wadler, P. (2015). "Propositions as Types." *Communications of the ACM*,
   58(12), 75–84.
