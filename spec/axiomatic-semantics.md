<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# SPDX-License-Identifier: CC-BY-SA-4.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

# Error-Lang Axiomatic Semantics: Paradox Axioms

**Version:** 1.0.0
**Date:** 2026-03-14

---

## 1. Overview

Error-Lang's axiomatic semantics formalise the ten design paradoxes as
Hoare-style preconditions and postconditions. The key insight is that
paradoxes are *not* bugs — they are formally specified behaviours with
well-defined pre/postconditions.

### 1.1 Stability Hoare Triple

```
{P, σ} S {Q, σ'}

where σ ∈ [0, 100] is the stability score
```

Every statement has a stability impact: `σ' ≤ σ` (stability only decreases).

---

## 2. Paradox 1: Type Quantum Superposition

### 2.1 Superposition Axiom

```
    annotation = None
    ──────────────────────────────────────────────────────────  [P1-Super]
    {true, σ} let x = lit {Q(x) = Superposition(…), σ - 15}
    (TypeInstability penalty applied)
```

### 2.2 Collapse Axiom

```
    Q(x) = Superposition(types, seed, loc)     context ∈ {Arith, String, …}
    hash = (seed + context_hash) mod |types|
    τ = types[hash]
    ──────────────────────────────────────────────────────────────  [P1-Collapse]
    {Q(x) = Superposition, σ} use(x, context) {Q(x) = Collapsed(τ), σ}
```

### 2.3 Annotation Prevention Axiom

```
    ──────────────────────────────────────────────────────────  [P1-Annotate]
    {true, σ} let x: τ = lit {Q(x) = Collapsed(τ), σ}
    (no stability penalty)
```

**Pedagogical theorem:** Annotations always preserve or improve stability:
`∀σ. σ_annotated ≥ σ_superposition`.

---

## 3. Paradox 2: Positional Operator Semantics

```
    column(op) mod 2 = 0
    ──────────────────────────────────────────────  [P2-Even]
    {true, σ} e₁ + e₂ {result = arithmetic(e₁, e₂), σ - 12}

    column(op) mod 2 = 1
    ──────────────────────────────────────────────  [P2-Odd]
    {true, σ} e₁ + e₂ {result = concat(toString(e₁), toString(e₂)), σ - 12}
```

**Axiom (determinism):** For a fixed source position, the operator semantics
are deterministic. Reformatting the code may change behaviour.

---

## 4. Paradox 3: Context-Collapse Keywords

```
    depth ≥ 1
    ────────────────────────────────────────────  [P3-Collapse]
    {nesting_depth = depth, σ} let end = e {end ∈ dom(ρ), σ}
    (keyword becomes identifier)

    depth = 0
    ────────────────────────────────────────────  [P3-Reserved]
    {nesting_depth = 0, σ} let end = e {⊥}
    (parse error: keyword used as identifier)
```

---

## 5. Paradox 4: Scope Leakage on Primes

```
    is_prime(run_counter) ∨ is_palindrome(x) ∨ is_fibonacci(line)
    ────────────────────────────────────────────────────────────────  [P4-Leak]
    {true, σ} { let x = v; } {x ∈ dom(ρ_parent), σ}
    (variable leaks out of block scope)

    ¬is_prime(run_counter) ∧ ¬is_palindrome(x) ∧ ¬is_fibonacci(line)
    ──────────────────────────────────────────────────────────────────  [P4-Normal]
    {true, σ} { let x = v; } {x ∉ dom(ρ_parent), σ}
    (standard lexical scoping)
```

**Axiom (leakage determinism):** Leakage is a pure function of
`(run_counter, variable_name, line_number)`.

---

## 6. Paradox 5: Temporal Corruption

```
    temporal_history ≠ []     value affected by history
    ──────────────────────────────────────────────────  [P5-Corrupt]
    {temporal_history = H, σ} eval(e) {result depends on H, σ}

    temporal_history = []     (first run)
    ──────────────────────────────────────────────  [P5-Clean]
    {temporal_history = [], σ} eval(e) {result independent of H, σ}
```

---

## 7. Stability Axioms

### 7.1 Stability Monotonicity

```
    {P, σ} S {Q, σ'}
    ──────────────────  [Stab-Mono]
    σ' ≤ σ              (stability never increases)
```

### 7.2 Stability Penalty Accumulation

```
    {P, σ} S₁ {Q, σ₁}     {Q, σ₁} S₂ {T, σ₂}
    ─────────────────────────────────────────────  [Stab-Seq]
    {P, σ} S₁; S₂ {T, σ₂}
    where σ₂ = σ - penalty(S₁) - penalty(S₂)
```

### 7.3 Stability Floor

```
    σ - penalty(S) < 0
    ──────────────────────  [Stab-Floor]
    σ' = 0                 (stability clamped to 0, never negative)
```

---

## 8. Gutter Block Axioms

```
    body contains parse errors
    ────────────────────────────────────────────────  [Gutter-Recovery]
    {true, σ} gutter { body } end {errors collected, σ}
    (parser always recovers; no crash; errors available for inspection)
```

**Safety axiom:** A gutter block never causes program termination.

---

## 9. Key Theorems

### 9.1 Annotation Optimality

**Theorem:** For any program P, the variant P' with all type annotations added
has stability(P') ≥ stability(P). Type annotations are always beneficial.

### 9.2 Paradox Determinism

**Theorem:** All ten paradoxes are deterministic given the same
`(source, run_counter, seed)` triple. Non-determinism is apparent, not actual.

### 9.3 Stability as Loop Variant

**Theorem:** If every loop body consumes at least δ > 0 stability, then all
loops terminate within ⌈100/δ⌉ iterations (since stability starts at 100 and
is bounded below by 0).

### 9.4 Pedagogical Completeness

**Theorem:** Every violation of a "standard" programming principle corresponds
to a measurable stability penalty, ensuring no design tradeoff is invisible.
