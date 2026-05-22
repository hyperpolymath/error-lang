# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

# Error-Lang Type System: Quantum Type Superposition

**Version:** 1.0.0
**Date:** 2026-03-14

---

## 1. Type Language

```
τ ::= Int | Float | String | Bool          primitive types
    | Nil                                  void/nil
    | [τ]                                  array
    | (τ₁, …, τₙ) → τᵣ                   function type
    | Struct(name)                         named struct
    | Any                                  wildcard (unifies with all)
    | α                                    type variable (unification)
```

---

## 2. Quantum Types

### 2.1 Quantum State

Variables in Error-Lang exist in one of two states:

```
Q ::= Collapsed(τ)                         type determined
    | Superposition(possible: [τ₁,…,τₙ], seed: ℤ, loc: Loc)
                                            type undetermined
```

### 2.2 Superposition Assignment

When a variable is declared **without** a type annotation, its type enters
superposition based on the literal's possible interpretations:

```
    annotation = None     lit = IntLit
    ────────────────────────────────────────────────────  [Q-Int]
    Q(x) = Superposition([Int, Float, String], seed, loc)

    annotation = None     lit = FloatLit
    ────────────────────────────────────────────────  [Q-Float]
    Q(x) = Superposition([Float, String], seed, loc)

    annotation = None     lit = StringLit
    ──────────────────────────────────────────────────────  [Q-String]
    Q(x) = Superposition([String, Int, Float], seed, loc)

    annotation = None     lit = BoolLit
    ──────────────────────────────────────────────────  [Q-Bool]
    Q(x) = Superposition([Bool, Int, String], seed, loc)
```

### 2.3 Annotation Prevents Superposition

```
    annotation = Some(τ)
    ──────────────────────────────  [Q-Annotated]
    Q(x) = Collapsed(τ)            (immediate, no superposition)
```

---

## 3. Wavefunction Collapse

### 3.1 Observation Contexts

There are six observation contexts, each assigned a hash value:

| Context | Hash | Trigger |
|---------|------|---------|
| Arithmetic | 0 | `x + y`, `x - y`, `x * y`, `x / y` |
| StringOp | 1 | `x ++ y`, string interpolation |
| Comparison | 2 | `x == y`, `x < y`, etc. |
| Print | 3 | `println(x)` |
| Assignment | 4 | `let y: T = x` (assigned to typed variable) |
| FunctionArg | 5 | `f(x)` where parameter has type annotation |

### 3.2 Collapse Algorithm

```
collapse(Q, context) =
  match Q with
  | Collapsed(τ) → τ                               (already collapsed)
  | Superposition(possible, seed, loc) →
      hash = (seed + context_hash(context)) mod len(possible)
      τ = possible[hash]
      Q ← Collapsed(τ)                             (mutate to collapsed)
      τ
```

### 3.3 Determinism Guarantee

Given the same `seed` and `context`, collapse always produces the same type.
Seeds are derived from the variable's declaration location and the run counter:

```
seed = hash(source_file, line, column, run_counter)
```

This means:
- **Within a run:** Deterministic (same program, same types)
- **Across runs:** May differ (different run_counter → different seed → different collapse)

---

## 4. Standard Type Checking

### 4.1 Unification

Error-Lang uses Robinson's unification for non-quantum types:

```
unify(τ₁, τ₂) =
  | Ok(∅)               if τ₁ = τ₂
  | Ok({α ↦ τ₂})        if τ₁ = α, no occurs check failure
  | Ok({α ↦ τ₁})        if τ₂ = α, no occurs check failure
  | Ok(∅)               if τ₁ = Any or τ₂ = Any
  | unify_structure     for functions, arrays (recursive)
  | Err(Mismatch)       otherwise
```

### 4.2 Typing Rules

```
    ──────────────────  [T-Int]        ──────────────────  [T-Bool]
    Γ ⊢ n : Int                       Γ ⊢ b : Bool

    ──────────────────  [T-String]     ──────────────────  [T-Float]
    Γ ⊢ s : String                    Γ ⊢ f : Float

    (x : Q) ∈ Γ     collapse(Q, context) = τ
    ──────────────────────────────────────────────  [T-Var-Quantum]
    Γ ⊢ x : τ     (in context)

    Γ ⊢ e₁ : τ₁     Γ ⊢ e₂ : τ₂     τ₁, τ₂ numeric
    ──────────────────────────────────────────────────  [T-Arith]
    Γ ⊢ e₁ ⊕ e₂ : wider(τ₁, τ₂)

    Γ ⊢ e₁ : τ     Γ ⊢ e₂ : τ
    ──────────────────────────────  [T-Compare]
    Γ ⊢ e₁ ⊕ e₂ : Bool

    Γ, x₁: τ₁, …, xₙ: τₙ ⊢ body : τᵣ
    ──────────────────────────────────────────────────────  [T-Function]
    Γ ⊢ function f(x₁: τ₁, …, xₙ: τₙ) → τᵣ { body } : (τ₁,…,τₙ) → τᵣ

    Γ ⊢ f : (τ₁,…,τₙ) → τᵣ     ∀i. Γ ⊢ aᵢ : τᵢ
    ──────────────────────────────────────────────────  [T-Call]
    Γ ⊢ f(a₁, …, aₙ) : τᵣ
```

---

## 5. Stability Impact of Types

Type-related stability penalties:

```
    Q(x) = Superposition(…)     (variable in superposition)
    ──────────────────────────────────────────────────────  [Stab-Super]
    stability -= 15     (TypeInstability penalty)

    x previously Collapsed(τ₁)     new value has type τ₂     τ₁ ≠ τ₂
    ──────────────────────────────────────────────────────────────────  [Stab-Reassign]
    stability -= 15     (TypeInstability penalty for type-changing reassignment)
```

---

## 6. Properties

1. **Collapse determinism:** Given same seed and context, same type is selected.
2. **Annotation safety:** Type annotations prevent superposition entirely.
3. **Pedagogical monotonicity:** Annotations always improve stability (never penalised).
4. **Gradual typing compatible:** `Any` unifies with everything, enabling partial typing.
5. **No implicit narrowing:** Numeric widening only (Int → Float, never Float → Int).
