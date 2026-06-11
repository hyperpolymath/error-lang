<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
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
    | Echo<τₐ, τᵦ>                         echo / fibre (structured loss)
    | EchoR<τₐ, τᵦ>                        echo residue (witness erased)
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

    e : Echo<A, B>     echo_to_residue(e) : EchoR<A, B>
    ──────────────────────────────────────────────────────  [Stab-Erase]
    stability -= 15     (erasure of a witness is a thermodynamic act)
```

See §7 for Echo types. The `[Stab-Erase]` rule is Error-Lang's signature move:
structured loss is permitted, but **structure is not free** — collapsing an `Echo`
to its `EchoR` residue destroys the input witness and incurs a Landauer-style debit
(cf. `fiber_erasure_bound` in the EchoTypes.jl companion).

---

## 6. Properties

1. **Collapse determinism:** Given same seed and context, same type is selected.
2. **Annotation safety:** Type annotations prevent superposition entirely.
3. **Pedagogical monotonicity:** Annotations always improve stability (never penalised).
4. **Gradual typing compatible:** `Any` unifies with everything, enabling partial typing.
5. **No implicit narrowing:** Numeric widening only (Int → Float, never Float → Int).
6. **Erasure irreversibility:** `Echo<A, B>` does not unify with `EchoR<A, B>`; once a
   witness is erased, the residue cannot be used where a recoverable echo is required.

---

## 7. Echo Types (Structured Loss)

Echo types give Error-Lang a first-class, runnable model of **structured loss** —
*non-total erasure* — adapted from the constructive Agda library
[`echo-types`](https://github.com/hyperpolymath/echo-types) and its finite,
executable companion
[`EchoTypes.jl`](https://github.com/hyperpolymath/EchoTypes.jl).

### 7.1 Formation

For a (conceptual) function `f : A → B` and an output `y : B`, the *echo* is the
**fibre** of `f` over `y`: the proof-relevant collection of inputs that reach `y`.
In Agda:

```
Echo f y  :=  Σ (x : A) , (f x ≡ y)
```

Error-Lang surfaces this as a type constructor indexed by the domain `A` and
codomain `B`:

```
    A type     B type
    ──────────────────────  [T-Echo]
    Echo<A, B> type

Sugar:
    Echo<A>   ≡  Echo<A, ?>     (codomain inferred — treated as Any)
    Echo       ≡  Echo<?, ?>     (opaque fallback / unresolved)
```

A runtime echo value is a **single fibre witness** `VEcho{input, output}`: one `x`
that reached `y`. (The whole-fibre `fiber(f, domain, y)` of EchoTypes.jl awaits
first-class functions in the VM; the witness is the faithful runtime compromise.)

### 7.2 Residue and erasure

`echo_to_residue` weakens an echo to its **residue** `EchoR<A, B>`: the input
witness is **erased** (non-recoverable); only reachability of the output `y : B`
is retained. This is the operational meaning of *structured loss* — the output
constraint survives, the witness does not.

```
    Γ ⊢ e : Echo<A, B>
    ──────────────────────────────────  [T-Erase]
    Γ ⊢ echo_to_residue(e) : EchoR<A, B>     (+ [Stab-Erase], §5)
```

### 7.3 Unification

`Echo` and `EchoR` unify structurally with their own kind, component-wise, and
**never with each other** — encoding the irreversibility of erasure in the type
system itself:

```
unify(Echo<A₁,B₁>,  Echo<A₂,B₂>)  = unify(A₁,A₂) ∧ unify(B₁,B₂)
unify(EchoR<A₁,B₁>, EchoR<A₂,B₂>) = unify(A₁,A₂) ∧ unify(B₁,B₂)
unify(Echo<…>,      EchoR<…>)     = ✗   (residue is not a recoverable echo)
```

### 7.4 Builtins

Named to mirror EchoTypes.jl, so concepts map 1:1 across the three codebases:

| Builtin | Type | Meaning |
|---|---|---|
| `echo(x, y)` | `(A, B) → Echo<A, B>` | construct a fibre witness: `x` reached `y` |
| `echo_to_residue(e)` | `Echo<A, B> → EchoR<A, B>` | erase the witness (incurs `[Stab-Erase]`) |
| `residue_strictly_loses(r)` | `EchoR<A, B> → Bool` | witness non-recoverability |
| `echo_input(e)` | `Echo<A, B> → A` | recover the witness — **illegal on a residue** |
| `echo_output(e)` | `Echo<A, B> \| EchoR<A, B> → B` | the retained output (survives erasure) |

`echo_input` on an `EchoR` is a **type error** (and a runtime error): the witness
is gone. This is the type system enforcing that loss, once structured, is real.

### 7.5 Decomposition obligations (decomposition must be visible)

Error-Lang is a *decompositional* language: Echo's correctness is not "Echo
typechecks" but "the code's decomposition behaviour is represented syntactically,
semantically, and in type checking." The governing invariant is:

> **Decomposition must be visible.** `echo_to_residue` is never a silent cast;
> `EchoR` never behaves as an `Echo` with a missing field; the stability debit is
> never hidden in incidental runtime behaviour.

Echo is therefore specified and tested across three planes:

1. **Syntactic** — parse/pretty-print round-trip for `Echo`/`EchoR`; malformed
   Echo fails clearly; sugar lowers predictably (§7.1); nested forms
   (`Echo<Echo<Int,String>>`) survive the greedy `>>` lexing.
2. **Semantic / runtime** — `echo` builds `VEcho{input,output}`; `echo_input`
   works on `VEcho` and fails on `VResidue`; `echo_output` works on both;
   `echo_to_residue` yields `VResidue` and the witness becomes genuinely
   unavailable; `residue_strictly_loses` reports non-recoverability; stability is
   debited **exactly once** by `echo_to_residue` and **never** by projection.
3. **Type-checking** — the unification and builtin rules of §7.3–§7.4, including
   `EchoR` not unifying back into `Echo` and no implicit `Echo → EchoR` or
   `Echo → B` coercion.

See `docs/Echo-Decomposition.adoc` for the narrative form of these obligations.

### 7.6 Fidelity note

Error-Lang is a runnable scripting language, not a proof assistant: the equality
proof `f x ≡ y` is carried as a runtime-checkable pairing, not a HoTT path. The
`echo-types` Agda library remains the source of mechanized truth; `EchoTypes.jl`
is the executable finite-domain model; Error-Lang's `Echo`/`EchoR` are the
operational, stability-aware embedding of the same lineage.
