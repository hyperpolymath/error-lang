<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# SPDX-License-Identifier: CC-BY-SA-4.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

# Error-Lang Operational Semantics

**Version:** 1.0.0
**Date:** 2026-03-14

---

## 1. Notation

- `ρ` — Environment (variable bindings)
- `Σ` — Interpreter state (stability score, run counter, paradox state)
- `ρ, Σ ⊢ e ⇓ v, Σ'` — Expression `e` evaluates to value `v` with updated state
- `⊥` — Error

---

## 2. Values

```
v ∈ Value ::=
    ()                                  unit
  | b ∈ {true, false}                   boolean
  | n ∈ ℤ                               integer
  | f ∈ ℝ                               float
  | s ∈ String                          string
  | [v₁, …, vₙ]                         array
  | Fn(name, params, body, ρ_closure)    function closure
  | Builtin(name, impl)                 built-in function
  | Quantum(possible_types, seed, loc)   type in superposition
```

---

## 3. Interpreter State

```
Σ = ⟨ stability     : ℝ ∈ [0, 100]     (initially 100),
      run_counter    : ℕ                 (persistent across runs),
      paradox_state  : ParadoxState,
      rng_seed       : ℕ ⟩

ParadoxState = ⟨
  positional_active   : Bool,           Paradox 2: positional operators
  context_depth       : ℕ,             Paradox 3: context-collapse keywords
  scope_leaks         : Set<Ident>,    Paradox 4: leaked variables
  temporal_history    : List<Value>    Paradox 5: previous run state
⟩
```

---

## 4. Stability Score

The stability score is a real-time metric updated by a penalty function:

```
penalty : Decision → ℝ
penalty(MutableState)      = 10
penalty(MutableReader)     = 5
penalty(TypeInstability)   = 15
penalty(NullPropagation)   = 20
penalty(GlobalMutation)    = 30
penalty(UnhandledError)    = 25
penalty(AlgorithmCost(t))  = t / 10      (t = milliseconds)
penalty(MemoryLeak(kb))    = kb × 10
penalty(RaceCondition)     = 40

apply_penalty(Σ, d) = Σ[stability ↦ max(0, Σ.stability − penalty(d))]
```

---

## 5. Quantum Type Collapse (Paradox 1)

### 5.1 Superposition Creation

```
    no type annotation on x     seed = hash(loc, Σ.run_counter)
    ─────────────────────────────────────────────────────────────  [Super-Create]
    ρ, Σ ⊢ let x = 42 ⇒ ρ[x ↦ Quantum([Int, Float, String], seed, loc)], Σ'
    where Σ' = apply_penalty(Σ, TypeInstability)
```

### 5.2 Collapse Rules

```
    ρ(x) = Quantum(types, seed, loc)     context = Arithmetic
    collapsed = deterministic_select(types ∩ Numeric, seed)
    ───────────────────────────────────────────────────────  [Collapse-Arith]
    ρ, Σ ⊢ x + y ⇓ collapsed(ρ(x)) + eval(y)

    ρ(x) = Quantum(types, seed, loc)     context = StringConcat
    ─────────────────────────────────────────────────────────  [Collapse-String]
    ρ, Σ ⊢ x ++ y ⇓ toString(ρ(x)) ++ eval(y)

    ρ(x) = Quantum(types, seed, loc)     context = Print
    ──────────────────────────────────────────────────────  [Collapse-Print]
    ρ, Σ ⊢ println(x) ⇓ println(toString(ρ(x)))
```

### 5.3 Annotation Prevents Superposition

```
    type annotation present
    ──────────────────────────────────────────────  [No-Super]
    ρ, Σ ⊢ let x: Int = 42 ⇒ ρ[x ↦ 42], Σ       (no penalty)
```

---

## 6. Positional Operator Semantics (Paradox 2)

```
    column(+) = c     c mod 2 = 0
    ρ, Σ ⊢ e₁ ⇓ v₁     ρ, Σ ⊢ e₂ ⇓ v₂
    ──────────────────────────────────────  [Pos-Add]
    ρ, Σ ⊢ e₁ + e₂ ⇓ v₁ + v₂             (arithmetic addition)

    column(+) = c     c mod 2 = 1
    ρ, Σ ⊢ e₁ ⇓ v₁     ρ, Σ ⊢ e₂ ⇓ v₂
    ──────────────────────────────────────  [Pos-Concat]
    ρ, Σ ⊢ e₁ + e₂ ⇓ toString(v₁) ++ toString(v₂)   (string concatenation)

    Σ' = apply_penalty(Σ, PositionalSemantics)
```

---

## 7. Context-Collapse Keywords (Paradox 3)

```
    Σ.paradox_state.context_depth = d     d ≥ 1     "end" used as identifier
    ─────────────────────────────────────────────────────────────────────────  [Ctx-Collapse]
    ρ, Σ ⊢ let end = 42 ⇒ ρ[end ↦ 42], Σ    (keyword becomes identifier)

    Σ.paradox_state.context_depth = 0     "end" used as identifier
    ───────────────────────────────────────────────────────────────────  [Ctx-Reserved]
    ρ, Σ ⊢ let end = 42 ⇓ ⊥("unexpected keyword 'end'")
```

---

## 8. Scope Leakage (Paradox 4)

```
    is_prime(Σ.run_counter) = true
    ∨ is_palindrome(x) = true
    ∨ is_fibonacci(line_number) = true
    ─────────────────────────────────────────────  [Scope-Leak]
    Variable x declared in inner block leaks to parent scope

    is_prime(Σ.run_counter) = false
    ∧ is_palindrome(x) = false
    ∧ is_fibonacci(line_number) = false
    ─────────────────────────────────────────────  [Scope-Normal]
    Variable x follows standard lexical scoping (does not leak)
```

---

## 9. Standard Expression Evaluation

### 9.1 Literals

```
    ──────────────  [Lit-Int]        ──────────────  [Lit-Float]
    ρ, Σ ⊢ n ⇓ n                   ρ, Σ ⊢ f ⇓ f

    ──────────────  [Lit-String]     ──────────────  [Lit-Bool]
    ρ, Σ ⊢ s ⇓ s                   ρ, Σ ⊢ b ⇓ b
```

### 9.2 Variables

```
    x ∈ dom(ρ) ∪ Σ.paradox_state.scope_leaks
    ──────────────────────────────────────────  [Var]
    ρ, Σ ⊢ x ⇓ ρ(x)

    x ∉ dom(ρ) ∧ x ∉ Σ.paradox_state.scope_leaks
    ───────────────────────────────────────────────  [Var-Undef]
    ρ, Σ ⊢ x ⇓ ⊥("undefined variable: " ++ x)
```

### 9.3 Binary Operations (non-positional)

```
    ρ, Σ ⊢ e₁ ⇓ v₁     ρ, Σ ⊢ e₂ ⇓ v₂     v₁, v₂ numeric
    ─────────────────────────────────────────────────────────  [Arith]
    ρ, Σ ⊢ e₁ ⊕ e₂ ⇓ v₁ ⊕ v₂     for ⊕ ∈ {+, -, *, /, %}

    ρ, Σ ⊢ e₁ ⇓ v₁     ρ, Σ ⊢ e₂ ⇓ v₂
    ──────────────────────────────────────  [Compare]
    ρ, Σ ⊢ e₁ ⊕ e₂ ⇓ v₁ ⊕ v₂     for ⊕ ∈ {==, !=, <, <=, >, >=}

    ρ, Σ ⊢ e₁ ⇓ v₁     truthy(v₁) = false
    ──────────────────────────────────────────  [And-Short]
    ρ, Σ ⊢ e₁ and e₂ ⇓ false

    ρ, Σ ⊢ e₁ ⇓ v₁     truthy(v₁) = true
    ──────────────────────────────────────────  [Or-Short]
    ρ, Σ ⊢ e₁ or e₂ ⇓ true
```

### 9.4 Unary

```
    ρ, Σ ⊢ e ⇓ v     v numeric
    ──────────────────────────  [Neg]
    ρ, Σ ⊢ -e ⇓ -v

    ρ, Σ ⊢ e ⇓ v
    ──────────────────────────  [Not]
    ρ, Σ ⊢ not e ⇓ ¬truthy(v)
```

---

## 10. Statements

### 10.1 Let (with stability tracking)

```
    ρ, Σ ⊢ e ⇓ v
    ρ' = ρ[x ↦ v]
    Σ' = if mutable then apply_penalty(Σ, MutableState) else Σ
    ─────────────────────────────────────────────────────────  [Let]
    ρ, Σ ⊢ let [mutable] x = e ⇒ ρ', Σ'
```

### 10.2 Assignment (stability penalty)

```
    x ∈ dom(ρ)     ρ, Σ ⊢ e ⇓ v
    Σ' = apply_penalty(Σ, MutableState)
    type(ρ(x)) ≠ type(v) ⟹ Σ'' = apply_penalty(Σ', TypeInstability)
    ─────────────────────────────────────────────────────────  [Assign]
    ρ, Σ ⊢ x = e ⇒ ρ[x ↦ v], Σ''
```

### 10.3 Control Flow

```
    ρ, Σ ⊢ cond ⇓ v     truthy(v) = true     ρ, Σ ⊢ then ⇒ ρ', Σ'
    ───────────────────────────────────────────────────────────────────  [If-True]
    ρ, Σ ⊢ if cond { then } [else { els }] ⇒ ρ', Σ'

    ρ, Σ ⊢ cond ⇓ v     truthy(v) = false     ρ, Σ ⊢ els ⇒ ρ', Σ'
    ────────────────────────────────────────────────────────────────────  [If-False]
    ρ, Σ ⊢ if cond { then } else { els } ⇒ ρ', Σ'

    ρ, Σ ⊢ e ⇓ n     n ∈ ℤ, n > 0
    ∀i ∈ 1..n: ρ, Σᵢ ⊢ body ⇒ ρ, Σᵢ₊₁
    ─────────────────────────────────────────────────  [For-Range]
    ρ, Σ ⊢ for i in [1..n] { body } ⇒ ρ, Σₙ₊₁

    ρ, Σ ⊢ cond ⇓ v     truthy(v) = false
    ──────────────────────────────────────────  [While-Done]
    ρ, Σ ⊢ while cond { body } ⇒ ρ, Σ
```

### 10.4 Return

```
    ρ, Σ ⊢ e ⇓ v
    ──────────────────────────────────  [Return]
    ρ, Σ ⊢ return e ⇒ raise Return(v)
```

### 10.5 Gutter Block (Error Injection Zone)

```
    parse(body) = errors     errors recovered
    ────────────────────────────────────────────  [Gutter]
    ρ, Σ ⊢ gutter { body } end ⇒ ρ, Σ
    (parser recovers; errors collected for pedagogical display)
```

---

## 11. Function Calls

```
    ρ, Σ ⊢ f ⇓ Fn(name, [p₁,…,pₙ], body, ρ_clos)
    ∀i. ρ, Σ ⊢ aᵢ ⇓ vᵢ     m = n
    ρ_call = ρ_clos[p₁ ↦ v₁, …, pₙ ↦ vₙ]
    ρ_call, Σ ⊢ body ⇓ v'     (catch Return(v') → v')
    ────────────────────────────────────────────────────  [Call]
    ρ, Σ ⊢ f(a₁, …, aₘ) ⇓ v'
```

---

## 12. Pattern Matching

```
    ρ, Σ ⊢ scrutinee ⇓ v
    ∃i: match(armᵢ.pat, v) = binds
    ρ ∪ binds, Σ ⊢ armᵢ.body ⇓ v'
    ────────────────────────────────────────────  [Decide]
    ρ, Σ ⊢ decide based on scrutinee { arms } ⇓ v'

match(_, v)          = {}               [Wild]
match(x, v)          = {x ↦ v}         [Var]
match(lit, v)        = {} if lit = v    [Lit]
```

---

## 13. Stability Query

```
    ──────────────────────────────────────  [Stability]
    ρ, Σ ⊢ stability() ⇓ Σ.stability
```

---

## 14. Five-Layer Navigation

The interpreter tracks which layer (Grammar, Parser, AST, Semantics, Runtime)
an error originates from. Each error carries a `layer : Layer` tag:

```
Layer ::= Grammar | Parser | AST | Semantics | Runtime

error_with_layer(msg, layer) = Error(msg, layer, line, column)
```

This enables the Five Whys debugging methodology: tracing from Runtime
down through Semantics → AST → Parser → Grammar.

---

## 15. Program Execution

```
    ρ₀ = ∅     register_builtins(ρ₀)
    Σ₀ = ⟨100, load_run_counter(), fresh_paradox_state(), seed⟩
    ∀item: register(item, ρ₀)
    ρ₀, Σ₀ ⊢ main { body } ⇓ v, Σ_final
    save_run_counter(Σ_final.run_counter + 1)
    ──────────────────────────────────────────────  [Program]
    run(file) ⇓ (v, Σ_final.stability)
```

The run counter is persisted to `~/.config/error-lang/run_counter`,
enabling Paradox 4 (scope leakage on primes) and Paradox 5 (temporal corruption).

---

## 16. Invariants

1. **Stability monotonically decreasing:** Penalties only subtract; no operation increases stability.
2. **Type collapse determinism:** Given same seed and context, collapse produces the same type.
3. **Scope leak determinism:** Leakage is a pure function of run_counter, variable name, and line number.
4. **Positional determinism:** Operator semantics are a pure function of source column.
5. **Run counter persistence:** Counter survives across process invocations.
6. **Gutter recovery:** Parser always recovers from gutter block errors; they never crash the program.
