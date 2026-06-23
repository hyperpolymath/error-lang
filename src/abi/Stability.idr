-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Stability-score bound (error-lang formal core, property 1 of 3).
|||
||| Mirrors the stability calculation in `compiler/src/Types.res`
||| (`stabilityImpact` / `calculateStability`, lines 258-274).
|||
||| THEOREM.  The stability score is always within [0, 100].
|||
||| The ReScript `calculateStability` computes `Int.max(0, 100 + penalties)`
||| where every penalty is non-positive — i.e. `max(0, 100 - totalPenalty)`.
||| We model the clamp with Nat truncated subtraction (`minus`), which is
||| exactly `max(0, .)`: `minus 100 p` is 0 once `p >= 100`. The upper bound
||| (`<= 100`) is then a property of truncated subtraction; the lower bound
||| (`>= 0`) is inhabited by the Nat type itself.
|||
||| This module is self-contained (only `Data.Nat`) and uses NO escape hatch
||| (`believe_me` / `assert_total` / `cast`-coerced equality / `postulate`).
||| It replaces the previous `Foreign.idr :: stabilityBounded`, which faked
||| the bound with `cast ()` over an `IO` action.
|||
||| Status: written to typecheck under Idris2 (>= 0.7.0). Machine-check is a
||| CI obligation (no idris2 in the current dev image). See PROOF-NEEDS.md.
module Stability

import Data.Nat

%default total

-- ───────────────────────────────────────────────────────────────────────
-- Self-contained Nat <= lemmas (no reliance on stdlib lemma names)
-- ───────────────────────────────────────────────────────────────────────

||| Reflexivity of <=.
lteRefl' : (n : Nat) -> LTE n n
lteRefl' Z     = LTEZero
lteRefl' (S k) = LTESucc (lteRefl' k)

||| Weakening on the right: m <= n  =>  m <= S n.
lteSuccR : LTE m n -> LTE m (S n)
lteSuccR LTEZero     = LTEZero
lteSuccR (LTESucc p) = LTESucc (lteSuccR p)

||| Truncated subtraction never exceeds the minuend: (n - m) <= n.
subLTE : (n, m : Nat) -> LTE (minus n m) n
subLTE Z     _     = LTEZero
subLTE (S k) Z     = lteRefl' (S k)
subLTE (S k) (S j) = lteSuccR (subLTE k j)

-- ───────────────────────────────────────────────────────────────────────
-- Faithful model of compiler/src/Types.res stability factors
-- ───────────────────────────────────────────────────────────────────────

||| A consequence factor and its magnitude inputs (mirrors `stabilityFactor`).
public export
data Factor
  = MutableState        Nat Nat  -- mutations, readers
  | TypeInstability     Nat      -- reassignments
  | NullPropagation     Nat      -- depth
  | GlobalState         Nat Nat  -- mutations, dependencies
  | UnhandledError      Nat      -- failure paths
  | AlgorithmComplexity Nat      -- amplified time units
  | MemoryLeak          Nat      -- kilobytes
  | RaceCondition       Nat      -- conflicts

||| Penalty magnitude of a factor (mirrors `stabilityImpact`, expressed as a
||| non-negative cost that is subtracted from the base of 100).
public export
factorCost : Factor -> Nat
factorCost (MutableState m r)      = 10 * m + 5 * r
factorCost (TypeInstability r)     = 15 * r
factorCost (NullPropagation d)     = 20 * d
factorCost (GlobalState m d)       = 30 * m + 5 * d
factorCost (UnhandledError p)      = 25 * p
factorCost (AlgorithmComplexity t) = t
factorCost (MemoryLeak kb)         = 10 * kb
factorCost (RaceCondition c)       = 40 * c

||| Total penalty across all active factors.
public export
totalCost : List Factor -> Nat
totalCost []        = 0
totalCost (f :: fs) = factorCost f + totalCost fs

||| Stability score = base 100 minus total penalty, clamped at 0.
||| (Nat `minus` is truncated, modelling `Int.max(0, 100 + penalties)`.)
public export
stabilityScore : List Factor -> Nat
stabilityScore fs = minus 100 (totalCost fs)

-- ───────────────────────────────────────────────────────────────────────
-- THEOREM: 0 <= stabilityScore fs <= 100
-- ───────────────────────────────────────────────────────────────────────

||| Upper bound: the score never exceeds 100.
public export
stabilityUpperBound : (fs : List Factor) -> LTE (stabilityScore fs) 100
stabilityUpperBound fs = subLTE 100 (totalCost fs)

||| Lower bound: the score is never negative (inhabited by the Nat type).
public export
stabilityLowerBound : (fs : List Factor) -> LTE 0 (stabilityScore fs)
stabilityLowerBound _ = LTEZero

-- ───────────────────────────────────────────────────────────────────────
-- Sanity evaluations (closed terms; reduce by computation)
-- ───────────────────────────────────────────────────────────────────────

||| No factors: full stability.
sanityFull : stabilityScore [] = 100
sanityFull = Refl

||| One mutation with two readers: 100 - (10*1 + 5*2) = 80.
sanityOneMutation : stabilityScore [MutableState 1 2] = 80
sanityOneMutation = Refl

||| Penalties exceeding 100 clamp to 0 (never negative): 40*3 = 120 -> 0.
sanityClamp : stabilityScore [RaceCondition 3] = 0
sanityClamp = Refl
