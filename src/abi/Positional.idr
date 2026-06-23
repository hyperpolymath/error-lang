-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Positional-operator determinism (error-lang formal core, property 2 of 3).
|||
||| Mirrors the Zig FFI `error_lang_positional_operator`
||| (`ffi/zig/src/main.zig`, lines 222-272): for `+` the behaviour is
||| `addition` when the column is even and `concatenation` when odd; for `*`
||| it is `multiplication` when the column is a multiple of 3 and
||| `exponentiation` otherwise.
|||
||| THEOREM.  Operator behaviour is a deterministic (pure, total) function of
||| the operator and the column: `behavior op col = behavior op col`.
|||
||| This is `Refl` -- *because* the model is a pure total function. That is
||| exactly the point: the previous `Foreign.idr :: positionalDeterministic`
||| stated the same property over an `IO Bits8` FFI action and could only
||| "close" it with `cast Refl`, coercing `Refl : x = x` onto two distinct IO
||| results -- a non-proof. Modelling the operation as the pure function it
||| actually is makes determinism genuine.
|||
||| Self-contained; no escape hatches. Machine-check is a CI obligation.
|||
||| NOTE (conformance). `compiler/src/Stability.res` uses a *different* rule
||| (`(line*31+column) mod 4`, four-way). The Zig FFI and the ReScript path
||| therefore disagree; reconciling the two implementations is an open
||| obligation recorded in PROOF-NEEDS.md.
module Positional

%default total

||| Operators that carry positional behaviour.
public export
data Op = Plus | Star | OtherOp

||| Resolved operator behaviours (mirrors Zig `OperatorBehavior`).
public export
data Behavior
  = Addition
  | Concatenation
  | Multiplication
  | Exponentiation

||| Column parity (total, structural).
public export
isEven : Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S k)) = isEven k

||| Divisibility by three (total, structural).
public export
multipleOfThree : Nat -> Bool
multipleOfThree Z             = True
multipleOfThree (S Z)         = False
multipleOfThree (S (S Z))     = False
multipleOfThree (S (S (S k))) = multipleOfThree k

||| The positional behaviour function (pure model of the Zig FFI).
public export
behavior : Op -> Nat -> Behavior
behavior Plus    col = if isEven col then Addition else Concatenation
behavior Star    col = if multipleOfThree col then Multiplication else Exponentiation
behavior OtherOp _   = Addition

||| THEOREM: behaviour is deterministic -- a genuine `Refl` over a pure
||| total function (contrast the original IO-based `cast Refl`).
public export
positionalDeterministic : (op : Op) -> (col : Nat) -> behavior op col = behavior op col
positionalDeterministic _ _ = Refl

-- ───────────────────────────────────────────────────────────────────────
-- Sanity evaluations (match the Zig integration tests + README example)
-- ───────────────────────────────────────────────────────────────────────

||| Column 12 (even): `+` is addition.  (Zig test "positional semantics".)
exEvenAddition : behavior Plus 12 = Addition
exEvenAddition = Refl

||| Column 13 (odd): `+` is concatenation.
exOddConcatenation : behavior Plus 13 = Concatenation
exOddConcatenation = Refl

||| Column 9 (multiple of 3): `*` is multiplication.
exStarMultiplication : behavior Star 9 = Multiplication
exStarMultiplication = Refl

||| Column 10 (not a multiple of 3): `*` is exponentiation.
exStarExponentiation : behavior Star 10 = Exponentiation
exStarExponentiation = Refl
