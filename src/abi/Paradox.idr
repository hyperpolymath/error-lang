-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Paradox-detection monotonicity (error-lang formal core, property 3 of 3).
|||
||| Mirrors the Zig FFI `error_lang_detect_paradoxes`
||| (`ffi/zig/src/main.zig`, lines 280-315), which sets:
|||   * type_superposition  when var_count > 10   (threshold; monotone)
|||   * scope_leakage       when isPrime(line)    (prime-gated; NOT monotone)
|||   * temporal_corruption when depth > 5         (threshold; monotone)
|||
||| HONEST FINDING.  The previously-claimed blanket theorem "paradox detection
||| is monotonic with complexity" (README, WHITEPAPER §7, COMPLETION report)
||| is FALSE of the implementation: scope_leakage is gated on the PRIMALITY of
||| the line number, which is not monotone (line 7 is prime -> active; line 8
||| is composite -> inactive, although 8 > 7). The original
||| `Foreign.idr :: paradoxMonotonic` hid this with `cast Refl`; attempting the
||| proof honestly surfaces that the blanket claim cannot hold.
|||
||| What IS true, and is proved here: the two THRESHOLD-gated factors are
||| monotone in their driving metric. The blanket claim is therefore retracted
||| in favour of these two component lemmas (see PROOF-NEEDS.md). Non-monotone
||| scope leakage is intentional -- it is the pedagogical point of the paradox.
|||
||| Self-contained; no escape hatches. Machine-check is a CI obligation.
module Paradox

import Data.Nat

%default total

||| Transitivity of <= (self-contained; the standard definition).
lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero     _           = LTEZero
lteTrans (LTESucc p) (LTESucc q) = LTESucc (lteTrans p q)

-- ───────────────────────────────────────────────────────────────────────
-- Threshold-gated factors (monotone)
-- ───────────────────────────────────────────────────────────────────────

||| type_superposition fires when var_count exceeds 10 (11 <= var_count).
public export
SuperpositionActive : (varCount : Nat) -> Type
SuperpositionActive varCount = LTE 11 varCount

||| temporal_corruption fires when depth exceeds 5 (6 <= depth).
public export
TemporalActive : (depth : Nat) -> Type
TemporalActive depth = LTE 6 depth

||| THEOREM: type_superposition is monotone in var_count -- growing the
||| variable count never deactivates it.
public export
superpositionMonotone : (v1, v2 : Nat) -> LTE v1 v2 ->
                        SuperpositionActive v1 -> SuperpositionActive v2
superpositionMonotone _ _ le active = lteTrans active le

||| THEOREM: temporal_corruption is monotone in depth.
public export
temporalMonotone : (d1, d2 : Nat) -> LTE d1 d2 ->
                   TemporalActive d1 -> TemporalActive d2
temporalMonotone _ _ le active = lteTrans active le

-- ───────────────────────────────────────────────────────────────────────
-- Scope leakage is NOT monotone (the retraction, made precise)
-- ───────────────────────────────────────────────────────────────────────

||| scope_leakage fires on prime line numbers. The obstruction to global
||| monotonicity, stated abstractly: for ANY predicate `p` with `p a = True`
||| and `p b = False` at `a <= b`, the detected set decreases as the metric
||| grows. Primality is such a `p` (witness a = 7, b = 8). Hence no global
||| monotonicity theorem exists -- and that is by design.
public export
scopeLeakObstruction : (p : Nat -> Bool) -> (a, b : Nat) -> LTE a b ->
                       p a = True -> p b = False ->
                       (p a = True, p b = False)
scopeLeakObstruction _ _ _ _ activeAtA inactiveAtB = (activeAtA, inactiveAtB)
