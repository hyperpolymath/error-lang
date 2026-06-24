<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# PROOF-NEEDS.md

> Engineering ledger for the error-lang **formal core**. This is the honest
> substrate beneath the language's deliberately tongue-in-cheek "100%
> production-ready, formally verified" self-presentation: it records what is
> *actually* machine-checked, what is not, and how to reproduce the checks.
> The language may dissemble about itself on purpose — this file does not.

## Formal core (`src/abi/`)

Three properties of the computational-haptics engine are proved in Idris2 and
**machine-checked under Idris 2, version 0.8.0**, with **no escape hatches**
(no `believe_me`, `assert_total`, `cast`-coerced equality, or `postulate`):

| Property | Module | Status |
|---|---|---|
| Stability score ∈ [0, 100] | `src/abi/Stability.idr` | ✅ proved (`stabilityUpperBound`, `stabilityLowerBound`) |
| Positional-operator determinism | `src/abi/Positional.idr` | ✅ proved (`positionalDeterministic`) + sanity evaluations |
| Paradox-factor monotonicity | `src/abi/Paradox.idr` | ⚠️ partial — two factors proved, blanket claim retracted (below) |

`src/abi/Foreign.idr` is an honest, self-contained ABI **binding-declaration**
layer (it asserts no theorems). All four modules are listed in
`src/abi/error-lang-abi.ipkg`.

### Reproduce

```sh
# idris2 is not in apt here, and the ziglang/deno mirrors are blocked by the
# environment network policy, so build the proof checker from source via Chez:
sudo apt-get install -y chezscheme libgmp-dev make gcc
git clone https://github.com/idris-lang/Idris2 && cd Idris2
make bootstrap SCHEME=chezscheme && make install
export PATH="$HOME/.idris2/bin:$PATH"

# then, from the error-lang repo root:
./verification/check-proofs.sh
# or:  cd src/abi && idris2 --typecheck error-lang-abi.ipkg
```

### The monotonicity retraction (an honest finding)

The previously-advertised property *"paradox detection is monotonic with
complexity"* is **false of the implementation** — and attempting to prove it
honestly is what surfaced that. `error_lang_detect_paradoxes`
(`ffi/zig/src/main.zig`) gates `scope_leakage` on `isPrime(line_count)`, which
is not monotone: line **7** is prime → active, line **8** is composite →
inactive, even though 8 > 7.

`src/abi/Paradox.idr` therefore proves the part that *is* true — the two
threshold-gated factors are monotone in their driving metric
(`superpositionMonotone` for `var_count > 10`; `temporalMonotone` for
`depth > 5`) — and retracts the blanket claim, recording the scope-leakage
obstruction explicitly. Non-monotone scope leakage is intentional; it is the
pedagogical point of the paradox. The difference now is that the proof says so
out loud, instead of hiding it behind `cast Refl`.

## What was removed (2026-06-23)

`src/abi/Foreign.idr` previously carried three "Safety Proofs" —
`stabilityBounded`, `positionalDeterministic`, `paradoxMonotonic` — that were
**not proofs**. Each manufactured its evidence with `cast ()` / `cast Refl`
over an `IO` action (e.g. calling an FFI function twice and coercing
`Refl : x = x` onto the two distinct results, with a comment that it "should
hold in practice"). An earlier note in this file claimed these files had been
removed; in fact `Foreign.idr` was still present and still exported the fakes.

They are now deleted and replaced by the genuine, machine-checked modules above.

## Open obligations

1. **CI gate.** Add an Idris2 `--typecheck error-lang-abi.ipkg` job so the core
   is checked on every push. (The dev image has no idris2 by default; it was
   built from source for this change.)
2. **Implementation conformance.** The proofs are stated over abstract models
   that mirror `ffi/zig/src/main.zig` and `compiler/src/Types.res`. Two of those
   implementations **disagree**: positional behaviour is `column % 2` (two-way)
   in the Zig FFI but `(line*31 + column) mod 4` (four-way) in `Stability.res`.
   Reconcile them, then bind the proofs to the chosen implementation by
   extraction or conformance tests rather than parallel models.
3. **Zig weighted-average path.** `error_lang_calculate_stability` is a convex
   combination (weights sum to 1) of per-factor scores in [0,100]; its [0,100]
   bound holds for a *different* reason than the `Stability.res` clamp proved
   here. Prove that path too.
4. **Programs not executed in this environment.** Under the current network
   policy the Deno runtime's JSR std deps (`jsr.io`) and Zig 0.13.0
   (`ziglang.org`) are unreachable, and the ReScript compiler does not currently
   build (`return` is not valid ReScript — `VM.res:407`; `dict<string, int>`
   applies the one-argument `dict` constructor to two arguments —
   `Types.res:233`). These were **not** run or fixed as part of this change and
   are tracked as separate work — they are not claimed to pass.
