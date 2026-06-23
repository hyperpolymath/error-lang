#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-affinescript.sh — typecheck every ported `.affine` compiler source with
# the AffineScript compiler. Companion to verification/check-proofs.sh.
# Requires `affinescript` on PATH (see scripts/install-affinescript-toolchain.sh).
set -euo pipefail

# Check from compiler/src so sibling-module imports (`use Types::{...}`) resolve
# via the loader's current-dir search.
cd "$(dirname "$0")/../compiler/src"

if ! command -v affinescript >/dev/null 2>&1; then
  echo "affinescript not found — run scripts/install-affinescript-toolchain.sh" >&2
  exit 127
fi

shopt -s nullglob
sources=(*.affine)
if [ ${#sources[@]} -eq 0 ]; then
  echo "no .affine sources yet (ReScript->AffineScript migration in progress)."
  exit 0
fi

fail=0
for f in "${sources[@]}"; do
  printf 'checking %-28s ... ' "$f"
  if affinescript check "$f" >/tmp/as_check.out 2>&1; then
    echo ok
  else
    echo FAIL
    cat /tmp/as_check.out
    fail=1
  fi
done

if [ "$fail" -eq 0 ]; then
  echo "all .affine sources check."
else
  echo "affinescript check failures." >&2
  exit 1
fi
