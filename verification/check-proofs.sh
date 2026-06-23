#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Machine-check the error-lang formal core (src/abi/*.idr) with Idris2.
#
# Requires idris2 >= 0.8.0 on PATH. Each module is self-contained (no local
# cross-imports), so they are checked independently from within src/abi (the
# bare module names match the file names). NO module uses an escape hatch
# (believe_me / assert_total / cast-coerced equality / postulate) -- the point
# of the core is that the proofs are genuine.
set -euo pipefail

ABI_DIR="$(cd "$(dirname "$0")/../src/abi" && pwd)"

if ! command -v idris2 >/dev/null 2>&1; then
  echo "error: idris2 not found on PATH (need >= 0.8.0)." >&2
  echo "build it from source via Chez Scheme (see PROOF-NEEDS.md), then re-run." >&2
  exit 127
fi

echo "idris2: $(idris2 --version)"
cd "$ABI_DIR"
status=0
for m in Stability Positional Paradox Foreign; do
  printf 'checking %-12s ... ' "$m"
  if idris2 --check "$m.idr" >/dev/null 2>&1; then
    echo ok
  else
    echo FAIL
    idris2 --check "$m.idr" || true
    status=1
  fi
done

if [ "$status" -eq 0 ]; then
  echo "all proofs check."
else
  echo "one or more proofs failed to check." >&2
fi
exit "$status"
