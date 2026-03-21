#!/bin/bash
# Test scope leakage across multiple runs

cd ~/Documents/hyperpolymath-repos/error-lang

# Clean state
rm -rf .error-lang

echo "Testing scope leakage across 7 runs..."
echo "Runs 2, 3, 5, 7 should show scope leakage (prime numbers)"
echo ""

for i in {1..7}; do
  echo "=== RUN #$i ==="
  deno run --allow-read --allow-write cli/runtime.js examples/04-scope-leakage.err 2>&1 | \
    grep -A 2 "Run:" | head -3

  # Show if scope leaked
  deno run --allow-read --allow-write cli/runtime.js examples/04-scope-leakage.err 2>&1 | \
    grep "leakage\|properly scoped" | head -1

  echo ""
done
