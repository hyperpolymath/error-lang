# SPDX-License-Identifier: AGPL-3.0-or-later
# justfile - Error-Lang build commands

set shell := ["bash", "-uc"]

# Default recipe
default: help

# Show help
help:
    @echo "Error-Lang Build Commands"
    @echo "========================="
    @echo ""
    @echo "Usage: just <recipe>"
    @echo ""
    @echo "Recipes:"
    @echo "  run <file>    Run an Error-Lang program"
    @echo "  doctor        Check environment"
    @echo "  explain       Explain an error code"
    @echo "  test          Run tests"
    @echo "  lint          Lint code"
    @echo "  fmt           Format code"
    @echo "  build         Build ReScript compiler"
    @echo "  clean         Clean build artifacts"

# Run an Error-Lang program
run file:
    deno run --allow-read --allow-write cli/main.js run {{file}}

# Check environment
doctor:
    deno run cli/main.js doctor

# Explain an error code
explain code="--all":
    deno run cli/main.js explain {{code}}

# Run tests
test:
    deno test --allow-read --allow-write

# Lint code
lint:
    deno lint
    @if command -v rescript &> /dev/null; then \
        cd compiler && npx rescript build; \
    fi

# Format code
fmt:
    deno fmt

# Build ReScript compiler
build:
    @if [ -d "compiler" ] && [ -f "compiler/rescript.json" ]; then \
        cd compiler && npx rescript build; \
    else \
        echo "ReScript compiler not yet configured"; \
    fi

# Clean build artifacts
clean:
    rm -rf compiler/lib
    rm -f compiler/src/*.res.js
    rm -rf .error-lang

# Run all examples
examples:
    @for f in examples/*.err; do \
        echo "=== Running $$f ==="; \
        just run "$$f"; \
        echo ""; \
    done

# Initialize state for a fresh start
init:
    rm -rf .error-lang
    @echo "State cleared. Next run will be Run #1."
