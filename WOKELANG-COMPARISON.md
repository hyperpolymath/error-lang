<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# Error-Lang vs WokeLang Feature Comparison

## Task Summary

**User Request:** Apply the same 4 features implemented for WokeLang to Error-Lang.

**The 4 WokeLang Features:**
1. Record field access with dot notation
2. Full stdlib integration with interpreter
3. Worker message passing
4. Enhanced error messages with hints

## Error-Lang Current State

**Project Status:** 45% complete (Alpha - Foundation Complete)
**Language:** Pedagogical language with intentional fragility and paradoxes
**Tech Stack:**
- Compiler: ReScript
- Runtime: Deno (JavaScript)
- Verification: Idris2 (planned)

### What Error-Lang Has

**Core Language Features:**
- ✅ Lexer, Parser, AST (ReScript compiler)
- ✅ Runtime interpreter (Deno/JS)
- ✅ Stability tracking system (computational haptics)
- ✅ 7/10 paradoxes implemented
- ✅ Five Whys root cause analysis
- ✅ Layer navigation (Grammar → AST → Semantics → Runtime)
- ✅ Visual feedback system (animated stability bar)

**AST Support (from Types.res):**
- ✅ `Member(expr, string, location)` - Field access defined
- ✅ `StructDecl` - Struct declarations defined
- ✅ `Struct` keyword exists
- ✅ `Dot` operator exists
- ✅ Diagnostics with `hint: option<string>` field

## Feature-by-Feature Analysis

### Feature 1: Record Field Access ✅ (Partially)

**Status:** AST defined, need to verify runtime implementation

**What's defined in AST:**
- Types.res line 114: `Member(expr, string, location)` - field access
- Types.res line 171: `StructDecl` - struct declarations
- Types.res line 26: `Struct` keyword
- Types.res line 85: `Dot` token

**Need to check:**
- Is `Member` expression evaluated in runtime.js?
- Can you create struct instances?
- Can you access fields with dot notation?

**Action required:** Test with example program and implement if missing

---

### Feature 2: Stdlib Integration ❓

**Status:** Need to investigate

Error-Lang appears to have built-in functions (print, println, stability()) but no formal stdlib system like WokeLang.

**Evidence:**
- Examples use `println()`, `stability()`
- No stdlib directory found
- No `Std.*` module calls in examples

**Questions:**
1. Are there builtin functions beyond print/println?
2. Is there a planned stdlib?
3. What functions would make sense for a pedagogical language?

**Recommendation:** Error-Lang is a pedagogical language, not general-purpose. Stdlib should be **minimal and educational**:
- Stability tracking functions (already has)
- Diagnostic helpers
- Basic I/O (print/println)
- No need for 96 functions like WokeLang

**Action required:** Document existing builtins, add any missing core functions

---

### Feature 3: Worker Concurrency ⚠️

**Status:** Not applicable (pedagogical language)

**Reasoning:**
Error-Lang is designed to teach systems thinking through paradoxes and instability, not general-purpose concurrent programming.

**Language design conflicts:**
- Adding workers would introduce concurrency complexity
- Paradoxes are about sequential execution consequences
- Stability tracking assumes deterministic execution
- Educational focus is on cause-and-effect, not parallelism

**Recommendation:** **Do not add workers** - incompatible with pedagogical mission

---

### Feature 4: Enhanced Error Messages with Hints ✅ (Partially)

**Status:** Infrastructure exists, needs implementation

**What's defined:**
- Types.res line 196-202: `diagnostic` type with `hint: option<string>`
- Types.res line 184-194: Error codes E0001-E0010
- Types.res line 282-286: `formatDiagnostic` function

**What's missing:**
- Hints not populated (always `None` in parser)
- No suggestion engine
- No "did you mean...?" for typos
- No Levenshtein distance matching

**Comparison to Phronesis:**
- Phronesis: 967+ lines of comprehensive diagnostics
- Error-Lang: Basic diagnostic structure, minimal implementation

**Action required:**
1. Add hint population to parser error handling
2. Create suggestion engine for common mistakes
3. Add educational hints for paradox discovery
4. Context-aware error messages

**Educational hints examples:**
```
Error: Variable 'x' changed type from Int to String
Hint: This is Type Quantum Superposition! Variables in Error-Lang can exist
      in multiple types until observed. This teaches how type systems work.
```

## Summary

| Feature | WokeLang | Error-Lang Status | Work Needed |
|---------|----------|------------------|-------------|
| **1. Record field access** | ✅ Complete | ⚠️ AST defined, runtime unclear | Verify + possibly implement |
| **2. Stdlib integration** | ✅ 96 functions | ⚠️ Minimal builtins | Add educational builtins |
| **3. Worker concurrency** | ⚠️ Partial | ❌ Not applicable | **None - don't add** |
| **4. Enhanced error messages** | ⚠️ Design | ⚠️ Infrastructure only | Implement hints + suggestions |

## Recommended Work Order

### Priority 1: Error Messages with Educational Hints
**Why:** Core to pedagogical mission
**Work:**
1. Add hint population in Parser.res error handling
2. Create paradox-specific error messages
3. Add "what you're discovering" explanations
4. Implement suggestion engine for typos

### Priority 2: Verify/Complete Record Field Access
**Why:** Basic language feature
**Work:**
1. Test struct creation and field access
2. Implement runtime evaluation if missing
3. Add examples showing struct usage
4. Document struct syntax

### Priority 3: Educational Stdlib
**Why:** Enhance teaching capabilities
**Work:**
1. Document existing builtins (println, stability, etc.)
2. Add diagnostic helpers (getDriftMagnitude, getCascadePath)
3. Add educational introspection functions
4. Keep minimal - this is not a production language

### Priority 4: Do NOT Add Workers
**Why:** Incompatible with educational focus
**Reasoning:** Error-Lang teaches consequence propagation in sequential code, not concurrency

## Key Differences from WokeLang

**WokeLang:**
- General-purpose programming language
- Needs full stdlib (96 functions)
- Workers make sense for concurrent programming
- Type inference engine with polymorphism

**Error-Lang:**
- Pedagogical language with intentional fragility
- Minimal stdlib (10-15 educational functions)
- Workers would obscure learning goals
- Stability tracking and paradox detection

## Estimated Work

**Total implementation time:** Much less than WokeLang session

**Breakdown:**
- Enhanced error hints: ~2-3 hours (main work)
- Verify/complete field access: ~1 hour
- Educational stdlib: ~1-2 hours
- Documentation: ~1 hour

**Total:** ~5-7 hours vs. WokeLang's ~8-10 hours

**Simpler because:**
- No complex type inference system to fix
- No thread-safety concerns
- Smaller scope (pedagogical vs general-purpose)
- AST already has field access defined
- Diagnostic infrastructure exists

## Next Steps

1. **Investigate runtime.js** - Check if `Member` expression is evaluated
2. **Test struct field access** - Create example program
3. **Implement error hints** - Add educational context to diagnostics
4. **Document builtins** - What functions exist and what they do
5. **Create educational stdlib** - Minimal set of teaching-focused functions

## Files to Create

- `ERROR-LANG-ANALYSIS.md` - Detailed implementation plan
- `examples/10-struct-fields.err` - Test struct field access
- `docs/Builtins.adoc` - Document existing functions
- Updated `Parser.res` - Add hint population
- Updated `runtime.js` - Ensure Member evaluation works
