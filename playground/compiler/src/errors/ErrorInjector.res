// SPDX-License-Identifier: PMPL-1.0-or-later
// Error-Lang Playground - Error Injection System
// The core pedagogical mechanism: intentionally inject errors into gutter blocks

open Types

// Seeded random number generator for reproducible error injection
type rng = {
  mutable seed: int,
}

let makeRng = (seed: int): rng => {
  {seed: seed}
}

// Linear congruential generator
let nextInt = (rng: rng): int => {
  // Parameters from Numerical Recipes
  rng.seed = (rng.seed * 1103515245 + 12345) land 0x7FFFFFFF
  rng.seed
}

let nextFloat = (rng: rng): float => {
  Float.fromInt(nextInt(rng)) /. 2147483647.0
}

let nextBool = (rng: rng, probability: float): bool => {
  nextFloat(rng) < probability
}

// Pick a random element from array
let pickRandom = (rng: rng, arr: array<'a>): option<'a> => {
  let len = Array.length(arr)
  if len == 0 {
    None
  } else {
    let idx = nextInt(rng) mod len
    Some(arr[idx])
  }
}

// Error templates for injection
type errorTemplate = {
  code: errorCode,
  description: string,
  applyTo: string => string,  // Transform to inject error
  learningObjective: string,
  recoveryHint: string,
  lesson: int,
}

// Library of injectable errors
let syntaxErrorTemplates: array<errorTemplate> = [
  {
    code: E0002,
    description: "Remove closing quote from string",
    applyTo: s => {
      // Find a string and remove its closing quote
      let re = %re("/\"([^\"]*)\"/g")
      Js.String2.replaceByRe(s, re, "\"$1")
    },
    learningObjective: "Understand string literal termination",
    recoveryHint: "Add the missing closing quote",
    lesson: 2,
  },
  {
    code: E0003,
    description: "Add invalid escape sequence",
    applyTo: s => {
      Js.String2.replace(s, "\\n", "\\q")
    },
    learningObjective: "Learn valid escape sequences",
    recoveryHint: "Use \\n, \\t, \\r, \\\\, or \\\"",
    lesson: 3,
  },
  {
    code: E0005,
    description: "Remove 'end' keyword",
    applyTo: s => {
      Js.String2.replace(s, "end", "")
    },
    learningObjective: "Understand block structure",
    recoveryHint: "Add 'end' to close the block",
    lesson: 5,
  },
  {
    code: E0006,
    description: "Remove closing parenthesis",
    applyTo: s => {
      // Remove last )
      let idx = Js.String2.lastIndexOf(s, ")")
      if idx >= 0 {
        Js.String2.slice(s, ~from=0, ~to_=idx) ++ Js.String2.sliceToEnd(s, ~from=idx + 1)
      } else {
        s
      }
    },
    learningObjective: "Match parentheses correctly",
    recoveryHint: "Add the missing closing parenthesis",
    lesson: 6,
  },
  {
    code: E0007,
    description: "Replace straight quote with smart quote",
    applyTo: s => {
      Js.String2.replace(s, "\"", "\u{201C}")
    },
    learningObjective: "Distinguish Unicode from ASCII",
    recoveryHint: "Use straight quotes (\") not curly quotes",
    lesson: 1,
  },
  {
    code: E0008,
    description: "Add invalid character to identifier",
    applyTo: s => {
      // Add @ to first identifier
      let re = %re("/([a-zA-Z_][a-zA-Z0-9_]*)/")
      Js.String2.replaceByRe(s, re, "@$1")
    },
    learningObjective: "Learn valid identifier characters",
    recoveryHint: "Identifiers can only contain letters, digits, and underscores",
    lesson: 8,
  },
  {
    code: E0010,
    description: "Add problematic whitespace",
    applyTo: s => {
      // Add a non-breaking space
      Js.String2.replace(s, " ", "\u{00A0}")
    },
    learningObjective: "Understand whitespace characters",
    recoveryHint: "Use regular spaces, not special Unicode spaces",
    lesson: 10,
  },
]

// Injector state
type injectorState = {
  rng: rng,
  config: injectionConfig,
  mutable injectedErrors: array<diagnostic>,
  mutable stabilityScore: int,
}

// Create injector
let make = (config: injectionConfig): injectorState => {
  let seed = switch config.seed {
  | Some(s) => s
  | None => Int.fromFloat(Js.Date.now())
  }
  {
    rng: makeRng(seed),
    config,
    injectedErrors: [],
    stabilityScore: 100,
  }
}

// Inject errors into gutter content
let injectErrors = (state: injectorState, gutterContent: string, span: span): (string, array<diagnostic>) => {
  let mutableContent = ref(gutterContent)
  let injected = ref([])
  let errorCount = ref(0)

  // Filter templates by enabled error codes
  let availableTemplates = syntaxErrorTemplates->Array.filter(t => {
    state.config.enabledErrorCodes->Array.some(c => c == t.code)
  })

  // Try to inject errors based on probability
  availableTemplates->Array.forEach(template => {
    if errorCount.contents < state.config.maxErrorsPerGutter {
      if nextBool(state.rng, state.config.errorProbability) {
        // Apply the error transformation
        let newContent = template.applyTo(mutableContent.contents)
        if newContent != mutableContent.contents {
          mutableContent := newContent
          errorCount := errorCount.contents + 1

          // Create diagnostic
          let diag: diagnostic = {
            code: template.code,
            severity: Error,
            message: `[INJECTED] ${template.description}`,
            span,
            learningObjective: template.learningObjective,
            recoveryHint: template.recoveryHint,
            curriculumLesson: Some(template.lesson),
          }
          injected := Array.concat(injected.contents, [diag])

          // Reduce stability score
          state.stabilityScore = state.stabilityScore - 10
          if state.stabilityScore < 0 {
            state.stabilityScore = 0
          }
        }
      }
    }
  })

  state.injectedErrors = Array.concat(state.injectedErrors, injected.contents)
  (mutableContent.contents, injected.contents)
}

// Get current stability score
let getStabilityScore = (state: injectorState): int => {
  state.stabilityScore
}

// Get all injected errors
let getInjectedErrors = (state: injectorState): array<diagnostic> => {
  state.injectedErrors
}

// Describe stability level
let stabilityDescription = (score: int): string => {
  if score >= 90 {
    "Rock solid - code is highly stable"
  } else if score >= 70 {
    "Stable - minor wobbles detected"
  } else if score >= 50 {
    "Wobbly - errors are accumulating"
  } else if score >= 30 {
    "Unstable - output may be unreliable"
  } else if score >= 10 {
    "Critical - program barely running"
  } else {
    "Collapsed - too many errors to continue"
  }
}

// Default configuration
let defaultConfig: injectionConfig = {
  seed: None,
  errorProbability: 0.3,
  maxErrorsPerGutter: 3,
  enabledErrorCodes: [E0002, E0003, E0005, E0006, E0007, E0008, E0010],
}
