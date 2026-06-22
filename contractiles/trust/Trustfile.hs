-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Trustfile.hs — Error-Lang trust and verification semantics
--
-- This file describes the trust model for the project: what we verify,
-- what we trust transitively, and how to confirm integrity.
-- It is written in Haskell syntax as a specification document.

module ErrorLang.Trust where

-- | The trust model for Error-Lang sources.
data TrustSource
  = RepoOwner       -- hyperpolymath — unconditional trust
  | SignedCommit    -- GPG-signed commit from a listed key
  | CI              -- Any check from .github/workflows/ passing
  | ExternalDep     -- Third-party code (Deno std, etc.)
  deriving (Show, Eq)

-- | Verification steps, in order of execution.
-- Run these to confirm project integrity before a release.
data VerificationStep = VerificationStep
  { stepName    :: String
  , stepCommand :: String
  , stepReason  :: String
  } deriving (Show)

verificationSteps :: [VerificationStep]
verificationSteps =
  [ VerificationStep
      { stepName    = "licence-consistency"
      , stepCommand = "git ls-files | xargs grep -L 'SPDX-License-Identifier' \
                      \| grep -E '\\.(res|js|zig|a2ml)$' | grep -v node_modules"
      , stepReason  = "All source must carry MPL-2.0 header. Never AGPL."
      }
  , VerificationStep
      { stepName    = "echo-decomposition-invariant"
      , stepCommand = "grep -q 'echoEraseCost = 15.0' compiler/src/VM.res \
                      \&& grep -q 'TyEchoR' compiler/src/TypeChecker.res"
      , stepReason  = "decomposition must be visible: erasure costs stability, \
                      \EchoR does not unify with Echo."
      }
  , VerificationStep
      { stepName    = "grammar-canonical"
      , stepCommand = "test -s spec/grammar.ebnf"
      , stepReason  = "spec/grammar.ebnf is the canonical source of truth for \
                      \Error-Lang syntax. Must never be empty or deleted."
      }
  , VerificationStep
      { stepName    = "spec-type-system"
      , stepCommand = "grep -q 'Echo' spec/type-system.md \
                      \&& grep -q 'Stab-Erase' spec/type-system.md"
      , stepReason  = "The type-system spec must document Echo types (§7) \
                      \and the [Stab-Erase] stability rule."
      }
  , VerificationStep
      { stepName    = "no-banned-runtime-languages"
      , stepCommand = "! grep -rn 'require(\"typescript\")' cli/ 2>/dev/null"
      , stepReason  = "TypeScript, Python, Go, Node.js are banned at runtime \
                      \boundaries. The compiler/*.res reference frontend is \
                      \exempt (legacy, pending AffineScript re-target)."
      }
  , VerificationStep
      { stepName    = "ci-checks-passing"
      , stepCommand = "gh pr view --json statusCheckRollup 2>/dev/null \
                      \| jq '.statusCheckRollup[] | select(.state != \"SUCCESS\")'"
      , stepReason  = "All CI checks except governance/Language/package-anti-pattern \
                      \must pass. That check fails pre-existingly on main (ReScript \
                      \ban; resolves with AffineScript re-target)."
      }
  , VerificationStep
      { stepName    = "machine-readable-not-at-root"
      , stepCommand = "for f in STATE.a2ml META.a2ml AGENTIC.a2ml; do \
                      \  [ ! -e \"$f\" ] || [ -L \"$f\" ] || exit 1; done"
      , stepReason  = "SCM files must live in .machine_readable/. Root copies \
                      \must be symlinks, never plain files."
      }
  ]

-- | The external dependencies we transitively trust.
-- These should be reviewed on major version bumps.
trustedDependencies :: [(String, String, String)]
trustedDependencies =
  [ ( "deno.land/std"
    , "Deno standard library"
    , "Used in cli/ runtime tools; trust the Deno project."
    )
  , ( "hyperpolymath/standards"
    , "Estate-wide governance reusable workflow"
    , "Called by .github/workflows/governance.yml via @main; \
      \trust the estate owner (same as repo owner)."
    )
  , ( "hyperpolymath/echo-types"
    , "Agda mechanization of Echo fiber types"
    , "Source of mechanized truth for Echo<A,B> := Σ(x:A), (f x ≡ y). \
      \Error-Lang's Echo is a single-witness runtime model; Agda is the proof."
    )
  ]

-- | Release gate: all verificationSteps must pass,
-- and no dependency in trustedDependencies has a pending security advisory.
releaseGate :: String
releaseGate =
  "Run all verificationSteps. Check Dependabot for advisories. \
  \Tag only after `just validate-rsr` passes."
