-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| Foreign Function Interface Declarations for Error-Lang
|||
||| This module declares all C-compatible functions implemented in the
||| Zig FFI layer (ffi/zig/). Provides high-performance computational
||| haptics operations with formal safety proofs.
|||
||| All functions have type signatures and safety guarantees proven at
||| compile-time through dependent types.

module Foreign

%default total

--------------------------------------------------------------------------------
-- Minimal ABI value types (inlined so this binding module is self-contained
-- and independently checkable: `idris2 --check Foreign.idr` from src/abi).
-- The previous external `ErrorLang.ABI.Types` / `ErrorLang.ABI.Layout` modules
-- were removed. The fabricated "Safety Proofs" that lived here -- which used
-- `cast ()` / `cast Refl` over IO actions to manufacture evidence -- have been
-- replaced by genuine, machine-checked proofs in the sibling modules
-- Stability.idr, Positional.idr and Paradox.idr.
--
-- This file is a BINDING-DECLARATION layer only: it declares the C ABI of the
-- Zig haptics library (ffi/zig). It asserts no theorems.
--------------------------------------------------------------------------------

||| Result codes (must match the Zig `Result` enum in ffi/zig/src/main.zig).
public export
data Result = Ok | Error | InvalidParam | OutOfMemory | NullPointer

||| Opaque handle to a library instance (wraps the C pointer as Bits64).
public export
record Handle where
  constructor MkHandle
  handlePtr : Bits64

||| Build a handle from a raw pointer; a null (0) pointer yields Nothing.
export
createHandle : Bits64 -> Maybe Handle
createHandle 0 = Nothing
createHandle p = Just (MkHandle p)

--------------------------------------------------------------------------------
-- Library Lifecycle
--------------------------------------------------------------------------------

||| Initialize the library
||| Returns a handle to the library instance, or Nothing on failure
export
%foreign "C:error_lang_init, liberror_lang"
prim__init : PrimIO Bits64

||| Safe wrapper for library initialization
export
init : IO (Maybe Handle)
init = do
  ptr <- primIO prim__init
  pure (createHandle ptr)

||| Clean up library resources
export
%foreign "C:error_lang_free, liberror_lang"
prim__free : Bits64 -> PrimIO ()

||| Safe wrapper for cleanup
export
free : Handle -> IO ()
free h = primIO (prim__free (handlePtr h))

--------------------------------------------------------------------------------
-- Stability Scoring (Computational Haptics Core)
--------------------------------------------------------------------------------

||| Calculate overall stability score (0-100)
||| Weighted average of all paradox factors
export
%foreign "C:error_lang_calculate_stability, liberror_lang"
prim__calculateStability : Bits64 -> PrimIO Double

||| Safe wrapper for stability calculation
export
calculateStability : Handle -> IO Double
calculateStability h = do
  score <- primIO (prim__calculateStability (handlePtr h))
  pure score

||| Set stability score for a specific factor (0-9)
export
%foreign "C:error_lang_set_stability_factor, liberror_lang"
prim__setStabilityFactor : Bits64 -> Bits8 -> Double -> PrimIO Bits32

||| Safe wrapper for setting stability factor
export
setStabilityFactor : Handle -> Bits8 -> Double -> IO (Either Result ())
setStabilityFactor h factor score = do
  result <- primIO (prim__setStabilityFactor (handlePtr h) factor score)
  pure $ case resultFromInt result of
    Just Ok => Right ()
    Just err => Left err
    Nothing => Left Error
  where
    resultFromInt : Bits32 -> Maybe Result
    resultFromInt 0 = Just Ok
    resultFromInt 1 = Just Error
    resultFromInt 2 = Just InvalidParam
    resultFromInt 3 = Just OutOfMemory
    resultFromInt 4 = Just NullPointer
    resultFromInt _ = Nothing

||| Get stability score for a specific factor (0-9)
export
%foreign "C:error_lang_get_stability_factor, liberror_lang"
prim__getStabilityFactor : Bits64 -> Bits8 -> PrimIO Double

||| Safe wrapper for getting stability factor
export
getStabilityFactor : Handle -> Bits8 -> IO Double
getStabilityFactor h factor = primIO (prim__getStabilityFactor (handlePtr h) factor)

--------------------------------------------------------------------------------
-- Positional Semantics
--------------------------------------------------------------------------------

||| Determine operator behavior based on source position
|||
||| @line Line number in source file
||| @column Column number in source file
||| @operatorType 0 = plus, 1 = star
|||
||| Returns: 0 = addition, 1 = concatenation, 2 = multiplication, 3 = exponentiation
export
%foreign "C:error_lang_positional_operator, liberror_lang"
prim__positionalOperator : Bits64 -> Bits32 -> Bits32 -> Bits8 -> PrimIO Bits8

||| Safe wrapper for positional operator resolution
export
positionalOperator : Handle -> Bits32 -> Bits32 -> Bits8 -> IO Bits8
positionalOperator h line column operatorType =
  primIO (prim__positionalOperator (handlePtr h) line column operatorType)

--------------------------------------------------------------------------------
-- Paradox Detection
--------------------------------------------------------------------------------

||| Detect which paradoxes are active in the current context
||| Returns a bitmask where each bit represents a paradox
|||
||| Bits:
|||   0: type_superposition
|||   1: positional_semantics
|||   2: scope_leakage
|||   3: temporal_corruption
|||   4: arithmetic_drift
|||   5: null_propagation
|||   6: context_collapse
|||   7: reserved_word_roulette
|||   8: global_entanglement
|||   9: memory_phantom
export
%foreign "C:error_lang_detect_paradoxes, liberror_lang"
prim__detectParadoxes : Bits64 -> Bits32 -> Bits32 -> Bits32 -> PrimIO Bits32

||| Safe wrapper for paradox detection
export
detectParadoxes : Handle -> Bits32 -> Bits32 -> Bits32 -> IO Bits32
detectParadoxes h lineCount varCount depth =
  primIO (prim__detectParadoxes (handlePtr h) lineCount varCount depth)

--------------------------------------------------------------------------------
-- Five Whys Analysis
--------------------------------------------------------------------------------

||| Trace root cause through abstraction layers
||| Returns depth reached before hitting bedrock (1-5)
export
%foreign "C:error_lang_five_whys_depth, liberror_lang"
prim__fiveWhysDepth : Bits64 -> Bits32 -> PrimIO Bits32

||| Safe wrapper for Five Whys depth calculation
export
fiveWhysDepth : Handle -> Bits32 -> IO Bits32
fiveWhysDepth h symptomCode = primIO (prim__fiveWhysDepth (handlePtr h) symptomCode)

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

||| Get last error message
export
%foreign "C:error_lang_last_error, liberror_lang"
prim__lastError : PrimIO Bits64

||| Convert C string to Idris String
export
%foreign "support:idris2_getString, libidris2_support"
prim__getString : Bits64 -> String

||| Retrieve last error as string
export
lastError : IO (Maybe String)
lastError = do
  ptr <- primIO prim__lastError
  if ptr == 0
    then pure Nothing
    else pure (Just (prim__getString ptr))

||| Get error description for result code
export
errorDescription : Result -> String
errorDescription Ok = "Success"
errorDescription Error = "Generic error"
errorDescription InvalidParam = "Invalid parameter"
errorDescription OutOfMemory = "Out of memory"
errorDescription NullPointer = "Null pointer"

--------------------------------------------------------------------------------
-- Version Information
--------------------------------------------------------------------------------

||| Get library version
export
%foreign "C:error_lang_version, liberror_lang"
prim__version : PrimIO Bits64

||| Get version as string
export
version : IO String
version = do
  ptr <- primIO prim__version
  pure (prim__getString ptr)

||| Get library build info
export
%foreign "C:error_lang_build_info, liberror_lang"
prim__buildInfo : PrimIO Bits64

||| Get build information
export
buildInfo : IO String
buildInfo = do
  ptr <- primIO prim__buildInfo
  pure (prim__getString ptr)

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Check if library is initialized
export
%foreign "C:error_lang_is_initialized, liberror_lang"
prim__isInitialized : Bits64 -> PrimIO Bits32

||| Check initialization status
export
isInitialized : Handle -> IO Bool
isInitialized h = do
  result <- primIO (prim__isInitialized (handlePtr h))
  pure (result /= 0)

--------------------------------------------------------------------------------
-- Safety properties
--------------------------------------------------------------------------------
-- The properties this ABI relies on are proved -- genuinely, with no escape
-- hatch and machine-checked under Idris2 0.8.0 -- in the sibling modules, NOT
-- here:
--   * stability score in [0,100]      -> Stability.idr  (stabilityUpperBound)
--   * positional operator determinism -> Positional.idr (positionalDeterministic)
--   * paradox-factor monotonicity     -> Paradox.idr    (superpositionMonotone,
--                                                         temporalMonotone)
--
-- The earlier `stabilityBounded` / `positionalDeterministic` / `paradoxMonotonic`
-- definitions here were unsound: they used `cast ()` / `cast Refl` over `IO`
-- actions to fabricate evidence. Removed 2026-06-23. Proving the third one
-- honestly also revealed that the *global* monotonicity claim is false of the
-- implementation -- scope leakage is prime-gated; see Paradox.idr.
