// SPDX-License-Identifier: MPL-2.0
// EchoRuntimeTest.res — semantic / runtime decomposition plane for Echo types.
//
// Proves the runtime half of "decomposition must be visible":
//   - echo(x, y) builds VEcho{input, output}
//   - echo_input works on VEcho, FAILS on a residue (witness genuinely gone)
//   - echo_output works on BOTH VEcho and VResidue (output survives)
//   - echo_to_residue yields VResidue and debits stability EXACTLY ONCE
//   - projection (echo_input / echo_output) never debits stability

open Bytecode
open TestHelpers

let dummyLoc: Types.location = {
  start: {line: 1, column: 1, offset: 0},
  end_: {line: 1, column: 1, offset: 0},
  file: "<echo-runtime-test>",
}

// Build a chunk from a bare opcode list (locations unused by these opcodes).
let chunkOf = (code: array<opcode>): chunk => {
  code,
  constants: [],
  locations: [],
}

// Run a chunk; return (result, final stability score).
let runChunk = (code: array<opcode>): (result<value, string>, float) => {
  let vm = VM.make(chunkOf(code))
  let r = VM.run(vm)
  (r, vm.stabilityScore)
}

// echo(1, "a") — input pushed first, output second (OpEcho pops output then input).
let echoProgram: array<opcode> = [OpPush(VInt(1)), OpPush(VString("a")), OpEcho]

let testEchoConstructsWitness = () => {
  suite("Echo runtime: construction")
  let (r, _) = runChunk(Array.concat(echoProgram, [OpHalt]))
  assertEqual("echo(1, \"a\") builds VEcho{input:1, output:\"a\"}", r, Ok(VEcho({input: VInt(1), output: VString("a")})))
}

let testProjections = () => {
  suite("Echo runtime: projection")
  let (inp, _) = runChunk(Array.concat(echoProgram, [OpEchoInput, OpHalt]))
  assertEqual("echo_input recovers the witness", inp, Ok(VInt(1)))

  let (out, _) = runChunk(Array.concat(echoProgram, [OpEchoOutput, OpHalt]))
  assertEqual("echo_output recovers the output", out, Ok(VString("a")))
}

let testToResidue = () => {
  suite("Echo runtime: decomposition to residue")
  let (res, _) = runChunk(Array.concat(echoProgram, [OpEchoToResidue, OpHalt]))
  assertEqual("echo_to_residue yields VResidue{output:\"a\"}", res, Ok(VResidue({output: VString("a")})))

  // Output still recoverable from the residue.
  let (out, _) = runChunk(Array.concat(echoProgram, [OpEchoToResidue, OpEchoOutput, OpHalt]))
  assertEqual("echo_output survives erasure", out, Ok(VString("a")))

  // residue_strictly_loses reports non-recoverability for a residue, not for an echo.
  let (lossR, _) = runChunk(Array.concat(echoProgram, [OpEchoToResidue, OpResidueStrictlyLoses, OpHalt]))
  assertEqual("residue_strictly_loses(residue) = true", lossR, Ok(VBool(true)))
  let (lossE, _) = runChunk(Array.concat(echoProgram, [OpResidueStrictlyLoses, OpHalt]))
  assertEqual("residue_strictly_loses(echo) = false", lossE, Ok(VBool(false)))
}

let testWitnessActuallyGone = () => {
  suite("Echo runtime: witness is genuinely unavailable after erasure")
  // echo_input on a residue must be a runtime error, not silently nil.
  let (r, _) = runChunk(Array.concat(echoProgram, [OpEchoToResidue, OpEchoInput, OpHalt]))
  switch r {
  | Error(_) => assertTrue("echo_input on residue fails at runtime", true)
  | Ok(_) => assertTrue("echo_input on residue fails at runtime", false)
  }
}

let testStabilityDebitedExactlyOnce = () => {
  suite("Echo runtime: stability debit is visible and charged exactly once")
  // No erasure → no debit.
  let (_, sPlain) = runChunk(Array.concat(echoProgram, [OpEchoOutput, OpHalt]))
  assertEqual("projection alone does not debit stability", sPlain, 100.0)

  // One erasure → exactly one debit (echoEraseCost = 15.0).
  let (_, sErase) = runChunk(Array.concat(echoProgram, [OpEchoToResidue, OpHalt]))
  assertEqual("echo_to_residue debits stability once", sErase, 100.0 -. VM.echoEraseCost)

  // Erasure then projection → still exactly one debit (projection is free).
  let (_, sEraseThenProj) = runChunk(Array.concat(echoProgram, [OpEchoToResidue, OpEchoOutput, OpHalt]))
  assertEqual("projection after erasure adds no further debit", sEraseThenProj, 100.0 -. VM.echoEraseCost)
}

let runAll = () => {
  Console.log("\n========================================")
  Console.log("  ERROR-LANG ECHO RUNTIME TESTS")
  Console.log("========================================")
  testEchoConstructsWitness()
  testProjections()
  testToResidue()
  testWitnessActuallyGone()
  testStabilityDebitedExactlyOnce()
  summarize()
}

let _ = runAll()
