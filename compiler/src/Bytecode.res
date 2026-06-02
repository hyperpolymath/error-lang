// SPDX-License-Identifier: MPL-2.0
// Bytecode.res - Bytecode instruction set for Error-Lang VM
//
// Stack-based bytecode VM designed for:
// - Computational haptics integration
// - Positional semantics preservation
// - Paradox visualization
// - Educational transparency

// Bytecode instructions
type opcode =
  // Stack manipulation
  | OpPush(value)          // Push constant onto stack
  | OpPop                   // Pop top of stack
  | OpDup                   // Duplicate top of stack

  // Variables
  | OpGetLocal(int)         // Get local variable
  | OpSetLocal(int)         // Set local variable
  | OpGetGlobal(string)     // Get global variable
  | OpSetGlobal(string)     // Set global variable

  // Arithmetic (with positional semantics metadata)
  | OpAdd(positionMetadata)     // Addition (or concatenation based on position!)
  | OpSub                       // Subtraction
  | OpMul(positionMetadata)     // Multiplication (or exponentiation based on position!)
  | OpDiv                       // Division
  | OpMod                       // Modulo
  | OpNegate                    // Unary negation

  // Comparison
  | OpEq | OpNeq | OpLt | OpGt | OpLte | OpGte

  // Logical
  | OpAnd | OpOr | OpNot

  // Control flow
  | OpJump(int)             // Unconditional jump
  | OpJumpIfFalse(int)      // Conditional jump
  | OpCall(int)             // Call function (arity)
  | OpReturn                // Return from function

  // Arrays
  | OpArray(int)            // Create array with N elements
  | OpIndex                 // Array indexing

  // Special (computational haptics)
  | OpTrace(string)         // Add trace point
  | OpCheckpoint(string)    // Add checkpoint
  | OpUpdateStability       // Recalculate stability score
  | OpInjectParadox(paradoxType)  // Activate a paradox

  // Echo types (structured loss)
  | OpEcho                  // Pop output then input, push a fiber witness VEcho{input, output}
  | OpEchoToResidue         // Pop an echo, push its residue (erases the witness; costs stability)
  | OpResidueStrictlyLoses  // Pop a value, push VBool(true) iff it is a residue
  | OpEchoInput             // Pop an echo, push its input witness (runtime error on a residue)
  | OpEchoOutput            // Pop an echo or residue, push its retained output

  // Debug
  | OpPrint(bool)           // Print (println if true)
  | OpHalt                  // Stop execution

// Position metadata for operators that change behavior based on location
and positionMetadata = {
  line: int,
  column: int,
  operatorType: operatorType,
}

and operatorType =
  | PlusOp    // Can be addition OR concatenation
  | StarOp    // Can be multiplication OR exponentiation

and paradoxType =
  | TypeSuperposition
  | PositionalSemantics
  | ScopeLeakage
  | TemporalCorruption
  | ArithmeticDrift
  | NullPropagation
  | ContextCollapse
  | ReservedWordRoulette
  | GlobalEntanglement
  | MemoryPhantom

// Runtime values
and value =
  | VInt(int)
  | VFloat(float)
  | VString(string)
  | VBool(bool)
  | VNil
  | VArray(array<value>)
  | VFunction({arity: int, address: int, name: string})
  // A single fiber witness: input `x` reached output `y` (one element of `Echo<A, B>`).
  | VEcho({input: value, output: value})
  // The residue of an echo after erasure: the input witness is gone (non-recoverable),
  // only the reached output is retained. This is `EchoR<A, B>` at runtime.
  | VResidue({output: value})

// Compiled bytecode chunk
type chunk = {
  code: array<opcode>,
  constants: array<value>,
  // Source location mapping for error reporting
  locations: array<location>,
}

and location = Types.location

// Bytecode program
type bytecodeProgram = {
  main: chunk,
  functions: array<(string, chunk)>,
}

// ============================================
// Utilities
// ============================================

let valueToString = (v: value): string => {
  switch v {
  | VInt(n) => Int.toString(n)
  | VFloat(f) => Float.toString(f)
  | VString(s) => `"${s}"`
  | VBool(b) => b ? "true" : "false"
  | VNil => "nil"
  | VArray(arr) => {
      let items = arr->Array.map(valueToString)->Array.joinWith(", ")
      `[${items}]`
    }
  | VFunction({name}) => `<function ${name}>`
  | VEcho({input, output}) => `echo(${valueToString(input)} ↦ ${valueToString(output)})`
  | VResidue({output}) => `residue(↦ ${valueToString(output)})`
  }
}

let opcodeToString = (op: opcode): string => {
  switch op {
  | OpPush(v) => `PUSH ${valueToString(v)}`
  | OpPop => "POP"
  | OpDup => "DUP"
  | OpGetLocal(i) => `GET_LOCAL ${Int.toString(i)}`
  | OpSetLocal(i) => `SET_LOCAL ${Int.toString(i)}`
  | OpGetGlobal(name) => `GET_GLOBAL ${name}`
  | OpSetGlobal(name) => `SET_GLOBAL ${name}`
  | OpAdd(_) => "ADD"
  | OpSub => "SUB"
  | OpMul(_) => "MUL"
  | OpDiv => "DIV"
  | OpMod => "MOD"
  | OpNegate => "NEGATE"
  | OpEq => "EQ"
  | OpNeq => "NEQ"
  | OpLt => "LT"
  | OpGt => "GT"
  | OpLte => "LTE"
  | OpGte => "GTE"
  | OpAnd => "AND"
  | OpOr => "OR"
  | OpNot => "NOT"
  | OpJump(offset) => `JUMP ${Int.toString(offset)}`
  | OpJumpIfFalse(offset) => `JUMP_IF_FALSE ${Int.toString(offset)}`
  | OpCall(arity) => `CALL ${Int.toString(arity)}`
  | OpReturn => "RETURN"
  | OpArray(size) => `ARRAY ${Int.toString(size)}`
  | OpIndex => "INDEX"
  | OpTrace(msg) => `TRACE "${msg}"`
  | OpCheckpoint(name) => `CHECKPOINT "${name}"`
  | OpUpdateStability => "UPDATE_STABILITY"
  | OpInjectParadox(_) => "INJECT_PARADOX"
  | OpEcho => "ECHO"
  | OpEchoToResidue => "ECHO_TO_RESIDUE"
  | OpResidueStrictlyLoses => "RESIDUE_STRICTLY_LOSES"
  | OpEchoInput => "ECHO_INPUT"
  | OpEchoOutput => "ECHO_OUTPUT"
  | OpPrint(println) => println ? "PRINTLN" : "PRINT"
  | OpHalt => "HALT"
  }
}

// Disassemble chunk for debugging
let disassemble = (chunk: chunk, name: string): unit => {
  Console.log(`=== ${name} ===`)
  chunk.code->Array.forEachWithIndex((i, op) => {
    Console.log(`${Int.toString(i)->String.padStart(4, "0")}  ${opcodeToString(op)}`)
  })
}
