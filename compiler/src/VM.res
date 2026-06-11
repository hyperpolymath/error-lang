// SPDX-License-Identifier: MPL-2.0
// VM.res - Bytecode virtual machine for Error-Lang
//
// Stack-based interpreter with computational haptics integration

open Bytecode

// Runtime error
exception RuntimeError(string)

// Stability debited when an Echo is erased to its residue. Echo-Lang's conceit
// is that loss can be *structured* but is never free: erasing the witness is a
// thermodynamic act (a Landauer-style cost; cf. echo-types `fiber_erasure_bound`,
// k·T·⌊log₂ n⌋). Modelled here as a fixed symbolic debit until fibre cardinality
// is computable at runtime.
let echoEraseCost = 15.0

// VM state
type vm = {
  // Execution state
  mutable stack: array<value>,
  mutable sp: int,  // Stack pointer
  mutable ip: int,  // Instruction pointer
  mutable frames: array<callFrame>,
  mutable fp: int,  // Frame pointer

  // Variables
  globals: Map.t<string, value>,

  // Program
  chunk: chunk,

  // Computational haptics state
  mutable stabilityScore: float,
  mutable activeParadoxes: int,  // Bitmask
  mutable traceHistory: array<(string, value)>,

  // Debug
  mutable debug: bool,
}

and callFrame = {
  functionName: string,
  returnAddress: int,
  localBase: int,  // Base of local variables in stack
}

// Create new VM
let make = (chunk: chunk): vm => {
  {
    stack: [],
    sp: 0,
    ip: 0,
    frames: [],
    fp: 0,
    globals: Map.make(),
    chunk,
    stabilityScore: 100.0,
    activeParadoxes: 0,
    traceHistory: [],
    debug: false,
  }
}

// Stack operations
let push = (vm: vm, value: value): unit => {
  vm.stack->Array.push(value)->ignore
  vm.sp = vm.sp + 1
}

let pop = (vm: vm): value => {
  if vm.sp == 0 {
    raise(RuntimeError("Stack underflow"))
  }
  vm.sp = vm.sp - 1
  vm.stack[vm.sp]->Option.getExn
}

let peek = (vm: vm, offset: int): value => {
  let index = vm.sp - 1 - offset
  if index < 0 || index >= vm.sp {
    raise(RuntimeError("Stack peek out of bounds"))
  }
  vm.stack[index]->Option.getExn
}

// Positional operator resolution (uses Zig FFI if available)
let resolvePositionalOperator = (
  pos: positionMetadata,
  left: value,
  right: value,
): value => {
  // For PlusOp: even column = addition, odd column = concatenation
  // For StarOp: column % 3 == 0 = multiplication, else = exponentiation

  switch pos.operatorType {
  | PlusOp if mod(pos.column, 2) == 0 => {
      // Addition
      switch (left, right) {
      | (VInt(a), VInt(b)) => VInt(a + b)
      | (VFloat(a), VFloat(b)) => VFloat(a +. b)
      | (VInt(a), VFloat(b)) => VFloat(Int.toFloat(a) +. b)
      | (VFloat(a), VInt(b)) => VFloat(a +. Int.toFloat(b))
      | _ => raise(RuntimeError("Cannot add these types"))
      }
    }
  | PlusOp => {
      // Concatenation
      let leftStr = valueToString(left)
      let rightStr = valueToString(right)
      VString(leftStr ++ rightStr)
    }
  | StarOp if mod(pos.column, 3) == 0 => {
      // Multiplication
      switch (left, right) {
      | (VInt(a), VInt(b)) => VInt(a * b)
      | (VFloat(a), VFloat(b)) => VFloat(a *. b)
      | (VInt(a), VFloat(b)) => VFloat(Int.toFloat(a) *. b)
      | (VFloat(a), VInt(b)) => VFloat(a *. Int.toFloat(b))
      | _ => raise(RuntimeError("Cannot multiply these types"))
      }
    }
  | StarOp => {
      // Exponentiation
      switch (left, right) {
      | (VInt(a), VInt(b)) => VFloat(Math.pow(Int.toFloat(a), Int.toFloat(b)))
      | (VFloat(a), VFloat(b)) => VFloat(Math.pow(a, b))
      | (VInt(a), VFloat(b)) => VFloat(Math.pow(Int.toFloat(a), b))
      | (VFloat(a), VInt(b)) => VFloat(Math.pow(a, Int.toFloat(b)))
      | _ => raise(RuntimeError("Cannot exponentiate these types"))
      }
    }
  }
}

// Execute one instruction
let executeInstruction = (vm: vm): bool => {
  if vm.ip >= vm.chunk.code->Array.length {
    return false  // Halt
  }

  let instruction = vm.chunk.code[vm.ip]->Option.getExn

  if vm.debug {
    Console.log(`IP: ${Int.toString(vm.ip)} ${opcodeToString(instruction)}`)
  }

  vm.ip = vm.ip + 1

  switch instruction {
  // Stack manipulation
  | OpPush(value) => push(vm, value)
  | OpPop => {pop(vm)->ignore}
  | OpDup => {
      let value = peek(vm, 0)
      push(vm, value)
    }

  // Variables
  | OpGetLocal(index) => {
      let base = vm.frames->Array.length > 0
        ? vm.frames[vm.fp]->Option.getExn.localBase
        : 0
      let value = vm.stack[base + index]->Option.getExn
      push(vm, value)
    }
  | OpSetLocal(index) => {
      let value = peek(vm, 0)
      let base = vm.frames->Array.length > 0
        ? vm.frames[vm.fp]->Option.getExn.localBase
        : 0
      vm.stack[base + index] = value
    }
  | OpGetGlobal(name) => {
      switch vm.globals->Map.get(name) {
      | Some(value) => push(vm, value)
      | None => raise(RuntimeError(`Undefined variable '${name}'`))
      }
    }
  | OpSetGlobal(name) => {
      let value = peek(vm, 0)
      vm.globals->Map.set(name, value)
    }

  // Arithmetic with positional semantics
  | OpAdd(pos) => {
      let right = pop(vm)
      let left = pop(vm)
      let result = resolvePositionalOperator(pos, left, right)
      push(vm, result)
    }
  | OpSub => {
      let right = pop(vm)
      let left = pop(vm)
      switch (left, right) {
      | (VInt(a), VInt(b)) => push(vm, VInt(a - b))
      | (VFloat(a), VFloat(b)) => push(vm, VFloat(a -. b))
      | (VInt(a), VFloat(b)) => push(vm, VFloat(Int.toFloat(a) -. b))
      | (VFloat(a), VInt(b)) => push(vm, VFloat(a -. Int.toFloat(b)))
      | _ => raise(RuntimeError("Cannot subtract these types"))
      }
    }
  | OpMul(pos) => {
      let right = pop(vm)
      let left = pop(vm)
      let result = resolvePositionalOperator(pos, left, right)
      push(vm, result)
    }
  | OpDiv => {
      let right = pop(vm)
      let left = pop(vm)
      switch (left, right) {
      | (VInt(a), VInt(b)) if b != 0 => push(vm, VInt(a / b))
      | (VFloat(a), VFloat(b)) if b != 0.0 => push(vm, VFloat(a /. b))
      | _ => raise(RuntimeError("Division by zero or invalid types"))
      }
    }
  | OpMod => {
      let right = pop(vm)
      let left = pop(vm)
      switch (left, right) {
      | (VInt(a), VInt(b)) if b != 0 => push(vm, VInt(mod(a, b)))
      | _ => raise(RuntimeError("Modulo requires integers"))
      }
    }
  | OpNegate => {
      let value = pop(vm)
      switch value {
      | VInt(n) => push(vm, VInt(-n))
      | VFloat(f) => push(vm, VFloat(-.f))
      | _ => raise(RuntimeError("Cannot negate this type"))
      }
    }

  // Comparison
  | OpEq => {
      let right = pop(vm)
      let left = pop(vm)
      push(vm, VBool(left == right))
    }
  | OpNeq => {
      let right = pop(vm)
      let left = pop(vm)
      push(vm, VBool(left != right))
    }
  | OpLt => {
      let right = pop(vm)
      let left = pop(vm)
      switch (left, right) {
      | (VInt(a), VInt(b)) => push(vm, VBool(a < b))
      | (VFloat(a), VFloat(b)) => push(vm, VBool(a < b))
      | _ => raise(RuntimeError("Cannot compare these types"))
      }
    }
  | OpGt => {
      let right = pop(vm)
      let left = pop(vm)
      switch (left, right) {
      | (VInt(a), VInt(b)) => push(vm, VBool(a > b))
      | (VFloat(a), VFloat(b)) => push(vm, VBool(a > b))
      | _ => raise(RuntimeError("Cannot compare these types"))
      }
    }
  | OpLte | OpGte => raise(RuntimeError("Not implemented"))

  // Logical
  | OpAnd => {
      let right = pop(vm)
      let left = pop(vm)
      switch (left, right) {
      | (VBool(a), VBool(b)) => push(vm, VBool(a && b))
      | _ => raise(RuntimeError("AND requires booleans"))
      }
    }
  | OpOr => {
      let right = pop(vm)
      let left = pop(vm)
      switch (left, right) {
      | (VBool(a), VBool(b)) => push(vm, VBool(a || b))
      | _ => raise(RuntimeError("OR requires booleans"))
      }
    }
  | OpNot => {
      let value = pop(vm)
      switch value {
      | VBool(b) => push(vm, VBool(!b))
      | _ => raise(RuntimeError("NOT requires boolean"))
      }
    }

  // Control flow
  | OpJump(offset) => vm.ip = vm.ip + offset
  | OpJumpIfFalse(offset) => {
      let condition = pop(vm)
      switch condition {
      | VBool(false) => vm.ip = vm.ip + offset
      | VBool(true) => ()
      | _ => raise(RuntimeError("Condition must be boolean"))
      }
    }
  | OpCall(_arity) => raise(RuntimeError("Function calls not yet implemented"))
  | OpReturn => {
      if vm.frames->Array.length == 0 {
        return false  // Halt
      }
      let frame = vm.frames[vm.fp]->Option.getExn
      vm.ip = frame.returnAddress
      vm.fp = vm.fp - 1
    }

  // Arrays
  | OpArray(size) => {
      let arr = []
      for _ in 0 to size - 1 {
        arr->Array.push(pop(vm))->ignore
      }
      arr->Array.reverse->ignore
      push(vm, VArray(arr))
    }
  | OpIndex => {
      let index = pop(vm)
      let array = pop(vm)
      switch (array, index) {
      | (VArray(arr), VInt(i)) if i >= 0 && i < arr->Array.length => {
          push(vm, arr[i]->Option.getExn)
        }
      | _ => raise(RuntimeError("Invalid array indexing"))
      }
    }

  // Computational haptics
  | OpTrace(msg) => {
      let value = peek(vm, 0)
      vm.traceHistory->Array.push((msg, value))->ignore
    }
  | OpCheckpoint(name) => {
      // For now, just trace it
      Console.log(`📍 Checkpoint: ${name}`)
    }
  | OpUpdateStability => {
      // Recalculate based on active paradoxes
      // For now, simple heuristic
      let paradoxCount = vm.activeParadoxes->Int.toFloat
      vm.stabilityScore = 100.0 -. (paradoxCount *. 10.0)
    }
  | OpInjectParadox(_) => {
      // Activate a paradox (set bit in bitmask)
      vm.activeParadoxes = vm.activeParadoxes + 1
      vm.stabilityScore = vm.stabilityScore -. 5.0
    }

  // Echo types (structured loss)
  | OpEcho => {
      let output = pop(vm)
      let input = pop(vm)
      push(vm, VEcho({input, output}))
    }
  | OpEchoToResidue => {
      let e = pop(vm)
      switch e {
      | VEcho({output}) => {
          // Erasure is not free: destroying the witness costs stability.
          vm.stabilityScore = vm.stabilityScore -. echoEraseCost
          vm.traceHistory->Array.push(("echo_to_residue: witness erased", e))->ignore
          push(vm, VResidue({output: output}))
        }
      | VResidue(_) => push(vm, e)  // already a residue — idempotent, no further loss
      | _ => raise(RuntimeError("echo_to_residue expects an Echo value"))
      }
    }
  | OpResidueStrictlyLoses => {
      let e = pop(vm)
      switch e {
      | VResidue(_) => push(vm, VBool(true))
      | VEcho(_) => push(vm, VBool(false))
      | _ => raise(RuntimeError("residue_strictly_loses expects an Echo or residue value"))
      }
    }
  | OpEchoInput => {
      let e = pop(vm)
      switch e {
      | VEcho({input}) => push(vm, input)
      | VResidue(_) =>
        raise(RuntimeError("echo_input: the witness was erased — a residue is non-recoverable"))
      | _ => raise(RuntimeError("echo_input expects an Echo value"))
      }
    }
  | OpEchoOutput => {
      let e = pop(vm)
      switch e {
      | VEcho({output}) => push(vm, output)
      | VResidue({output}) => push(vm, output)
      | _ => raise(RuntimeError("echo_output expects an Echo or residue value"))
      }
    }

  // Debug
  | OpPrint(println) => {
      let value = pop(vm)
      let str = valueToString(value)
      if println {
        Console.log(str)
      } else {
        Console.log(str)  // For now, same behavior
      }
    }
  | OpHalt => return false
  }

  true  // Continue execution
}

// Run the VM
let run = (vm: vm): result<value, string> => {
  try {
    let continue = ref(true)
    while continue.contents && vm.ip < vm.chunk.code->Array.length {
      continue := executeInstruction(vm)
    }

    // Return top of stack or nil
    if vm.sp > 0 {
      Ok(peek(vm, 0))
    } else {
      Ok(VNil)
    }
  } catch {
  | RuntimeError(msg) => Error(`Runtime error: ${msg}`)
  | exn => Error(`Unexpected error: ${exn->Exn.message->Option.getOr("unknown")}`)
  }
}

// Execute a bytecode program
let execute = (program: bytecodeProgram, debug: bool): result<value, string> => {
  let vm = make(program.main)
  vm.debug = debug
  run(vm)
}
