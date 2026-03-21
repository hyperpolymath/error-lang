// SPDX-License-Identifier: PMPL-1.0-or-later
// Codegen.res - AST to bytecode compiler for Error-Lang
//
// Compiles parsed AST into bytecode while preserving:
// - Positional semantics metadata
// - Computational haptics trace points
// - Paradox injection points

open Types
open Bytecode

// Compiler state
type compiler = {
  mutable code: array<opcode>,
  mutable constants: array<value>,
  mutable locations: array<location>,
  mutable localCount: int,
  mutable locals: array<string>,
}

let make = (): compiler => {
  {
    code: [],
    constants: [],
    locations: [],
    localCount: 0,
    locals: [],
  }
}

// Emit bytecode instruction
let emit = (c: compiler, op: opcode, loc: location): unit => {
  c.code->Array.push(op)->ignore
  c.locations->Array.push(loc)->ignore
}

// Add constant to constant pool
let addConstant = (c: compiler, value: value): int => {
  c.constants->Array.push(value)->ignore
  c.constants->Array.length - 1
}

// Resolve local variable
let resolveLocal = (c: compiler, name: string): option<int> => {
  c.locals->Array.findIndexOpt(local => local == name)
}

// Add local variable
let addLocal = (c: compiler, name: string): int => {
  c.locals->Array.push(name)->ignore
  c.localCount = c.localCount + 1
  c.localCount - 1
}

// Compile expression to bytecode
let rec compileExpr = (c: compiler, expr: expr): unit => {
  switch expr {
  | IntLit(n, loc) => {
      emit(c, OpPush(VInt(n)), loc)
    }
  | FloatLit(f, loc) => {
      emit(c, OpPush(VFloat(f)), loc)
    }
  | StringLit(s, loc) => {
      emit(c, OpPush(VString(s)), loc)
    }
  | BoolLit(b, loc) => {
      emit(c, OpPush(VBool(b)), loc)
    }
  | NilLit(loc) => {
      emit(c, OpPush(VNil), loc)
    }
  | Ident(name, loc) => {
      // Try local first, then global
      switch resolveLocal(c, name) {
      | Some(index) => emit(c, OpGetLocal(index), loc)
      | None => emit(c, OpGetGlobal(name), loc)
      }
    }
  | Array(elements, loc) => {
      // Compile each element
      elements->Array.forEach(elem => compileExpr(c, elem))
      // Create array with N elements
      emit(c, OpArray(elements->Array.length), loc)
    }
  | Binary(left, op, right, loc) => {
      // Compile operands
      compileExpr(c, left)
      compileExpr(c, right)

      // Emit operator with positional metadata
      let opcode = switch op {
      | Add => {
          // Extract position from location
          let pos: positionMetadata = {
            line: loc.start.line,
            column: loc.start.column,
            operatorType: PlusOp,
          }
          OpAdd(pos)
        }
      | Sub => OpSub
      | Mul => {
          let pos: positionMetadata = {
            line: loc.start.line,
            column: loc.start.column,
            operatorType: StarOp,
          }
          OpMul(pos)
        }
      | Div => OpDiv
      | Mod => OpMod
      | Eq => OpEq
      | Neq => OpNeq
      | Lt => OpLt
      | Gt => OpGt
      | Lte => OpLte
      | Gte => OpGte
      | LAnd => OpAnd
      | LOr => OpOr
      | BAnd | BOr | BXor | Shl | Shr => OpPush(VNil)  // Not implemented yet
      }
      emit(c, opcode, loc)
    }
  | Unary(op, operand, loc) => {
      compileExpr(c, operand)
      switch op {
      | Neg => emit(c, OpNegate, loc)
      | LNot => emit(c, OpNot, loc)
      | BNot => ()  // Not implemented
      }
    }
  | Call(_func, _args, loc) => {
      // Function calls not fully implemented yet
      emit(c, OpPush(VNil), loc)
    }
  | Index(array, index, loc) => {
      compileExpr(c, array)
      compileExpr(c, index)
      emit(c, OpIndex, loc)
    }
  | Member(_obj, _field, loc) => {
      // Member access not implemented
      emit(c, OpPush(VNil), loc)
    }
  | Ternary(cond, then_, else_, loc) => {
      // Compile condition
      compileExpr(c, cond)

      // Jump if false (to else branch)
      let elseJump = c.code->Array.length
      emit(c, OpJumpIfFalse(0), loc)  // Patch later

      // Compile then branch
      compileExpr(c, then_)

      // Jump over else branch
      let endJump = c.code->Array.length
      emit(c, OpJump(0), loc)  // Patch later

      // Patch else jump
      let elseStart = c.code->Array.length
      c.code[elseJump] = OpJumpIfFalse(elseStart - elseJump - 1)

      // Compile else branch
      compileExpr(c, else_)

      // Patch end jump
      let end = c.code->Array.length
      c.code[endJump] = OpJump(end - endJump - 1)
    }
  | Lambda(_params, _returnType, _body, loc) => {
      // Lambdas not implemented
      emit(c, OpPush(VNil), loc)
    }
  }
}

// Compile statement to bytecode
let rec compileStmt = (c: compiler, stmt: stmt): unit => {
  switch stmt {
  | LetStmt({mutable_: _, name, type_: _, value, loc}) => {
      // Compile initializer
      compileExpr(c, value)

      // Add local variable
      let index = addLocal(c, name)
      emit(c, OpSetLocal(index), loc)
    }
  | AssignStmt({target, value, loc}) => {
      // Compile value
      compileExpr(c, value)

      // Assign to target
      switch target {
      | Ident(name, _) => {
          switch resolveLocal(c, name) {
          | Some(index) => emit(c, OpSetLocal(index), loc)
          | None => emit(c, OpSetGlobal(name), loc)
          }
        }
      | Index(array, index, _) => {
          // Array assignment not fully implemented
          compileExpr(c, array)
          compileExpr(c, index)
        }
      | _ => ()  // Other assignment targets not implemented
      }
    }
  | IfStmt({cond, then_, elseifs, else_, loc}) => {
      // Compile condition
      compileExpr(c, cond)

      // Jump if false to elseif/else
      let thenJump = c.code->Array.length
      emit(c, OpJumpIfFalse(0), loc)

      // Compile then branch
      then_->Array.forEach(stmt => compileStmt(c, stmt))

      // Jump to end
      let endJump = c.code->Array.length
      emit(c, OpJump(0), loc)

      // Patch then jump
      let elseStart = c.code->Array.length
      c.code[thenJump] = OpJumpIfFalse(elseStart - thenJump - 1)

      // Compile elseifs (simplified - not handling multiple elseifs)
      elseifs->Array.forEach(((cond, body)) => {
        compileExpr(c, cond)
        let elseifJump = c.code->Array.length
        emit(c, OpJumpIfFalse(0), loc)
        body->Array.forEach(stmt => compileStmt(c, stmt))
        let elseifEnd = c.code->Array.length
        c.code[elseifJump] = OpJumpIfFalse(elseifEnd - elseifJump - 1)
      })

      // Compile else branch
      switch else_ {
      | Some(body) => body->Array.forEach(stmt => compileStmt(c, stmt))
      | None => ()
      }

      // Patch end jump
      let end = c.code->Array.length
      c.code[endJump] = OpJump(end - endJump - 1)
    }
  | WhileStmt({cond, body, loc}) => {
      let loopStart = c.code->Array.length

      // Compile condition
      compileExpr(c, cond)

      // Jump if false to end
      let exitJump = c.code->Array.length
      emit(c, OpJumpIfFalse(0), loc)

      // Compile body
      body->Array.forEach(stmt => compileStmt(c, stmt))

      // Jump back to loop start
      let loopEnd = c.code->Array.length
      emit(c, OpJump(loopStart - loopEnd - 1), loc)

      // Patch exit jump
      let end = c.code->Array.length
      c.code[exitJump] = OpJumpIfFalse(end - exitJump - 1)
    }
  | ForStmt({var, iter, body, loc}) => {
      // For loops over arrays (simplified)
      compileExpr(c, iter)

      // Add loop variable
      let varIndex = addLocal(c, var)

      // For now, just a placeholder
      emit(c, OpSetLocal(varIndex), loc)
      body->Array.forEach(stmt => compileStmt(c, stmt))
    }
  | ReturnStmt({value, loc}) => {
      switch value {
      | Some(expr) => {
          compileExpr(c, expr)
          emit(c, OpReturn, loc)
        }
      | None => {
          emit(c, OpPush(VNil), loc)
          emit(c, OpReturn, loc)
        }
      }
    }
  | BreakStmt(loc) => {
      // Break not fully implemented
      emit(c, OpJump(0), loc)
    }
  | ContinueStmt(loc) => {
      // Continue not fully implemented
      emit(c, OpJump(0), loc)
    }
  | PrintStmt({println, args, loc}) => {
      // Compile each argument and print
      args->Array.forEach(arg => {
        compileExpr(c, arg)
        emit(c, OpPrint(println), loc)
      })
    }
  | GutterBlock({tokens: _, recovered: _, loc}) => {
      // Gutter blocks are error recovery - emit checkpoint
      emit(c, OpCheckpoint("gutter"), loc)
    }
  | ExprStmt(expr) => {
      compileExpr(c, expr)
      emit(c, OpPop, expr->getLocation)
    }
  }
}

and getLocation = (expr: expr): location => {
  switch expr {
  | IntLit(_, loc)
  | FloatLit(_, loc)
  | StringLit(_, loc)
  | BoolLit(_, loc)
  | NilLit(loc)
  | Ident(_, loc)
  | Array(_, loc)
  | Binary(_, _, _, loc)
  | Unary(_, _, loc)
  | Call(_, _, loc)
  | Index(_, _, loc)
  | Member(_, _, loc)
  | Ternary(_, _, _, loc)
  | Lambda(_, _, _, loc) => loc
  }
}

// Compile declaration
let compileDecl = (c: compiler, decl: decl): unit => {
  switch decl {
  | FunctionDecl({name: _, params: _, returnType: _, body, loc}) => {
      // Functions not fully implemented
      emit(c, OpCheckpoint("function"), loc)
      body->Array.forEach(stmt => compileStmt(c, stmt))
    }
  | StructDecl({name: _, fields: _, loc}) => {
      // Structs not implemented
      emit(c, OpCheckpoint("struct"), loc)
    }
  | MainBlock({body, loc}) => {
      emit(c, OpCheckpoint("main_start"), loc)
      body->Array.forEach(stmt => compileStmt(c, stmt))
      emit(c, OpCheckpoint("main_end"), loc)
    }
  | StmtDecl(stmt) => compileStmt(c, stmt)
  }
}

// Compile program
let compile = (program: program): result<bytecodeProgram, string> => {
  try {
    let c = make()

    // Compile all declarations
    program.declarations->Array.forEach(decl => compileDecl(c, decl))

    // Emit halt
    emit(c, OpHalt, program.loc)

    // Build chunk
    let chunk: chunk = {
      code: c.code,
      constants: c.constants,
      locations: c.locations,
    }

    // Build program
    let bytecodeProgram: bytecodeProgram = {
      main: chunk,
      functions: [],  // Functions not implemented yet
    }

    Ok(bytecodeProgram)
  } catch {
  | exn => Error(`Compilation error: ${exn->Exn.message->Option.getOr("unknown")}`)
  }
}
