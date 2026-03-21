// SPDX-License-Identifier: PMPL-1.0-or-later
// TypeCheckerTest.res - Tests for Error-Lang type checker

open Types
open TypeChecker

// ============================================
// Test Helpers
// ============================================

let dummyLoc: location = {
  start: {line: 1, column: 1, offset: 0},
  end_: {line: 1, column: 1, offset: 0},
  file: "<test>",
}

let dummyLoc2: location = {
  start: {line: 2, column: 1, offset: 0},
  end_: {line: 2, column: 1, offset: 0},
  file: "<test>",
}

let assertNoErrors = (result: checkResult, testName: string): unit => {
  if Array.length(result.errors) > 0 {
    Console.error(`FAIL: ${testName}`)
    Array.forEach(result.errors, e => Console.error(`  Error: ${e.message}`))
  } else {
    Console.log(`PASS: ${testName}`)
  }
}

let assertHasErrors = (result: checkResult, testName: string): unit => {
  if Array.length(result.errors) == 0 {
    Console.error(`FAIL: ${testName} — expected errors but got none`)
  } else {
    Console.log(`PASS: ${testName} (${Int.toString(Array.length(result.errors))} error(s))`)
  }
}

let assertErrorCount = (result: checkResult, count: int, testName: string): unit => {
  let actual = Array.length(result.errors)
  if actual != count {
    Console.error(
      `FAIL: ${testName} — expected ${Int.toString(count)} error(s), got ${Int.toString(actual)}`,
    )
    Array.forEach(result.errors, e => Console.error(`  Error: ${e.message}`))
  } else {
    Console.log(`PASS: ${testName}`)
  }
}

// ============================================
// Test: Let Bindings
// ============================================

let testLetBinding = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        LetStmt({
          mutable_: false,
          name: "x",
          type_: None,
          value: IntLit(42, dummyLoc),
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "let binding infers Int")
}

let testLetWithAnnotation = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        LetStmt({
          mutable_: false,
          name: "x",
          type_: Some(Types.TyInt),
          value: IntLit(42, dummyLoc),
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "let binding with matching annotation")
}

let testLetAnnotationMismatch = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        LetStmt({
          mutable_: false,
          name: "x",
          type_: Some(Types.TyBool),
          value: IntLit(42, dummyLoc),
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "let binding with mismatched annotation")
}

// ============================================
// Test: Mutable Assignment
// ============================================

let testMutableAssignment = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        LetStmt({
          mutable_: true,
          name: "x",
          type_: None,
          value: IntLit(1, dummyLoc),
          loc: dummyLoc,
        }),
      ),
      StmtDecl(
        AssignStmt({
          target: Ident("x", dummyLoc2),
          value: IntLit(2, dummyLoc2),
          loc: dummyLoc2,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "mutable variable reassignment with same type")
}

let testImmutableAssignment = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        LetStmt({
          mutable_: false,
          name: "x",
          type_: None,
          value: IntLit(1, dummyLoc),
          loc: dummyLoc,
        }),
      ),
      StmtDecl(
        AssignStmt({
          target: Ident("x", dummyLoc2),
          value: IntLit(2, dummyLoc2),
          loc: dummyLoc2,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "immutable variable reassignment rejected")
}

let testMutableTypeMismatch = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        LetStmt({
          mutable_: true,
          name: "x",
          type_: None,
          value: IntLit(1, dummyLoc),
          loc: dummyLoc,
        }),
      ),
      StmtDecl(
        AssignStmt({
          target: Ident("x", dummyLoc2),
          value: StringLit("oops", dummyLoc2),
          loc: dummyLoc2,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "mutable reassignment with different type")
}

// ============================================
// Test: If Statement
// ============================================

let testIfBoolCondition = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        IfStmt({
          cond: BoolLit(true, dummyLoc),
          then_: [ExprStmt(IntLit(1, dummyLoc))],
          elseifs: [],
          else_: None,
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "if with boolean condition")
}

let testIfNonBoolCondition = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        IfStmt({
          cond: IntLit(42, dummyLoc),
          then_: [ExprStmt(IntLit(1, dummyLoc))],
          elseifs: [],
          else_: None,
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "if with non-boolean condition")
}

// ============================================
// Test: While Loop
// ============================================

let testWhileBoolCondition = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        WhileStmt({
          cond: BoolLit(true, dummyLoc),
          body: [ExprStmt(IntLit(1, dummyLoc))],
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "while with boolean condition")
}

let testWhileNonBoolCondition = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        WhileStmt({
          cond: StringLit("nope", dummyLoc),
          body: [ExprStmt(IntLit(1, dummyLoc))],
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "while with non-boolean condition")
}

// ============================================
// Test: For Loop
// ============================================

let testForOverArray = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ForStmt({
          var: "i",
          iter: Array([IntLit(1, dummyLoc), IntLit(2, dummyLoc)], dummyLoc),
          body: [ExprStmt(Ident("i", dummyLoc))],
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "for over array")
}

let testForOverNonArray = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ForStmt({
          var: "i",
          iter: IntLit(42, dummyLoc),
          body: [ExprStmt(Ident("i", dummyLoc))],
          loc: dummyLoc,
        }),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "for over non-array")
}

// ============================================
// Test: Arithmetic Operators
// ============================================

let testArithmeticIntInt = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ExprStmt(Binary(IntLit(1, dummyLoc), Add, IntLit(2, dummyLoc), dummyLoc)),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "Int + Int")
}

let testArithmeticMixed = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ExprStmt(Binary(IntLit(1, dummyLoc), Mul, FloatLit(2.0, dummyLoc), dummyLoc)),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "Int * Float widens to Float")
}

let testArithmeticStringFails = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ExprStmt(
          Binary(StringLit("a", dummyLoc), Sub, IntLit(1, dummyLoc), dummyLoc),
        ),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "String - Int fails")
}

// ============================================
// Test: Logical Operators
// ============================================

let testLogicalAnd = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ExprStmt(
          Binary(BoolLit(true, dummyLoc), LAnd, BoolLit(false, dummyLoc), dummyLoc),
        ),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "Bool && Bool")
}

let testLogicalNonBool = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ExprStmt(
          Binary(IntLit(1, dummyLoc), LAnd, BoolLit(true, dummyLoc), dummyLoc),
        ),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "Int && Bool fails")
}

// ============================================
// Test: Comparison
// ============================================

let testComparisonReturnsBool = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ExprStmt(Binary(IntLit(1, dummyLoc), Lt, IntLit(2, dummyLoc), dummyLoc)),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "Int < Int returns Bool")
}

// ============================================
// Test: Undefined Variable
// ============================================

let testUndefinedVariable = () => {
  let prog: program = {
    declarations: [
      StmtDecl(ExprStmt(Ident("unknown", dummyLoc))),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertHasErrors(result, "undefined variable")
}

// ============================================
// Test: Gutter Block (skipped)
// ============================================

let testGutterBlockSkipped = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        GutterBlock({tokens: [], recovered: false, loc: dummyLoc}),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "gutter block skipped")
}

// ============================================
// Test: Print Accepts Any Type
// ============================================

let testPrintAnyType = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        PrintStmt({println: false, args: [IntLit(1, dummyLoc), StringLit("hi", dummyLoc)], loc: dummyLoc}),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "print accepts any type")
}

// ============================================
// Test: Function Declaration
// ============================================

let testFunctionDecl = () => {
  let prog: program = {
    declarations: [
      FunctionDecl({
        name: "add",
        params: [
          {name: "a", type_: Some(Types.TyInt), loc: dummyLoc},
          {name: "b", type_: Some(Types.TyInt), loc: dummyLoc},
        ],
        returnType: Some(Types.TyInt),
        body: [
          ExprStmt(Binary(Ident("a", dummyLoc), Add, Ident("b", dummyLoc), dummyLoc)),
        ],
        loc: dummyLoc,
      }),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "function declaration with typed params")
}

// ============================================
// Test: String Concatenation
// ============================================

let testStringConcat = () => {
  let prog: program = {
    declarations: [
      StmtDecl(
        ExprStmt(
          Binary(StringLit("hello ", dummyLoc), Add, StringLit("world", dummyLoc), dummyLoc),
        ),
      ),
    ],
    loc: dummyLoc,
  }
  let result = checkProgram(prog)
  assertNoErrors(result, "String + String concatenation")
}

// ============================================
// Run All Tests
// ============================================

let runAllTests = () => {
  Console.log("=== Error-Lang TypeChecker Tests ===")
  Console.log("")

  // Let bindings
  testLetBinding()
  testLetWithAnnotation()
  testLetAnnotationMismatch()

  // Mutable assignment
  testMutableAssignment()
  testImmutableAssignment()
  testMutableTypeMismatch()

  // If statement
  testIfBoolCondition()
  testIfNonBoolCondition()

  // While loop
  testWhileBoolCondition()
  testWhileNonBoolCondition()

  // For loop
  testForOverArray()
  testForOverNonArray()

  // Arithmetic
  testArithmeticIntInt()
  testArithmeticMixed()
  testArithmeticStringFails()

  // Logical
  testLogicalAnd()
  testLogicalNonBool()

  // Comparison
  testComparisonReturnsBool()

  // Undefined variable
  testUndefinedVariable()

  // Gutter block
  testGutterBlockSkipped()

  // Print
  testPrintAnyType()

  // Function
  testFunctionDecl()

  // String concat
  testStringConcat()

  Console.log("")
  Console.log("=== Tests Complete ===")
}

// Auto-run when loaded
let _ = runAllTests()
