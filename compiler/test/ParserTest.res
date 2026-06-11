// SPDX-License-Identifier: MPL-2.0
// ParserTest.res - Comprehensive tests for the Error-Lang parser
//
// Covers: main/end blocks, let statements, print/println, gutter blocks,
//         if/elseif/else, while loops, for/in loops, function definitions,
//         lambda expressions, arithmetic precedence, ternary expressions,
//         array literals, member access, nested structures, and error cases.

open Types
open TestHelpers

// ============================================
// Helper: extract statement from StmtDecl
// ============================================

let extractStmt = (d: decl): option<stmt> =>
  switch d {
  | StmtDecl(s) => Some(s)
  | _ => None
  }

let extractMainBody = (d: decl): option<array<stmt>> =>
  switch d {
  | MainBlock({body}) => Some(body)
  | _ => None
  }

let extractFuncDecl = (d: decl): option<(string, array<param>, array<stmt>)> =>
  switch d {
  | FunctionDecl({name, params, body}) => Some((name, params, body))
  | _ => None
  }

// ============================================
// Main/End Block Tests
// ============================================

let testMainBlock = () => {
  suite("Parser: main/end blocks")

  // Basic main block
  let prog = parseSource("main\nend")
  assertEqual("main block: decl count", Array.length(prog.declarations), 1)
  switch prog.declarations[0] {
  | Some(MainBlock({body})) =>
    assertEqual("empty main block: body length", Array.length(body), 0)
  | _ => assertTrue("main block parsed", false)
  }

  // Main block with statement
  let prog2 = parseSource("main\nlet x = 1\nend")
  switch prog2.declarations[0] {
  | Some(MainBlock({body})) =>
    assertEqual("main with let: body length", Array.length(body), 1)
  | _ => assertTrue("main with statement parsed", false)
  }

  // Main block with multiple statements
  let prog3 = parseSource("main\nlet x = 1\nlet y = 2\nlet z = 3\nend")
  switch prog3.declarations[0] {
  | Some(MainBlock({body})) =>
    assertEqual("main with 3 stmts: body length", Array.length(body), 3)
  | _ => assertTrue("main with multiple stmts parsed", false)
  }
}

// ============================================
// Let Statement Tests
// ============================================

let testLetStatements = () => {
  suite("Parser: let statements")

  // Simple let
  let prog = parseSource("let x = 42")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({name, mutable_, value}))) => {
      assertEqual("let name", name, "x")
      assertFalse("let not mutable", mutable_)
      switch value {
      | IntLit(n, _) => assertEqual("let value", n, 42)
      | _ => assertTrue("let value is int", false)
      }
    }
  | _ => assertTrue("let statement parsed", false)
  }

  // Mutable let
  let prog2 = parseSource("let mutable counter = 0")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({name, mutable_}))) => {
      assertEqual("mutable let name", name, "counter")
      assertTrue("mutable let is mutable", mutable_)
    }
  | _ => assertTrue("mutable let parsed", false)
  }

  // Let with string value
  let prog3 = parseSource("let greeting = \"hello\"")
  switch prog3.declarations[0] {
  | Some(StmtDecl(LetStmt({name, value}))) => {
      assertEqual("let string name", name, "greeting")
      switch value {
      | StringLit(s, _) => assertEqual("let string value", s, "hello")
      | _ => assertTrue("let value is string", false)
      }
    }
  | _ => assertTrue("let string parsed", false)
  }

  // Let with boolean
  let prog4 = parseSource("let flag = true")
  switch prog4.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | BoolLit(b, _) => assertTrue("let bool value", b)
    | _ => assertTrue("let value is bool", false)
    }
  | _ => assertTrue("let bool parsed", false)
  }

  // Let with nil
  let prog5 = parseSource("let nothing = nil")
  switch prog5.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | NilLit(_) => assertTrue("let nil value", true)
    | _ => assertTrue("let value is nil", false)
    }
  | _ => assertTrue("let nil parsed", false)
  }

  // Let with expression
  let prog6 = parseSource("let sum = 1 + 2")
  switch prog6.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(_, Add, _, _) => assertTrue("let with addition expr", true)
    | _ => assertTrue("let value is binary add", false)
    }
  | _ => assertTrue("let expr parsed", false)
  }
}

// ============================================
// Print/Println Tests
// ============================================

let testPrintStatements = () => {
  suite("Parser: print/println calls")

  // print with single arg
  let prog = parseSource("print(\"hello\")")
  switch prog.declarations[0] {
  | Some(StmtDecl(PrintStmt({println, args}))) => {
      assertFalse("print not println", println)
      assertEqual("print args count", Array.length(args), 1)
    }
  | _ => assertTrue("print parsed", false)
  }

  // println with single arg
  let prog2 = parseSource("println(\"world\")")
  switch prog2.declarations[0] {
  | Some(StmtDecl(PrintStmt({println, args}))) => {
      assertTrue("println is println", println)
      assertEqual("println args count", Array.length(args), 1)
    }
  | _ => assertTrue("println parsed", false)
  }

  // print with multiple args
  let prog3 = parseSource("print(\"a\", \"b\", \"c\")")
  switch prog3.declarations[0] {
  | Some(StmtDecl(PrintStmt({args}))) =>
    assertEqual("print multi-args count", Array.length(args), 3)
  | _ => assertTrue("print multi-args parsed", false)
  }

  // print with expression arg
  let prog4 = parseSource("println(1 + 2)")
  switch prog4.declarations[0] {
  | Some(StmtDecl(PrintStmt({args}))) => {
      assertEqual("println expr: args count", Array.length(args), 1)
      switch args[0] {
      | Some(Binary(_, Add, _, _)) => assertTrue("println arg is Add expr", true)
      | _ => assertTrue("println arg is expression", false)
      }
    }
  | _ => assertTrue("println expr parsed", false)
  }
}

// ============================================
// Gutter Block Tests
// ============================================

let testGutterBlocks = () => {
  suite("Parser: gutter blocks (error injection zones)")

  // Basic gutter block
  let prog = parseSource("gutter\nlet x = broken\nend")
  switch prog.declarations[0] {
  | Some(StmtDecl(GutterBlock({recovered}))) =>
    assertTrue("gutter block recovered", recovered)
  | _ => assertTrue("gutter block parsed", false)
  }

  // Gutter with multiple tokens inside
  let prog2 = parseSource("gutter\na + b * c\n1 2 3\nend")
  switch prog2.declarations[0] {
  | Some(StmtDecl(GutterBlock({tokens, recovered}))) => {
      assertTrue("gutter multi-line recovered", recovered)
      assertTrue("gutter has tokens", Array.length(tokens) > 0)
    }
  | _ => assertTrue("gutter multi-line parsed", false)
  }
}

// ============================================
// If/Elseif/Else Tests
// ============================================

let testIfStatements = () => {
  suite("Parser: if/elseif/else/end")

  // Simple if
  let prog = parseSource("if true\nlet x = 1\nend")
  switch prog.declarations[0] {
  | Some(StmtDecl(IfStmt({cond, then_, elseifs, else_}))) => {
      switch cond {
      | BoolLit(true, _) => assertTrue("if cond is true", true)
      | _ => assertTrue("if cond is bool", false)
      }
      assertEqual("if then body count", Array.length(then_), 1)
      assertEqual("if no elseifs", Array.length(elseifs), 0)
      assertEqual("if no else", else_, None)
    }
  | _ => assertTrue("simple if parsed", false)
  }

  // If/else
  let prog2 = parseSource("if false\nlet x = 1\nelse\nlet y = 2\nend")
  switch prog2.declarations[0] {
  | Some(StmtDecl(IfStmt({then_, else_}))) => {
      assertEqual("if/else then count", Array.length(then_), 1)
      switch else_ {
      | Some(elseBody) => assertEqual("if/else else count", Array.length(elseBody), 1)
      | None => assertTrue("if/else has else body", false)
      }
    }
  | _ => assertTrue("if/else parsed", false)
  }

  // If/elseif/else
  let prog3 = parseSource("if x == 1\nlet a = 1\nelseif x == 2\nlet b = 2\nelse\nlet c = 3\nend")
  switch prog3.declarations[0] {
  | Some(StmtDecl(IfStmt({then_, elseifs, else_}))) => {
      assertEqual("if/elseif: then count", Array.length(then_), 1)
      assertEqual("if/elseif: elseif count", Array.length(elseifs), 1)
      assertTrue("if/elseif: has else", Option.isSome(else_))
    }
  | _ => assertTrue("if/elseif/else parsed", false)
  }

  // Multiple elseifs
  let prog4 = parseSource("if a == 1\nlet x = 1\nelseif a == 2\nlet x = 2\nelseif a == 3\nlet x = 3\nelse\nlet x = 0\nend")
  switch prog4.declarations[0] {
  | Some(StmtDecl(IfStmt({elseifs}))) =>
    assertEqual("multiple elseifs count", Array.length(elseifs), 2)
  | _ => assertTrue("multiple elseifs parsed", false)
  }

  // If with expression condition
  let prog5 = parseSource("if x > 0 and y < 10\nlet z = 1\nend")
  switch prog5.declarations[0] {
  | Some(StmtDecl(IfStmt({cond}))) =>
    switch cond {
    | Binary(_, LAnd, _, _) => assertTrue("if with logical and", true)
    | _ => assertTrue("if cond is binary and", false)
    }
  | _ => assertTrue("if with compound cond parsed", false)
  }
}

// ============================================
// While Loop Tests
// ============================================

let testWhileLoops = () => {
  suite("Parser: while/end loops")

  // Simple while
  let prog = parseSource("while true\nlet x = 1\nend")
  switch prog.declarations[0] {
  | Some(StmtDecl(WhileStmt({cond, body}))) => {
      switch cond {
      | BoolLit(true, _) => assertTrue("while cond is true", true)
      | _ => assertTrue("while cond is bool", false)
      }
      assertEqual("while body count", Array.length(body), 1)
    }
  | _ => assertTrue("while loop parsed", false)
  }

  // While with comparison
  let prog2 = parseSource("while i < 10\nlet i = i + 1\nend")
  switch prog2.declarations[0] {
  | Some(StmtDecl(WhileStmt({cond}))) =>
    switch cond {
    | Binary(_, Lt, _, _) => assertTrue("while with less-than cond", true)
    | _ => assertTrue("while cond is comparison", false)
    }
  | _ => assertTrue("while with comparison parsed", false)
  }

  // While with break
  let prog3 = parseSource("while true\nbreak\nend")
  switch prog3.declarations[0] {
  | Some(StmtDecl(WhileStmt({body}))) =>
    switch body[0] {
    | Some(BreakStmt(_)) => assertTrue("while contains break", true)
    | _ => assertTrue("break in while body", false)
    }
  | _ => assertTrue("while with break parsed", false)
  }

  // While with continue
  let prog4 = parseSource("while true\ncontinue\nend")
  switch prog4.declarations[0] {
  | Some(StmtDecl(WhileStmt({body}))) =>
    switch body[0] {
    | Some(ContinueStmt(_)) => assertTrue("while contains continue", true)
    | _ => assertTrue("continue in while body", false)
    }
  | _ => assertTrue("while with continue parsed", false)
  }
}

// ============================================
// For Loop Tests
// ============================================

let testForLoops = () => {
  suite("Parser: for/in/end loops")

  // Simple for
  let prog = parseSource("for i in items\nlet x = i\nend")
  switch prog.declarations[0] {
  | Some(StmtDecl(ForStmt({var, body}))) => {
      assertEqual("for var name", var, "i")
      assertEqual("for body count", Array.length(body), 1)
    }
  | _ => assertTrue("for loop parsed", false)
  }

  // For with array literal
  let prog2 = parseSource("for x in [1, 2, 3]\nlet y = x\nend")
  switch prog2.declarations[0] {
  | Some(StmtDecl(ForStmt({var, iter}))) => {
      assertEqual("for-array var", var, "x")
      switch iter {
      | Array(elems, _) => assertEqual("for-array iter count", Array.length(elems), 3)
      | _ => assertTrue("for iter is array", false)
      }
    }
  | _ => assertTrue("for with array parsed", false)
  }

  // For with multiple body statements
  let prog3 = parseSource("for i in list\nlet a = 1\nlet b = 2\nend")
  switch prog3.declarations[0] {
  | Some(StmtDecl(ForStmt({body}))) =>
    assertEqual("for multi-stmt body", Array.length(body), 2)
  | _ => assertTrue("for multi-stmt parsed", false)
  }
}

// ============================================
// Function Definition Tests
// ============================================

let testFunctionDefinitions = () => {
  suite("Parser: function definitions")

  // No-param function
  let prog = parseSource("function greet()\nprintln(\"hi\")\nend")
  switch prog.declarations[0] {
  | Some(FunctionDecl({name, params, body})) => {
      assertEqual("func name", name, "greet")
      assertEqual("func no params", Array.length(params), 0)
      assertEqual("func body count", Array.length(body), 1)
    }
  | _ => assertTrue("no-param function parsed", false)
  }

  // Single-param function
  let prog2 = parseSource("function double(x)\nreturn x * 2\nend")
  switch prog2.declarations[0] {
  | Some(FunctionDecl({name, params, body})) => {
      assertEqual("func1 name", name, "double")
      assertEqual("func1 param count", Array.length(params), 1)
      switch params[0] {
      | Some({name: pname}) => assertEqual("func1 param name", pname, "x")
      | None => assertTrue("func1 has param", false)
      }
      assertEqual("func1 body count", Array.length(body), 1)
    }
  | _ => assertTrue("single-param function parsed", false)
  }

  // Multi-param function
  let prog3 = parseSource("function add(a, b)\nreturn a + b\nend")
  switch prog3.declarations[0] {
  | Some(FunctionDecl({name, params})) => {
      assertEqual("func2 name", name, "add")
      assertEqual("func2 param count", Array.length(params), 2)
    }
  | _ => assertTrue("multi-param function parsed", false)
  }

  // Function with return
  let prog4 = parseSource("function identity(x)\nreturn x\nend")
  switch prog4.declarations[0] {
  | Some(FunctionDecl({body})) =>
    switch body[0] {
    | Some(ReturnStmt({value})) =>
      assertTrue("return has value", Option.isSome(value))
    | _ => assertTrue("function has return stmt", false)
    }
  | _ => assertTrue("function with return parsed", false)
  }

  // Return without value
  let prog5 = parseSource("function noop()\nreturn\nend")
  switch prog5.declarations[0] {
  | Some(FunctionDecl({body})) =>
    switch body[0] {
    | Some(ReturnStmt({value})) =>
      assertEqual("bare return: no value", value, None)
    | _ => assertTrue("function has bare return", false)
    }
  | _ => assertTrue("bare return function parsed", false)
  }
}

// ============================================
// Lambda Expression Tests
// ============================================

let testLambdaExpressions = () => {
  suite("Parser: lambda expressions")

  // Single-param lambda
  let prog = parseSource("let f = fn(x) -> x + 1")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Lambda(params, _, LambdaExpr(_body), _) => {
        assertEqual("lambda param count", Array.length(params), 1)
        switch params[0] {
        | Some({name}) => assertEqual("lambda param name", name, "x")
        | None => assertTrue("lambda has param", false)
        }
      }
    | _ => assertTrue("let value is lambda", false)
    }
  | _ => assertTrue("lambda parsed", false)
  }

  // Multi-param lambda
  let prog2 = parseSource("let add = fn(a, b) -> a + b")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Lambda(params, _, _, _) =>
      assertEqual("multi-param lambda count", Array.length(params), 2)
    | _ => assertTrue("multi-param lambda value", false)
    }
  | _ => assertTrue("multi-param lambda parsed", false)
  }

  // No-param lambda
  let prog3 = parseSource("let zero = fn() -> 0")
  switch prog3.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Lambda(params, _, _, _) =>
      assertEqual("no-param lambda count", Array.length(params), 0)
    | _ => assertTrue("no-param lambda value", false)
    }
  | _ => assertTrue("no-param lambda parsed", false)
  }
}

// ============================================
// Arithmetic Precedence Tests
// ============================================

let testArithmeticPrecedence = () => {
  suite("Parser: arithmetic precedence")

  // Multiplication binds tighter than addition: 1 + 2 * 3 = 1 + (2*3)
  let prog = parseSource("let x = 1 + 2 * 3")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(IntLit(1, _), Add, Binary(IntLit(2, _), Mul, IntLit(3, _), _), _) =>
      assertTrue("precedence: 1 + (2 * 3)", true)
    | _ => assertTrue("precedence: add over mul", false)
    }
  | _ => assertTrue("precedence test parsed", false)
  }

  // Parenthesized override: (1 + 2) * 3
  let prog2 = parseSource("let x = (1 + 2) * 3")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(Binary(IntLit(1, _), Add, IntLit(2, _), _), Mul, IntLit(3, _), _) =>
      assertTrue("precedence: (1 + 2) * 3", true)
    | _ => assertTrue("precedence: parens override", false)
    }
  | _ => assertTrue("paren precedence test parsed", false)
  }

  // Subtraction: a - b - c = (a - b) - c (left-associative)
  let prog3 = parseSource("let x = 10 - 3 - 2")
  switch prog3.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(Binary(_, Sub, _, _), Sub, IntLit(2, _), _) =>
      assertTrue("left-assoc subtraction", true)
    | _ => assertTrue("subtraction left-assoc", false)
    }
  | _ => assertTrue("left-assoc test parsed", false)
  }

  // Division: a / b * c = (a / b) * c (left-associative)
  let prog4 = parseSource("let x = 12 / 3 * 2")
  switch prog4.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(Binary(_, Div, _, _), Mul, _, _) =>
      assertTrue("left-assoc div/mul", true)
    | _ => assertTrue("div/mul left-assoc", false)
    }
  | _ => assertTrue("div/mul test parsed", false)
  }

  // Modulo same precedence as mul/div
  let prog5 = parseSource("let x = 10 % 3 + 1")
  switch prog5.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(Binary(_, Mod, _, _), Add, _, _) =>
      assertTrue("modulo precedence over add", true)
    | _ => assertTrue("modulo precedence", false)
    }
  | _ => assertTrue("modulo test parsed", false)
  }

  // Unary negation
  let prog6 = parseSource("let x = -5")
  switch prog6.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Unary(Neg, IntLit(5, _), _) =>
      assertTrue("unary negation", true)
    | _ => assertTrue("unary neg parsed", false)
    }
  | _ => assertTrue("unary neg test parsed", false)
  }

  // Unary not
  let prog7 = parseSource("let x = not true")
  switch prog7.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Unary(LNot, BoolLit(true, _), _) =>
      assertTrue("unary logical not", true)
    | _ => assertTrue("unary not parsed", false)
    }
  | _ => assertTrue("unary not test parsed", false)
  }

  // Bitwise not
  let prog8 = parseSource("let x = ~0")
  switch prog8.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Unary(BNot, IntLit(0, _), _) =>
      assertTrue("bitwise not", true)
    | _ => assertTrue("bitwise not parsed", false)
    }
  | _ => assertTrue("bitwise not test parsed", false)
  }
}

// ============================================
// Comparison and Logical Operator Tests
// ============================================

let testComparisonsAndLogical = () => {
  suite("Parser: comparisons and logical operators")

  // Equality
  let prog = parseSource("let x = a == b")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(_, Eq, _, _) => assertTrue("equality operator", true)
    | _ => assertTrue("equality parsed", false)
    }
  | _ => assertTrue("equality test parsed", false)
  }

  // Not equal
  let prog2 = parseSource("let x = a != b")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(_, Neq, _, _) => assertTrue("not-equal operator", true)
    | _ => assertTrue("neq parsed", false)
    }
  | _ => assertTrue("neq test parsed", false)
  }

  // Less than
  let prog3 = parseSource("let x = a < b")
  switch prog3.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(_, Lt, _, _) => assertTrue("less-than operator", true)
    | _ => assertTrue("lt parsed", false)
    }
  | _ => assertTrue("lt test parsed", false)
  }

  // Greater equal
  let prog4 = parseSource("let x = a >= b")
  switch prog4.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(_, Gte, _, _) => assertTrue("greater-equal operator", true)
    | _ => assertTrue("gte parsed", false)
    }
  | _ => assertTrue("gte test parsed", false)
  }

  // Logical AND: comparison binds tighter
  let prog5 = parseSource("let x = a > 0 and b > 0")
  switch prog5.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(Binary(_, Gt, _, _), LAnd, Binary(_, Gt, _, _), _) =>
      assertTrue("logical AND with comparisons", true)
    | _ => assertTrue("logical AND parsed", false)
    }
  | _ => assertTrue("logical AND test parsed", false)
  }

  // Logical OR
  let prog6 = parseSource("let x = a or b")
  switch prog6.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(_, LOr, _, _) => assertTrue("logical OR operator", true)
    | _ => assertTrue("logical OR parsed", false)
    }
  | _ => assertTrue("logical OR test parsed", false)
  }

  // OR has lower precedence than AND
  // a or b and c => a or (b and c)
  let prog7 = parseSource("let x = a or b and c")
  switch prog7.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Binary(_, LOr, Binary(_, LAnd, _, _), _) =>
      assertTrue("OR lower precedence than AND", true)
    | _ => assertTrue("OR/AND precedence", false)
    }
  | _ => assertTrue("OR/AND precedence test parsed", false)
  }
}

// ============================================
// Ternary Expression Tests
// ============================================

let testTernaryExpressions = () => {
  suite("Parser: ternary expressions (? :)")

  // Basic ternary
  let prog = parseSource("let x = true ? 1 : 0")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Ternary(BoolLit(true, _), IntLit(1, _), IntLit(0, _), _) =>
      assertTrue("basic ternary", true)
    | _ => assertTrue("ternary parsed", false)
    }
  | _ => assertTrue("ternary test parsed", false)
  }

  // Ternary with comparison condition
  let prog2 = parseSource("let x = a > 0 ? a : 0")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Ternary(Binary(_, Gt, _, _), _, IntLit(0, _), _) =>
      assertTrue("ternary with comparison", true)
    | _ => assertTrue("ternary comparison parsed", false)
    }
  | _ => assertTrue("ternary comparison test parsed", false)
  }

  // Ternary with expression branches
  let prog3 = parseSource("let x = flag ? 1 + 2 : 3 * 4")
  switch prog3.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Ternary(_, Binary(_, Add, _, _), Binary(_, Mul, _, _), _) =>
      assertTrue("ternary with expression branches", true)
    | _ => assertTrue("ternary expr branches parsed", false)
    }
  | _ => assertTrue("ternary expr test parsed", false)
  }
}

// ============================================
// Array Literal Tests
// ============================================

let testArrayLiterals = () => {
  suite("Parser: array literals")

  // Empty array
  let prog = parseSource("let a = []")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Array(elems, _) => assertEqual("empty array", Array.length(elems), 0)
    | _ => assertTrue("empty array parsed", false)
    }
  | _ => assertTrue("empty array test parsed", false)
  }

  // Single element
  let prog2 = parseSource("let a = [42]")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Array(elems, _) => {
        assertEqual("single element array", Array.length(elems), 1)
        switch elems[0] {
        | Some(IntLit(42, _)) => assertTrue("array elem is 42", true)
        | _ => assertTrue("array elem is int", false)
        }
      }
    | _ => assertTrue("single elem array parsed", false)
    }
  | _ => assertTrue("single elem test parsed", false)
  }

  // Multiple elements
  let prog3 = parseSource("let a = [1, 2, 3, 4, 5]")
  switch prog3.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Array(elems, _) => assertEqual("multi-elem array count", Array.length(elems), 5)
    | _ => assertTrue("multi-elem array parsed", false)
    }
  | _ => assertTrue("multi-elem test parsed", false)
  }

  // Mixed types in array
  let prog4 = parseSource("let a = [1, \"two\", true, nil]")
  switch prog4.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Array(elems, _) => assertEqual("mixed array count", Array.length(elems), 4)
    | _ => assertTrue("mixed array parsed", false)
    }
  | _ => assertTrue("mixed array test parsed", false)
  }

  // Nested arrays
  let prog5 = parseSource("let a = [[1, 2], [3, 4]]")
  switch prog5.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Array(elems, _) => {
        assertEqual("nested array outer count", Array.length(elems), 2)
        switch elems[0] {
        | Some(Array(inner, _)) => assertEqual("nested array inner count", Array.length(inner), 2)
        | _ => assertTrue("nested array inner is array", false)
        }
      }
    | _ => assertTrue("nested array parsed", false)
    }
  | _ => assertTrue("nested array test parsed", false)
  }
}

// ============================================
// Member Access (Dot Notation) Tests
// ============================================

let testMemberAccess = () => {
  suite("Parser: member access (dot notation)")

  // Simple member access
  let prog = parseSource("let x = obj.field")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Member(Ident("obj", _), "field", _) =>
      assertTrue("simple member access", true)
    | _ => assertTrue("member access parsed", false)
    }
  | _ => assertTrue("member access test parsed", false)
  }

  // Chained member access
  let prog2 = parseSource("let x = a.b.c")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({value}))) =>
    switch value {
    | Member(Member(Ident("a", _), "b", _), "c", _) =>
      assertTrue("chained member access", true)
    | _ => assertTrue("chained member parsed", false)
    }
  | _ => assertTrue("chained member test parsed", false)
  }
}

// ============================================
// Function Call Tests
// ============================================

let testFunctionCalls = () => {
  suite("Parser: function calls")

  // No-arg call
  let prog = parseSource("foo()")
  switch prog.declarations[0] {
  | Some(StmtDecl(ExprStmt(Call(Ident("foo", _), args, _)))) =>
    assertEqual("no-arg call", Array.length(args), 0)
  | _ => assertTrue("no-arg call parsed", false)
  }

  // Single-arg call
  let prog2 = parseSource("bar(42)")
  switch prog2.declarations[0] {
  | Some(StmtDecl(ExprStmt(Call(Ident("bar", _), args, _)))) =>
    assertEqual("single-arg call", Array.length(args), 1)
  | _ => assertTrue("single-arg call parsed", false)
  }

  // Multi-arg call
  let prog3 = parseSource("baz(1, 2, 3)")
  switch prog3.declarations[0] {
  | Some(StmtDecl(ExprStmt(Call(Ident("baz", _), args, _)))) =>
    assertEqual("multi-arg call", Array.length(args), 3)
  | _ => assertTrue("multi-arg call parsed", false)
  }

  // Chained call (method-like)
  let prog4 = parseSource("a.b()")
  switch prog4.declarations[0] {
  | Some(StmtDecl(ExprStmt(Call(Member(Ident("a", _), "b", _), args, _)))) =>
    assertEqual("method-like call", Array.length(args), 0)
  | _ => assertTrue("method-like call parsed", false)
  }

  // Index access
  let prog5 = parseSource("arr[0]")
  switch prog5.declarations[0] {
  | Some(StmtDecl(ExprStmt(Index(Ident("arr", _), IntLit(0, _), _)))) =>
    assertTrue("index access", true)
  | _ => assertTrue("index access parsed", false)
  }
}

// ============================================
// Nested Structure Tests
// ============================================

let testNestedStructures = () => {
  suite("Parser: nested structures")

  // If inside while
  let prog = parseSource("while true\nif x > 0\nbreak\nend\nend")
  switch prog.declarations[0] {
  | Some(StmtDecl(WhileStmt({body}))) => {
      assertEqual("while body count", Array.length(body), 1)
      switch body[0] {
      | Some(IfStmt({then_})) =>
        switch then_[0] {
        | Some(BreakStmt(_)) => assertTrue("nested if/break in while", true)
        | _ => assertTrue("break inside nested if", false)
        }
      | _ => assertTrue("if inside while", false)
      }
    }
  | _ => assertTrue("nested while/if parsed", false)
  }

  // Function with if
  let prog2 = parseSource("function abs(x)\nif x < 0\nreturn -x\nelse\nreturn x\nend\nend")
  switch prog2.declarations[0] {
  | Some(FunctionDecl({name, body})) => {
      assertEqual("abs function name", name, "abs")
      assertEqual("abs body has if", Array.length(body), 1)
      switch body[0] {
      | Some(IfStmt({then_, else_})) => {
          assertEqual("abs if then", Array.length(then_), 1)
          assertTrue("abs if has else", Option.isSome(else_))
        }
      | _ => assertTrue("abs body is if stmt", false)
      }
    }
  | _ => assertTrue("function with if parsed", false)
  }

  // Nested for loops
  let prog3 = parseSource("for i in rows\nfor j in cols\nlet cell = i * j\nend\nend")
  switch prog3.declarations[0] {
  | Some(StmtDecl(ForStmt({var: outerVar, body}))) => {
      assertEqual("outer for var", outerVar, "i")
      switch body[0] {
      | Some(ForStmt({var: innerVar, body: innerBody})) => {
          assertEqual("inner for var", innerVar, "j")
          assertEqual("inner for body count", Array.length(innerBody), 1)
        }
      | _ => assertTrue("nested for inside for", false)
      }
    }
  | _ => assertTrue("nested for loops parsed", false)
  }

  // Main with multiple constructs
  let prog4 = parseSource("main\nlet x = 0\nwhile x < 5\nlet x = x + 1\nend\nprintln(x)\nend")
  switch prog4.declarations[0] {
  | Some(MainBlock({body})) =>
    assertEqual("main with mixed stmts", Array.length(body), 3)
  | _ => assertTrue("main with mixed stmts parsed", false)
  }
}

// ============================================
// Struct Declaration Tests
// ============================================

let testStructDeclarations = () => {
  suite("Parser: struct declarations")

  // Basic struct
  let prog = parseSource("struct Point\nx: Int\ny: Int\nend")
  switch prog.declarations[0] {
  | Some(StructDecl({name, fields})) => {
      assertEqual("struct name", name, "Point")
      assertEqual("struct field count", Array.length(fields), 2)
    }
  | _ => assertTrue("struct parsed", false)
  }

  // Struct with different types
  let prog2 = parseSource("struct Person\nname: String\nage: Int\nactive: Bool\nend")
  switch prog2.declarations[0] {
  | Some(StructDecl({name, fields})) => {
      assertEqual("person struct name", name, "Person")
      assertEqual("person struct fields", Array.length(fields), 3)
    }
  | _ => assertTrue("multi-type struct parsed", false)
  }
}

// ============================================
// Expression Statement Tests
// ============================================

let testExpressionStatements = () => {
  suite("Parser: expression statements")

  // Bare expression
  let prog = parseSource("42")
  switch prog.declarations[0] {
  | Some(StmtDecl(ExprStmt(IntLit(42, _)))) =>
    assertTrue("bare int expression", true)
  | _ => assertTrue("expr stmt parsed", false)
  }

  // Bare identifier
  let prog2 = parseSource("x")
  switch prog2.declarations[0] {
  | Some(StmtDecl(ExprStmt(Ident("x", _)))) =>
    assertTrue("bare identifier expression", true)
  | _ => assertTrue("ident expr stmt parsed", false)
  }

  // Grouped expression
  let prog3 = parseSource("(1 + 2)")
  switch prog3.declarations[0] {
  | Some(StmtDecl(ExprStmt(Binary(_, Add, _, _)))) =>
    assertTrue("grouped expression", true)
  | _ => assertTrue("grouped expr parsed", false)
  }
}

// ============================================
// Error Case Tests
// ============================================

let testErrorCases = () => {
  suite("Parser: error cases")

  // Missing 'end' for if (parser still produces something)
  let (_prog, diags) = parseWithDiagnostics("if true\nlet x = 1")
  // The parser should still produce output even with missing end
  assertTrue(
    "missing end: parser produces output",
    true,
  )

  // Missing equals in let
  let (_prog2, diags2) = parseWithDiagnostics("let x 42")
  assertTrue(
    "missing equals: diagnostics produced",
    Array.length(diags2) > 0,
  )

  // Missing identifier after let
  let (_prog3, diags3) = parseWithDiagnostics("let = 42")
  assertTrue(
    "missing ident after let: diagnostics",
    Array.length(diags3) > 0,
  )

  // Missing paren in print
  let (_prog4, diags4) = parseWithDiagnostics("print \"hello\")")
  assertTrue(
    "missing open paren: diagnostics",
    Array.length(diags4) > 0,
  )

  // Unexpected token
  let (_prog5, diags5) = parseWithDiagnostics("let x = )")
  assertTrue(
    "unexpected token: diagnostics",
    Array.length(diags5) > 0,
  )

  // Missing 'in' in for loop
  let (_prog6, diags6) = parseWithDiagnostics("for i items\nend")
  assertTrue(
    "missing 'in': diagnostics",
    Array.length(diags6) > 0,
  )

  // Missing function name
  let (_prog7, diags7) = parseWithDiagnostics("function ()\nend")
  assertTrue(
    "missing function name: diagnostics",
    Array.length(diags7) > 0,
  )

  // Unterminated string (lexer error propagated)
  let (_prog8, diags8) = parseWithDiagnostics("let x = \"unclosed")
  assertTrue(
    "unterminated string: diagnostics",
    Array.length(diags8) > 0,
  )

  // Gutter without end
  let (_prog9, diags9) = parseWithDiagnostics("gutter\nbroken code here")
  assertTrue(
    "gutter without end: diagnostics",
    Array.length(diags9) > 0,
  )

  // Missing identifier after dot
  let (_prog10, diags10) = parseWithDiagnostics("obj.123")
  assertTrue(
    "missing ident after dot: diagnostics",
    Array.length(diags10) > 0,
  )
}

// ============================================
// Multiple Declaration Tests
// ============================================

let testMultipleDeclarations = () => {
  suite("Parser: multiple top-level declarations")

  // Function then main
  let prog = parseSource("function foo()\nreturn 1\nend\n\nmain\nlet x = foo()\nend")
  assertEqual("func+main: decl count", Array.length(prog.declarations), 2)
  switch prog.declarations[0] {
  | Some(FunctionDecl({name})) => assertEqual("first decl is function", name, "foo")
  | _ => assertTrue("first decl is function", false)
  }
  switch prog.declarations[1] {
  | Some(MainBlock(_)) => assertTrue("second decl is main", true)
  | _ => assertTrue("second decl is main", false)
  }

  // Multiple functions
  let prog2 = parseSource("function a()\nend\nfunction b()\nend\nfunction c()\nend")
  assertEqual("three functions: decl count", Array.length(prog2.declarations), 3)

  // Mixed top-level
  let prog3 = parseSource("let x = 1\nfunction f()\nend\nlet y = 2")
  assertEqual("mixed top-level: decl count", Array.length(prog3.declarations), 3)
}

// ============================================
// Echo type annotations
// ============================================

let testEchoAnnotations = () => {
  suite("Parser: Echo type annotations")

  // Full fibre annotation: Echo<Int, String>
  let prog = parseSource("let e: Echo<Int, String> = 1")
  switch prog.declarations[0] {
  | Some(StmtDecl(LetStmt({name, type_}))) => {
      assertEqual("echo let name", name, "e")
      switch type_ {
      | Some(TyEcho(Some(TyInt), Some(TyString))) =>
        assertTrue("annotation is Echo<Int, String>", true)
      | _ => assertTrue("annotation is Echo<Int, String>", false)
      }
    }
  | _ => assertTrue("echo annotated let parsed", false)
  }

  // Residue annotation: EchoR<Int, String>
  let prog2 = parseSource("let r: EchoR<Int, String> = 1")
  switch prog2.declarations[0] {
  | Some(StmtDecl(LetStmt({type_}))) =>
    switch type_ {
    | Some(TyEchoResidue(Some(TyInt), Some(TyString))) =>
      assertTrue("annotation is EchoR<Int, String>", true)
    | _ => assertTrue("annotation is EchoR<Int, String>", false)
    }
  | _ => assertTrue("residue annotated let parsed", false)
  }

  // Sugar: bare Echo (opaque) and single-arg Echo<Int>
  let prog3 = parseSource("let e: Echo = 1")
  switch prog3.declarations[0] {
  | Some(StmtDecl(LetStmt({type_}))) =>
    switch type_ {
    | Some(TyEcho(None, None)) => assertTrue("bare Echo is opaque", true)
    | _ => assertTrue("bare Echo is opaque", false)
    }
  | _ => assertTrue("bare echo annotated let parsed", false)
  }

  let prog4 = parseSource("let e: Echo<Int> = 1")
  switch prog4.declarations[0] {
  | Some(StmtDecl(LetStmt({type_}))) =>
    switch type_ {
    | Some(TyEcho(Some(TyInt), None)) => assertTrue("Echo<Int> infers codomain", true)
    | _ => assertTrue("Echo<Int> infers codomain", false)
    }
  | _ => assertTrue("single-arg echo annotated let parsed", false)
  }

  // Nested form: the closing `>>` must be split, not read as a shift token.
  let prog5 = parseSource("let e: Echo<Echo<Int, String>> = 1")
  switch prog5.declarations[0] {
  | Some(StmtDecl(LetStmt({type_}))) =>
    switch type_ {
    | Some(TyEcho(Some(TyEcho(Some(TyInt), Some(TyString))), None)) =>
      assertTrue("nested Echo<Echo<Int, String>> parses (>> split)", true)
    | _ => assertTrue("nested Echo<Echo<Int, String>> parses (>> split)", false)
    }
  | _ => assertTrue("nested echo annotated let parsed", false)
  }

  // Malformed Echo annotation must fail clearly (a diagnostic is raised).
  let (_, diags) = parseWithDiagnostics("let e: Echo<Int = 1")
  assertTrue("unclosed Echo type argument raises a diagnostic", Array.length(diags) > 0)
}

// ============================================
// Run All Parser Tests
// ============================================

let runAll = () => {
  Console.log("\n========================================")
  Console.log("  ERROR-LANG PARSER TESTS")
  Console.log("========================================")

  testMainBlock()
  testLetStatements()
  testPrintStatements()
  testGutterBlocks()
  testIfStatements()
  testWhileLoops()
  testForLoops()
  testFunctionDefinitions()
  testLambdaExpressions()
  testArithmeticPrecedence()
  testComparisonsAndLogical()
  testTernaryExpressions()
  testArrayLiterals()
  testMemberAccess()
  testFunctionCalls()
  testNestedStructures()
  testStructDeclarations()
  testEchoAnnotations()
  testExpressionStatements()
  testErrorCases()
  testMultipleDeclarations()

  summarize()
}

runAll()
