// SPDX-License-Identifier: PMPL-1.0-or-later
// IncrementalParserTest.res - Tests for the Error-Lang incremental parser

open Types
open TestHelpers

// ============================================
// Helpers
// ============================================

let makeIncremental = (source: string): IncrementalParser.t => {
  IncrementalParser.make(source, "<test>")
}

let findSubstring = (haystack: string, needle: string): int => {
  let pos = String.indexOf(haystack, needle)
  if pos < 0 {
    0
  } else {
    pos
  }
}

// ============================================
// Tests
// ============================================

let testEditInsideFunctionBody = () => {
  suite("IncrementalParser: edit inside function body")

  let source = "function foo()\n  let x = 42\nend\n\nfunction bar()\n  let y = 99\nend\n"
  let t = makeIncremental(source)
  let originalCount = Array.length(IncrementalParser.items(t))
  assertTrue("has at least 2 items", originalCount >= 2)

  // Change 42 to 100
  let pos = findSubstring(source, "42")
  let _diags = IncrementalParser.edit(t, {start: pos, oldEnd: pos + 2, newText: "100"}, "<test>")
  assertEqual("item count unchanged", Array.length(IncrementalParser.items(t)), originalCount)
  assertTrue("source contains 100", String.includes(IncrementalParser.source(t), "100"))
}

let testAddNewFunction = () => {
  suite("IncrementalParser: add new function")

  let source = "function foo()\n  let x = 1\nend\n"
  let t = makeIncremental(source)
  let originalCount = Array.length(IncrementalParser.items(t))

  let newFn = "\nfunction bar()\n  let y = 2\nend\n"
  let _diags = IncrementalParser.edit(t, {
    start: String.length(source),
    oldEnd: String.length(source),
    newText: newFn,
  }, "<test>")

  assertTrue("at least as many items", Array.length(IncrementalParser.items(t)) >= originalCount)
  assertTrue("source contains bar", String.includes(IncrementalParser.source(t), "function bar"))
}

let testDeleteFunction = () => {
  suite("IncrementalParser: delete function")

  let source = "function a()\n  let x = 1\nend\n\nfunction b()\n  let y = 2\nend\n\nfunction c()\n  let z = 3\nend\n"
  let t = makeIncremental(source)
  let originalCount = Array.length(IncrementalParser.items(t))

  // Delete function b
  let bStart = findSubstring(source, "function b")
  let bEnd = findSubstring(source, "function c")
  let _diags = IncrementalParser.edit(t, {start: bStart, oldEnd: bEnd, newText: ""}, "<test>")

  assertTrue("fewer items", Array.length(IncrementalParser.items(t)) < originalCount)
  assertFalse("no function b", String.includes(IncrementalParser.source(t), "function b"))
  assertTrue("still has function c", String.includes(IncrementalParser.source(t), "function c"))
}

let testEditFunctionSignature = () => {
  suite("IncrementalParser: edit function signature")

  let source = "function add(x, y)\n  x\nend\n"
  let t = makeIncremental(source)

  let pos = findSubstring(source, "add")
  let _diags = IncrementalParser.edit(t, {start: pos, oldEnd: pos + 3, newText: "sum"}, "<test>")

  assertTrue("source contains sum", String.includes(IncrementalParser.source(t), "function sum"))
  assertFalse("no function add", String.includes(IncrementalParser.source(t), "function add"))
  assertTrue("has at least 1 item", Array.length(IncrementalParser.items(t)) >= 1)
}

let testEditAcrossBoundary = () => {
  suite("IncrementalParser: edit across boundary")

  let source = "function foo()\n  1\nend\n\nfunction bar()\n  2\nend\n"
  let t = makeIncremental(source)

  // Replace everything with a single function
  let _diags = IncrementalParser.edit(t, {
    start: 0,
    oldEnd: String.length(source),
    newText: "function merged()\n  42\nend\n",
  }, "<test>")

  assertTrue("source contains merged", String.includes(IncrementalParser.source(t), "function merged"))
}

let testInsertBetweenItems = () => {
  suite("IncrementalParser: insert between items")

  let source = "function foo()\n  1\nend\n\nfunction bar()\n  2\nend\n"
  let t = makeIncremental(source)
  let originalCount = Array.length(IncrementalParser.items(t))

  let insertPos = findSubstring(source, "function bar")
  let newFn = "function middle()\n  5\nend\n\n"
  let _diags = IncrementalParser.edit(t, {
    start: insertPos,
    oldEnd: insertPos,
    newText: newFn,
  }, "<test>")

  assertTrue("at least as many items", Array.length(IncrementalParser.items(t)) >= originalCount)
  assertTrue("source contains middle", String.includes(IncrementalParser.source(t), "function middle"))
}

let testNoopEdit = () => {
  suite("IncrementalParser: no-op edit")

  let source = "function foo()\n  42\nend\n"
  let t = makeIncremental(source)
  let originalCount = Array.length(IncrementalParser.items(t))

  let pos = findSubstring(source, "42")
  let _diags = IncrementalParser.edit(t, {start: pos, oldEnd: pos + 2, newText: "42"}, "<test>")

  assertEqual("same count", Array.length(IncrementalParser.items(t)), originalCount)
  assertEqual("same source", IncrementalParser.source(t), source)
}

let testEditSyntaxError = () => {
  suite("IncrementalParser: edit producing syntax error")

  let source = "function foo()\n  42\nend\n"
  let t = makeIncremental(source)

  // Break the function by removing "end"
  let endPos = findSubstring(source, "end")
  let _diags = IncrementalParser.edit(t, {start: endPos, oldEnd: endPos + 3, newText: ""}, "<test>")

  // Should not crash
  assertTrue("source changed", String.length(IncrementalParser.source(t)) > 0)
}

let testMultipleSequentialEdits = () => {
  suite("IncrementalParser: multiple sequential edits")

  let source = "function a()\n  1\nend\nfunction b()\n  2\nend\nfunction c()\n  3\nend\n"
  let t = makeIncremental(source)

  // Edit 1: change a's body
  let pos1 = findSubstring(IncrementalParser.source(t), "  1\n") + 2
  let _ = IncrementalParser.edit(t, {start: pos1, oldEnd: pos1 + 1, newText: "10"}, "<test>")
  assertTrue("source has 10", String.includes(IncrementalParser.source(t), "10"))

  // Edit 2: change c's body
  let pos2 = findSubstring(IncrementalParser.source(t), "  3\n") + 2
  let _ = IncrementalParser.edit(t, {start: pos2, oldEnd: pos2 + 1, newText: "30"}, "<test>")
  assertTrue("source has 30", String.includes(IncrementalParser.source(t), "30"))
  assertTrue("source still has 2", String.includes(IncrementalParser.source(t), "  2"))
}

let testFullAstReconstruction = () => {
  suite("IncrementalParser: full AST reconstruction")

  let source = "function foo()\n  42\nend\n\nfunction bar()\n  99\nend\n"
  let t = makeIncremental(source)
  let ast = IncrementalParser.fullAst(t)
  assertTrue("has declarations", Array.length(ast.declarations) >= 2)
}

let testEmptySource = () => {
  suite("IncrementalParser: empty source")

  let t = makeIncremental("")
  assertEqual("0 items", Array.length(IncrementalParser.items(t)), 0)
  let ast = IncrementalParser.fullAst(t)
  assertEqual("0 declarations", Array.length(ast.declarations), 0)
}

let testCachedDeclKindTags = () => {
  suite("IncrementalParser: cached decl kind tags")

  let source = "function foo()\n  1\nend\n"
  let t = makeIncremental(source)
  let items = IncrementalParser.items(t)
  assertTrue("has items", Array.length(items) >= 1)
  assertEqual("first is Function", items[0].kind, "Function")
}

let testMainBlock = () => {
  suite("IncrementalParser: main block")

  let source = "main\n  println(\"hello\")\nend\n"
  let t = makeIncremental(source)
  let items = IncrementalParser.items(t)
  assertTrue("has items", Array.length(items) >= 1)
  assertEqual("first is Main", items[0].kind, "Main")
}

let testReplaceEntireSource = () => {
  suite("IncrementalParser: replace entire source")

  let source = "function old()\n  0\nend\n"
  let t = makeIncremental(source)

  let newSource = "function new_fn()\n  42\nend\n"
  let _ = IncrementalParser.edit(t, {
    start: 0,
    oldEnd: String.length(source),
    newText: newSource,
  }, "<test>")

  assertTrue("source contains new_fn", String.includes(IncrementalParser.source(t), "function new_fn"))
  assertFalse("no old function", String.includes(IncrementalParser.source(t), "function old"))
  assertTrue("has items", Array.length(IncrementalParser.items(t)) >= 1)
}

// ============================================
// Run all tests
// ============================================

let runAll = () => {
  testEditInsideFunctionBody()
  testAddNewFunction()
  testDeleteFunction()
  testEditFunctionSignature()
  testEditAcrossBoundary()
  testInsertBetweenItems()
  testNoopEdit()
  testEditSyntaxError()
  testMultipleSequentialEdits()
  testFullAstReconstruction()
  testEmptySource()
  testCachedDeclKindTags()
  testMainBlock()
  testReplaceEntireSource()
  summarize()
}
