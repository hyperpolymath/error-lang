// SPDX-License-Identifier: MPL-2.0
// CstTest.res - Tests for the Concrete Syntax Tree module

let runTests = () => {
  Console.log("=== CST Tests ===")

  let pass = ref(0)
  let fail = ref(0)

  let assert_ = (name: string, actual: bool) => {
    if actual {
      pass := pass.contents + 1
    } else {
      Console.error(`FAIL: ${name}`)
      fail := fail.contents + 1
    }
  }

  // Round-trip: simple let
  {
    let source = "let x = 42"
    let cst = Cst.parseToCst(source, "test.el", 1)
    assert_("round_trip_simple_let", Cst.treeToSource(cst) == source)
  }

  // Round-trip: with comment
  {
    let source = "# comment\nlet x = 42"
    let cst = Cst.parseToCst(source, "test.el", 1)
    assert_("round_trip_comment", Cst.treeToSource(cst) == source)
  }

  // Round-trip: extra whitespace
  {
    let source = "  let  x  =  42  "
    let cst = Cst.parseToCst(source, "test.el", 1)
    assert_("round_trip_whitespace", Cst.treeToSource(cst) == source)
  }

  // Round-trip: multiline
  {
    let source = "# Header\n\nlet x = 1\nlet y = 2\n"
    let cst = Cst.parseToCst(source, "test.el", 1)
    assert_("round_trip_multiline", Cst.treeToSource(cst) == source)
  }

  // Empty source
  {
    let cst = Cst.parseToCst("", "test.el", 1)
    assert_("empty_source", Cst.treeToSource(cst) == "")
  }

  // Tokens in order
  {
    let source = "let x = 1"
    let cst = Cst.parseToCst(source, "test.el", 1)
    let toks = Cst.tokens(cst)
    let texts = toks->Array.map(t => t.text)->Array.filter(t => t != "" && t != "\\n")
    assert_("tokens_in_order", texts == ["let", "x", "=", "1"])
  }

  // Trivia preserved
  {
    let source = "# comment\nlet x = 1"
    let cst = Cst.parseToCst(source, "test.el", 1)
    let toks = Cst.tokens(cst)
    // Find the "let" token (should have comment as trivia on prior token or as leading)
    let letTok = toks->Array.find(t => t.text == "let")
    switch letTok {
    | Some(tok) => {
        let hasComment = tok.leadingTrivia->Array.some(t => t.kind == Cst.LineComment)
        assert_("trivia_preserved_comment", hasComment)
      }
    | None => assert_("trivia_preserved_found_let", false)
    }
  }

  // Node at offset
  {
    let source = "let x = 1"
    let cst = Cst.parseToCst(source, "test.el", 1)
    switch Cst.nodeAt(cst, 0) {
    | Some(CstToken(tok)) => assert_("node_at_offset_0", tok.text == "let")
    | _ => assert_("node_at_offset_0", false)
    }
  }

  Console.log(`CST tests: ${pass.contents->Int.toString} passed, ${fail.contents->Int.toString} failed`)
}
