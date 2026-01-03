// SPDX-License-Identifier: AGPL-3.0-or-later
// Parser.res - Error-tolerant parser for Error-Lang

open Types

type parserState = {
  tokens: array<token>,
  mutable pos: int,
  mutable diagnostics: array<diagnostic>,
  runNumber: int,
  file: string,
}

let make = (tokens: array<token>, file: string, runNumber: int): parserState => {
  tokens,
  pos: 0,
  diagnostics: [],
  runNumber,
  file,
}

// Peek at current token
let peek = (state: parserState): option<token> => {
  if state.pos < Array.length(state.tokens) {
    Some(state.tokens[state.pos])
  } else {
    None
  }
}

// Peek ahead
let peekAhead = (state: parserState, offset: int): option<token> => {
  let idx = state.pos + offset
  if idx < Array.length(state.tokens) {
    Some(state.tokens[idx])
  } else {
    None
  }
}

// Advance and return current token
let advance = (state: parserState): option<token> => {
  let tok = peek(state)
  if state.pos < Array.length(state.tokens) {
    state.pos = state.pos + 1
  }
  tok
}

// Check if at end
let isAtEnd = (state: parserState): bool => {
  switch peek(state) {
  | Some({type_: EOF}) | None => true
  | _ => false
  }
}

// Check token type
let check = (state: parserState, type_: tokenType): bool => {
  switch peek(state) {
  | Some(tok) => tok.type_ == type_
  | None => false
  }
}

// Match and consume if matches
let match_ = (state: parserState, types: array<tokenType>): bool => {
  Array.some(types, type_ => {
    if check(state, type_) {
      advance(state)->ignore
      true
    } else {
      false
    }
  })
}

// Skip newlines
let skipNewlines = (state: parserState): unit => {
  while check(state, Newline) {
    advance(state)->ignore
  }
}

// Add diagnostic
let addDiagnostic = (state: parserState, code: errorCode, message: string, loc: location): unit => {
  let diag = {
    code,
    message,
    loc,
    runNumber: state.runNumber,
    hint: None,
  }
  state.diagnostics = Array.concat(state.diagnostics, [diag])
}

// Expect a token type
let expect = (state: parserState, type_: tokenType, message: string): result<token, unit> => {
  switch peek(state) {
  | Some(tok) if tok.type_ == type_ => {
      advance(state)->ignore
      Ok(tok)
    }
  | Some(tok) => {
      addDiagnostic(state, E0001, message, tok.loc)
      Error()
    }
  | None => {
      let loc = {
        start: {line: 0, column: 0, offset: 0},
        end_: {line: 0, column: 0, offset: 0},
        file: state.file,
      }
      addDiagnostic(state, E0001, message, loc)
      Error()
    }
  }
}

// Get location from current token
let currentLoc = (state: parserState): location => {
  switch peek(state) {
  | Some(tok) => tok.loc
  | None => {
      start: {line: 0, column: 0, offset: 0},
      end_: {line: 0, column: 0, offset: 0},
      file: state.file,
    }
  }
}

// ============================================
// Expression Parsing
// ============================================

// Forward declarations (ReScript handles these via rec)
let rec parseExpression = (state: parserState): option<expr> => {
  parseTernary(state)
}

and parseTernary = (state: parserState): option<expr> => {
  let left = parseLogicalOr(state)

  switch left {
  | Some(cond) if check(state, Question) => {
      let startLoc = currentLoc(state)
      advance(state)->ignore // ?
      switch parseExpression(state) {
      | Some(then_) => {
          if match_(state, [Colon]) {
            switch parseExpression(state) {
            | Some(else_) => {
                let loc = {
                  start: startLoc.start,
                  end_: currentLoc(state).end_,
                  file: state.file,
                }
                Some(Ternary(cond, then_, else_, loc))
              }
            | None => Some(cond)
            }
          } else {
            Some(cond)
          }
        }
      | None => Some(cond)
      }
    }
  | _ => left
  }
}

and parseLogicalOr = (state: parserState): option<expr> => {
  let left = ref(parseLogicalAnd(state))

  while match_(state, [Or]) && Option.isSome(left.contents) {
    let right = parseLogicalAnd(state)
    switch (left.contents, right) {
    | (Some(l), Some(r)) => {
        let loc = {
          start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
          end_: currentLoc(state).end_,
          file: state.file,
        }
        left := Some(Binary(l, LOr, r, loc))
      }
    | _ => ()
    }
  }

  left.contents
}

and parseLogicalAnd = (state: parserState): option<expr> => {
  let left = ref(parseEquality(state))

  while match_(state, [And]) && Option.isSome(left.contents) {
    let right = parseEquality(state)
    switch (left.contents, right) {
    | (Some(l), Some(r)) => {
        let loc = {
          start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
          end_: currentLoc(state).end_,
          file: state.file,
        }
        left := Some(Binary(l, LAnd, r, loc))
      }
    | _ => ()
    }
  }

  left.contents
}

and parseEquality = (state: parserState): option<expr> => {
  let left = ref(parseComparison(state))

  while match_(state, [EqualEqual, BangEqual]) && Option.isSome(left.contents) {
    let opTok = state.tokens[state.pos - 1]
    let op = switch opTok.type_ {
    | EqualEqual => Eq
    | BangEqual => Neq
    | _ => Eq
    }
    let right = parseComparison(state)
    switch (left.contents, right) {
    | (Some(l), Some(r)) => {
        let loc = {
          start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
          end_: currentLoc(state).end_,
          file: state.file,
        }
        left := Some(Binary(l, op, r, loc))
      }
    | _ => ()
    }
  }

  left.contents
}

and parseComparison = (state: parserState): option<expr> => {
  let left = ref(parseTerm(state))

  while match_(state, [Less, Greater, LessEqual, GreaterEqual]) && Option.isSome(left.contents) {
    let opTok = state.tokens[state.pos - 1]
    let op = switch opTok.type_ {
    | Less => Lt
    | Greater => Gt
    | LessEqual => Lte
    | GreaterEqual => Gte
    | _ => Lt
    }
    let right = parseTerm(state)
    switch (left.contents, right) {
    | (Some(l), Some(r)) => {
        let loc = {
          start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
          end_: currentLoc(state).end_,
          file: state.file,
        }
        left := Some(Binary(l, op, r, loc))
      }
    | _ => ()
    }
  }

  left.contents
}

and parseTerm = (state: parserState): option<expr> => {
  let left = ref(parseFactor(state))

  while match_(state, [Plus, Minus]) && Option.isSome(left.contents) {
    let opTok = state.tokens[state.pos - 1]
    let op = switch opTok.type_ {
    | Plus => Add
    | Minus => Sub
    | _ => Add
    }
    let right = parseFactor(state)
    switch (left.contents, right) {
    | (Some(l), Some(r)) => {
        let loc = {
          start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
          end_: currentLoc(state).end_,
          file: state.file,
        }
        left := Some(Binary(l, op, r, loc))
      }
    | _ => ()
    }
  }

  left.contents
}

and parseFactor = (state: parserState): option<expr> => {
  let left = ref(parseUnary(state))

  while match_(state, [Star, Slash, Percent]) && Option.isSome(left.contents) {
    let opTok = state.tokens[state.pos - 1]
    let op = switch opTok.type_ {
    | Star => Mul
    | Slash => Div
    | Percent => Mod
    | _ => Mul
    }
    let right = parseUnary(state)
    switch (left.contents, right) {
    | (Some(l), Some(r)) => {
        let loc = {
          start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
          end_: currentLoc(state).end_,
          file: state.file,
        }
        left := Some(Binary(l, op, r, loc))
      }
    | _ => ()
    }
  }

  left.contents
}

and parseUnary = (state: parserState): option<expr> => {
  if match_(state, [Minus, Not, Tilde]) {
    let opTok = state.tokens[state.pos - 1]
    let op = switch opTok.type_ {
    | Minus => Neg
    | Not => LNot
    | Tilde => BNot
    | _ => Neg
    }
    let right = parseUnary(state)
    switch right {
    | Some(r) => {
        let loc = {
          start: opTok.loc.start,
          end_: currentLoc(state).end_,
          file: state.file,
        }
        Some(Unary(op, r, loc))
      }
    | None => None
    }
  } else {
    parsePostfix(state)
  }
}

and parsePostfix = (state: parserState): option<expr> => {
  let left = ref(parsePrimary(state))

  while Option.isSome(left.contents) {
    if check(state, LParen) {
      // Function call
      advance(state)->ignore
      skipNewlines(state)
      let args = ref([])
      if !check(state, RParen) {
        switch parseExpression(state) {
        | Some(e) => args := Array.concat(args.contents, [e])
        | None => ()
        }
        while match_(state, [Comma]) {
          skipNewlines(state)
          switch parseExpression(state) {
          | Some(e) => args := Array.concat(args.contents, [e])
          | None => ()
          }
        }
      }
      skipNewlines(state)
      expect(state, RParen, "Expected ')' after arguments")->ignore
      switch left.contents {
      | Some(l) => {
          let loc = {
            start: switch l { | IntLit(_, loc) => loc.start | Ident(_, loc) => loc.start | _ => currentLoc(state).start },
            end_: currentLoc(state).end_,
            file: state.file,
          }
          left := Some(Call(l, args.contents, loc))
        }
      | None => ()
      }
    } else if check(state, LBracket) {
      // Index
      advance(state)->ignore
      skipNewlines(state)
      switch parseExpression(state) {
      | Some(idx) => {
          skipNewlines(state)
          expect(state, RBracket, "Expected ']' after index")->ignore
          switch left.contents {
          | Some(l) => {
              let loc = {
                start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
                end_: currentLoc(state).end_,
                file: state.file,
              }
              left := Some(Index(l, idx, loc))
            }
          | None => ()
          }
        }
      | None => ()
      }
    } else if check(state, Dot) {
      // Member access
      advance(state)->ignore
      switch peek(state) {
      | Some({type_: Identifier(name), loc: memberLoc}) => {
          advance(state)->ignore
          switch left.contents {
          | Some(l) => {
              let loc = {
                start: switch l { | IntLit(_, loc) => loc.start | _ => currentLoc(state).start },
                end_: memberLoc.end_,
                file: state.file,
              }
              left := Some(Member(l, name, loc))
            }
          | None => ()
          }
        }
      | _ => {
          addDiagnostic(state, E0001, "Expected identifier after '.'", currentLoc(state))
        }
      }
    } else {
      break
    }
  }

  left.contents
}

and parsePrimary = (state: parserState): option<expr> => {
  switch peek(state) {
  | Some({type_: Integer(n), loc}) => {
      advance(state)->ignore
      Some(IntLit(n, loc))
    }
  | Some({type_: Float(f), loc}) => {
      advance(state)->ignore
      Some(FloatLit(f, loc))
    }
  | Some({type_: String(s), loc}) => {
      advance(state)->ignore
      Some(StringLit(s, loc))
    }
  | Some({type_: True, loc}) => {
      advance(state)->ignore
      Some(BoolLit(true, loc))
    }
  | Some({type_: False, loc}) => {
      advance(state)->ignore
      Some(BoolLit(false, loc))
    }
  | Some({type_: Nil, loc}) => {
      advance(state)->ignore
      Some(NilLit(loc))
    }
  | Some({type_: Identifier(name), loc}) => {
      advance(state)->ignore
      Some(Ident(name, loc))
    }
  | Some({type_: LBracket, loc: startLoc}) => {
      // Array literal
      advance(state)->ignore
      skipNewlines(state)
      let elems = ref([])
      if !check(state, RBracket) {
        switch parseExpression(state) {
        | Some(e) => elems := Array.concat(elems.contents, [e])
        | None => ()
        }
        while match_(state, [Comma]) {
          skipNewlines(state)
          switch parseExpression(state) {
          | Some(e) => elems := Array.concat(elems.contents, [e])
          | None => ()
          }
        }
      }
      skipNewlines(state)
      expect(state, RBracket, "Expected ']' after array elements")->ignore
      let loc = {
        start: startLoc.start,
        end_: currentLoc(state).end_,
        file: state.file,
      }
      Some(Array(elems.contents, loc))
    }
  | Some({type_: LParen, loc: startLoc}) => {
      // Grouped expression
      advance(state)->ignore
      skipNewlines(state)
      let expr = parseExpression(state)
      skipNewlines(state)
      expect(state, RParen, "Expected ')' after expression")->ignore
      expr
    }
  | Some({type_: Fn, loc: startLoc}) => {
      // Lambda
      advance(state)->ignore
      expect(state, LParen, "Expected '(' after 'fn'")->ignore
      // Parse params (simplified)
      let params = ref([])
      skipNewlines(state)
      if !check(state, RParen) {
        switch peek(state) {
        | Some({type_: Identifier(name), loc: paramLoc}) => {
            advance(state)->ignore
            params := Array.concat(params.contents, [{name, type_: None, loc: paramLoc}])
          }
        | _ => ()
        }
        while match_(state, [Comma]) {
          skipNewlines(state)
          switch peek(state) {
          | Some({type_: Identifier(name), loc: paramLoc}) => {
              advance(state)->ignore
              params := Array.concat(params.contents, [{name, type_: None, loc: paramLoc}])
            }
          | _ => ()
          }
        }
      }
      expect(state, RParen, "Expected ')' after parameters")->ignore

      if match_(state, [Arrow]) {
        // Single expression body
        switch parseExpression(state) {
        | Some(body) => {
            let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
            Some(Lambda(params.contents, None, LambdaExpr(body), loc))
          }
        | None => None
        }
      } else {
        // Block body (future)
        None
      }
    }
  | Some(tok) => {
      addDiagnostic(state, E0001, `Unexpected token '${tok.lexeme}'`, tok.loc)
      None
    }
  | None => None
  }
}

// ============================================
// Statement Parsing
// ============================================

let parseStatement = (state: parserState): option<stmt> => {
  skipNewlines(state)

  switch peek(state) {
  | Some({type_: Let, loc: startLoc}) => {
      advance(state)->ignore
      let mutable_ = match_(state, [Mutable])

      switch peek(state) {
      | Some({type_: Identifier(name)}) => {
          advance(state)->ignore
          // Optional type annotation (skip for now)
          expect(state, Equal, "Expected '=' after variable name")->ignore
          switch parseExpression(state) {
          | Some(value) => {
              let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
              Some(LetStmt({mutable_, name, type_: None, value, loc}))
            }
          | None => None
          }
        }
      | _ => {
          addDiagnostic(state, E0001, "Expected identifier after 'let'", currentLoc(state))
          None
        }
      }
    }

  | Some({type_: If, loc: startLoc}) => {
      advance(state)->ignore
      switch parseExpression(state) {
      | Some(cond) => {
          skipNewlines(state)
          let then_ = parseBlock(state)

          // Parse elseif chains
          let elseifs = ref([])
          while check(state, Elseif) {
            advance(state)->ignore
            switch parseExpression(state) {
            | Some(elifCond) => {
                skipNewlines(state)
                let elifBody = parseBlock(state)
                elseifs := Array.concat(elseifs.contents, [(elifCond, elifBody)])
              }
            | None => ()
            }
          }

          // Optional else
          let else_ = if check(state, Else) {
            advance(state)->ignore
            skipNewlines(state)
            Some(parseBlock(state))
          } else {
            None
          }

          let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
          Some(IfStmt({cond, then_, elseifs: elseifs.contents, else_, loc}))
        }
      | None => None
      }
    }

  | Some({type_: While, loc: startLoc}) => {
      advance(state)->ignore
      switch parseExpression(state) {
      | Some(cond) => {
          skipNewlines(state)
          let body = parseBlock(state)
          let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
          Some(WhileStmt({cond, body, loc}))
        }
      | None => None
      }
    }

  | Some({type_: For, loc: startLoc}) => {
      advance(state)->ignore
      switch peek(state) {
      | Some({type_: Identifier(var)}) => {
          advance(state)->ignore
          expect(state, In, "Expected 'in' after loop variable")->ignore
          switch parseExpression(state) {
          | Some(iter) => {
              skipNewlines(state)
              let body = parseBlock(state)
              let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
              Some(ForStmt({var, iter, body, loc}))
            }
          | None => None
          }
        }
      | _ => {
          addDiagnostic(state, E0001, "Expected identifier after 'for'", currentLoc(state))
          None
        }
      }
    }

  | Some({type_: Return, loc: startLoc}) => {
      advance(state)->ignore
      let value = if check(state, Newline) || check(state, End) || check(state, EOF) {
        None
      } else {
        parseExpression(state)
      }
      let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
      Some(ReturnStmt({value, loc}))
    }

  | Some({type_: Break, loc}) => {
      advance(state)->ignore
      Some(BreakStmt(loc))
    }

  | Some({type_: Continue, loc}) => {
      advance(state)->ignore
      Some(ContinueStmt(loc))
    }

  | Some({type_: Identifier("print"), loc: startLoc}) => {
      advance(state)->ignore
      expect(state, LParen, "Expected '(' after 'print'")->ignore
      skipNewlines(state)
      let args = ref([])
      if !check(state, RParen) {
        switch parseExpression(state) {
        | Some(e) => args := Array.concat(args.contents, [e])
        | None => ()
        }
        while match_(state, [Comma]) {
          skipNewlines(state)
          switch parseExpression(state) {
          | Some(e) => args := Array.concat(args.contents, [e])
          | None => ()
          }
        }
      }
      skipNewlines(state)
      expect(state, RParen, "Expected ')' after arguments")->ignore
      let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
      Some(PrintStmt({println: false, args: args.contents, loc}))
    }

  | Some({type_: Identifier("println"), loc: startLoc}) => {
      advance(state)->ignore
      expect(state, LParen, "Expected '(' after 'println'")->ignore
      skipNewlines(state)
      let args = ref([])
      if !check(state, RParen) {
        switch parseExpression(state) {
        | Some(e) => args := Array.concat(args.contents, [e])
        | None => ()
        }
        while match_(state, [Comma]) {
          skipNewlines(state)
          switch parseExpression(state) {
          | Some(e) => args := Array.concat(args.contents, [e])
          | None => ()
          }
        }
      }
      skipNewlines(state)
      expect(state, RParen, "Expected ')' after arguments")->ignore
      let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
      Some(PrintStmt({println: true, args: args.contents, loc}))
    }

  | Some({type_: Gutter, loc: startLoc}) => {
      // GUTTER BLOCK - Error injection zone!
      // Collect all tokens until 'end', recover from errors
      advance(state)->ignore
      skipNewlines(state)

      let gutterTokens = ref([])
      let recovered = ref(false)

      // Scan until we find 'end'
      while !check(state, End) && !isAtEnd(state) {
        switch peek(state) {
        | Some(tok) => {
            gutterTokens := Array.concat(gutterTokens.contents, [tok])
            advance(state)->ignore
          }
        | None => ()
        }
      }

      // If we hit end, we recovered
      if check(state, End) {
        advance(state)->ignore
        recovered := true
      } else {
        addDiagnostic(state, E0005, "Missing 'end' for gutter block", startLoc)
      }

      let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
      Some(GutterBlock({tokens: gutterTokens.contents, recovered: recovered.contents, loc}))
    }

  | Some(_) => {
      // Expression statement
      switch parseExpression(state) {
      | Some(expr) => Some(ExprStmt(expr))
      | None => {
          // Skip bad token
          advance(state)->ignore
          None
        }
      }
    }

  | None => None
  }
}

and parseBlock = (state: parserState): array<stmt> => {
  let stmts = ref([])

  skipNewlines(state)
  while !check(state, End) && !check(state, Elseif) && !check(state, Else) && !isAtEnd(state) {
    switch parseStatement(state) {
    | Some(stmt) => stmts := Array.concat(stmts.contents, [stmt])
    | None => ()
    }
    skipNewlines(state)
  }

  if check(state, End) {
    advance(state)->ignore
  }

  stmts.contents
}

// ============================================
// Declaration Parsing
// ============================================

let parseDeclaration = (state: parserState): option<decl> => {
  skipNewlines(state)

  switch peek(state) {
  | Some({type_: Function, loc: startLoc}) => {
      advance(state)->ignore

      switch peek(state) {
      | Some({type_: Identifier(name)}) => {
          advance(state)->ignore
          expect(state, LParen, "Expected '(' after function name")->ignore

          // Parse parameters
          let params = ref([])
          skipNewlines(state)
          if !check(state, RParen) {
            switch peek(state) {
            | Some({type_: Identifier(pname), loc: ploc}) => {
                advance(state)->ignore
                params := Array.concat(params.contents, [{name: pname, type_: None, loc: ploc}])
              }
            | _ => ()
            }
            while match_(state, [Comma]) {
              skipNewlines(state)
              switch peek(state) {
              | Some({type_: Identifier(pname), loc: ploc}) => {
                  advance(state)->ignore
                  params := Array.concat(params.contents, [{name: pname, type_: None, loc: ploc}])
                }
              | _ => ()
              }
            }
          }
          expect(state, RParen, "Expected ')' after parameters")->ignore

          // Optional return type (skip for now)
          skipNewlines(state)

          let body = parseBlock(state)
          let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
          Some(FunctionDecl({name, params: params.contents, returnType: None, body, loc}))
        }
      | _ => {
          addDiagnostic(state, E0001, "Expected function name", currentLoc(state))
          None
        }
      }
    }

  | Some({type_: Main, loc: startLoc}) => {
      advance(state)->ignore
      skipNewlines(state)
      let body = parseBlock(state)
      let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
      Some(MainBlock({body, loc}))
    }

  | Some({type_: Struct, loc: startLoc}) => {
      advance(state)->ignore
      switch peek(state) {
      | Some({type_: Identifier(name)}) => {
          advance(state)->ignore
          skipNewlines(state)

          // Parse fields (simplified)
          let fields = ref([])
          while !check(state, End) && !isAtEnd(state) {
            switch peek(state) {
            | Some({type_: Identifier(fname)}) => {
                advance(state)->ignore
                expect(state, Colon, "Expected ':' after field name")->ignore
                // Simple type parsing
                switch peek(state) {
                | Some({type_: TInt}) => {
                    advance(state)->ignore
                    fields := Array.concat(fields.contents, [(fname, TyInt)])
                  }
                | Some({type_: TFloat}) => {
                    advance(state)->ignore
                    fields := Array.concat(fields.contents, [(fname, TyFloat)])
                  }
                | Some({type_: TString}) => {
                    advance(state)->ignore
                    fields := Array.concat(fields.contents, [(fname, TyString)])
                  }
                | Some({type_: TBool}) => {
                    advance(state)->ignore
                    fields := Array.concat(fields.contents, [(fname, TyBool)])
                  }
                | Some({type_: Identifier(tname)}) => {
                    advance(state)->ignore
                    fields := Array.concat(fields.contents, [(fname, TyIdent(tname))])
                  }
                | _ => ()
                }
              }
            | _ => ()
            }
            skipNewlines(state)
          }

          if check(state, End) {
            advance(state)->ignore
          }

          let loc = {start: startLoc.start, end_: currentLoc(state).end_, file: state.file}
          Some(StructDecl({name, fields: fields.contents, loc}))
        }
      | _ => {
          addDiagnostic(state, E0001, "Expected struct name", currentLoc(state))
          None
        }
      }
    }

  | _ => {
      // Top-level statement
      switch parseStatement(state) {
      | Some(stmt) => Some(StmtDecl(stmt))
      | None => None
      }
    }
  }
}

// ============================================
// Main Entry Point
// ============================================

let parse = (tokens: array<token>, file: string, runNumber: int): (program, array<diagnostic>) => {
  let state = make(tokens, file, runNumber)
  let declarations = ref([])

  while !isAtEnd(state) {
    switch parseDeclaration(state) {
    | Some(decl) => declarations := Array.concat(declarations.contents, [decl])
    | None => ()
    }
    skipNewlines(state)
  }

  let program = {
    declarations: declarations.contents,
    loc: {
      start: {line: 1, column: 1, offset: 0},
      end_: currentLoc(state).end_,
      file,
    },
  }

  (program, state.diagnostics)
}
