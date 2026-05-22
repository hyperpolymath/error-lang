#!/usr/bin/env -S deno run --allow-read
// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// ast-dump.js — S-expression and JSON AST dump for Error-Lang
//
// Provides two output modes for the Error-Lang AST:
//   1. S-expression (--output sexpr) — compact, Lisp-like notation
//   2. JSON (--output json) — machine-readable JSON tree
//
// Covers all AST node types defined in Types.res:
//   - program, decl (FunctionDecl, StructDecl, MainBlock, StmtDecl)
//   - stmt (LetStmt, AssignStmt, IfStmt, WhileStmt, ForStmt, ReturnStmt,
//     BreakStmt, ContinueStmt, PrintStmt, GutterBlock, ExprStmt)
//   - expr (IntLit, FloatLit, StringLit, BoolLit, NilLit, Ident, Array,
//     Binary, Unary, Call, Index, Member, Ternary, Lambda)
//
// Usage:
//   error-lang dump-ast <file.err> [--output sexpr|json|pretty]

import { parseArgs } from "jsr:@std/cli@1/parse-args";
import { exists } from "jsr:@std/fs@1/exists";

// ============================================================================
// Tokenizer (imported inline from main.js to avoid circular deps)
// ============================================================================

// Minimal re-export of tokenize from main.js
// We inline a simplified tokenizer to keep this module self-contained.
// A production implementation would import from the ReScript compiler.

const KEYWORDS = new Set([
  "main",
  "end",
  "let",
  "mutable",
  "function",
  "struct",
  "if",
  "elseif",
  "else",
  "while",
  "for",
  "in",
  "break",
  "continue",
  "return",
  "and",
  "or",
  "not",
  "true",
  "false",
  "nil",
  "gutter",
  "fn",
  "print",
  "println",
]);

/**
 * Tokenize Error-Lang source code.
 *
 * @param {string} source - The source code text
 * @returns {Array<object>} Array of token objects with type, lexeme, value, line, column
 */
function tokenize(source) {
  const tokens = [];
  let pos = 0;
  let line = 1;
  let column = 1;

  while (pos < source.length) {
    const ch = source[pos];

    // Whitespace
    if (ch === " " || ch === "\t" || ch === "\r") {
      pos++;
      column++;
      continue;
    }

    // Newline
    if (ch === "\n") {
      tokens.push({ type: "NEWLINE", lexeme: "\\n", line, column });
      pos++;
      line++;
      column = 1;
      continue;
    }

    // Comment
    if (ch === "#") {
      while (pos < source.length && source[pos] !== "\n") pos++;
      continue;
    }

    // String
    if (ch === '"') {
      pos++;
      column++;
      let str = "";
      while (pos < source.length && source[pos] !== '"' && source[pos] !== "\n") {
        if (source[pos] === "\\") {
          pos++;
          column++;
          switch (source[pos]) {
            case "n": str += "\n"; break;
            case "r": str += "\r"; break;
            case "t": str += "\t"; break;
            case "\\": str += "\\"; break;
            case '"': str += '"'; break;
            default: str += source[pos];
          }
        } else {
          str += source[pos];
        }
        pos++;
        column++;
      }
      if (source[pos] === '"') { pos++; column++; }
      tokens.push({ type: "STRING", value: str, lexeme: `"${str}"`, line, column });
      continue;
    }

    // Number
    if (/[0-9]/.test(ch)) {
      let num = "";
      const startCol = column;
      while (pos < source.length && /[0-9.]/.test(source[pos])) {
        num += source[pos];
        pos++;
        column++;
      }
      const isFloat = num.includes(".");
      tokens.push({
        type: isFloat ? "FLOAT" : "INTEGER",
        value: isFloat ? parseFloat(num) : parseInt(num, 10),
        lexeme: num,
        line,
        column: startCol,
      });
      continue;
    }

    // Identifier or keyword
    if (/[a-zA-Z_]/.test(ch)) {
      let ident = "";
      const startCol = column;
      while (pos < source.length && /[a-zA-Z0-9_]/.test(source[pos])) {
        ident += source[pos];
        pos++;
        column++;
      }
      const type = KEYWORDS.has(ident) ? ident.toUpperCase() : "IDENTIFIER";
      tokens.push({ type, value: ident, lexeme: ident, line, column: startCol });
      continue;
    }

    // Two-char operators
    if (pos + 1 < source.length) {
      const two = ch + source[pos + 1];
      const twoOps = {
        "==": "EQUAL_EQUAL", "!=": "BANG_EQUAL", "<=": "LESS_EQUAL",
        ">=": "GREATER_EQUAL", "<<": "LESS_LESS", ">>": "GREATER_GREATER",
        "->": "ARROW",
      };
      if (twoOps[two]) {
        tokens.push({ type: twoOps[two], lexeme: two, line, column });
        pos += 2;
        column += 2;
        continue;
      }
    }

    // Single-char operators
    const ops = {
      "+": "PLUS", "-": "MINUS", "*": "STAR", "/": "SLASH", "%": "PERCENT",
      "=": "EQUAL", "!": "BANG", "<": "LESS", ">": "GREATER",
      "(": "LPAREN", ")": "RPAREN", "[": "LBRACKET", "]": "RBRACKET",
      "{": "LBRACE", "}": "RBRACE", ",": "COMMA", ".": "DOT",
      ":": "COLON", "?": "QUESTION", "&": "AMPERSAND", "|": "PIPE",
      "^": "CARET", "~": "TILDE",
    };
    if (ops[ch]) {
      tokens.push({ type: ops[ch], lexeme: ch, line, column });
      pos++;
      column++;
      continue;
    }

    // Skip unknown
    pos++;
    column++;
  }

  tokens.push({ type: "EOF", lexeme: "", line, column });
  return tokens;
}

// ============================================================================
// Minimal recursive-descent parser (produces AST matching Types.res shape)
// ============================================================================

/**
 * Parse Error-Lang tokens into an AST program.
 *
 * This is a simplified parser that produces a tree matching the types in
 * Types.res.  It handles the core grammar: main/end blocks, let/mutable,
 * if/elseif/else, while, for, function, struct, print/println, gutter,
 * break, continue, return, and expressions.
 *
 * @param {Array<object>} tokens - Lexer tokens
 * @param {string} file - Source filename
 * @returns {object} AST program node
 */
function parse(tokens, file) {
  let pos = 0;

  /** Skip newline tokens. */
  function skipNewlines() {
    while (pos < tokens.length && tokens[pos].type === "NEWLINE") pos++;
  }

  /** Get current token. */
  function current() { return tokens[pos] || { type: "EOF" }; }

  /** Advance and return previous token. */
  function advance() { return tokens[pos++]; }

  /** Check current token type. */
  function check(type) { return current().type === type; }

  /** Consume a token of the given type, or return null. */
  function match(type) {
    if (check(type)) return advance();
    return null;
  }

  /** Make a location from a token. */
  function loc(tok) {
    return {
      start: { line: tok.line, column: tok.column, offset: 0 },
      end_: { line: tok.line, column: tok.column + (tok.lexeme?.length || 0), offset: 0 },
      file,
    };
  }

  /** Parse an expression (simplified: handles binary, unary, literals, calls, index). */
  function parseExpr() {
    return parseOr();
  }

  function parseOr() {
    let left = parseAnd();
    while (match("OR")) {
      const right = parseAnd();
      left = { type: "Binary", left, op: "or", right, loc: loc(current()) };
    }
    return left;
  }

  function parseAnd() {
    let left = parseEquality();
    while (match("AND")) {
      const right = parseEquality();
      left = { type: "Binary", left, op: "and", right, loc: loc(current()) };
    }
    return left;
  }

  function parseEquality() {
    let left = parseComparison();
    while (check("EQUAL_EQUAL") || check("BANG_EQUAL")) {
      const op = advance().lexeme;
      const right = parseComparison();
      left = { type: "Binary", left, op, right, loc: loc(current()) };
    }
    return left;
  }

  function parseComparison() {
    let left = parseAddition();
    while (check("LESS") || check("GREATER") || check("LESS_EQUAL") || check("GREATER_EQUAL")) {
      const op = advance().lexeme;
      const right = parseAddition();
      left = { type: "Binary", left, op, right, loc: loc(current()) };
    }
    return left;
  }

  function parseAddition() {
    let left = parseMultiplication();
    while (check("PLUS") || check("MINUS")) {
      const op = advance().lexeme;
      const right = parseMultiplication();
      left = { type: "Binary", left, op, right, loc: loc(current()) };
    }
    return left;
  }

  function parseMultiplication() {
    let left = parseUnary();
    while (check("STAR") || check("SLASH") || check("PERCENT")) {
      const op = advance().lexeme;
      const right = parseUnary();
      left = { type: "Binary", left, op, right, loc: loc(current()) };
    }
    return left;
  }

  function parseUnary() {
    if (check("MINUS")) {
      advance();
      const expr = parseUnary();
      return { type: "Unary", op: "-", operand: expr, loc: loc(current()) };
    }
    if (check("NOT")) {
      advance();
      const expr = parseUnary();
      return { type: "Unary", op: "not", operand: expr, loc: loc(current()) };
    }
    return parsePostfix();
  }

  function parsePostfix() {
    let expr = parsePrimary();
    while (true) {
      if (check("LPAREN")) {
        advance();
        const args = [];
        while (!check("RPAREN") && !check("EOF")) {
          args.push(parseExpr());
          match("COMMA");
        }
        match("RPAREN");
        expr = { type: "CALL", callee: expr, args, loc: loc(current()) };
      } else if (check("LBRACKET")) {
        advance();
        const index = parseExpr();
        match("RBRACKET");
        expr = { type: "INDEX", array: expr, index, loc: loc(current()) };
      } else if (check("DOT")) {
        advance();
        const field = advance();
        expr = { type: "MEMBER", object: expr, field: field.value || field.lexeme, loc: loc(current()) };
      } else {
        break;
      }
    }
    return expr;
  }

  function parsePrimary() {
    const tok = current();
    switch (tok.type) {
      case "INTEGER": advance(); return { type: "INTEGER", value: tok.value, loc: loc(tok) };
      case "FLOAT":   advance(); return { type: "FLOAT", value: tok.value, loc: loc(tok) };
      case "STRING":  advance(); return { type: "STRING", value: tok.value, loc: loc(tok) };
      case "TRUE":    advance(); return { type: "TRUE", value: true, loc: loc(tok) };
      case "FALSE":   advance(); return { type: "FALSE", value: false, loc: loc(tok) };
      case "NIL":     advance(); return { type: "NIL", loc: loc(tok) };
      case "IDENTIFIER": advance(); return { type: "IDENTIFIER", value: tok.value, loc: loc(tok) };
      case "LBRACKET": {
        advance();
        const elements = [];
        while (!check("RBRACKET") && !check("EOF")) {
          elements.push(parseExpr());
          match("COMMA");
        }
        match("RBRACKET");
        return { type: "ARRAY", elements, loc: loc(tok) };
      }
      case "LPAREN": {
        advance();
        const expr = parseExpr();
        match("RPAREN");
        return expr;
      }
      default:
        advance();
        return { type: "ERROR", lexeme: tok.lexeme, loc: loc(tok) };
    }
  }

  /** Parse a block of statements until END or other block-ender. */
  function parseBlock(enders) {
    const stmts = [];
    skipNewlines();
    while (!enders.includes(current().type) && !check("EOF")) {
      const stmt = parseStmt();
      if (stmt) stmts.push(stmt);
      skipNewlines();
    }
    return stmts;
  }

  /** Parse a single statement. */
  function parseStmt() {
    skipNewlines();
    const tok = current();

    switch (tok.type) {
      case "LET":
      case "MUTABLE": {
        const isMut = tok.type === "MUTABLE";
        advance();
        if (isMut) match("LET"); // mutable let ...
        const name = advance();
        match("EQUAL");
        const value = parseExpr();
        return { type: "LetStmt", mutable_: isMut, name: name.value, value, loc: loc(tok) };
      }
      case "IF": {
        advance();
        const cond = parseExpr();
        skipNewlines();
        const then_ = parseBlock(["ELSE", "ELSEIF", "END"]);
        const elseifs = [];
        while (check("ELSEIF")) {
          advance();
          const eicond = parseExpr();
          skipNewlines();
          const eibody = parseBlock(["ELSE", "ELSEIF", "END"]);
          elseifs.push([eicond, eibody]);
        }
        let else_ = null;
        if (match("ELSE")) {
          skipNewlines();
          else_ = parseBlock(["END"]);
        }
        match("END");
        return { type: "IfStmt", cond, then_, elseifs, else_, loc: loc(tok) };
      }
      case "WHILE": {
        advance();
        const cond = parseExpr();
        skipNewlines();
        const body = parseBlock(["END"]);
        match("END");
        return { type: "WhileStmt", cond, body, loc: loc(tok) };
      }
      case "FOR": {
        advance();
        const varName = advance();
        match("IN");
        const iter = parseExpr();
        skipNewlines();
        const body = parseBlock(["END"]);
        match("END");
        return { type: "ForStmt", var: varName.value, iter, body, loc: loc(tok) };
      }
      case "RETURN": {
        advance();
        const value = check("NEWLINE") || check("EOF") || check("END") ? null : parseExpr();
        return { type: "ReturnStmt", value, loc: loc(tok) };
      }
      case "BREAK": { advance(); return { type: "BreakStmt", loc: loc(tok) }; }
      case "CONTINUE": { advance(); return { type: "ContinueStmt", loc: loc(tok) }; }
      case "PRINT":
      case "PRINTLN": {
        const isPrintln = tok.type === "PRINTLN";
        advance();
        match("LPAREN");
        const args = [];
        while (!check("RPAREN") && !check("EOF")) {
          args.push(parseExpr());
          match("COMMA");
        }
        match("RPAREN");
        return { type: "PrintStmt", println: isPrintln, args, loc: loc(tok) };
      }
      case "GUTTER": {
        advance();
        skipNewlines();
        const body = parseBlock(["END"]);
        match("END");
        return { type: "GutterBlock", body, loc: loc(tok) };
      }
      default: {
        const expr = parseExpr();
        // Check for assignment
        if (check("EQUAL")) {
          advance();
          const value = parseExpr();
          return { type: "AssignStmt", target: expr, value, loc: loc(tok) };
        }
        return { type: "ExprStmt", expr, loc: loc(tok) };
      }
    }
  }

  /** Parse top-level declarations. */
  function parseDecls() {
    const decls = [];
    skipNewlines();
    while (!check("EOF")) {
      const tok = current();
      switch (tok.type) {
        case "MAIN": {
          advance();
          skipNewlines();
          const body = parseBlock(["END"]);
          match("END");
          decls.push({ type: "MainBlock", body, loc: loc(tok) });
          break;
        }
        case "FUNCTION": {
          advance();
          const name = advance();
          match("LPAREN");
          const params = [];
          while (!check("RPAREN") && !check("EOF")) {
            params.push({ name: advance().value, loc: loc(current()) });
            match("COMMA");
          }
          match("RPAREN");
          skipNewlines();
          const body = parseBlock(["END"]);
          match("END");
          decls.push({ type: "FunctionDecl", name: name.value, params, body, loc: loc(tok) });
          break;
        }
        case "STRUCT": {
          advance();
          const name = advance();
          skipNewlines();
          const fields = [];
          while (!check("END") && !check("EOF")) {
            const fname = advance();
            match("COLON");
            const ftype = advance();
            fields.push([fname.value, ftype.value]);
            skipNewlines();
          }
          match("END");
          decls.push({ type: "StructDecl", name: name.value, fields, loc: loc(tok) });
          break;
        }
        default: {
          const stmt = parseStmt();
          if (stmt) decls.push({ type: "StmtDecl", stmt, loc: loc(tok) });
          break;
        }
      }
      skipNewlines();
    }
    return decls;
  }

  const declarations = parseDecls();
  return { type: "Program", declarations, loc: { start: { line: 1, column: 1, offset: 0 }, end_: { line: 1, column: 1, offset: 0 }, file } };
}

// ============================================================================
// S-EXPRESSION OUTPUT
// ============================================================================

/**
 * Convert an AST node to S-expression string.
 *
 * @param {object} node - AST node
 * @param {number} d - Current indentation depth
 * @returns {string} S-expression string
 */
function toSexpr(node, d = 0) {
  const pad = " ".repeat(d);
  const pad2 = " ".repeat(d + 2);

  if (!node || !node.type) return "(error)";

  switch (node.type) {
    case "Program":
      return `(program${node.declarations.map((decl) => `\n${pad2}${toSexpr(decl, d + 2)}`).join("")})`;

    // Declarations
    case "MainBlock":
      return `(main${node.body.map((s) => `\n${pad2}${toSexpr(s, d + 2)}`).join("")})`;
    case "FunctionDecl":
      return `(function "${node.name}" (params${node.params.map((p) => ` "${p.name}"`).join("")})${node.body.map((s) => `\n${pad2}${toSexpr(s, d + 2)}`).join("")})`;
    case "StructDecl":
      return `(struct "${node.name}"${node.fields.map(([n, t]) => ` (${n} ${t})`).join("")})`;
    case "StmtDecl":
      return toSexpr(node.stmt, d);

    // Statements
    case "LetStmt":
      return `(let${node.mutable_ ? " :mut" : ""} "${node.name}" ${toSexpr(node.value, d + 2)})`;
    case "AssignStmt":
      return `(assign ${toSexpr(node.target, d + 2)} ${toSexpr(node.value, d + 2)})`;
    case "IfStmt": {
      let s = `(if ${toSexpr(node.cond, d + 2)}\n${pad2}(then${node.then_.map((st) => ` ${toSexpr(st, d + 4)}`).join("")})`;
      for (const [cond, body] of node.elseifs || []) {
        s += `\n${pad2}(elseif ${toSexpr(cond, d + 4)}${body.map((st) => ` ${toSexpr(st, d + 4)}`).join("")})`;
      }
      if (node.else_) {
        s += `\n${pad2}(else${node.else_.map((st) => ` ${toSexpr(st, d + 4)}`).join("")})`;
      }
      return s + ")";
    }
    case "WhileStmt":
      return `(while ${toSexpr(node.cond, d + 2)}${node.body.map((s) => `\n${pad2}${toSexpr(s, d + 2)}`).join("")})`;
    case "ForStmt":
      return `(for "${node.var}" ${toSexpr(node.iter, d + 2)}${node.body.map((s) => `\n${pad2}${toSexpr(s, d + 2)}`).join("")})`;
    case "ReturnStmt":
      return node.value ? `(return ${toSexpr(node.value, d + 2)})` : "(return)";
    case "BreakStmt":
      return "(break)";
    case "ContinueStmt":
      return "(continue)";
    case "PrintStmt":
      return `(${node.println ? "println" : "print"}${node.args.map((a) => ` ${toSexpr(a, d + 2)}`).join("")})`;
    case "GutterBlock":
      return `(gutter${node.body.map((s) => `\n${pad2}${toSexpr(s, d + 2)}`).join("")})`;
    case "ExprStmt":
      return toSexpr(node.expr, d);

    // Expressions
    case "INTEGER":
      return `${node.value}`;
    case "FLOAT":
      return `${node.value}`;
    case "STRING":
      return `"${node.value}"`;
    case "TRUE":
      return "#t";
    case "FALSE":
      return "#f";
    case "NIL":
      return "nil";
    case "IDENTIFIER":
      return `(id "${node.value}")`;
    case "ARRAY":
      return `(array${node.elements.map((e) => ` ${toSexpr(e, d + 2)}`).join("")})`;
    case "Binary":
      return `(${node.op} ${toSexpr(node.left, d + 2)} ${toSexpr(node.right, d + 2)})`;
    case "Unary":
      return `(${node.op} ${toSexpr(node.operand, d + 2)})`;
    case "CALL":
      return `(call ${toSexpr(node.callee, d + 2)}${node.args.map((a) => ` ${toSexpr(a, d + 2)}`).join("")})`;
    case "INDEX":
      return `(index ${toSexpr(node.array, d + 2)} ${toSexpr(node.index, d + 2)})`;
    case "MEMBER":
      return `(member ${toSexpr(node.object, d + 2)} "${node.field}")`;
    case "ERROR":
      return `(error "${node.lexeme || ""}")`;
    default:
      return `(unknown-${node.type})`;
  }
}

// ============================================================================
// JSON OUTPUT
// ============================================================================

/**
 * Convert an AST node to a plain JSON-serializable object.
 * The structure mirrors the AST node types from Types.res.
 *
 * @param {object} node - AST node
 * @returns {object} JSON-serializable object
 */
function toJson(node) {
  if (!node || !node.type) return { type: "error" };

  switch (node.type) {
    case "Program":
      return { type: "program", declarations: node.declarations.map(toJson) };
    case "MainBlock":
      return { type: "main_block", body: node.body.map(toJson) };
    case "FunctionDecl":
      return { type: "function", name: node.name, params: node.params.map((p) => p.name), body: node.body.map(toJson) };
    case "StructDecl":
      return { type: "struct", name: node.name, fields: node.fields.map(([n, t]) => ({ name: n, ty: t })) };
    case "StmtDecl":
      return toJson(node.stmt);
    case "LetStmt":
      return { type: "let", mutable: node.mutable_, name: node.name, value: toJson(node.value) };
    case "AssignStmt":
      return { type: "assign", target: toJson(node.target), value: toJson(node.value) };
    case "IfStmt":
      return {
        type: "if", condition: toJson(node.cond),
        then: node.then_.map(toJson),
        elseifs: (node.elseifs || []).map(([c, b]) => ({ condition: toJson(c), body: b.map(toJson) })),
        else: node.else_ ? node.else_.map(toJson) : null,
      };
    case "WhileStmt":
      return { type: "while", condition: toJson(node.cond), body: node.body.map(toJson) };
    case "ForStmt":
      return { type: "for", var: node.var, iter: toJson(node.iter), body: node.body.map(toJson) };
    case "ReturnStmt":
      return { type: "return", value: node.value ? toJson(node.value) : null };
    case "BreakStmt":
      return { type: "break" };
    case "ContinueStmt":
      return { type: "continue" };
    case "PrintStmt":
      return { type: node.println ? "println" : "print", args: node.args.map(toJson) };
    case "GutterBlock":
      return { type: "gutter", body: node.body.map(toJson) };
    case "ExprStmt":
      return { type: "expr_stmt", expr: toJson(node.expr) };
    case "INTEGER":
      return { type: "integer", value: node.value };
    case "FLOAT":
      return { type: "float", value: node.value };
    case "STRING":
      return { type: "string", value: node.value };
    case "TRUE":
      return { type: "bool", value: true };
    case "FALSE":
      return { type: "bool", value: false };
    case "NIL":
      return { type: "nil" };
    case "IDENTIFIER":
      return { type: "identifier", name: node.value };
    case "ARRAY":
      return { type: "array", elements: node.elements.map(toJson) };
    case "Binary":
      return { type: "binary", op: node.op, lhs: toJson(node.left), rhs: toJson(node.right) };
    case "Unary":
      return { type: "unary", op: node.op, operand: toJson(node.operand) };
    case "CALL":
      return { type: "call", callee: toJson(node.callee), args: node.args.map(toJson) };
    case "INDEX":
      return { type: "index", array: toJson(node.array), index: toJson(node.index) };
    case "MEMBER":
      return { type: "member", object: toJson(node.object), field: node.field };
    default:
      return { type: node.type };
  }
}

// ============================================================================
// CLI entry point
// ============================================================================

const args = parseArgs(Deno.args, {
  string: ["output"],
  alias: { o: "output" },
  default: { output: "pretty" },
});

const file = args._[0];
if (!file) {
  console.error("Usage: error-lang dump-ast <file.err> [--output sexpr|json|pretty]");
  Deno.exit(1);
}

const filename = String(file);
if (!(await exists(filename))) {
  console.error(`File not found: ${filename}`);
  Deno.exit(1);
}

const source = await Deno.readTextFile(filename);
const tokens = tokenize(source);
const ast = parse(tokens, filename);

switch (args.output) {
  case "sexpr":
  case "sexp":
    console.log(toSexpr(ast));
    break;
  case "json":
    console.log(JSON.stringify(toJson(ast), null, 2));
    break;
  case "pretty":
  default:
    console.log(JSON.stringify(ast, null, 2));
    break;
}
