#!/usr/bin/env -S deno run --allow-read --allow-write
// SPDX-License-Identifier: AGPL-3.0-or-later
// Error-Lang CLI - Main entry point

import { parseArgs } from "jsr:@std/cli@1/parse-args";
import { dirname, join } from "jsr:@std/path@1";
import { exists } from "jsr:@std/fs@1/exists";

// State file location
const STATE_DIR = ".error-lang";
const STATE_FILE = "state.json";

// Default state
const defaultState = {
  runCounter: 0,
  stabilityScore: 100,
  lastError: null,
  seed: Math.floor(Math.random() * 1000000),
};

// Load or create state
async function loadState(dir) {
  const stateDir = join(dir, STATE_DIR);
  const statePath = join(stateDir, STATE_FILE);

  if (await exists(statePath)) {
    const content = await Deno.readTextFile(statePath);
    return JSON.parse(content);
  }

  return { ...defaultState };
}

// Save state
async function saveState(dir, state) {
  const stateDir = join(dir, STATE_DIR);
  const statePath = join(stateDir, STATE_FILE);

  try {
    await Deno.mkdir(stateDir, { recursive: true });
  } catch {
    // Directory exists
  }

  await Deno.writeTextFile(statePath, JSON.stringify(state, null, 2));
}

// Error codes and their descriptions
const ERROR_CODES = {
  E0001: {
    name: "Unexpected token",
    lesson: "Token classification",
    description:
      "The parser encountered a token it wasn't expecting at this position.",
    fix: "Check for typos, missing operators, or misplaced keywords.",
  },
  E0002: {
    name: "Unterminated string",
    lesson: "String literal rules",
    description: "A string was opened with a quote but never closed.",
    fix: 'Add a closing quote (") to the end of your string.',
  },
  E0003: {
    name: "Invalid escape sequence",
    lesson: "Escape processing",
    description: "An unrecognized escape sequence was found in a string.",
    fix: 'Valid escapes: \\n, \\r, \\t, \\\\, \\", \\0, \\xNN',
  },
  E0004: {
    name: "Illegal character",
    lesson: "Character sets",
    description:
      "A character was found that isn't valid in Error-Lang source code.",
    fix: "Remove the character or replace it with a valid one.",
  },
  E0005: {
    name: "Missing 'end'",
    lesson: "Block structure",
    description:
      "A block (main, function, if, while, for, gutter) was never closed.",
    fix: "Add 'end' to close the block.",
  },
  E0006: {
    name: "Unmatched parenthesis",
    lesson: "Expression nesting",
    description: "An opening parenthesis has no matching closing parenthesis.",
    fix: "Add the missing ) or remove the extra (.",
  },
  E0007: {
    name: "Unicode/smart quote",
    lesson: "Encoding awareness",
    description:
      'Smart quotes (\u201C \u201D) were used instead of straight quotes (").',
    fix: 'Replace curly quotes with straight double quotes (").',
  },
  E0008: {
    name: "Identifier rules violation",
    lesson: "Naming conventions",
    description:
      "An identifier contains invalid characters or starts with a number.",
    fix:
      "Identifiers must start with a letter or underscore, followed by letters, digits, or underscores.",
  },
  E0009: {
    name: "Reserved keyword misuse",
    lesson: "Keyword awareness",
    description:
      "A reserved keyword was used where an identifier was expected.",
    fix: "Choose a different name that isn't a reserved keyword.",
  },
  E0010: {
    name: "Whitespace significance",
    lesson: "Indentation rules",
    description: "Unexpected indentation or whitespace issue.",
    fix: "Check your indentation is consistent.",
  },
};

// Inject an error into gutter block based on run number
function selectErrorForRun(runNumber, seed) {
  const codes = Object.keys(ERROR_CODES);
  const index = (runNumber + seed) % codes.length;
  return codes[index];
}

// Simple tokenizer (placeholder - would import from ReScript compiler)
function tokenize(source, _filename, runNumber) {
  const tokens = [];
  const diagnostics = [];
  let pos = 0;
  let line = 1;
  let column = 1;

  const keywords = new Set([
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

  while (pos < source.length) {
    const start = { line, column, offset: pos };
    const ch = source[pos];

    // Skip whitespace (except newlines)
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
      while (pos < source.length && source[pos] !== "\n") {
        pos++;
      }
      continue;
    }

    // String
    if (ch === '"') {
      pos++;
      column++;
      let str = "";
      while (pos < source.length && source[pos] !== '"') {
        if (source[pos] === "\n") {
          diagnostics.push({
            code: "E0002",
            message: "Unterminated string literal",
            line: start.line,
            column: start.column,
            runNumber,
          });
          break;
        }
        if (source[pos] === "\\") {
          pos++;
          column++;
          const esc = source[pos];
          switch (esc) {
            case "n":
              str += "\n";
              break;
            case "r":
              str += "\r";
              break;
            case "t":
              str += "\t";
              break;
            case "\\":
              str += "\\";
              break;
            case '"':
              str += '"';
              break;
            default:
              diagnostics.push({
                code: "E0003",
                message: `Invalid escape sequence: \\${esc}`,
                line,
                column: column - 1,
                runNumber,
              });
          }
        } else {
          str += source[pos];
        }
        pos++;
        column++;
      }
      if (source[pos] === '"') {
        pos++;
        column++;
      }
      tokens.push({
        type: "STRING",
        value: str,
        lexeme: `"${str}"`,
        line: start.line,
        column: start.column,
      });
      continue;
    }

    // Smart quotes (common error!)
    if (ch === "\u201C" || ch === "\u201D") {
      diagnostics.push({
        code: "E0007",
        message: `Smart quote '${ch}' found - use straight quote '"' instead`,
        line,
        column,
        runNumber,
      });
      pos++;
      column++;
      continue;
    }

    // Number
    if (/[0-9]/.test(ch)) {
      let num = "";
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
        line: start.line,
        column: start.column,
      });
      continue;
    }

    // Identifier or keyword
    if (/[a-zA-Z_]/.test(ch)) {
      let ident = "";
      while (pos < source.length && /[a-zA-Z0-9_]/.test(source[pos])) {
        ident += source[pos];
        pos++;
        column++;
      }
      const type = keywords.has(ident) ? ident.toUpperCase() : "IDENTIFIER";
      tokens.push({
        type,
        value: ident,
        lexeme: ident,
        line: start.line,
        column: start.column,
      });
      continue;
    }

    // Operators and punctuation
    const ops = {
      "+": "PLUS",
      "-": "MINUS",
      "*": "STAR",
      "/": "SLASH",
      "%": "PERCENT",
      "=": "EQUAL",
      "!": "BANG",
      "<": "LESS",
      ">": "GREATER",
      "(": "LPAREN",
      ")": "RPAREN",
      "[": "LBRACKET",
      "]": "RBRACKET",
      "{": "LBRACE",
      "}": "RBRACE",
      ",": "COMMA",
      ".": "DOT",
      ":": "COLON",
      "?": "QUESTION",
      "&": "AMPERSAND",
      "|": "PIPE",
      "^": "CARET",
      "~": "TILDE",
    };

    // Two-character operators
    if (pos + 1 < source.length) {
      const two = ch + source[pos + 1];
      const twoOps = {
        "==": "EQUAL_EQUAL",
        "!=": "BANG_EQUAL",
        "<=": "LESS_EQUAL",
        ">=": "GREATER_EQUAL",
        "<<": "LESS_LESS",
        ">>": "GREATER_GREATER",
        "->": "ARROW",
      };
      if (twoOps[two]) {
        tokens.push({ type: twoOps[two], lexeme: two, line, column });
        pos += 2;
        column += 2;
        continue;
      }
    }

    if (ops[ch]) {
      tokens.push({ type: ops[ch], lexeme: ch, line, column });
      pos++;
      column++;
      continue;
    }

    // Unknown character
    diagnostics.push({
      code: "E0004",
      message: `Illegal character '${ch}'`,
      line,
      column,
      runNumber,
    });
    pos++;
    column++;
  }

  tokens.push({ type: "EOF", lexeme: "", line, column });
  return { tokens, diagnostics };
}

// Simple evaluator for basic expressions
function evaluate(expr, env) {
  switch (expr.type) {
    case "INTEGER":
    case "FLOAT":
      return expr.value;
    case "STRING":
      return expr.value;
    case "TRUE":
      return true;
    case "FALSE":
      return false;
    case "NIL":
      return null;
    case "IDENTIFIER":
      return env[expr.value] ?? null;
    case "BINARY": {
      const left = evaluate(expr.left, env);
      const right = evaluate(expr.right, env);
      switch (expr.op) {
        case "+":
          return left + right;
        case "-":
          return left - right;
        case "*":
          return left * right;
        case "/":
          return left / right;
        case "%":
          return left % right;
        case "==":
          return left === right;
        case "!=":
          return left !== right;
        case "<":
          return left < right;
        case ">":
          return left > right;
        case "<=":
          return left <= right;
        case ">=":
          return left >= right;
        case "and":
          return left && right;
        case "or":
          return left || right;
        default:
          return null;
      }
    }
    case "UNARY": {
      const operand = evaluate(expr.operand, env);
      switch (expr.op) {
        case "-":
          return -operand;
        case "not":
          return !operand;
        default:
          return null;
      }
    }
    case "CALL": {
      const fn = env[expr.callee];
      if (typeof fn === "function") {
        const args = expr.args.map((a) => evaluate(a, env));
        return fn(...args);
      }
      return null;
    }
    case "ARRAY":
      return expr.elements.map((e) => evaluate(e, env));
    case "INDEX": {
      const arr = evaluate(expr.array, env);
      const idx = evaluate(expr.index, env);
      return arr?.[idx];
    }
    default:
      return null;
  }
}

// Format diagnostic for output
function formatDiagnostic(diag, filename) {
  return `Error-LangError: ${filename}:${diag.line}:${diag.column}: ${diag.message} [code=${diag.code} run=${diag.runNumber}]`;
}

// Run command
async function runCommand(args) {
  const file = args._[0];
  if (!file) {
    console.error("Usage: error-lang run <file.err>");
    Deno.exit(1);
  }

  const filename = String(file);
  if (!await exists(filename)) {
    console.error(`File not found: ${filename}`);
    Deno.exit(1);
  }

  const source = await Deno.readTextFile(filename);
  const dir = dirname(filename) || ".";

  // Load state
  const state = await loadState(dir);
  state.runCounter++;

  // Determine seed
  const seed = args.seed ?? state.seed;
  const runId = args["run-id"] ?? state.runCounter;

  console.log(
    `[Run #${runId}, Seed: ${seed}, Stability: ${state.stabilityScore}]`,
  );
  console.log();

  // Tokenize
  const { tokens, diagnostics } = tokenize(source, filename, runId);

  // Inject error into gutter if present
  const gutterStart = tokens.findIndex((t) => t.type === "GUTTER");
  if (gutterStart !== -1) {
    const errorCode = selectErrorForRun(runId, seed);
    const errorInfo = ERROR_CODES[errorCode];

    // Find gutter end
    let gutterEnd = gutterStart + 1;
    let depth = 1;
    while (gutterEnd < tokens.length && depth > 0) {
      if (tokens[gutterEnd].type === "GUTTER") depth++;
      if (tokens[gutterEnd].type === "END") depth--;
      gutterEnd++;
    }

    // Add injected error
    const gutterToken = tokens[gutterStart];
    diagnostics.push({
      code: errorCode,
      message: `[INJECTED] ${errorInfo.name}: ${errorInfo.description}`,
      line: gutterToken.line + 1,
      column: 5,
      runNumber: runId,
      injected: true,
    });

    // Reduce stability
    state.stabilityScore = Math.max(0, state.stabilityScore - 5);
    state.lastError = errorCode;
  }

  // Output diagnostics
  for (const diag of diagnostics) {
    console.error(formatDiagnostic(diag, filename));
    if (diag.injected) {
      console.error(`  → Lesson: ${ERROR_CODES[diag.code].lesson}`);
      console.error(`  → Fix: ${ERROR_CODES[diag.code].fix}`);
    }
  }

  if (diagnostics.length > 0) {
    console.log();
  }

  // Simple execution (very basic - just handle print/println in main)
  const _env = {
    print: (...args) =>
      Deno.stdout.writeSync(new TextEncoder().encode(args.join(" "))),
    println: (...args) => console.log(...args),
  };

  // Find main block and execute prints
  let inMain = false;
  let inGutter = false;
  for (let i = 0; i < tokens.length; i++) {
    const tok = tokens[i];

    if (tok.type === "MAIN") {
      inMain = true;
      continue;
    }
    if (tok.type === "GUTTER") {
      inGutter = true;
      continue;
    }
    if (tok.type === "END") {
      if (inGutter) {
        inGutter = false;
      } else if (inMain) {
        inMain = false;
      }
      continue;
    }

    if (inMain && !inGutter) {
      // Handle print/println
      if (
        (tok.type === "IDENTIFIER" &&
          (tok.value === "print" || tok.value === "println")) ||
        tok.type === "PRINT" || tok.type === "PRINTLN"
      ) {
        const isPrintln = tok.value === "println" || tok.type === "PRINTLN";
        // Find arguments (simple: look for string after paren)
        if (tokens[i + 1]?.type === "LPAREN") {
          i += 2; // Skip print and (
          const args = [];
          while (i < tokens.length && tokens[i].type !== "RPAREN") {
            if (tokens[i].type === "STRING") {
              args.push(tokens[i].value);
            } else if (
              tokens[i].type === "INTEGER" || tokens[i].type === "FLOAT"
            ) {
              args.push(tokens[i].value);
            } else if (tokens[i].type === "TRUE") {
              args.push(true);
            } else if (tokens[i].type === "FALSE") {
              args.push(false);
            }
            i++;
          }
          if (isPrintln) {
            console.log(...args);
          } else {
            Deno.stdout.writeSync(new TextEncoder().encode(args.join(" ")));
          }
        }
      }
    }
  }

  // Save state
  await saveState(dir, state);

  console.log();
  console.log(`[Stability: ${state.stabilityScore}/100]`);
}

// Doctor command
function doctorCommand() {
  console.log("Error-Lang Environment Check");
  console.log("============================");
  console.log();
  console.log(`✓ Deno version: ${Deno.version.deno}`);
  console.log(`✓ V8 version: ${Deno.version.v8}`);
  console.log(`✓ TypeScript version: ${Deno.version.typescript}`);
  console.log(`✓ Platform: ${Deno.build.os} ${Deno.build.arch}`);
  console.log();
  console.log("All systems operational!");
}

// Explain command
function explainCommand(args) {
  const code = args._[0];

  if (args.all || !code) {
    console.log("Error-Lang Error Codes");
    console.log("======================");
    console.log();
    for (const [code, info] of Object.entries(ERROR_CODES)) {
      console.log(`${code}: ${info.name}`);
      console.log(`  Lesson: ${info.lesson}`);
      console.log(`  ${info.description}`);
      console.log(`  Fix: ${info.fix}`);
      console.log();
    }
    return;
  }

  const info = ERROR_CODES[code];
  if (!info) {
    console.error(`Unknown error code: ${code}`);
    console.error("Use --all to see all error codes.");
    Deno.exit(1);
  }

  console.log(`${code}: ${info.name}`);
  console.log();
  console.log(`Lesson: ${info.lesson}`);
  console.log();
  console.log("Description:");
  console.log(`  ${info.description}`);
  console.log();
  console.log("How to fix:");
  console.log(`  ${info.fix}`);
}

// Main
const args = parseArgs(Deno.args, {
  boolean: ["help", "all", "verbose"],
  string: ["seed", "run-id"],
  alias: { h: "help", v: "verbose" },
});

const command = args._[0];
args._ = args._.slice(1);

switch (command) {
  case "run":
    await runCommand(args);
    break;
  case "doctor":
    await doctorCommand();
    break;
  case "explain":
    explainCommand(args);
    break;
  case "help":
  case undefined:
    console.log(`
Error-Lang - A teaching language where code breaks intentionally

USAGE:
  error-lang <command> [options]

COMMANDS:
  run <file.err>     Run an Error-Lang program
  doctor             Check environment setup
  explain <code>     Explain an error code (e.g., E0002)
  explain --all      List all error codes
  help               Show this help message

OPTIONS:
  --seed <n>         Use deterministic error injection
  --run-id <n>       Simulate a specific run number
  --verbose          Show detailed output

EXAMPLES:
  error-lang run hello.err
  error-lang run hello.err --seed 42
  error-lang explain E0002
  error-lang explain --all
`);
    break;
  default:
    console.error(`Unknown command: ${command}`);
    console.error("Run 'error-lang help' for usage.");
    Deno.exit(1);
}
