#!/usr/bin/env -S deno run --allow-read
// SPDX-License-Identifier: MPL-2.0
// explore.js - Explore code through abstraction layers

import { exploreCode, traceLayerTransformation } from './layer-navigator.js';
import { parseArgs } from '@std/cli/parse-args';

/**
 * Error-Lang Layer Explorer
 *
 * Shows how code transforms through 5 layers:
 * Grammar → Parser → AST → Semantics → Runtime
 */

async function main() {
  const args = parseArgs(Deno.args, {
    string: ['code', 'file'],
    boolean: ['help', 'trace'],
    alias: { h: 'help', c: 'code', f: 'file', t: 'trace' },
  });

  if (args.help) {
    console.log(`
Error-Lang Layer Explorer

Usage:
  deno run --allow-read cli/explore.js [options]

Options:
  -c, --code <code>    Code snippet to explore
  -f, --file <file>    File to explore (reads first line)
  -t, --trace          Show transformation trace
  -h, --help           Show this help

Examples:
  # Explore a code snippet
  deno run --allow-read cli/explore.js --code "let x = 42"

  # Trace transformation
  deno run --allow-read cli/explore.js --code "let x = 42" --trace

  # Explore from file
  deno run --allow-read cli/explore.js --file examples/01-paradox-discovery.err
`);
    Deno.exit(0);
  }

  let code;

  if (args.code) {
    code = args.code;
  } else if (args.file) {
    const content = await Deno.readTextFile(args.file);
    const lines = content.split('\n').filter(l => !l.trim().startsWith('#') && l.trim());
    code = lines[0] || 'let x = 42';
  } else {
    code = 'let x = 42';
  }

  if (args.trace) {
    traceLayerTransformation(code, 1);
  } else {
    await exploreCode(code);
  }
}

if (import.meta.main) {
  main();
}
