// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#!/usr/bin/env -S deno run
// analyze.js - Five Whys analyzer CLI

import { interactiveFiveWhys } from './five-whys.js';
import { parseArgs } from '@std/cli/parse-args';

/**
 * Error-Lang Five Whys Analyzer
 *
 * Demonstrates root cause analysis for common issues
 */

async function main() {
  const args = parseArgs(Deno.args, {
    string: ['type'],
    boolean: ['help'],
    alias: { h: 'help', t: 'type' },
  });

  if (args.help) {
    console.log(`
Error-Lang Five Whys Analyzer

Demonstrates root cause analysis for common programming issues.

Usage:
  deno run cli/analyze.js --type <issue-type>

Issue Types:
  type-mismatch    Type error analysis
  mutation         Mutable state analysis
  null-cascade     Null propagation analysis
  performance      Performance cliff analysis

Examples:
  deno run cli/analyze.js --type mutation
  deno run cli/analyze.js --type performance
`);
    Deno.exit(0);
  }

  const issueType = args.type || 'mutation';

  // Demo issues
  const issues = {
    'type-mismatch': {
      type: 'type-mismatch',
      varName: 'x',
      expectedType: 'Int',
      actualType: 'String',
      line: 15,
    },
    'mutation': {
      type: 'mutation',
      varName: 'counter',
      line: 8,
      readerCount: 5,
    },
    'null-cascade': {
      type: 'null-cascade',
      line: 12,
      cascadeDepth: 4,
    },
    'performance': {
      type: 'performance',
      operation: 'array search',
      timeMs: 5000,
      expectedMs: 50,
    },
  };

  const issue = issues[issueType];

  if (!issue) {
    console.error(`Unknown issue type: ${issueType}`);
    console.error('Available types: type-mismatch, mutation, null-cascade, performance');
    Deno.exit(1);
  }

  await interactiveFiveWhys(issue);
}

if (import.meta.main) {
  main();
}
