// SPDX-License-Identifier: PMPL-1.0-or-later
// five-whys.js - Five Whys root cause analyzer

/**
 * Five Whys Analyzer
 *
 * Traces from symptom → root cause through iterative "why" questions.
 * Teaches causal reasoning instead of pattern matching.
 */

const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  cyan: '\x1b[36m',
  green: '\x1b[32m',
  magenta: '\x1b[35m',
};

/**
 * Five Whys analysis for type mismatch
 */
export function analyzeTypeMismatch(varName, expectedType, actualType, line) {
  const whys = [
    {
      num: 1,
      question: 'Why did we get a type mismatch?',
      answer: `Variable '${varName}' has type ${actualType}, but ${expectedType} was expected`,
      evidence: [`Line ${line}: type mismatch detected`],
      layer: 'Semantics',
    },
    {
      num: 2,
      question: `Why does '${varName}' have type ${actualType}?`,
      answer: 'The variable was assigned a value of that type earlier',
      evidence: ['Type inference from initial assignment'],
      layer: 'Semantics',
    },
    {
      num: 3,
      question: 'Why was this type assignment allowed?',
      answer: 'No explicit type annotation was provided',
      evidence: ['Variable declared without type constraint'],
      layer: 'Grammar',
    },
    {
      num: 4,
      question: 'Why does missing type annotation cause problems?',
      answer: 'Without annotations, the compiler infers types, which may not match intent',
      evidence: ['Type inference is permissive without constraints'],
      layer: 'Semantics',
    },
    {
      num: 5,
      question: 'Why have type inference instead of required annotations?',
      answer: 'Design tradeoff: flexibility vs safety',
      evidence: [
        'Flexibility: Less code to write',
        'Safety: More runtime errors possible',
      ],
      layer: null,
    },
  ];

  return {
    symptom: `Type mismatch: expected ${expectedType}, got ${actualType}`,
    whys,
    rootCause: 'Design tradeoff between flexibility and safety',
    recommendation: `Add explicit type annotation: let ${varName}: ${expectedType} = ...`,
  };
}

/**
 * Five Whys analysis for mutation
 */
export function analyzeMutation(varName, mutationLine, readerCount) {
  const whys = [
    {
      num: 1,
      question: 'Why is stability dropping?',
      answer: `Mutable variable '${varName}' is being modified`,
      evidence: [`Mutation at line ${mutationLine}`],
      layer: 'Runtime',
    },
    {
      num: 2,
      question: 'Why does mutation reduce stability?',
      answer: `Mutation affects ${readerCount} other locations that read this variable`,
      evidence: ['Each mutation creates action-at-a-distance'],
      layer: 'Semantics',
    },
    {
      num: 3,
      question: 'Why do readers get affected?',
      answer: 'Shared mutable state creates invisible dependencies',
      evidence: [
        'Each reader depends on current value',
        'Mutation changes value for ALL readers',
        'Order of reads matters',
      ],
      layer: 'Semantics',
    },
    {
      num: 4,
      question: 'Why use shared mutable state?',
      answer: "Variable was declared with 'mut' keyword",
      evidence: ['Explicit mutability declaration'],
      layer: 'Grammar',
    },
    {
      num: 5,
      question: 'Why does mutability exist in the language?',
      answer: 'Design tradeoff: performance vs simplicity',
      evidence: [
        'Mutation: Fast in-place updates',
        'Immutability: Easier to reason about, no side effects',
      ],
      layer: null,
    },
  ];

  return {
    symptom: `Stability dropped due to mutation of '${varName}'`,
    whys,
    rootCause: 'Design tradeoff between performance and simplicity',
    recommendation: 'Use immutable data with functional updates (map, filter, reduce)',
  };
}

/**
 * Five Whys analysis for null cascade
 */
export function analyzeNullCascade(originLine, cascadeDepth) {
  const whys = [
    {
      num: 1,
      question: 'Why did the program crash?',
      answer: 'Null pointer access occurred',
      evidence: [`Null access at line ${originLine}`],
      layer: 'Runtime',
    },
    {
      num: 2,
      question: 'Why was the value null?',
      answer: 'A function returned null instead of a value',
      evidence: ['Function can return T or Nil'],
      layer: 'Semantics',
    },
    {
      num: 3,
      question: "Why wasn't the null case handled?",
      answer: 'No null check or pattern matching was performed',
      evidence: [
        'Direct access without checking',
        `Propagated through ${cascadeDepth} levels`,
      ],
      layer: 'Semantics',
    },
    {
      num: 4,
      question: 'Why is null checking optional?',
      answer: 'The language allows nullable types without forcing checks',
      evidence: ['No Option type enforcement'],
      layer: 'Grammar',
    },
    {
      num: 5,
      question: 'Why does the language allow nullable types?',
      answer: 'Design tradeoff: convenience vs safety',
      evidence: [
        'Convenience: No boilerplate for null checks',
        'Safety: Runtime errors from unchecked nulls',
        "Tony Hoare called null his 'billion dollar mistake'",
      ],
      layer: null,
    },
  ];

  return {
    symptom: 'Null pointer exception with cascade',
    whys,
    rootCause: 'Design tradeoff between convenience and safety',
    recommendation: 'Use Option type with pattern matching to force null handling',
  };
}

/**
 * Five Whys analysis for performance cliff
 */
export function analyzePerformance(operation, timeMs, expectedMs) {
  const slowdown = Math.floor(timeMs / expectedMs);

  const whys = [
    {
      num: 1,
      question: 'Why is the code slow?',
      answer: `Operation took ${timeMs}ms, expected ${expectedMs}ms (${slowdown}x slower)`,
      evidence: [`Performance measurement: ${operation}`],
      layer: 'Runtime',
    },
    {
      num: 2,
      question: `Why is ${operation} so slow?`,
      answer: 'Algorithm has O(n²) or worse complexity',
      evidence: ['Nested loops detected', 'Repeated scans through data'],
      layer: 'AST',
    },
    {
      num: 3,
      question: 'Why use an O(n²) algorithm?',
      answer: 'Wrong data structure chosen for the operation',
      evidence: [
        'Using array/list for lookups',
        'Should use hash table or index',
      ],
      layer: 'Semantics',
    },
    {
      num: 4,
      question: 'Why was the wrong data structure chosen?',
      answer: 'No explicit data structure selection in code',
      evidence: ['Default collection used without optimization'],
      layer: 'Grammar',
    },
    {
      num: 5,
      question: "Why doesn't the language enforce efficient structures?",
      answer: 'Design tradeoff: ease of use vs performance',
      evidence: [
        'Simple syntax: Easy to write, potentially slow',
        'Explicit structures: More code, but faster',
      ],
      layer: null,
    },
  ];

  return {
    symptom: `Performance cliff: ${slowdown}x slower than expected`,
    whys,
    rootCause: 'Design tradeoff between ease of use and performance',
    recommendation:
      'Use hash-based data structure for O(1) lookups instead of O(n) scans',
  };
}

/**
 * Format and print Five Whys analysis
 */
export function printAnalysis(analysis) {
  console.log(`\n${colors.cyan}${colors.bright}🔍 FIVE WHYS ROOT CAUSE ANALYSIS${colors.reset}`);
  console.log('═'.repeat(60));

  console.log(`\n${colors.red}Symptom:${colors.reset} ${analysis.symptom}\n`);

  // Print each "why" with indentation showing depth
  for (const why of analysis.whys) {
    const layerColor = getLayerColor(why.layer);
    const layerTag = why.layer ? ` ${colors.dim}[${why.layer}]${colors.reset}` : '';

    console.log(
      `${colors.yellow}WHY ${why.num}:${layerTag}${colors.reset} ${why.question}`
    );
    console.log(`  ${colors.green}→${colors.reset} ${why.answer}`);

    if (why.evidence.length > 0) {
      for (const evidence of why.evidence) {
        console.log(`     ${colors.dim}•${colors.reset} ${evidence}`);
      }
    }

    if (why.num < analysis.whys.length) {
      console.log(`  ${colors.dim}↓${colors.reset}`);
    }
  }

  console.log(`\n${colors.magenta}ROOT CAUSE:${colors.reset} ${analysis.rootCause}`);
  console.log(`\n${colors.cyan}💡 RECOMMENDATION:${colors.reset}`);
  console.log(`   ${analysis.recommendation}`);
  console.log('\n' + '═'.repeat(60));
}

/**
 * Get color for layer
 */
function getLayerColor(layer) {
  const layerColors = {
    Runtime: colors.green,
    Semantics: colors.yellow,
    AST: colors.cyan,
    Parser: colors.blue,
    Grammar: colors.magenta,
  };

  return layerColors[layer] || colors.reset;
}

/**
 * Interactive Five Whys session
 */
export async function interactiveFiveWhys(issue) {
  console.log(`\n${colors.bright}Starting Five Whys Analysis...${colors.reset}\n`);

  let analysis;

  // Determine which analyzer to use based on issue type
  if (issue.type === 'type-mismatch') {
    analysis = analyzeTypeMismatch(
      issue.varName,
      issue.expectedType,
      issue.actualType,
      issue.line
    );
  } else if (issue.type === 'mutation') {
    analysis = analyzeMutation(issue.varName, issue.line, issue.readerCount || 3);
  } else if (issue.type === 'null-cascade') {
    analysis = analyzeNullCascade(issue.line, issue.cascadeDepth || 3);
  } else if (issue.type === 'performance') {
    analysis = analyzePerformance(
      issue.operation,
      issue.timeMs,
      issue.expectedMs
    );
  } else {
    console.log('Unknown issue type');
    return;
  }

  printAnalysis(analysis);
}
