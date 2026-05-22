// SPDX-License-Identifier: MPL-2.0
// layer-navigator.js - Terminal-based layer navigation

/**
 * Layer Navigator CLI
 *
 * Terminal interface for navigating through the 5 abstraction layers:
 * Grammar → Parser → AST → Semantics → Runtime
 *
 * Students can explore how their code transforms at each layer.
 */

const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
};

/**
 * Layer stack visualization
 */
export class LayerStack {
  constructor() {
    this.layers = [
      {
        name: 'Grammar',
        description: 'EBNF rules that define syntax',
        color: colors.magenta,
        example: 'letStmt ::= "let" identifier "=" expression',
      },
      {
        name: 'Parser',
        description: 'How text becomes tree structure',
        color: colors.blue,
        example: 'letStmt\n├─ "let"\n├─ identifier("x")\n└─ expression',
      },
      {
        name: 'AST',
        description: 'Abstract syntax tree (simplified)',
        color: colors.cyan,
        example: 'LetStmt { name: "x", value: IntLit(42) }',
      },
      {
        name: 'Semantics',
        description: 'Type checking and scope analysis',
        color: colors.yellow,
        example: 'Variable: x, Type: Int, Scope: local',
      },
      {
        name: 'Runtime',
        description: 'Actual execution and values',
        color: colors.green,
        example: 'x = 42 (stored at 0x7ff...)',
      },
    ];

    this.currentLayerIndex = 0;
  }

  /**
   * Navigate to next layer
   */
  next() {
    this.currentLayerIndex = Math.min(this.layers.length - 1, this.currentLayerIndex + 1);
    return this.getCurrentLayer();
  }

  /**
   * Navigate to previous layer
   */
  prev() {
    this.currentLayerIndex = Math.max(0, this.currentLayerIndex - 1);
    return this.getCurrentLayer();
  }

  /**
   * Jump to specific layer
   */
  goto(layerName) {
    const index = this.layers.findIndex(l =>
      l.name.toLowerCase() === layerName.toLowerCase()
    );

    if (index !== -1) {
      this.currentLayerIndex = index;
    }

    return this.getCurrentLayer();
  }

  /**
   * Get current layer
   */
  getCurrentLayer() {
    return this.layers[this.currentLayerIndex];
  }

  /**
   * Render the layer stack with current layer highlighted
   */
  render() {
    const lines = [];

    lines.push(`\n${colors.bright}═══ LAYER NAVIGATOR ═══${colors.reset}\n`);
    lines.push(`Navigate: [↑/↓] Previous/Next  [1-5] Jump  [q] Quit\n`);

    for (let i = 0; i < this.layers.length; i++) {
      const layer = this.layers[i];
      const isCurrent = i === this.currentLayerIndex;

      const marker = isCurrent ? '→' : ' ';
      const highlight = isCurrent ? colors.bright : colors.dim;
      const layerColor = isCurrent ? layer.color : colors.dim;

      lines.push(
        `${marker} ${highlight}[${i + 1}] ${layerColor}${layer.name}${colors.reset}` +
        ` ${colors.dim}${layer.description}${colors.reset}`
      );

      // Show arrow connecting layers
      if (i < this.layers.length - 1) {
        const arrowColor = (i === this.currentLayerIndex || i + 1 === this.currentLayerIndex)
          ? colors.bright
          : colors.dim;

        lines.push(`      ${arrowColor}↓${colors.reset}`);
      }
    }

    lines.push('');

    return lines.join('\n');
  }

  /**
   * Render detailed view of current layer
   */
  renderDetail(codeSnippet) {
    const layer = this.getCurrentLayer();

    const lines = [];

    lines.push(`\n${layer.color}${colors.bright}◆ ${layer.name.toUpperCase()}${colors.reset}`);
    lines.push(`${colors.dim}${'─'.repeat(60)}${colors.reset}\n`);

    // Show what this layer does
    lines.push(`${layer.color}What:${colors.reset} ${layer.description}\n`);

    // Show example for this layer
    lines.push(`${layer.color}Example:${colors.reset}`);
    lines.push(`${colors.dim}${layer.example}${colors.reset}\n`);

    // Show code snippet transformed at this layer
    if (codeSnippet) {
      lines.push(`${layer.color}Your code at this layer:${colors.reset}`);
      lines.push(this.transformCodeForLayer(codeSnippet, layer.name));
    }

    return lines.join('\n');
  }

  /**
   * Transform code snippet for current layer view
   */
  transformCodeForLayer(code, layerName) {
    // Simplified transformation - in full implementation would use compiler
    switch (layerName.toLowerCase()) {
      case 'grammar':
        return this.showGrammarRules(code);

      case 'parser':
        return this.showParseTree(code);

      case 'ast':
        return this.showAST(code);

      case 'semantics':
        return this.showSemantics(code);

      case 'runtime':
        return this.showRuntime(code);

      default:
        return code;
    }
  }

  showGrammarRules(code) {
    // Show which EBNF rules would match this code
    const lines = [];

    if (code.includes('let ')) {
      lines.push(`  letStmt ::= "let" identifier "=" expression`);
    }

    if (code.includes('println(')) {
      lines.push(`  printStmt ::= "println" "(" expression* ")"`);
    }

    if (code.includes('if ')) {
      lines.push(`  ifStmt ::= "if" expression statement* "end"`);
    }

    return lines.length > 0
      ? lines.join('\n')
      : `  statement ::= /* grammar rules */`;
  }

  showParseTree(code) {
    // Simplified parse tree
    if (code.includes('let x = 42')) {
      return `  letStmt
  ├─ "let"
  ├─ identifier("x")
  ├─ "="
  └─ literal(42)`;
    }

    return `  parseTree
  └─ statement(...)`;
  }

  showAST(code) {
    if (code.includes('let x = 42')) {
      return `  LetStmt {
    name: "x",
    value: IntLit(42),
    loc: { line: 1, col: 0 }
  }`;
    }

    return `  Statement { ... }`;
  }

  showSemantics(code) {
    if (code.includes('let x = 42')) {
      return `  Variable: x
  Type: Int (inferred)
  Scope: local
  Mutable: false`;
    }

    return `  Semantic analysis: [pending]`;
  }

  showRuntime(code) {
    if (code.includes('let x = 42')) {
      return `  Allocation: Stack frame
  Variable: x
  Value: 42 (0x2a)
  Memory: 8 bytes`;
    }

    return `  Runtime: [not executed]`;
  }
}

/**
 * Interactive layer explorer
 */
export async function exploreCode(codeSnippet) {
  const stack = new LayerStack();

  console.log('\n' + '═'.repeat(60));
  console.log(`${colors.bright}  ERROR-LANG LAYER EXPLORER${colors.reset}`);
  console.log('═'.repeat(60));

  console.log(`\nExploring: ${colors.cyan}${codeSnippet.trim()}${colors.reset}`);

  // Show all layers
  console.log(stack.render());

  // Show detail for each layer
  for (let i = 0; i < 5; i++) {
    stack.goto(['grammar', 'parser', 'ast', 'semantics', 'runtime'][i]);
    console.log(stack.renderDetail(codeSnippet));
  }

  console.log('\n' + '═'.repeat(60));
}

/**
 * Show layer trace for a specific line
 */
export function traceLayerTransformation(code, lineNumber) {
  console.log(`\n${colors.bright}LAYER TRANSFORMATION TRACE${colors.reset}`);
  console.log(`Line ${lineNumber}: ${colors.cyan}${code}${colors.reset}\n`);

  const transformations = [
    {
      layer: 'Grammar',
      color: colors.magenta,
      content: 'letStmt ::= "let" identifier "=" expression',
      description: 'Grammar rule matched',
    },
    {
      layer: 'Parser',
      color: colors.blue,
      content: 'letStmt { "let", identifier("x"), "=", literal(42) }',
      description: 'Text parsed into tokens',
    },
    {
      layer: 'AST',
      color: colors.cyan,
      content: 'LetStmt { name: "x", value: IntLit(42) }',
      description: 'Abstract syntax tree node',
    },
    {
      layer: 'Semantics',
      color: colors.yellow,
      content: 'Variable x: Int, Scope: local',
      description: 'Type and scope resolved',
    },
    {
      layer: 'Runtime',
      color: colors.green,
      content: 'x = 42 (allocated on stack)',
      description: 'Value stored in memory',
    },
  ];

  for (let i = 0; i < transformations.length; i++) {
    const t = transformations[i];

    console.log(`${t.color}${colors.bright}${i + 1}. ${t.layer}${colors.reset}`);
    console.log(`   ${colors.dim}${t.description}${colors.reset}`);
    console.log(`   ${t.content}`);

    if (i < transformations.length - 1) {
      console.log(`   ${colors.dim}↓${colors.reset}`);
    }
  }

  console.log('');
}
