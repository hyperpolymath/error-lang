// SPDX-License-Identifier: MPL-2.0
// runtime.js - Error-Lang runtime with stability tracking

import { StabilityTracker, isPrime } from './stability-tracker.js';
import { parseArgs } from '@std/cli/parse-args';
import { join } from '@std/path';

/**
 * Error-Lang Runtime
 *
 * Executes .err files with real-time stability tracking.
 * Implements paradoxes (scope leakage, positional operators, etc.)
 * Provides haptic feedback through stability changes.
 */

class ErrorLangRuntime {
  constructor(options = {}) {
    this.stability = new StabilityTracker();
    this.stateDir = options.stateDir || '.error-lang';
    this.seed = options.seed || Date.now();
    this.variables = new Map();
    this.scopes = []; // Stack of scopes
    this.currentLine = 0;
    this.quantumTypes = new Map(); // Track quantum type states
    this.arithmeticOps = 0; // Count arithmetic operations
    this.totalDrift = 0; // Cumulative drift
    this.nullInfected = new Map(); // Track null-infected variables
    this.nullCascadeDepth = 0; // Current cascade depth
  }

  /**
   * Execute an Error-Lang program
   */
  async execute(sourceFile) {
    console.log(`\n🎮 Error-Lang Runtime`);
    console.log(`   File: ${sourceFile}`);

    // Load previous state (for temporal effects)
    const prevState = await this.stability.loadState(this.stateDir);
    this.stability.runNumber = prevState.runNumber || 1;

    console.log(`   Run: #${this.stability.runNumber}`);
    console.log(`   Seed: ${this.seed}`);
    console.log('━'.repeat(60));

    // Read source file
    const source = await Deno.readTextFile(sourceFile);
    const lines = source.split('\n');

    // Execute line by line (simplified interpreter for demo)
    for (let i = 0; i < lines.length; i++) {
      this.currentLine = i + 1;
      const line = lines[i].trim();

      // Skip comments and empty lines
      if (line.startsWith('#') || line === '') continue;

      // Execute line
      try {
        await this.executeLine(line);
      } catch (error) {
        console.error(`\n❌ Error at line ${this.currentLine}: ${error.message}`);
        this.stability.recordEvent('unhandled-error', {
          line: this.currentLine,
          paths: 1,
        });
      }
    }

    // Print stability report
    console.log('\n');
    this.stability.printReport();

    // Save state for next run
    await this.stability.saveState(this.stateDir);

    return this.stability.getScore();
  }

  /**
   * Execute a single line (simplified interpreter)
   */
  async executeLine(line) {
    // Very simplified parsing - just handle basic constructs
    // In full implementation, would use the ReScript compiler

    // Variable declaration: let x = value
    if (line.startsWith('let ')) {
      const mutMatch = line.match(/let\s+(mut\s+)?(\w+)\s*=\s*(.+)/);
      if (mutMatch) {
        const isMutable = !!mutMatch[1];
        const varName = mutMatch[2];
        const valueExpr = mutMatch[3];

        // Check for context-collapse keyword paradox
        const keywords = ['end', 'let', 'if', 'while', 'function'];
        if (keywords.includes(varName)) {
          console.log(`   ⚡ Context-collapse: '${varName}' is both keyword and identifier`);
          this.stability.recordEvent('paradox-discovered', {
            type: 'context-collapse',
            keyword: varName,
            line: this.currentLine,
          });
        }

        const value = this.evaluateExpression(valueExpr);

        // NULL PROPAGATION CASCADE
        if (value === null) {
          // Track null infection
          const sourceVar = valueExpr.trim();
          const source = this.nullInfected.get(sourceVar);
          const depth = source ? source.depth + 1 : 1;

          this.nullInfected.set(varName, {
            depth,
            source: sourceVar,
            infectedAt: this.currentLine,
          });

          console.log(`   🦠 Null infection: '${varName}' infected from '${sourceVar}' (depth ${depth})`);

          // Record cascade event
          this.stability.recordEvent('null-access', {
            variable: varName,
            source: sourceVar,
            depth,
            line: this.currentLine,
            cascadePath: this.buildCascadePath(varName),
          });

          this.nullCascadeDepth = Math.max(this.nullCascadeDepth, depth);
        }

        if (isMutable) {
          // Mutable variable - record stability impact
          this.stability.recordEvent('mutation', {
            variable: varName,
            line: this.currentLine,
            readers: 0, // Will update when read
          });
        }

        // Check if this creates a quantum type (no explicit type annotation)
        // In full implementation, would check for type annotation syntax
        // For now, assume all variables without type are quantum
        if (value !== null) {
          const possibleTypes = this.inferPossibleTypes(value);
          if (possibleTypes.length > 1) {
            console.log(`   🌀 Type superposition: '${varName}' could be ${possibleTypes.join(' | ')}`);

            this.quantumTypes.set(varName, {
              possibleTypes,
              collapsed: false,
              collapsedType: null,
              declaredLine: this.currentLine,
            });
          }
        }

        // Store variable in current scope or global
        if (this.scopes.length > 0) {
          const currentScope = this.scopes[this.scopes.length - 1];
          currentScope.variables.set(varName, { value, mutable: isMutable });
        } else {
          this.variables.set(varName, { value, mutable: isMutable });
        }

        return;
      }
    }

    // Assignment: x = value
    if (line.includes('=') && !line.startsWith('let')) {
      const [varName, valueExpr] = line.split('=').map(s => s.trim());

      const varInfo = this.variables.get(varName);
      if (varInfo && varInfo.mutable) {
        const oldValue = varInfo.value;
        const newValue = this.evaluateExpression(valueExpr);

        // Check for type change
        if (typeof oldValue !== typeof newValue) {
          this.stability.recordEvent('type-change', {
            variable: varName,
            oldType: typeof oldValue,
            newType: typeof newValue,
            line: this.currentLine,
          });
        }

        varInfo.value = newValue;
      }

      return;
    }

    // Print: println(...)
    if (line.startsWith('println(')) {
      const argsMatch = line.match(/println\((.*)\)/);
      if (argsMatch) {
        const args = argsMatch[1].split(',').map(arg => {
          const argName = arg.trim();

          // Check if this is a quantum variable being observed
          if (this.quantumTypes.has(argName)) {
            const quantum = this.quantumTypes.get(argName);

            if (!quantum.collapsed) {
              // COLLAPSE THE WAVEFUNCTION!
              const collapsedType = this.collapseQuantumType(
                quantum.possibleTypes,
                'print',
                this.currentLine
              );

              quantum.collapsed = true;
              quantum.collapsedType = collapsedType;

              console.log(`   🌀 Type collapse: '${argName}' → ${collapsedType} (observed via println)`);

              this.stability.recordEvent('paradox-discovered', {
                type: 'type-superposition',
                variable: argName,
                collapsedType,
                line: this.currentLine,
              });
            }
          }

          const value = this.evaluateExpression(argName);
          return value;
        });

        console.log(`   ${args.join(' ')}`);
      }
      return;
    }

    // Scope: main, if, etc.
    if (line === 'main' || line.startsWith('if ')) {
      this.scopes.push({
        variables: new Map(),
        startLine: this.currentLine,
      });
      return;
    }

    // End scope
    if (line === 'end') {
      // Check for scope leakage paradox
      // On prime runs, variables "leak" out of scope
      const isPrimeRun = isPrime(this.stability.runNumber);

      if (this.scopes.length > 0) {
        const scope = this.scopes.pop();

        if (isPrimeRun && scope.variables.size > 0) {
          console.log(`   🕐 Scope leakage active (run #${this.stability.runNumber} is prime)`);

          // LEAK variables into parent scope (or global if no parent)
          for (const [name, info] of scope.variables.entries()) {
            console.log(`      → Variable '${name}' leaked from scope!`);
            this.variables.set(name, info);
          }

          this.stability.recordEvent('paradox-discovered', {
            type: 'scope-leakage',
            runNumber: this.stability.runNumber,
            leakedVars: Array.from(scope.variables.keys()),
            line: this.currentLine,
          });
        } else if (scope.variables.size > 0) {
          // Normal behavior - variables stay in scope, not accessible outside
          for (const [name, _] of scope.variables.entries()) {
            console.log(`   ✓ Variable '${name}' properly scoped (non-prime run)`);
          }
        }
      }

      return;
    }
  }

  /**
   * Evaluate an expression (simplified)
   */
  evaluateExpression(expr) {
    expr = expr.trim();

    // Null literal
    if (expr === 'null' || expr === 'undefined') {
      return null;
    }

    // String literal
    if (expr.startsWith('"') && expr.endsWith('"')) {
      return expr.slice(1, -1);
    }

    // Number literal
    if (/^-?\d+(\.\d+)?$/.test(expr)) {
      return expr.includes('.') ? parseFloat(expr) : parseInt(expr);
    }

    // Variable reference - check scopes from innermost to outermost
    // First check current scopes
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      const scope = this.scopes[i];
      if (scope.variables.has(expr)) {
        const varInfo = scope.variables.get(expr);
        return varInfo.value;
      }
    }

    // Then check global variables
    if (this.variables.has(expr)) {
      const varInfo = this.variables.get(expr);
      return varInfo.value;
    }

    // Binary operation - handle +, -, *, /
    const operators = [
      { op: ' + ', name: 'addition', behavior: '+' },
      { op: ' - ', name: 'subtraction', behavior: '-' },
      { op: ' * ', name: 'multiplication', behavior: '*' },
      { op: ' / ', name: 'division', behavior: '/' },
    ];

    for (const { op, name, behavior } of operators) {
      if (expr.includes(op)) {
        const [left, right] = expr.split(op).map(e => this.evaluateExpression(e.trim()));

        // NULL PROPAGATION CASCADE
        // If either operand is null, result is null (the infection spreads!)
        if (left === null || right === null) {
          console.log(`   🦠 Null cascade: ${expr} → null (${left === null ? 'left' : 'right'} operand is null)`);

          // Track which variables were involved
          const leftExpr = expr.split(op)[0].trim();
          const rightExpr = expr.split(op)[1].trim();
          const sourceVar = left === null ? leftExpr : rightExpr;

          const source = this.nullInfected.get(sourceVar);
          const depth = source ? source.depth + 1 : 2;

          // Record the cascade
          this.stability.recordEvent('null-access', {
            operation: name,
            source: sourceVar,
            depth,
            line: this.currentLine,
            cascadePath: this.buildCascadePath(sourceVar),
          });

          return null; // The infection spreads!
        }

        let result;

        // Positional operator paradox (only for +)
        if (behavior === '+') {
          const opBehavior = this.getOperatorBehavior('+', this.currentLine, 10);

          switch (opBehavior) {
            case 'addition':
              result = left + right;
              break;
            case 'concatenation':
              return String(left) + String(right); // No drift for string concat
            case 'subtraction':
              result = left - right;
              break;
            default:
              result = left + right;
          }
        } else {
          // Standard arithmetic
          switch (behavior) {
            case '-':
              result = left - right;
              break;
            case '*':
              result = left * right;
              break;
            case '/':
              result = left / right;
              break;
          }
        }

        // ARITHMETIC DRIFT PARADOX
        // Add small random error that accumulates over time
        if (typeof result === 'number' && !Number.isNaN(result)) {
          this.arithmeticOps++;

          // Drift increases with number of operations
          const driftMagnitude = this.getDriftMagnitude(name, this.arithmeticOps);
          const drift = (Math.random() - 0.5) * 2 * driftMagnitude;

          result += drift;
          this.totalDrift += Math.abs(drift);

          if (this.arithmeticOps % 10 === 0 || this.totalDrift > 0.01) {
            console.log(`   📊 Arithmetic drift: operation #${this.arithmeticOps}, ` +
                        `result=${result.toFixed(6)}, total drift=${this.totalDrift.toFixed(6)}`);
          }

          // Record drift event periodically
          if (this.arithmeticOps % 5 === 0) {
            this.stability.recordEvent('arithmetic-drift', {
              operation: name,
              operationCount: this.arithmeticOps,
              totalDrift: this.totalDrift,
              line: this.currentLine,
            });
          }
        }

        return result;
      }
    }

    // Default: return as string
    return expr;
  }

  /**
   * Infer possible types for a value
   */
  inferPossibleTypes(value) {
    const actualType = typeof value;

    switch (actualType) {
      case 'number':
        // Number could be Int, Float, or String
        return Number.isInteger(value)
          ? ['Int', 'Float', 'String']
          : ['Float', 'String'];

      case 'string':
        // String could be String, or coercible to Int/Float
        return ['String', 'Int', 'Float'];

      case 'boolean':
        // Bool could be Bool, Int (0/1), or String
        return ['Bool', 'Int', 'String'];

      default:
        return ['Unknown'];
    }
  }

  /**
   * Collapse quantum type based on observation context
   */
  collapseQuantumType(possibleTypes, context, line) {
    // Deterministic collapse based on:
    // 1. Context (print, arithmetic, etc.)
    // 2. Line number
    // 3. Seed

    const contextHash = {
      'print': 3,
      'arithmetic': 0,
      'comparison': 2,
      'assignment': 4,
    }[context] || 0;

    const hash = (this.seed + line * 17 + contextHash) % possibleTypes.length;
    return possibleTypes[hash];
  }

  /**
   * Get operator behavior based on position (paradox)
   */
  getOperatorBehavior(op, line, column) {
    if (op === '+') {
      const hash = (line * 31 + column) % 4;

      const behaviors = ['addition', 'concatenation', 'subtraction', 'xor'];
      const behavior = behaviors[hash];

      // Record paradox discovery
      if (behavior !== 'addition') {
        console.log(`   🎲 Positional operator: '+' at line ${line} means ${behavior}`);
        this.stability.recordEvent('paradox-discovered', {
          type: 'positional-operator',
          operator: op,
          behavior,
          line,
        });
      }

      return behavior;
    }

    return 'unknown';
  }

  /**
   * Get drift magnitude for an operation
   * Drift increases with operation count
   */
  getDriftMagnitude(operation, opCount) {
    // Base drift by operation type
    const baseDrift = {
      'addition': 0.0001,
      'subtraction': 0.0001,
      'multiplication': 0.001,
      'division': 0.01,
    }[operation] || 0.0001;

    // Drift compounds as operations accumulate
    if (opCount <= 10) {
      return baseDrift;
    } else if (opCount <= 50) {
      return baseDrift * 5;
    } else {
      return baseDrift * 20;
    }
  }

  /**
   * Build the cascade path for null propagation visualization
   */
  buildCascadePath(varName) {
    const path = [];
    let current = varName;

    while (current && this.nullInfected.has(current)) {
      const infection = this.nullInfected.get(current);
      path.push({
        variable: current,
        line: infection.infectedAt,
        depth: infection.depth,
      });
      current = infection.source;
    }

    // Add the source
    if (current) {
      path.push({
        variable: current,
        line: this.currentLine,
        depth: 0,
      });
    }

    return path.reverse();
  }
}

/**
 * CLI Entry Point
 */
async function main() {
  const args = parseArgs(Deno.args, {
    string: ['seed'],
    default: {
      seed: Date.now().toString(),
    },
  });

  if (args._.length === 0) {
    console.log('Usage: deno run --allow-read --allow-write cli/runtime.js <file.err>');
    console.log('\nOptions:');
    console.log('  --seed <number>  Set random seed for deterministic behavior');
    Deno.exit(1);
  }

  const sourceFile = args._[0];
  const seed = parseInt(args.seed);

  const runtime = new ErrorLangRuntime({ seed });

  try {
    const finalScore = await runtime.execute(sourceFile);
    Deno.exit(finalScore >= 60 ? 0 : 1);
  } catch (error) {
    console.error(`\n💥 Runtime error: ${error.message}`);
    Deno.exit(1);
  }
}

// Run if executed directly
if (import.meta.main) {
  main();
}

export { ErrorLangRuntime };
