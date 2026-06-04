// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// stability-tracker.js - Real-time stability tracking and consequence amplification

import {
  AnimatedStabilityBar,
  RippleEffect,
  CascadeVisualization,
  PerformanceCliff,
  LayerIndicator,
  StabilityTrend,
  getConsequenceIcon,
} from './visual-feedback.js';

/**
 * Stability Tracker - The "Haptic Feedback" System
 *
 * This module tracks stability score in real-time as code executes,
 * providing immediate feedback on design decisions.
 *
 * Like feeling hammer weight or stone resistance - students FEEL
 * the computational substrate through stability changes.
 */

export class StabilityTracker {
  constructor(options = {}) {
    this.baseScore = 100;
    this.currentScore = 100;
    this.factors = [];
    this.history = [];
    this.runNumber = 1;
    this.seed = Date.now();

    // Visual feedback components
    this.visualMode = options.visual !== false;
    if (this.visualMode) {
      this.stabilityBar = new AnimatedStabilityBar(100);
      this.ripples = new RippleEffect();
      this.trend = new StabilityTrend();
    }
  }

  /**
   * Record a stability-affecting event
   */
  recordEvent(eventType, details) {
    const impact = this.calculateImpact(eventType, details);

    const event = {
      type: eventType,
      impact,
      details,
      timestamp: Date.now(),
      line: details.line || 0,
    };

    this.factors.push(event);
    this.currentScore = Math.max(0, this.currentScore + impact);
    this.history.push({ score: this.currentScore, event });

    // Update visual feedback
    if (this.visualMode) {
      this.stabilityBar.setScore(this.currentScore);
      this.trend.addPoint(this.currentScore);

      // Show ripple for mutations
      if (eventType === 'mutation' && details.readers) {
        const affectedLines = details.affectedLines || [];
        console.log(this.ripples.render(details.line, affectedLines));
      }

      // Show cascade for null propagation
      if (eventType === 'null-access' && details.depth > 1) {
        const cascadePath = details.cascadePath || [{ type: 'null-access', line: details.line }];
        console.log(CascadeVisualization.render(cascadePath));
      }

      // Show performance cliff
      if (eventType === 'performance-cliff') {
        console.log(PerformanceCliff.render(
          details.operation || 'operation',
          details.time_ms || 0,
          details.expected_ms || 10
        ));
      }
    }

    return impact;
  }

  /**
   * Calculate impact of an event on stability
   * This implements the "consequence amplification" - making costs visible
   */
  calculateImpact(eventType, details) {
    switch (eventType) {
      case 'mutation':
        // Mutable state: -10 per mutation, -5 per reader
        return -(10 + (details.readers || 0) * 5);

      case 'type-change':
        // Dynamic type reassignment: -15 per change
        return -15;

      case 'null-access':
        // Null propagation: -20 per level of cascade
        return -(20 * (details.depth || 1));

      case 'global-mutation':
        // Global state mutation: -30 base + -5 per dependency
        return -(30 + (details.dependencies || 0) * 5);

      case 'unhandled-error':
        // Error path not handled: -25 per path
        return -(25 * (details.paths || 1));

      case 'performance-cliff':
        // Algorithm complexity: -(time_ms / 10)
        // Amplify slow algorithms!
        return -Math.floor((details.time_ms || 0) / 10);

      case 'memory-leak':
        // Memory not freed: -10 per KB
        return -Math.floor((details.bytes || 0) / 1024) * 10;

      case 'race-condition':
        // Unsynchronized shared access: -40 per conflict
        return -(40 * (details.conflicts || 1));

      case 'arithmetic-drift':
        // Floating-point precision errors accumulate
        // Penalty based on cumulative drift magnitude
        const drift = details.totalDrift || 0;
        if (drift < 0.01) return -5;      // Small drift: -5
        if (drift < 0.1) return -15;      // Medium drift: -15
        if (drift < 1.0) return -30;      // Large drift: -30
        return -50;                        // Massive drift: -50

      case 'paradox-discovered':
        // Discovering a paradox is educational!
        return 0; // No penalty, just record it

      default:
        return 0;
    }
  }

  /**
   * Get current stability score
   */
  getScore() {
    return this.currentScore;
  }

  /**
   * Get breakdown by category
   */
  getBreakdown() {
    const breakdown = {
      mutability: 0,
      types: 0,
      'null-handling': 0,
      'global-state': 0,
      'error-handling': 0,
      performance: 0,
      memory: 0,
      concurrency: 0,
      'arithmetic-precision': 0,
    };

    for (const event of this.factors) {
      const category = this.eventToCategory(event.type);
      breakdown[category] += event.impact;
    }

    return breakdown;
  }

  /**
   * Map event type to category
   */
  eventToCategory(eventType) {
    const mapping = {
      'mutation': 'mutability',
      'type-change': 'types',
      'null-access': 'null-handling',
      'global-mutation': 'global-state',
      'unhandled-error': 'error-handling',
      'performance-cliff': 'performance',
      'memory-leak': 'memory',
      'race-condition': 'concurrency',
      'arithmetic-drift': 'arithmetic-precision',
    };

    return mapping[eventType] || 'other';
  }

  /**
   * Generate recommendations for stabilization
   */
  getRecommendations() {
    const recommendations = [];
    const breakdown = this.getBreakdown();

    // Analyze each category and suggest fixes
    if (breakdown.mutability < -20) {
      recommendations.push({
        category: 'mutability',
        severity: 'high',
        message: 'High mutation cost detected',
        suggestions: [
          'Consider using immutable data structures',
          'Use functional updates (map, filter, reduce)',
          'Pass values as parameters instead of mutating',
        ],
      });
    }

    if (breakdown.types < -15) {
      recommendations.push({
        category: 'types',
        severity: 'high',
        message: 'Type instability detected',
        suggestions: [
          'Add explicit type annotations',
          'Use different variable names for different types',
          'Consider static typing for this section',
        ],
      });
    }

    if (breakdown['null-handling'] < -30) {
      recommendations.push({
        category: 'null-handling',
        severity: 'critical',
        message: 'Null propagation cascade detected',
        suggestions: [
          'Use pattern matching to handle null cases',
          'Consider Option type with explicit handling',
          'Validate inputs at API boundaries',
        ],
      });
    }

    if (breakdown.performance < -50) {
      recommendations.push({
        category: 'performance',
        severity: 'critical',
        message: 'Performance cliff detected',
        suggestions: [
          'Review algorithm complexity (likely O(n²) or worse)',
          'Consider more efficient data structure',
          'Use indexing or hash-based lookup',
        ],
      });
    }

    if (breakdown['arithmetic-precision'] < -20) {
      recommendations.push({
        category: 'arithmetic-precision',
        severity: 'high',
        message: 'Arithmetic drift detected',
        suggestions: [
          'Use integer arithmetic when possible (e.g., cents instead of dollars)',
          'Consider arbitrary-precision decimal types for financial calculations',
          'Reduce number of chained operations to minimize error accumulation',
          'Use epsilon-based comparison for floating-point equality',
        ],
      });
    }

    return recommendations;
  }

  /**
   * Generate stability report (displayed after execution)
   */
  generateReport() {
    const breakdown = this.getBreakdown();
    const recommendations = this.getRecommendations();

    return {
      score: this.currentScore,
      breakdown,
      recommendations,
      factors: this.factors,
      history: this.history,
      runNumber: this.runNumber,
    };
  }

  /**
   * Visualize stability bar (ASCII art for terminal)
   */
  visualizeStabilityBar() {
    const score = this.currentScore;
    const filled = Math.floor(score / 5); // 20 blocks for 100 score
    const empty = 20 - filled;

    let color;
    if (score >= 80) color = '\x1b[32m'; // Green
    else if (score >= 60) color = '\x1b[33m'; // Yellow
    else if (score >= 40) color = '\x1b[31m'; // Red
    else color = '\x1b[35m'; // Magenta (critical)

    const reset = '\x1b[0m';
    const bar = '█'.repeat(filled) + '░'.repeat(empty);

    return `${color}[${bar}] ${score}/100${reset}`;
  }

  /**
   * Pretty-print stability report to console
   */
  printReport() {
    const report = this.generateReport();

    console.log('\n🎯 STABILITY REPORT');
    console.log('━'.repeat(60));

    // Use animated bar if available
    if (this.visualMode && this.stabilityBar) {
      console.log(`\n${this.stabilityBar.render()}`);

      // Show trend
      if (this.trend) {
        console.log(this.trend.render());
      }
    } else {
      console.log(`\n${this.visualizeStabilityBar()}\n`);
    }

    console.log('\nBreakdown:');
    for (const [category, impact] of Object.entries(report.breakdown)) {
      if (impact !== 0) {
        const emoji = this.categoryEmoji(category);
        console.log(`   ${emoji} ${category}: ${impact} points`);
      }
    }

    if (report.recommendations.length > 0) {
      console.log('\n💡 Top Recommendations:');
      for (let i = 0; i < Math.min(3, report.recommendations.length); i++) {
        const rec = report.recommendations[i];
        console.log(`   ${i + 1}. ${rec.message}`);
        if (rec.suggestions.length > 0) {
          console.log(`      → ${rec.suggestions[0]}`);
        }
      }
    }

    console.log('\n━'.repeat(60));
  }

  /**
   * Emoji for stability category
   */
  categoryEmoji(category) {
    const emojis = {
      'mutability': '🔴',
      'types': '⚠️',
      'null-handling': '☠️',
      'global-state': '🌍',
      'error-handling': '💥',
      'performance': '🐌',
      'memory': '💧',
      'concurrency': '⚡',
    };

    return emojis[category] || '❓';
  }

  /**
   * Load state from previous runs (temporal persistence)
   */
  async loadState(stateDir) {
    try {
      const statePath = `${stateDir}/state.json`;
      const data = await Deno.readTextFile(statePath);
      const state = JSON.parse(data);

      this.runNumber = (state.runNumber || 0) + 1;
      return state;
    } catch {
      // No previous state, first run
      return { runNumber: 1, totalRuns: 0 };
    }
  }

  /**
   * Save state for future runs (temporal corruption feature)
   */
  async saveState(stateDir) {
    const statePath = `${stateDir}/state.json`;

    const state = {
      runNumber: this.runNumber,
      totalRuns: this.runNumber,
      lastScore: this.currentScore,
      timestamp: Date.now(),
      history: this.history.slice(-10), // Keep last 10 events
    };

    await Deno.mkdir(stateDir, { recursive: true });
    await Deno.writeTextFile(statePath, JSON.stringify(state, null, 2));
  }
}

/**
 * Helper: Check if number is prime (for scope leakage paradox)
 */
export function isPrime(n) {
  if (n < 2) return false;
  if (n === 2) return true;
  if (n % 2 === 0) return false;

  for (let i = 3; i * i <= n; i += 2) {
    if (n % i === 0) return false;
  }

  return true;
}

/**
 * Helper: Check if number is Fibonacci (for scope leakage)
 */
export function isFibonacci(n) {
  const isPerfectSquare = (x) => {
    const sqrt = Math.floor(Math.sqrt(x));
    return sqrt * sqrt === x;
  };

  return isPerfectSquare(5 * n * n + 4) || isPerfectSquare(5 * n * n - 4);
}
