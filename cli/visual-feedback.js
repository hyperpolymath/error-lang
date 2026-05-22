// SPDX-License-Identifier: MPL-2.0
// visual-feedback.js - Visual haptic feedback system

/**
 * Visual Haptic Feedback System
 *
 * Makes computational forces visible and tangible:
 * - Animated stability bars (like feeling hammer weight)
 * - Ripple effects for mutations (like feeling stone resistance)
 * - Cascade visualizations for errors (like water flowing)
 * - Performance cliffs (like hitting a wall)
 *
 * This is the "feeling the substrate" - computational haptics.
 */

/**
 * ANSI color codes for terminal output
 */
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',

  // Foreground colors
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',

  // Background colors
  bgRed: '\x1b[41m',
  bgGreen: '\x1b[42m',
  bgYellow: '\x1b[43m',
};

/**
 * Animated stability bar that updates in real-time
 */
export class AnimatedStabilityBar {
  constructor(initialScore = 100) {
    this.score = initialScore;
    this.targetScore = initialScore;
    this.width = 40; // Width in characters
    this.animationFrame = 0;
  }

  /**
   * Update target score (will animate to this value)
   */
  setScore(newScore) {
    this.targetScore = Math.max(0, Math.min(100, newScore));
  }

  /**
   * Animate towards target score
   */
  tick() {
    if (this.score < this.targetScore) {
      this.score += Math.min(2, this.targetScore - this.score);
    } else if (this.score > this.targetScore) {
      this.score -= Math.min(2, this.score - this.targetScore);
    }

    this.animationFrame++;
  }

  /**
   * Render the bar with color coding
   */
  render() {
    const filled = Math.floor((this.score / 100) * this.width);
    const empty = this.width - filled;

    let color;
    let emoji;

    if (this.score >= 90) {
      color = colors.green;
      emoji = '✨';
    } else if (this.score >= 70) {
      color = colors.cyan;
      emoji = '💫';
    } else if (this.score >= 50) {
      color = colors.yellow;
      emoji = '⚠️';
    } else if (this.score >= 30) {
      color = colors.red;
      emoji = '🔴';
    } else {
      color = colors.magenta;
      emoji = '💥';
    }

    // Add pulse effect at low stability
    const pulse = this.score < 50 && (this.animationFrame % 4 < 2) ? colors.bright : '';

    const bar = '█'.repeat(filled) + '░'.repeat(empty);

    return `${emoji} ${pulse}${color}[${bar}] ${Math.floor(this.score)}/100${colors.reset}`;
  }
}

/**
 * Ripple effect visualization for mutations
 */
export class RippleEffect {
  constructor() {
    this.ripples = [];
    this.maxRadius = 10;
  }

  /**
   * Create a new ripple at source location
   */
  createRipple(source, affectedLocations) {
    this.ripples.push({
      source,
      affected: affectedLocations,
      radius: 0,
      intensity: 1.0,
    });
  }

  /**
   * Update all ripples (expand and fade)
   */
  tick() {
    for (const ripple of this.ripples) {
      ripple.radius += 0.5;
      ripple.intensity = Math.max(0, 1 - (ripple.radius / this.maxRadius));
    }

    // Remove faded ripples
    this.ripples = this.ripples.filter(r => r.intensity > 0);
  }

  /**
   * Render ripple visualization
   */
  render(sourceLine, affectedLines) {
    const lines = [];

    lines.push(`\n${colors.red}🔴 MUTATION RIPPLE${colors.reset}`);
    lines.push(`   Source: line ${sourceLine}`);

    if (affectedLines.length > 0) {
      lines.push(`   ${colors.yellow}↓ Affects:${colors.reset}`);

      for (const line of affectedLines.slice(0, 5)) {
        const arrow = '   '.repeat(Math.min(3, Math.floor(Math.abs(line - sourceLine) / 10)));
        lines.push(`   ${arrow}→ line ${line}`);
      }

      if (affectedLines.length > 5) {
        lines.push(`   ... and ${affectedLines.length - 5} more locations`);
      }
    }

    return lines.join('\n');
  }
}

/**
 * Cascade visualization for error propagation
 */
export class CascadeVisualization {
  /**
   * Render error cascade (like water flowing downhill)
   */
  static render(cascadePath) {
    const lines = [];

    lines.push(`\n${colors.red}☠️  CASCADE PROPAGATION${colors.reset}`);

    for (let i = 0; i < cascadePath.length; i++) {
      const step = cascadePath[i];
      const indent = '   '.repeat(i);
      const arrow = i > 0 ? '↓ ' : '  ';

      if (step.type === 'null-access') {
        lines.push(`${indent}${arrow}${colors.magenta}null access${colors.reset} at line ${step.line}`);
      } else if (step.type === 'type-error') {
        lines.push(`${indent}${arrow}${colors.yellow}type error${colors.reset} at line ${step.line}`);
      } else {
        lines.push(`${indent}${arrow}error at line ${step.line}`);
      }
    }

    lines.push(`\n${colors.dim}   Cascade depth: ${cascadePath.length}${colors.reset}`);

    return lines.join('\n');
  }
}

/**
 * Performance cliff visualization
 */
export class PerformanceCliff {
  /**
   * Render performance comparison
   */
  static render(operation, actualTime, expectedTime) {
    const ratio = actualTime / expectedTime;
    const slowdown = Math.floor(ratio);

    const lines = [];

    lines.push(`\n${colors.red}🐌 PERFORMANCE CLIFF${colors.reset}`);
    lines.push(`   Operation: ${operation}`);
    lines.push(`   Expected:  ${expectedTime}ms`);
    lines.push(`   Actual:    ${actualTime}ms ${colors.red}(${slowdown}x slower!)${colors.reset}`);

    // Visual bar showing slowdown
    const barWidth = Math.min(40, slowdown);
    const expectedBar = colors.green + '█'.repeat(Math.min(5, barWidth)) + colors.reset;
    const actualBar = colors.red + '█'.repeat(barWidth) + colors.reset;

    lines.push(`\n   Expected: ${expectedBar}`);
    lines.push(`   Actual:   ${actualBar}`);

    return lines.join('\n');
  }
}

/**
 * Layer indicator - shows which abstraction layer is unstable
 */
export class LayerIndicator {
  static layers = [
    { name: 'Runtime', color: colors.red },
    { name: 'Semantics', color: colors.yellow },
    { name: 'AST', color: colors.cyan },
    { name: 'Parser', color: colors.blue },
    { name: 'Grammar', color: colors.magenta },
  ];

  /**
   * Render layer stack with highlighted unstable layer
   */
  static render(unstableLayer) {
    const lines = [];

    lines.push(`\n${colors.bright}LAYER STACK${colors.reset}`);

    for (let i = 0; i < this.layers.length; i++) {
      const layer = this.layers[i];
      const isUnstable = layer.name === unstableLayer;
      const marker = isUnstable ? '⚠️ ' : '✓ ';
      const highlight = isUnstable ? colors.bright : colors.dim;

      lines.push(`   ${marker}${highlight}${layer.name}${colors.reset}`);

      // Show arrow connecting layers
      if (i < this.layers.length - 1) {
        lines.push(`      ${colors.dim}↓${colors.reset}`);
      }
    }

    return lines.join('\n');
  }
}

/**
 * Stability trend graph (sparkline)
 */
export class StabilityTrend {
  constructor(maxPoints = 20) {
    this.points = [];
    this.maxPoints = maxPoints;
  }

  addPoint(score) {
    this.points.push(score);
    if (this.points.length > this.maxPoints) {
      this.points.shift();
    }
  }

  render() {
    if (this.points.length < 2) {
      return '';
    }

    // Sparkline characters (vertical blocks)
    const blocks = ['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];

    const sparkline = this.points.map(score => {
      const index = Math.floor((score / 100) * (blocks.length - 1));
      return blocks[Math.max(0, Math.min(blocks.length - 1, index))];
    }).join('');

    const trend = this.points[this.points.length - 1] > this.points[0] ? '📈' : '📉';

    return `\n${trend} Trend: ${colors.cyan}${sparkline}${colors.reset}`;
  }
}

/**
 * Consequence icon mapper
 */
export function getConsequenceIcon(eventType) {
  const icons = {
    'mutation': '🔴',
    'type-change': '⚠️',
    'null-access': '☠️',
    'global-mutation': '🌍',
    'unhandled-error': '💥',
    'performance-cliff': '🐌',
    'memory-leak': '💧',
    'race-condition': '⚡',
    'paradox-discovered': '🌀',
  };

  return icons[eventType] || '❓';
}

/**
 * Live stability dashboard (updates in place)
 */
export class LiveDashboard {
  constructor() {
    this.stabilityBar = new AnimatedStabilityBar();
    this.ripples = new RippleEffect();
    this.trend = new StabilityTrend();
  }

  update(score, events) {
    this.stabilityBar.setScore(score);
    this.stabilityBar.tick();
    this.trend.addPoint(score);
    this.ripples.tick();
  }

  render() {
    const lines = [];

    lines.push('\n' + '═'.repeat(60));
    lines.push(this.stabilityBar.render());
    lines.push(this.trend.render());
    lines.push('═'.repeat(60));

    return lines.join('\n');
  }

  clear() {
    // Move cursor up to redraw (for animation)
    // In terminal: \x1b[<n>A moves cursor up n lines
    process.stdout.write('\x1b[6A'); // Move up 6 lines
    process.stdout.write('\x1b[J');  // Clear from cursor to end
  }
}
