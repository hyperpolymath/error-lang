// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Error-Lang FFI Implementation
//
// This module implements the C-compatible FFI for Error-Lang computational haptics.
// Provides high-performance native operations for stability scoring, positional
// semantics, and paradox detection.
//

const std = @import("std");

// Version information
const VERSION = "1.0.0";
const BUILD_INFO = "Error-Lang FFI built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[]const u8 = null;

/// Set the last error message
fn setError(msg: []const u8) void {
    last_error = msg;
}

/// Clear the last error
fn clearError() void {
    last_error = null;
}

//==============================================================================
// Core Types (must match src/abi/Types.idr)
//==============================================================================

/// Result codes (must match Idris2 Result type)
pub const Result = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
};

/// Stability factor types
pub const StabilityFactor = enum(u8) {
    type_superposition = 0,
    positional_semantics = 1,
    scope_leakage = 2,
    temporal_corruption = 3,
    arithmetic_drift = 4,
    null_propagation = 5,
    context_collapse = 6,
    reserved_word_roulette = 7,
    global_entanglement = 8,
    memory_phantom = 9,
};

/// Library handle
pub const Handle = struct {
    allocator: std.mem.Allocator,
    initialized: bool,
    stability_scores: [10]f32, // Track each paradox factor
    position_cache: std.AutoHashMap(PositionKey, OperatorBehavior),
};

/// Position in source code
const PositionKey = struct {
    line: u32,
    column: u32,
};

/// Operator behavior based on position
const OperatorBehavior = enum(u8) {
    addition,
    concatenation,
    multiplication,
    exponentiation,
    comparison,
    logical_and,
    logical_or,
};

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the library
/// Returns a handle, or null on failure
export fn error_lang_init() ?*Handle {
    const allocator = std.heap.c_allocator;

    const handle = allocator.create(Handle) catch {
        setError("Failed to allocate handle");
        return null;
    };

    // Initialize handle
    handle.* = .{
        .allocator = allocator,
        .initialized = true,
        .stability_scores = [_]f32{100.0} ** 10, // All start at 100%
        .position_cache = std.AutoHashMap(PositionKey, OperatorBehavior).init(allocator),
    };

    clearError();
    return handle;
}

/// Free the library handle
export fn error_lang_free(handle: ?*Handle) void {
    const h = handle orelse return;
    const allocator = h.allocator;

    // Clean up resources
    h.position_cache.deinit();
    h.initialized = false;

    allocator.destroy(h);
    clearError();
}

//==============================================================================
// Stability Scoring (Computational Haptics Core)
//==============================================================================

/// Calculate overall stability score (0-100)
/// Weighted average of all paradox factors
export fn error_lang_calculate_stability(handle: ?*Handle) f32 {
    const h = handle orelse {
        setError("Null handle");
        return 0.0;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return 0.0;
    }

    // Weighted average (some factors are more critical)
    const weights = [10]f32{
        0.15, // type_superposition (critical)
        0.12, // positional_semantics
        0.10, // scope_leakage
        0.08, // temporal_corruption
        0.10, // arithmetic_drift
        0.12, // null_propagation (critical)
        0.08, // context_collapse
        0.07, // reserved_word_roulette
        0.10, // global_entanglement
        0.08, // memory_phantom
    };

    var total: f32 = 0.0;
    for (h.stability_scores, weights) |score, weight| {
        total += score * weight;
    }

    clearError();
    return total;
}

/// Set stability score for a specific factor
export fn error_lang_set_stability_factor(
    handle: ?*Handle,
    factor: u8,
    score: f32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    if (factor >= 10) {
        setError("Invalid stability factor");
        return .invalid_param;
    }

    if (score < 0.0 or score > 100.0) {
        setError("Score must be between 0 and 100");
        return .invalid_param;
    }

    h.stability_scores[factor] = score;

    clearError();
    return .ok;
}

/// Get stability score for a specific factor
export fn error_lang_get_stability_factor(
    handle: ?*Handle,
    factor: u8,
) f32 {
    const h = handle orelse {
        setError("Null handle");
        return 0.0;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return 0.0;
    }

    if (factor >= 10) {
        setError("Invalid stability factor");
        return 0.0;
    }

    clearError();
    return h.stability_scores[factor];
}

//==============================================================================
// Positional Semantics
//==============================================================================

/// Determine operator behavior based on source position
/// Uses column position modulo to determine behavior
export fn error_lang_positional_operator(
    handle: ?*Handle,
    line: u32,
    column: u32,
    operator_type: u8,
) u8 {
    const h = handle orelse {
        setError("Null handle");
        return 0;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return 0;
    }

    // Check cache first
    const key = PositionKey{ .line = line, .column = column };
    if (h.position_cache.get(key)) |behavior| {
        clearError();
        return @intFromEnum(behavior);
    }

    // Calculate behavior based on position
    // This is the "magic" that makes operators change meaning!
    const behavior: OperatorBehavior = switch (operator_type) {
        0 => blk: { // Plus operator
            if (column % 2 == 0) {
                break :blk .addition;
            } else {
                break :blk .concatenation;
            }
        },
        1 => blk: { // Star operator
            if (column % 3 == 0) {
                break :blk .multiplication;
            } else {
                break :blk .exponentiation;
            }
        },
        else => .addition,
    };

    // Cache the result
    h.position_cache.put(key, behavior) catch {
        // Cache full, continue anyway
    };

    clearError();
    return @intFromEnum(behavior);
}

//==============================================================================
// Paradox Detection
//==============================================================================

/// Detect if a paradox is active in the current context
/// Returns a bitmask of active paradoxes
export fn error_lang_detect_paradoxes(
    handle: ?*Handle,
    line_count: u32,
    var_count: u32,
    depth: u32,
) u32 {
    const h = handle orelse {
        setError("Null handle");
        return 0;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return 0;
    }

    var paradoxes: u32 = 0;

    // Type superposition: active when var_count > 10
    if (var_count > 10) {
        paradoxes |= (1 << @intFromEnum(StabilityFactor.type_superposition));
    }

    // Scope leakage: active on prime-numbered lines
    if (isPrime(line_count)) {
        paradoxes |= (1 << @intFromEnum(StabilityFactor.scope_leakage));
    }

    // Temporal corruption: increases with depth
    if (depth > 5) {
        paradoxes |= (1 << @intFromEnum(StabilityFactor.temporal_corruption));
    }

    clearError();
    return paradoxes;
}

/// Helper: check if a number is prime (for scope leakage paradox)
fn isPrime(n: u32) bool {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;

    var i: u32 = 3;
    while (i * i <= n) : (i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}

//==============================================================================
// Five Whys Analysis
//==============================================================================

/// Trace root cause through abstraction layers
/// Returns depth reached before hitting bedrock
export fn error_lang_five_whys_depth(
    handle: ?*Handle,
    symptom_code: u32,
) u32 {
    const h = handle orelse {
        setError("Null handle");
        return 0;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return 0;
    }

    // Simple heuristic: more complex symptoms have deeper causes
    const depth = (symptom_code % 5) + 1;

    clearError();
    return depth;
}

//==============================================================================
// Error Handling
//==============================================================================

/// Get the last error message
/// Returns null if no error
export fn error_lang_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;

    // Return C string (static storage, no need to free)
    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

//==============================================================================
// Version Information
//==============================================================================

/// Get the library version
export fn error_lang_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information
export fn error_lang_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if handle is initialized
export fn error_lang_is_initialized(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return if (h.initialized) 1 else 0;
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    try std.testing.expect(error_lang_is_initialized(handle) == 1);
}

test "stability scoring" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Initial score should be 100.0
    const initial = error_lang_calculate_stability(handle);
    try std.testing.expectApproxEqAbs(@as(f32, 100.0), initial, 0.01);

    // Set a factor to 50
    _ = error_lang_set_stability_factor(handle, 0, 50.0);
    const updated = error_lang_calculate_stability(handle);
    try std.testing.expect(updated < initial);
}

test "positional semantics" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Even column: addition (0)
    const behavior1 = error_lang_positional_operator(handle, 1, 12, 0);
    try std.testing.expectEqual(@as(u8, 0), behavior1);

    // Odd column: concatenation (1)
    const behavior2 = error_lang_positional_operator(handle, 1, 13, 0);
    try std.testing.expectEqual(@as(u8, 1), behavior2);
}

test "paradox detection" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Prime line: scope leakage active
    const paradoxes = error_lang_detect_paradoxes(handle, 7, 5, 2);
    try std.testing.expect(paradoxes & (1 << 2) != 0); // scope_leakage bit set
}

test "five whys" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    const depth = error_lang_five_whys_depth(handle, 42);
    try std.testing.expect(depth >= 1 and depth <= 5);
}
