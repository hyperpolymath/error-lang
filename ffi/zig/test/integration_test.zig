// Error-Lang Integration Tests
// SPDX-License-Identifier: PMPL-1.0-or-later
//
// These tests verify that the Zig FFI correctly implements error-lang's
// computational haptics and pedagogical features.

const std = @import("std");
const testing = std.testing;

// Import FFI functions
extern fn error_lang_init() ?*opaque {};
extern fn error_lang_free(?*opaque {}) void;
extern fn error_lang_calculate_stability(?*opaque {}) f32;
extern fn error_lang_set_stability_factor(?*opaque {}, u8, f32) c_int;
extern fn error_lang_get_stability_factor(?*opaque {}, u8) f32;
extern fn error_lang_positional_operator(?*opaque {}, u32, u32, u8) u8;
extern fn error_lang_detect_paradoxes(?*opaque {}, u32, u32, u32) u32;
extern fn error_lang_five_whys_depth(?*opaque {}, u32) u32;
extern fn error_lang_last_error() ?[*:0]const u8;
extern fn error_lang_version() [*:0]const u8;
extern fn error_lang_build_info() [*:0]const u8;
extern fn error_lang_is_initialized(?*opaque {}) u32;

//==============================================================================
// Lifecycle Tests
//==============================================================================

test "create and destroy handle" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    try testing.expect(handle != null);
}

test "handle is initialized" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    const initialized = error_lang_is_initialized(handle);
    try testing.expectEqual(@as(u32, 1), initialized);
}

test "null handle is not initialized" {
    const initialized = error_lang_is_initialized(null);
    try testing.expectEqual(@as(u32, 0), initialized);
}

//==============================================================================
// Stability Scoring Tests (Computational Haptics)
//==============================================================================

test "initial stability is 100" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    const score = error_lang_calculate_stability(handle);
    try testing.expectApproxEqAbs(@as(f32, 100.0), score, 0.01);
}

test "set and get stability factor" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Set type_superposition (factor 0) to 75.0
    const result = error_lang_set_stability_factor(handle, 0, 75.0);
    try testing.expectEqual(@as(c_int, 0), result); // 0 = ok

    const factor_score = error_lang_get_stability_factor(handle, 0);
    try testing.expectApproxEqAbs(@as(f32, 75.0), factor_score, 0.01);
}

test "stability decreases when factor lowered" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    const initial = error_lang_calculate_stability(handle);

    // Lower a critical factor (type_superposition has 15% weight)
    _ = error_lang_set_stability_factor(handle, 0, 50.0);

    const updated = error_lang_calculate_stability(handle);
    try testing.expect(updated < initial);
}

test "invalid stability factor returns error" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Factor 10 is out of range (valid: 0-9)
    const result = error_lang_set_stability_factor(handle, 10, 50.0);
    try testing.expectEqual(@as(c_int, 2), result); // 2 = invalid_param
}

test "stability score out of range returns error" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Score must be [0, 100]
    const result1 = error_lang_set_stability_factor(handle, 0, -5.0);
    try testing.expectEqual(@as(c_int, 2), result1); // invalid_param

    const result2 = error_lang_set_stability_factor(handle, 0, 150.0);
    try testing.expectEqual(@as(c_int, 2), result2); // invalid_param
}

//==============================================================================
// Positional Semantics Tests
//==============================================================================

test "positional operator changes by column" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Even column: addition (0)
    const behavior1 = error_lang_positional_operator(handle, 1, 12, 0);
    try testing.expectEqual(@as(u8, 0), behavior1);

    // Odd column: concatenation (1)
    const behavior2 = error_lang_positional_operator(handle, 1, 13, 0);
    try testing.expectEqual(@as(u8, 1), behavior2);
}

test "positional operator is deterministic" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Same position should always give same result
    const b1 = error_lang_positional_operator(handle, 5, 20, 0);
    const b2 = error_lang_positional_operator(handle, 5, 20, 0);
    try testing.expectEqual(b1, b2);
}

test "star operator changes by column modulo 3" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Column 12 (12 % 3 = 0): multiplication (2)
    const behavior1 = error_lang_positional_operator(handle, 1, 12, 1);
    try testing.expectEqual(@as(u8, 2), behavior1);

    // Column 13 (13 % 3 = 1): exponentiation (3)
    const behavior2 = error_lang_positional_operator(handle, 1, 13, 1);
    try testing.expectEqual(@as(u8, 3), behavior2);
}

//==============================================================================
// Paradox Detection Tests
//==============================================================================

test "scope leakage on prime lines" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Line 7 is prime: scope_leakage (bit 2) should be set
    const paradoxes7 = error_lang_detect_paradoxes(handle, 7, 5, 2);
    try testing.expect((paradoxes7 & (1 << 2)) != 0);

    // Line 8 is not prime: scope_leakage should not be set
    const paradoxes8 = error_lang_detect_paradoxes(handle, 8, 5, 2);
    try testing.expect((paradoxes8 & (1 << 2)) == 0);
}

test "type superposition with many vars" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // > 10 vars: type_superposition (bit 0) should be set
    const paradoxes = error_lang_detect_paradoxes(handle, 10, 15, 2);
    try testing.expect((paradoxes & (1 << 0)) != 0);
}

test "temporal corruption at depth" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    // Depth > 5: temporal_corruption (bit 3) should be set
    const paradoxes = error_lang_detect_paradoxes(handle, 10, 5, 6);
    try testing.expect((paradoxes & (1 << 3)) != 0);
}

//==============================================================================
// Five Whys Tests
//==============================================================================

test "five whys depth is bounded" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    for (0..100) |symptom| {
        const depth = error_lang_five_whys_depth(handle, @intCast(symptom));
        try testing.expect(depth >= 1);
        try testing.expect(depth <= 5);
    }
}

test "five whys is deterministic" {
    const handle = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(handle);

    const d1 = error_lang_five_whys_depth(handle, 42);
    const d2 = error_lang_five_whys_depth(handle, 42);
    try testing.expectEqual(d1, d2);
}

//==============================================================================
// Version Tests
//==============================================================================

test "version string is not empty" {
    const ver = error_lang_version();
    const ver_str = std.mem.span(ver);

    try testing.expect(ver_str.len > 0);
}

test "version is semantic version format" {
    const ver = error_lang_version();
    const ver_str = std.mem.span(ver);

    // Should contain at least one dot
    try testing.expect(std.mem.count(u8, ver_str, ".") >= 1);
}

test "build info contains Zig" {
    const info = error_lang_build_info();
    const info_str = std.mem.span(info);

    try testing.expect(std.mem.indexOf(u8, info_str, "Zig") != null);
}

//==============================================================================
// Memory Safety Tests
//==============================================================================

test "multiple handles are independent" {
    const h1 = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(h1);

    const h2 = error_lang_init() orelse return error.InitFailed;
    defer error_lang_free(h2);

    try testing.expect(h1 != h2);

    // Set different stability factors
    _ = error_lang_set_stability_factor(h1, 0, 50.0);
    _ = error_lang_set_stability_factor(h2, 0, 75.0);

    const score1 = error_lang_get_stability_factor(h1, 0);
    const score2 = error_lang_get_stability_factor(h2, 0);

    try testing.expectApproxEqAbs(@as(f32, 50.0), score1, 0.01);
    try testing.expectApproxEqAbs(@as(f32, 75.0), score2, 0.01);
}

test "free null is safe" {
    error_lang_free(null); // Should not crash
}

//==============================================================================
// Error Handling Tests
//==============================================================================

test "error with null handle" {
    const score = error_lang_calculate_stability(null);
    try testing.expectEqual(@as(f32, 0.0), score);

    const err = error_lang_last_error();
    try testing.expect(err != null);

    if (err) |e| {
        const err_str = std.mem.span(e);
        try testing.expect(std.mem.indexOf(u8, err_str, "Null") != null);
    }
}
