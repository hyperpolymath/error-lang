// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Computational haptics for error-lang package manager
//!
//! Provides vibration feedback patterns for different error severities.
//! Integrates with platform-specific haptic APIs when available.

use std::time::Duration;

#[derive(Debug, Clone, Copy)]
pub enum HapticSeverity {
    Low,
    Medium,
    High,
    Critical,
}

pub struct ErrorHaptics;

impl ErrorHaptics {
    /// Trigger haptic feedback for download errors (Medium severity)
    pub fn trigger_download_error() {
        Self::vibrate(HapticSeverity::Medium);
    }

    /// Trigger haptic feedback for checksum errors (High severity)
    pub fn trigger_checksum_error() {
        Self::vibrate(HapticSeverity::High);
    }

    /// Trigger haptic feedback for dependency resolution errors (High severity)
    pub fn trigger_resolution_error() {
        Self::vibrate(HapticSeverity::High);
    }

    /// Trigger haptic feedback for network errors (Medium severity)
    pub fn trigger_network_error() {
        Self::vibrate(HapticSeverity::Medium);
    }

    /// Trigger generic error vibration based on severity
    fn vibrate(severity: HapticSeverity) {
        // Platform-specific implementation would go here
        // For now, we just log the haptic event

        let pattern = Self::get_pattern(severity);

        eprintln!("🔊 Haptic feedback: {} ms pattern", pattern.as_millis());

        // Future: Integrate with platform APIs
        // - Linux: /dev/input/event* with FF_RUMBLE
        // - Android: Vibrator service via JNI
        // - iOS: Core Haptics framework
        // - Windows: XInput rumble
    }

    fn get_pattern(severity: HapticSeverity) -> Duration {
        match severity {
            HapticSeverity::Low => Duration::from_millis(100),
            HapticSeverity::Medium => Duration::from_millis(200),
            HapticSeverity::High => Duration::from_millis(400),
            HapticSeverity::Critical => Duration::from_millis(800),
        }
    }

    /// Trigger haptic feedback with custom pattern
    pub fn vibrate_custom(duration_ms: u64) {
        eprintln!("🔊 Haptic feedback: {} ms", duration_ms);
    }

    /// Trigger success haptic (Low severity, short pulse)
    pub fn trigger_success() {
        Self::vibrate(HapticSeverity::Low);
    }
}

// Platform-specific haptic implementations (future)

#[cfg(target_os = "linux")]
mod linux {
    // TODO: Implement via /dev/input/event* with FF_RUMBLE
}

#[cfg(target_os = "android")]
mod android {
    // TODO: Implement via JNI to Android Vibrator service
}

#[cfg(target_os = "ios")]
mod ios {
    // TODO: Implement via Core Haptics framework
}

#[cfg(target_os = "windows")]
mod windows {
    // TODO: Implement via XInput rumble
}
