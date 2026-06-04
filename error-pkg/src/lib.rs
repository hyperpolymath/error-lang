// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! error-pkg: Package manager for error-lang with computational haptics
//!
//! Features:
//! - Dependency resolution
//! - Registry client
//! - Package installation
//! - Haptic feedback for errors (severity-based vibration patterns)

#![forbid(unsafe_code)]
pub mod types;
pub mod registry;
pub mod resolve;
pub mod install;
pub mod haptics;

pub use types::{Package, ResolvedPackage, Manifest};
pub use registry::RegistryClient;
pub use resolve::resolve_dependencies;
pub use install::install_package;

use anyhow::Result;
use std::path::Path;

/// Install all dependencies from an error.toml manifest
pub fn install(manifest_path: impl AsRef<Path>) -> Result<()> {
    let manifest = Manifest::load(manifest_path)?;
    let deps = manifest.dependencies.clone();

    if deps.is_empty() {
        println!("No dependencies to install");
        return Ok(());
    }

    println!("Resolving dependencies...");
    let resolved = resolve_dependencies(&deps)?;

    println!("Installing {} packages...", resolved.len());
    for pkg in resolved {
        install_package(&pkg)?;
    }

    println!("✓ All packages installed successfully");
    Ok(())
}

/// Add a package to the current project
pub fn add_package(name: &str, version: Option<&str>) -> Result<()> {
    let manifest_path = "error.toml";

    // Create manifest if it doesn't exist
    if !Path::new(manifest_path).exists() {
        Manifest::create_default(manifest_path)?;
    }

    // Fetch package metadata
    println!("Fetching {}...", name);
    let client = RegistryClient::new();
    let pkg = client.fetch_package(name, version.unwrap_or("latest"))?;

    // Add to manifest
    let mut manifest = Manifest::load(manifest_path)?;
    manifest.dependencies.insert(name.to_string(), pkg.version.clone());
    manifest.save(manifest_path)?;

    // Install package
    println!("Installing {}@{}...", name, pkg.version);
    install_package(&pkg)?;

    println!("✓ Added {}@{}", name, pkg.version);
    Ok(())
}

/// Remove a package from the current project
pub fn remove_package(name: &str) -> Result<()> {
    let manifest_path = "error.toml";
    let mut manifest = Manifest::load(manifest_path)?;

    if manifest.dependencies.remove(name).is_none() {
        anyhow::bail!("Package not in dependencies: {}", name);
    }

    manifest.save(manifest_path)?;
    println!("✓ Removed {}", name);
    Ok(())
}

/// Search for packages in the registry
pub fn search(query: &str, limit: usize) -> Result<Vec<Package>> {
    let client = RegistryClient::new();
    client.search(query, limit)
}
