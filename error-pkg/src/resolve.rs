// SPDX-License-Identifier: MPL-2.0

use crate::types::Package;
use crate::registry::RegistryClient;
use anyhow::Result;
use std::collections::{HashMap, HashSet};

/// Resolve dependency graph for a set of direct dependencies
///
/// Returns a flat list of all packages to install (including transitive deps).
/// Uses topological sort for now. Future: Implement PubGrub algorithm.
pub fn resolve_dependencies(deps: &HashMap<String, String>) -> Result<Vec<Package>> {
    let mut resolved = Vec::new();
    let mut visited = HashSet::new();
    let client = RegistryClient::new();

    for (name, version) in deps {
        resolve_recursive(name, version, &client, &mut resolved, &mut visited)?;
    }

    Ok(resolved)
}

fn resolve_recursive(
    name: &str,
    version: &str,
    client: &RegistryClient,
    resolved: &mut Vec<Package>,
    visited: &mut HashSet<String>,
) -> Result<()> {
    // Skip if already processed
    if visited.contains(name) {
        return Ok(());
    }

    // Fetch package metadata
    let pkg = client.fetch_package(name, version)?;

    // Mark as visited
    visited.insert(name.to_string());

    // Recursively resolve dependencies
    for (dep_name, dep_version) in &pkg.dependencies {
        resolve_recursive(dep_name, dep_version, client, resolved, visited)?;
    }

    // Add to resolved list after dependencies
    resolved.push(pkg);

    Ok(())
}
