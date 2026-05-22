// SPDX-License-Identifier: MPL-2.0

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use anyhow::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub repository: Option<String>,
    pub dependencies: HashMap<String, String>,
    pub tarball_url: Option<String>,
    pub checksum: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    pub package: Package,
    pub resolved_deps: Vec<ResolvedPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: PackageInfo,
    pub dependencies: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    pub authors: Vec<String>,
    pub license: String,
    pub description: Option<String>,
}

impl Manifest {
    /// Load manifest from file
    pub fn load(path: impl AsRef<Path>) -> Result<Self> {
        let content = fs::read_to_string(path)?;
        Ok(toml::from_str(&content)?)
    }

    /// Save manifest to file
    pub fn save(&self, path: impl AsRef<Path>) -> Result<()> {
        let content = toml::to_string_pretty(self)?;
        fs::write(path, content)?;
        Ok(())
    }

    /// Create default manifest
    pub fn create_default(path: impl AsRef<Path>) -> Result<Self> {
        let manifest = Manifest {
            package: PackageInfo {
                name: "my-project".to_string(),
                version: "0.1.0".to_string(),
                authors: vec!["Unknown".to_string()],
                license: "MPL-2.0".to_string(),
                description: None,
            },
            dependencies: HashMap::new(),
        };

        manifest.save(path)?;
        Ok(manifest)
    }
}
