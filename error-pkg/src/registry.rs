// SPDX-License-Identifier: MPL-2.0

use crate::types::Package;
use anyhow::Result;
use std::collections::HashMap;

const REGISTRY_URL: &str = "https://packages.error-lang.dev/api/v1";
const FALLBACK_MODE: bool = true;  // Use git until registry deployed

pub struct RegistryClient {
    client: reqwest::blocking::Client,
}

impl RegistryClient {
    pub fn new() -> Self {
        Self {
            client: reqwest::blocking::Client::new(),
        }
    }

    /// Fetch package metadata from registry or git
    pub fn fetch_package(&self, name: &str, version: &str) -> Result<Package> {
        if FALLBACK_MODE {
            self.fetch_from_git(name, version)
        } else {
            self.fetch_from_registry(name, version)
        }
    }

    /// Search for packages
    pub fn search(&self, query: &str, limit: usize) -> Result<Vec<Package>> {
        if FALLBACK_MODE {
            Ok(vec![])  // Git search not implemented yet
        } else {
            self.search_registry(query, limit)
        }
    }

    // Registry mode (future)

    fn fetch_from_registry(&self, name: &str, version: &str) -> Result<Package> {
        let url = format!("{}/packages/{}", REGISTRY_URL, name);
        let response = self.client.get(&url).send()?;

        if !response.status().is_success() {
            anyhow::bail!("Package not found: {}", name);
        }

        let mut pkg: Package = response.json()?;
        if version == "latest" {
            // Keep pkg.version as is
        } else {
            pkg.version = version.to_string();
        }

        Ok(pkg)
    }

    fn search_registry(&self, query: &str, limit: usize) -> Result<Vec<Package>> {
        let url = format!("{}/packages?q={}&limit={}", REGISTRY_URL, query, limit);
        let response = self.client.get(&url).send()?;
        let packages: Vec<Package> = response.json()?;
        Ok(packages)
    }

    // Git fallback mode (current)

    fn fetch_from_git(&self, name: &str, version: &str) -> Result<Package> {
        let urls = vec![
            format!("https://github.com/hyperpolymath/{}", name),
            format!("https://gitlab.com/hyperpolymath/{}", name),
            format!("https://git.sr.ht/~hyperpolymath/{}", name),
        ];

        for base_url in urls {
            if let Ok(pkg) = self.try_fetch_git(&base_url, name, version) {
                return Ok(pkg);
            }
        }

        anyhow::bail!("Package not found: {}", name)
    }

    fn try_fetch_git(&self, base_url: &str, name: &str, version: &str) -> Result<Package> {
        let branch = if version == "latest" { "main" } else { version };
        let manifest_url = format!("{}/raw/{}/error.toml", base_url, branch);

        let response = self.client.get(&manifest_url).send()?;

        if !response.status().is_success() {
            anyhow::bail!("Manifest not found");
        }

        let content = response.text()?;
        let manifest: toml::Value = toml::from_str(&content)?;

        let pkg_info = manifest.get("package")
            .ok_or_else(|| anyhow::anyhow!("Invalid manifest"))?;

        let dependencies = manifest.get("dependencies")
            .and_then(|d| d.as_table())
            .map(|table| {
                table.iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect()
            })
            .unwrap_or_default();

        Ok(Package {
            name: pkg_info.get("name")
                .and_then(|v| v.as_str())
                .unwrap_or(name)
                .to_string(),
            version: version.to_string(),
            description: pkg_info.get("description")
                .and_then(|v| v.as_str())
                .map(String::from),
            repository: Some(base_url.to_string()),
            dependencies,
            tarball_url: Some(format!("{}/archive/{}.tar.gz", base_url, branch)),
            checksum: None,  // No checksum in git mode
        })
    }
}

impl Default for RegistryClient {
    fn default() -> Self {
        Self::new()
    }
}
