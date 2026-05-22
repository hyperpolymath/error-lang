// SPDX-License-Identifier: MPL-2.0

use crate::types::Package;
use crate::haptics::ErrorHaptics;
use anyhow::Result;
use std::fs;
use std::path::PathBuf;
use sha2::{Sha256, Digest};

const PACKAGES_DIR: &str = ".error-lang/packages";

/// Install a single package to the local packages directory
pub fn install_package(pkg: &Package) -> Result<()> {
    // Create packages directory if it doesn't exist
    fs::create_dir_all(PACKAGES_DIR)?;

    // Package installation directory
    let pkg_dir = PathBuf::from(PACKAGES_DIR)
        .join(&pkg.name)
        .join(&pkg.version);

    // Skip if already installed
    if pkg_dir.exists() {
        println!("  ✓ {}@{} (cached)", pkg.name, pkg.version);
        return Ok(());
    }

    // Download tarball
    let tarball_url = pkg.tarball_url.as_ref()
        .ok_or_else(|| anyhow::anyhow!("No tarball URL for package: {}", pkg.name))?;

    println!("  ⬇ {}@{}", pkg.name, pkg.version);

    let tarball_path = PathBuf::from(PACKAGES_DIR)
        .join(format!("{}-{}.tar.gz", pkg.name, pkg.version));

    // Download with retry and haptic feedback on error
    match download_with_retry(tarball_url, &tarball_path) {
        Ok(_) => (),
        Err(e) => {
            ErrorHaptics::trigger_download_error();
            return Err(e);
        }
    }

    // Verify checksum if provided
    if let Some(checksum) = &pkg.checksum {
        if let Err(e) = verify_checksum(&tarball_path, checksum) {
            ErrorHaptics::trigger_checksum_error();
            fs::remove_file(&tarball_path).ok();
            return Err(e);
        }
    }

    // Extract tarball
    fs::create_dir_all(&pkg_dir)?;
    extract_tarball(&tarball_path, &pkg_dir)?;

    // Cleanup tarball
    fs::remove_file(&tarball_path)?;

    println!("  ✓ {}@{}", pkg.name, pkg.version);
    Ok(())
}

fn download_with_retry(url: &str, path: &PathBuf) -> Result<()> {
    let client = reqwest::blocking::Client::new();
    let mut attempts = 0;
    const MAX_ATTEMPTS: u32 = 3;

    loop {
        attempts += 1;

        match client.get(url).send() {
            Ok(response) if response.status().is_success() => {
                let bytes = response.bytes()?;
                fs::write(path, bytes)?;
                return Ok(());
            }
            Ok(response) => {
                if attempts >= MAX_ATTEMPTS {
                    anyhow::bail!("Download failed with status: {}", response.status());
                }
            }
            Err(e) => {
                if attempts >= MAX_ATTEMPTS {
                    return Err(e.into());
                }
            }
        }

        std::thread::sleep(std::time::Duration::from_secs(1));
    }
}

fn verify_checksum(file_path: &PathBuf, expected: &str) -> Result<()> {
    let bytes = fs::read(file_path)?;
    let mut hasher = Sha256::new();
    hasher.update(&bytes);
    let actual = format!("{:x}", hasher.finalize());

    if actual != expected {
        anyhow::bail!(
            "Checksum mismatch for {:?}\n  Expected: {}\n  Got: {}",
            file_path,
            expected,
            actual
        );
    }

    Ok(())
}

fn extract_tarball(tarball_path: &PathBuf, target_dir: &PathBuf) -> Result<()> {
    let file = fs::File::open(tarball_path)?;
    let gz = flate2::read::GzDecoder::new(file);
    let mut archive = tar::Archive::new(gz);
    archive.unpack(target_dir)?;
    Ok(())
}
