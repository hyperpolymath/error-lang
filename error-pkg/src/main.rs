// SPDX-License-Identifier: MPL-2.0

use clap::{Parser, Subcommand};
use error_pkg::{install, add_package, remove_package, search};
use anyhow::Result;

#[derive(Parser)]
#[command(author, version, about = "error-lang package manager with computational haptics", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Install all dependencies from error.toml
    Install {
        /// Path to manifest file
        #[arg(default_value = "error.toml")]
        manifest: String,
    },

    /// Add a package to the project
    Add {
        /// Package name
        name: String,

        /// Package version (default: latest)
        #[arg(short, long)]
        version: Option<String>,
    },

    /// Remove a package from the project
    Remove {
        /// Package name
        name: String,
    },

    /// Search for packages
    Search {
        /// Search query
        query: String,

        /// Maximum number of results
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Install { manifest } => {
            install(&manifest)?;
        }

        Commands::Add { name, version } => {
            add_package(&name, version.as_deref())?;
        }

        Commands::Remove { name } => {
            remove_package(&name)?;
        }

        Commands::Search { query, limit } => {
            let results = search(&query, limit)?;

            if results.is_empty() {
                println!("No packages found matching: {}", query);
            } else {
                println!("Found {} packages:", results.len());
                for pkg in results {
                    println!("  {} ({})", pkg.name, pkg.version);
                    if let Some(desc) = &pkg.description {
                        println!("    {}", desc);
                    }
                }
            }
        }
    }

    Ok(())
}
