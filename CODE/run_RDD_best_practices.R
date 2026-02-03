# ==============================================================================
# Runner Script for RDD Best Practices Analysis
# ==============================================================================
# This script runs the comprehensive RDD analysis and outputs to a separate folder
# ==============================================================================

# Set main path
main_path <- here::here()
if (!endsWith(main_path, "/")) main_path <- paste0(main_path, "/")

# Source configurations
source(file.path(main_path, "CODE/configurations.R"))

# Load required packages
library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(gridExtra)
library(scales)

# Define output paths for RDD best practices (separate subfolder)
path_figures_rdd <- file.path(main_path, "OUTPUT/RDD_best_practices/figures/")
path_tables_rdd <- file.path(main_path, "OUTPUT/RDD_best_practices/tables/")

# Create directories if they don't exist
dir.create(path_figures_rdd, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables_rdd, recursive = TRUE, showWarnings = FALSE)

cat("Output will be saved to:\n")
cat("  Figures:", path_figures_rdd, "\n")
cat("  Tables:", path_tables_rdd, "\n\n")

# Source the RDD best practices script
source(file.path(main_path, "CODE/RDD_best_practices.R"))

# Run the analysis
results <- run_rdd_best_practices(
  processed_data_path = processed_data_path,
  path_figures = path_figures_rdd,
  path_tables = path_tables_rdd,
  outcome_var = "FN2002",
  running_var = "x",
  cutoff = 0,
  controls = controls,
  cluster_var = "canton"
)

# Save results object
save(results, file = file.path(main_path, "OUTPUT/RDD_best_practices/rdd_results.RData"))
cat("\nResults object saved to: OUTPUT/RDD_best_practices/rdd_results.RData\n")
