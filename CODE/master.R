
# ==============================================================================
# MASTER SCRIPT - ZRR AND POPULIST VOTE ANALYSIS
# ==============================================================================
# This script runs the complete analysis pipeline for the ZRR program study.
#
# USAGE: Set your working directory to the project root folder, then run:
#        source("CODE/master.R")
#
# The script will:
#   1. Load required R packages
#   2. Prepare all datasets
#   3. Generate all tables (.tex files)
#   4. Generate all figures (.png files)
# ==============================================================================

rm(list=ls())

# Libraries

pacman::p_load(
  readxl, dplyr, tidyr, ggplot2, sf, stargazer, AER, broom, ggrepel, sf, viridis, stringr, ragg, fixest, sandwich, zoo,
  readr, rdd, rddtools, magrittr, rdrobust, estimatr, knitr, modelsummary, rddensity, gridExtra, rnaturalearth,
  rnaturalearthdata, forcats, cowplot, plm, grid, patchwork, reshape2,
  kableExtra, caret, htetree, glmnet, rpart, randomForest, purrr, devtools, SuperLearner, xgboost, psych, MatchIt,
  multiwayvcov, clubSandwich, patchwork, stringr, progress, scales, here
)


# Paths
# NOTE: Set your working directory to the project root folder before running
# Option 1: Use here() package (recommended)
# Option 2: Set main_path manually if here() doesn't work

# Try to detect project root using here package
if (requireNamespace("here", quietly = TRUE)) {
  main_path <- paste0(here::here(), "/")
} else {
  # Fallback: assume working directory is project root
  main_path <- paste0(getwd(), "/")
}

# Verify path exists
if (!dir.exists(paste0(main_path, "CODE"))) {
  stop("ERROR: Could not find CODE folder. Please set your working directory to the project root folder.")
}

source(paste0(main_path, "CODE/configurations.R"))



# Run Python files (manually) only once - done

  ## "defineBorders.py"           --> dataGeoRDD1.xlsx
  
  ## "border_pair.py"             --> Create border_pair.xlsx
  
  ## "distAgglo.py"               --> Create distAgglo.xlsx
  
  ## "defineBorders_randomZRR.py" --> Creates randomized treatment dataframes (dataGeoRDD_canton_random)


# ==============================================================================
# EXECUTION 
# ==============================================================================

# Prepare data

source(paste0(path_code_prepare_data, "prepare_data.R"))

# Produce Tables

source(paste0(path_code_prepare_tables, "prepare_tables.R"))

# Produce figures

source(paste0(path_code_prepare_figures, "prepare_figures.R"))

cat("\n===============================================\n")
cat("ALL FINAL OUTPUTS PRODUCED SUCCESSFULLY\n")
cat("===============================================\n")
