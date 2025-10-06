
rm(list=ls())

# Libraries

pacman::p_load(
  readxl, dplyr, tidyr, ggplot2, sf, stargazer, AER, broom, ggrepel, sf, viridis, stringr, ragg, fixest, sandwich, zoo, 
  readr, rdd, rddtools, magrittr, rdrobust, estimatr, knitr, modelsummary, rddensity, gridExtra, rnaturalearth, 
  rnaturalearthdata, forcats, cowplot, plm, grid, patchwork, reshape2,
  kableExtra, caret, htetree, glmnet, rpart, randomForest, purrr, devtools,  SuperLearner, xgboost, psych, MatchIt, 
  multiwayvcov, clubSandwich, patchwork, stringr, progress, scales
  
)


# Paths 


main_path <- "/Users/ilanpargamin/Desktop/ECONOMICS/thesis/THESIS_REPRO Sept 2025/"

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
