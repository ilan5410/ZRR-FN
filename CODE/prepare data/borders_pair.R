# borders_pair.RData


# ==============================================================================
# Borders Pair Data Processing Pipeline
# ==============================================================================
# This script creates border pair dataset for spatial discontinuity analysis
# by combining commune data with border distance information
# ==============================================================================

# ==============================================================================
# MAIN PROCESSING FUNCTION
# ==============================================================================

#' Process borders pair data for spatial discontinuity analysis
#' @param processed_data_path Path to processed data directory
#' @param year_control Control year for analysis (default: 1990)
#' @return Processed borders pair dataset
process_borders_pair_data <- function(processed_data_path, year_control = 1990) {
  
  cat("===============================================\n")
  cat("BORDERS PAIR DATA PROCESSING PIPELINE\n")
  cat("===============================================\n")
  cat("Control year:", year_control, "\n\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD BASE DATA
  # --------------------------------------------------------------------------
  cat("1. Loading base datasets...\n")
  
  # Load main processed data
  load(file.path(processed_data_path, "main.RData"))
  cat("✓ Loaded main.RData\n")
  cat("  - dfZRR_raw:", nrow(dfZRR_raw), "observations\n")
  cat("  - df_merged:", nrow(df_merged), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2. PROCESS BORDER DISTANCE DATA
  # --------------------------------------------------------------------------
  cat("\n2. Processing border distance data...\n")
  
  dfDistance <- read_excel(file.path(processed_data_path, "border_pair.xlsx")) %>%
    mutate(
      same_department = if_else(dep_locality1 == dep_locality2, 1, 0),
      codecommune = standardize_commune_codes(codecommune)
    ) %>%
    select(-dep, -nom, -year)
  
  n_border_pairs <- length(unique(dfDistance$border_pair))
  cat("✓ Processed border distance data\n")
  cat("  - Unique border pairs:", n_border_pairs, "\n")
  
  # --------------------------------------------------------------------------
  # 3. MERGE ZRR DATA WITH DISTANCE DATA
  # --------------------------------------------------------------------------
  cat("\n3. Merging ZRR data with border distances...\n")
  
  dfZRR <- dfZRR_raw %>%
    left_join(dfDistance, by = "codecommune", relationship = "many-to-many") %>%
    filter(!is.na(border_pair), year == 1995) %>%
    select(-year) %>%
    mutate(
      FN1988 = as.numeric(FN1988),
      FN2002 = as.numeric(FN2002),
      deltaFN = FN2002 - FN1988,
      y = FN2002,  # Outcome variable
      x = distance_to_border,  # Running variable
      z = treatment  # Treatment indicator
    ) %>%
    select(
      codecommune, x, y, z, canton, border_pair, RPR2002, deltaFN,
      turnout_2002, FN2007, FN2012, FN2017, FN2022, same_department
    )
  
  cat("✓ Merged datasets\n")
  cat("  - Border pairs in final data:", length(unique(dfZRR$border_pair)), "\n")
  cat("  - Unique communes:", length(unique(dfZRR$codecommune)), "\n")
  
  # --------------------------------------------------------------------------
  # 4. ADD CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n4. Adding control variables...\n")
  
  dfZRRControls <- dfZRR %>%
    left_join(df_merged, by = "codecommune", relationship = "many-to-many")
  
  cat("✓ Added control variables from df_merged\n")
  
  # --------------------------------------------------------------------------
  # 5. HANDLE MISSING VALUES WITH ALTERNATIVE YEARS
  # --------------------------------------------------------------------------
  cat("\n5. Handling missing values with alternative years...\n")
  
  # Fill missing revenue data with 1994 values
  dfZRRControls <- fill_missing_from_alternative_year(
    dfZRRControls, df_merged, "revenuPerK", 1994
  )
  
  # Fill missing age structure data with 1995 values
  dfZRRControls <- fill_missing_from_alternative_year(
    dfZRRControls, df_merged, "popYoungOld", 1995
  )
  
  # --------------------------------------------------------------------------
  # 6. DEFINE VARIABLES AND CLEAN DATA
  # --------------------------------------------------------------------------
  cat("\n6. Defining variables and cleaning data...\n")
  
  # Define control variables (exclude non-control columns)
  controls <- names(df_merged)
  excluded_vars <- c("codecommune", "year", "reg", "nomcommune", "dep", "FN1995", "vigne", "haie")
  controls <- setdiff(controls, excluded_vars)
  
  # Define all variables to keep and clean
  variables_to_clean <- c(
    "x", "y", "z", controls, "year", "border_pair", "canton", "dep", "reg",
    "deltaFN", "RPR2002", "turnout_2002", "FN2007", "FN2012", "FN2017", 
    "FN2022", "same_department"
  )
  
  cat("✓ Defined", length(controls), "control variables\n")
  cat("✓ Total variables to clean:", length(variables_to_clean), "\n")
  
  # Clean data
  dfZRRControls <- clean_data_variables(dfZRRControls, variables_to_clean)
  
  cat("✓ Merged datasets\n")
  cat("  - Border pairs in final data:", length(unique(dfZRR$border_pair)), "\n")
  cat("  - Unique communes:", length(unique(dfZRR$codecommune)), "\n")
  
  
  # --------------------------------------------------------------------------
  # 7. FINAL DATA PREPARATION
  # --------------------------------------------------------------------------
  cat("\n7. Final data preparation...\n")
  
  dfZRRControls <- dfZRRControls %>%
    select(all_of(variables_to_clean)) %>%
    mutate(
      y = as.numeric(as.character(y)),
      z = as.logical(z),
      RPR2002 = as.numeric(RPR2002)
    ) %>%
    filter(!is.na(y), year == year_control)
  
  final_rows <- nrow(dfZRRControls)
  final_border_pairs <- length(unique(dfZRRControls$border_pair))
  
  cat("✓ Final dataset prepared\n")
  cat("  - Final observations:", final_rows, "\n")
  cat("  - Final border pairs:", final_border_pairs, "\n")
  
  # --------------------------------------------------------------------------
  # 8. SUMMARY STATISTICS
  # --------------------------------------------------------------------------
  cat("\n8. Dataset Summary:\n")
  cat("   Treatment variable (z):\n")
  print(table(dfZRRControls$z, useNA = "ifany"))
  
  cat("   Outcome variable (y) summary:\n")
  print(summary(dfZRRControls$y))
  
  cat("   Running variable (x) summary:\n")
  print(summary(dfZRRControls$x))
  
  # --------------------------------------------------------------------------
  # 9. CLEANUP AND SAVE
  # --------------------------------------------------------------------------
  cat("\n9. Cleaning up and saving...\n")
  
  # Remove intermediate objects to save space
  rm(dfZRR_raw, df_merged, dfZRR, dfDistance, controls, variables_to_clean,
     excluded_vars, final_rows, final_communes, final_border_pairs, n_border_pairs,
     envir = .GlobalEnv)
  
  # Save the processed dataset
  save(dfZRRControls, file = file.path(processed_data_path, "borders_pair.RData"))
  
  cat("✓ Saved borders_pair.RData\n")
  cat("\n===============================================\n")
  cat("BORDERS PAIR PROCESSING COMPLETED SUCCESSFULLY\n")
  cat("===============================================\n")
  
  return(dfZRRControls)
}


# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

# Run with default parameters (year_control = 1990)
process_borders_pair_data(processed_data_path)
