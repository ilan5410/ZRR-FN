# ==============================================================================
# Sharp RDD Data Processing Pipeline
# ==============================================================================
# This script processes commune data for sharp regression discontinuity design
# analysis, focusing on 1995 treatment with 1990 control variables
# ==============================================================================

# ==============================================================================
# MAIN PROCESSING FUNCTION
# ==============================================================================

#' Process data for sharp RDD analysis
#' @param processed_data_path Path to processed data directory
#' @param treatment_year Year for treatment assignment (default: 1995)
#' @param control_year Year for control variables (default: 1990)
#' @return Processed sharp RDD dataset
process_sharp_rdd_data <- function(processed_data_path, treatment_year = 1995, control_year = 1990) {
  
  cat("===============================================\n")
  cat("SHARP RDD DATA PROCESSING PIPELINE\n")
  cat("===============================================\n")
  cat("Treatment year:", treatment_year, "\n")
  cat("Control year:", control_year, "\n\n")
  
  # --------------------------------------------------------------------------
  # 1. ENVIRONMENT SETUP AND DATA LOADING
  # --------------------------------------------------------------------------
  cat("1. Setting up environment and loading data...\n")
  
  # Load main processed data
  load(file.path(processed_data_path, "main.RData"))
  cat("✓ Loaded main.RData\n")
  cat("  - dfZRR_raw:", nrow(dfZRR_raw), "observations\n")
  cat("  - df_merged:", nrow(df_merged), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2. LOAD GEOGRAPHIC RDD DATA
  # --------------------------------------------------------------------------
  cat("\n2. Loading geographic RDD data...\n")
  
  dfDistance <- load_and_process_data(
    file.path(processed_data_path, "dataGeoRDD1.xlsx")
  ) %>%
    select(-year)
  
  cat("✓ Loaded geographic RDD data\n")
  cat("  - Distance observations:", nrow(dfDistance), "\n")
  
  # --------------------------------------------------------------------------
  # 3. PREPARE BASE ZRR DATASET
  # --------------------------------------------------------------------------
  cat("\n3. Preparing base ZRR dataset...\n")
  
  dfZRR <- dfZRR_raw %>%
    filter(year == treatment_year) %>%
    mutate(across(starts_with("FN"), as.numeric))
  
  # Remove specific FN years
  excluded_fn_years <- c("FN2019", "FN2014", "FN2004", "FN1999", "FN1994")
  available_excluded <- intersect(excluded_fn_years, names(dfZRR))
  
  if (length(available_excluded) > 0) {
    dfZRR <- dfZRR %>% select(-all_of(available_excluded))
    cat("✓ Removed FN variables:", paste(available_excluded, collapse = ", "), "\n")
  }
  
  # --------------------------------------------------------------------------
  # EXTRA VALIDATION: CHECK & FIX DUPLICATES IN dfZRR
  # --------------------------------------------------------------------------
  cat("\nExtra validation: Checking duplicates in dfZRR...\n")
  dup_codes <- dfZRR %>%
    count(codecommune) %>%
    filter(n > 1)
  
  if (nrow(dup_codes) > 0) {
    cat("⚠ Found duplicates in 'codecommune':", nrow(dup_codes), "communes duplicated\n")
    cat("  - Keeping the first occurrence only\n")
    dfZRR <- dfZRR %>%
      arrange(codecommune) %>%
      distinct(codecommune, .keep_all = TRUE)
    cat("✓ Deduplicated dfZRR | Remaining rows:", nrow(dfZRR), "\n")
  } else {
    cat("✓ No duplicates found in 'codecommune'\n")
  }
  
  cat("✓ Prepared base ZRR dataset for year", treatment_year, "\n")
  cat("  - Communes:", length(unique(dfZRR$codecommune)), "\n")
  
  # --------------------------------------------------------------------------
  # 4. MERGE WITH DISTANCE DATA AND CREATE VARIABLES
  # --------------------------------------------------------------------------
  cat("\n4. Merging with distance data and creating variables...\n")
  dfZRR <- dfZRR %>%
    left_join(dfDistance, by = "codecommune", relationship = "one-to-one") %>%
    filter(!is.na(codecommune), year == treatment_year) %>%
    select(-year) %>%
    mutate(
      FN1988 = as.numeric(FN1988),
      FN2002 = as.numeric(FN2002),
      deltaFN = FN2002 - FN1988,  # Change in FN vote share
      y = FN2002  # Outcome variable
    ) %>%
    select(
      codecommune, distance_to_border, treatment, canton, deltaFN, border,
      starts_with("FN"), starts_with("RPR"), starts_with("turnout")
    )
  
  
  
  cat("✓ Created outcome and treatment variables\n")
  cat("  - Final ZRR observations:", nrow(dfZRR), "\n")
  cat("  - Variables after selection:", ncol(dfZRR), "\n")
  
  # --------------------------------------------------------------------------
  # 5. PREPARE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n5. Preparing control variables for year", control_year, "...\n")
  
  df_merged_to_merge <- df_merged %>%
    mutate(codecommune = standardize_commune_codes(codecommune)) %>%
    filter(year == control_year) %>%
    select(-year, -FN1995, -FN1988)
  
  cat("✓ Prepared control variables\n")
  cat("  - Control observations:", nrow(df_merged_to_merge), "\n")
  cat("  - Control variables:", ncol(df_merged_to_merge) - 1, "\n")  # -1 for codecommune
  
  
  # --------------------------------------------------------------------------
  # 6. MERGE ZRR WITH CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n6. Merging ZRR with control variables...\n")
  
  dfZRRControls <- dfZRR %>%
    left_join(df_merged_to_merge, by = "codecommune")
  
  cat("✓ Merged datasets\n")
  cat("  - Merged observations:", nrow(dfZRRControls), "\n")
  cat("  - Total variables:", ncol(dfZRRControls), "\n")
  
  # --------------------------------------------------------------------------
  # 7. HANDLE MISSING VALUES WITH ALTERNATIVE YEARS
  # --------------------------------------------------------------------------
  cat("\n7. Handling missing values with alternative years...\n")
  
  # Fill missing revenue data with 1994 values
  dfZRRControls <- fill_missing_from_alternative_year(
    dfZRRControls, df_merged, "revenuPerK", 1994
  )
  
  # Fill missing age structure data with 1995 values
  dfZRRControls <- fill_missing_from_alternative_year(
    dfZRRControls, df_merged, "popYoungOld", 1995
  )
  
  # --------------------------------------------------------------------------
  # 8. DATA CLEANING
  # --------------------------------------------------------------------------
  cat("\n8. Cleaning data...\n")
  
  # # Define control variables for reference
  # controls <- names(df_merged)
  # excluded_control_vars <- c("codecommune", "year", "reg", "nomcommune", "dep", "FN1995", "haie", "vigne")
  # controls <- setdiff(controls, excluded_control_vars)
  # 
  # cat("✓ Defined", length(controls), "control variables\n")
  
  # Clean all variables (excluding empty string from original setdiff)
  # variables_to_clean <- setdiff(names(dfZRRControls), c("revenuPerK", "asso", "popYoungOld")) 
  # dfZRRControls <- clean_data_variables(dfZRRControls, variables_to_clean)
  
  # Convert treatment to logical
  dfZRRControls$treatment <- as.logical(dfZRRControls$treatment)
  


  # --------------------------------------------------------------------------
  # 9. CREATE RDD VARIABLES AND FINAL CLEANUP
  # --------------------------------------------------------------------------
  cat("\n9. Creating RDD variables and final cleanup...\n")
  
  # Create standard RDD variable names
  dfZRRControls <- dfZRRControls %>%
    mutate(
      x = distance_to_border,  # Running variable
      z = treatment            # Treatment indicator
    ) %>%
    unique()
  
  cat("✓ Created RDD variables (x = running variable, z = treatment)\n")
  
  # --------------------------------------------------------------------------
  # 10. FINAL DATASET SUMMARY
  # --------------------------------------------------------------------------
  cat("\n10. Final Dataset Summary:\n")
  
  final_communes <- length(unique(dfZRRControls$codecommune))
  final_observations <- nrow(dfZRRControls)
  final_variables <- ncol(dfZRRControls)
  
  cat("  - Final communes:", final_communes, "\n")
  cat("  - Final observations:", final_observations, "\n")
  cat("  - Final variables:", final_variables, "\n")
  
  cat("  - Treatment distribution:\n")
  print(table(dfZRRControls$z, useNA = "ifany"))
  
  cat("  - Outcome variable (y = FN2002) summary:\n")
  print(summary(dfZRRControls$FN2002))
  
  cat("  - Running variable (x = distance_to_border) summary:\n")
  print(summary(dfZRRControls$x))
  
  cat("  - FN change (deltaFN) summary:\n")
  print(summary(dfZRRControls$deltaFN))
  
  # --------------------------------------------------------------------------
  # 11. CLEANUP AND SAVE
  # --------------------------------------------------------------------------
  cat("\n11. Cleaning up and saving...\n")
  

  # Save the processed dataset
  save(dfDistance, dfZRRControls, file = file.path(processed_data_path, "script_sharp.RData"))
  
  cat("✓ Saved script_sharp.RData\n")
  cat("\n===============================================\n")
  cat("SHARP RDD PROCESSING COMPLETED SUCCESSFULLY\n")
  cat("===============================================\n")
  
}


# ==============================================================================
# EXECUTION 
# ==============================================================================

# Run with default parameters (treatment_year = 1995, control_year = 1990)
process_sharp_rdd_data(processed_data_path)
