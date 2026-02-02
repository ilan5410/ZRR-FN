# ==============================================================================
# FN Growth Data Processing Pipeline
# ==============================================================================
# This script processes Front National (FN) electoral data over time for 
# spatial discontinuity analysis, creating a longitudinal dataset
# ==============================================================================


# ==============================================================================
# MAIN PROCESSING FUNCTION
# ==============================================================================

#' Process FN growth data for longitudinal analysis
#' @param processed_data_path Path to processed data directory
#' @param reference_year Year to use for treatment assignment (default: 1995)
#' @param data_substitute_year Year to substitute for missing recent data (default: 2020->2022)
#' @return Processed FN growth dataset
process_fn_growth_data <- function(processed_data_path, reference_year = 1995, data_substitute_year = 2020) {
  
  cat("===============================================\n")
  cat("FN GROWTH DATA PROCESSING PIPELINE\n")
  cat("===============================================\n")
  cat("Reference year:", reference_year, "\n")
  cat("Data substitution:", data_substitute_year, "->", data_substitute_year + 2, "\n\n")
  
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
  # 2. PROCESS GEOGRAPHIC RDD DATA
  # --------------------------------------------------------------------------
  cat("\n2. Processing geographic RDD data...\n")
  
  dfDistance <- load_and_process_data(
    file.path(processed_data_path, "dataGeoRDD1.xlsx")
  ) %>%
    select(-year)
  
  cat("✓ Loaded geographic RDD data\n")
  cat("  - Observations:", nrow(dfDistance), "\n")
  cat("  - Unique communes:", length(unique(dfDistance$codecommune)), "\n")
  
  # --------------------------------------------------------------------------
  # 3. MERGE AND PREPARE FN ELECTORAL DATA
  # --------------------------------------------------------------------------
  cat("\n3. Merging and preparing FN electoral data...\n")
  
  dfZRR <- dfZRR_raw %>%
    left_join(dfDistance, by = "codecommune") %>%
    filter(year == reference_year) %>%
    select(codecommune, distance_to_border, treatment, canton, starts_with("FN")) %>%
    mutate(across(starts_with("FN"), as.numeric))
  
  # Remove specific FN years that are not needed
  excluded_fn_years <- c("FN2019", "FN2014", "FN2004", "FN1999", "FN1994")
  available_excluded <- intersect(excluded_fn_years, names(dfZRR))
  
  if (length(available_excluded) > 0) {
    dfZRR <- dfZRR %>% select(-all_of(available_excluded))
    cat("✓ Removed FN variables:", paste(available_excluded, collapse = ", "), "\n")
  }
  
  cat("✓ Prepared base FN dataset\n")
  cat("  - Communes:", length(unique(dfZRR$codecommune)), "\n")
  cat("  - FN variables:", sum(grepl("^FN", names(dfZRR))), "\n")
  
  # --------------------------------------------------------------------------
  # 4. RESHAPE TO LONG FORMAT
  # --------------------------------------------------------------------------
  cat("\n4. Reshaping FN data to long format...\n")
  
  dfZRR <- dfZRR %>%
    pivot_longer(
      cols = starts_with("FN"), 
      names_to = "year",        
      names_prefix = "FN",      
      values_to = "FN"          
    ) %>%
    mutate(year = as.integer(year))
  
  years_available <- sort(unique(dfZRR$year))
  cat("✓ Reshaped to long format\n")
  cat("  - Total observations:", nrow(dfZRR), "\n")
  cat("  - Years available:", paste(years_available, collapse = ", "), "\n")
  
  # --------------------------------------------------------------------------
  # 5. PREPARE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n5. Preparing control variables...\n")
  
  # Handle data substitution for missing recent years
  df_merged <- df_merged %>%
    mutate(codecommune = standardize_commune_codes(codecommune)) %>%
    mutate(year = ifelse(year == data_substitute_year, data_substitute_year + 2, year))
  
  cat("✓ Applied year substitution:", data_substitute_year, "->", data_substitute_year + 2, "\n")
  
  # --------------------------------------------------------------------------
  # 6. MERGE WITH CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n6. Merging with control variables...\n")
  
  dfZRRControls <- dfZRR %>%
    left_join(df_merged, by = c("codecommune", "year"), relationship = "many-to-many")
  
  # Remove unnecessary variables
  variables_to_remove <- c("nomcommune", "FN1995", "FN1988", "popYoungOld", "revenuPerK")
  available_to_remove <- intersect(variables_to_remove, names(dfZRRControls))
  
  if (length(available_to_remove) > 0) {
    dfZRRControls <- dfZRRControls %>% select(-all_of(available_to_remove))
    cat("✓ Removed variables:", paste(available_to_remove, collapse = ", "), "\n")
  }
  
  cat("✓ Merged datasets\n")
  cat("  - Final observations:", nrow(dfZRRControls), "\n")
  cat("  - Final variables:", ncol(dfZRRControls), "\n")
  cat("  - Communes:", length(unique(dfZRR$codecommune)), "\n")
  
  # --------------------------------------------------------------------------
  # 7. DATA CLEANING
  # --------------------------------------------------------------------------
  cat("\n7. Cleaning data...\n")
  
  # Clean all variables
  variables_to_clean <- names(dfZRRControls)
  dfZRRControls <- clean_data_variables(dfZRRControls, variables_to_clean)
  
  # Convert treatment to logical
  dfZRRControls$treatment <- as.logical(dfZRRControls$treatment)
  
  # --------------------------------------------------------------------------
  # 8. FINAL DATASET SUMMARY
  # --------------------------------------------------------------------------
  cat("\n8. Final Dataset Summary:\n")
  
  final_communes <- length(unique(dfZRRControls$codecommune))
  final_years <- sort(unique(dfZRRControls$year))
  final_observations <- nrow(dfZRRControls)
  
  cat("  - Final communes:", final_communes, "\n")
  cat("  - Final years:", paste(final_years, collapse = ", "), "\n")
  cat("  - Final observations:", final_observations, "\n")
  
  cat("  - Treatment distribution:\n")
  print(table(dfZRRControls$treatment, useNA = "ifany"))
  
  cat("  - FN vote share summary:\n")
  print(summary(dfZRRControls$FN))
  
  cat("  - Distance to border summary:\n")
  print(summary(dfZRRControls$distance_to_border))
  
  # --------------------------------------------------------------------------
  # 9. CLEANUP AND SAVE
  # --------------------------------------------------------------------------
  cat("\n9. Cleaning up and saving...\n")
  
  # Remove intermediate objects
  rm(dfZRR_raw, df_merged, dfZRR, dfDistance, 
     variables_to_clean, variables_to_remove, available_to_remove, available_excluded,
     years_available, final_communes, final_years, final_observations,
     envir = .GlobalEnv)
  
  # Save the processed dataset
  save(dfZRRControls, file = file.path(processed_data_path, "FN_growth.RData"))
  
  cat("✓ Saved FN_growth.RData\n")
  cat("\n===============================================\n")
  cat("FN GROWTH PROCESSING COMPLETED SUCCESSFULLY\n")
  cat("===============================================\n")
  
  return(dfZRRControls)
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

# Run with default parameters
process_fn_growth_data(processed_data_path)
