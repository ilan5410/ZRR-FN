# ================================================================================
# dataDes.RData DATA PROCESSING PIPELINE
# ================================================================================
# This script processes Zone de Revitalisation Rurale (ZRR) data for analysis
# and for descriptive statistics table
# ================================================================================

#' Process ZRR data for longitudinal analysis with treatment effects
#' @param processed_data_path Path to processed data directory
#' @param raw_data_path Path to raw data directory
#' @param reference_year Year to use for baseline analysis (default: 1995)
#' @param max_year Maximum year to include in analysis (default: 2020)
#' @return List containing processed ZRR datasets
process_zrr_data <- function(processed_data_path, raw_data_path, reference_year = 1995, max_year = 2020) {
  
  cat("===============================================\n")
  cat("ZRR DATA PROCESSING PIPELINE\n")
  cat("===============================================\n")
  cat("Reference year:", reference_year, "\n")
  cat("Maximum year:", max_year, "\n\n")
  
  # --------------------------------------------------------------------------
  # 1. ENVIRONMENT SETUP AND DATA LOADING
  # --------------------------------------------------------------------------
  cat("1. Setting up environment and loading data...\n")
  
  # Load main processed data
  load(file.path(processed_data_path, "main.RData"))
  
  cat("✓ Loaded main.RData\n")
  cat("  - dfZRR_raw:", nrow(dfZRR_raw), "observations\n")
  cat("  - df_merged:", nrow(df_merged), "observations\n")
  
  # Load ZRR treatment data
  cat("✓ Loading ZRR treatment data...\n")
  zrr_raw <- load_and_process_data(
    file.path(raw_data_path, "ZRR.csv"),
    select_cols = c("codecommune", "nom", "treatment", "year")
  )
  
  cat("✓ Loaded ZRR treatment data:", nrow(zrr_raw), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2. MERGE AND TREATMENT YEAR CALCULATION
  # --------------------------------------------------------------------------
  cat("\n2. Merging datasets and calculating treatment years...\n")
  
  dfZRR <- merge(dfZRR_raw, zrr_raw, by = c("codecommune", "year")) %>%
    group_by(codecommune) %>%
    # Calculate treatment year: first year when commune received ZRR designation
    mutate(
      year_treat = case_when(
        any(treatment == 1 & !is.na(year)) ~ min(year[treatment == 1 & !is.na(year)], na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    # Remove electoral data columns not needed for analysis
    select(-matches("^FN(2019|2014|2004|1999|1994)$"))
  
  cat("✓ Treatment year calculation complete\n")
  cat("  - Communes with treatment:", sum(!is.na(unique(dfZRR$year_treat))), "\n")
  
  # --------------------------------------------------------------------------
  # 3. BASELINE DATA PREPARATION
  # --------------------------------------------------------------------------
  cat("\n3. Preparing", reference_year, "baseline data...\n")
  
  dfZRR_baseline <- dfZRR %>%
    filter(year == reference_year) %>%
    select(-year) %>%
    mutate(
      across(matches("^(FN|RPR)"), ~ suppressWarnings(as.numeric(.))),
      deltaFN = {
        if (all(c("FN2002", "FN1988") %in% names(pick(everything())))) 
          FN2002 - FN1988 
        else 
          NA_real_
      },
      treatment_in_1995 = treatment
    )
  
  cat("✓ Baseline data prepared for", nrow(dfZRR_baseline), "communes\n")
  
  # --------------------------------------------------------------------------
  # 4. LONG FORMAT TRANSFORMATION
  # --------------------------------------------------------------------------
  cat("\n4. Converting to long format for time series analysis...\n")
  
  dfZRRlong <- dfZRR_baseline %>%
    # Reshape electoral data (FN/RPR) from wide to long format
    pivot_longer(
      cols = matches("^(FN|RPR)\\d+$"), 
      names_to = c(".value", "year"), 
      names_pattern = "(FN|RPR)(\\d+)"
    ) %>%
    mutate(
      year = as.numeric(year),
      # Set missing treatment years to 0 (never treated)
      year_treat = replace_na(year_treat, 0)
    ) %>%
    arrange(nom)
  
  years_available <- sort(unique(dfZRRlong$year))
  cat("✓ Long format dataset created\n")
  cat("  - Observations:", nrow(dfZRRlong), "\n")
  cat("  - Years available:", paste(years_available, collapse = ", "), "\n")
  
  # --------------------------------------------------------------------------
  # 5. FINAL CONTROLS DATASET CREATION
  # --------------------------------------------------------------------------
  cat("\n5. Creating final controls dataset...\n")
  
  
  dfZRRControls <- df_merged %>%
    # Merge with ZRR electoral data
    left_join(dfZRRlong, by = c("codecommune", "year")) %>%
    distinct() %>%
    # Restrict to analysis period
    filter(year <= max_year) %>%
    # Data cleaning and type conversion
    mutate(across(where(is.numeric), as.double)) %>%
    mutate(across(where(is.numeric), ~ na_if(.x, Inf))) %>%
    # Forward-fill within communes
    group_by(codecommune) %>%
      arrange(year, .by_group = TRUE) %>%
      # Make year_treat and treatment_in_1995 constant within commune
      mutate(
        year_treat = if (all(is.na(year_treat))) NA_real_ else max(year_treat, na.rm = TRUE),
        treatment_in_1995 = if (all(is.na(treatment_in_1995))) NA_real_ else max(treatment_in_1995, na.rm = TRUE)
      ) %>%
      # Fill text/categorical fields without using max()
      tidyr::fill(nom, canton, .direction = "downup") %>%
    ungroup() %>%
    # Compute treatment from year_treat (no need to down-fill treatment)
    mutate(treatment = if_else(!is.na(year_treat) & year >= year_treat, 1L, 0L))
  
  
  cat("✓ Final dataset created\n")
  cat("  - Observations:", nrow(dfZRRControls), "\n")
  cat("  - Communes:", length(unique(dfZRRControls$codecommune)), "\n")
  cat("  - Variables:", ncol(dfZRRControls), "\n")
  
  # --------------------------------------------------------------------------
  # 6. DATA CLEANING
  # --------------------------------------------------------------------------
  # cat("\n6. Cleaning data...\n")
  # 
  # # Clean all variables
  # variables_to_clean <- names(dfZRRControls)
  # dfZRRControls <- clean_data_variables(dfZRRControls, variables_to_clean)
  
  # --------------------------------------------------------------------------
  # 7. FINAL DATASET SUMMARY
  # --------------------------------------------------------------------------
  cat("\n7. Final Dataset Summary:\n")
  
  final_communes <- length(unique(dfZRRControls$codecommune))
  final_years <- sort(unique(dfZRRControls$year))
  final_observations <- nrow(dfZRRControls)
  
  cat("  - Final communes:", final_communes, "\n")
  cat("  - Final years:", paste(final_years, collapse = ", "), "\n")
  cat("  - Final observations:", final_observations, "\n")
  
  if ("treatment" %in% names(dfZRRControls)) {
    cat("  - Treatment distribution:\n")
    print(table(dfZRRControls$treatment, useNA = "ifany"))
  }
  
  if ("year_treat" %in% names(dfZRRControls)) {
    cat("  - Treatment year summary:\n")
    print(summary(dfZRRControls$year_treat))
  }
  
  # --------------------------------------------------------------------------
  # 8. SAVE PROCESSED DATA
  # --------------------------------------------------------------------------
  cat("\n8. Saving processed data...\n")
  
  dfZRR <- dfZRR_baseline

  rm(dfZRR_baseline, zrr_raw)
  
  # Save workspace
  save(
    dfZRR, dfZRRlong, dfZRRControls,
    file = file.path(processed_data_path, "dataDes.RData"),
    envir = environment()
  )
  
  cat("✓ Saved dataDes.RData\n")

  cat("\n===============================================\n")
  cat("ZRR DATA PROCESSING COMPLETED SUCCESSFULLY\n")
  cat("===============================================\n")
  
}

# ================================================================================
# EXECUTION 
# ================================================================================

process_zrr_data(processed_data_path, raw_data_path)
