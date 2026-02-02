# ================================================================================
# ECONOMIC OUTCOMES DATA PROCESSING PIPELINE
# ================================================================================
# This script processes economic and demographic outcomes data for ZRR analysis
# Created following the functional approach pattern
# ================================================================================

#' Process economic outcomes data for ZRR analysis
#' @param processed_data_path Path to processed data directory
#' @param raw_data_path Path to raw data directory
#' @param analysis_years Vector of years to include in analysis (default: c(1990, 1999))
#' @param outcome_variables Vector of outcome variable names (default: predefined set)
#' @return Processed economic outcomes dataset
process_eco_outcomes_data <- function(processed_data_path, raw_data_path, 
                                      analysis_years = c(1990, 1999),
                                      outcome_variables = NULL) {
  
  cat("===============================================\n")
  cat("ECONOMIC OUTCOMES DATA PROCESSING PIPELINE\n")
  cat("===============================================\n")
  cat("Analysis years:", paste(analysis_years, collapse = ", "), "\n")
  
  # --------------------------------------------------------------------------
  # 1. DEFINE OUTCOME VARIABLES
  # --------------------------------------------------------------------------
  cat("1. Setting up outcome variables...\n")
  
  # Default outcome variables if not provided
  if (is.null(outcome_variables)) {
    outcomes <- c(
      # Population variables
      "pop", "poph", "popf", "popYoungOld", "popDensity",
      # Employment and economic variables
      "pchom", "ratEmp", "ratForeigners", "pagri", "pindp", "ppint", "pempl",
      # Education variables
      "educNoDiplomaPerK", "educSUPPerK", "educBACPerK", "educCAPBEPPerK",
      # Social and economic indicators
      "asso", "pouvr", "revenuPerK"
    )
  } else {
    outcomes <- outcome_variables
  }
  
  cat("✓ Defined", length(outcomes), "outcome variables\n")
  cat("  - Population vars:", sum(grepl("^pop|poph|popf", outcomes)), "\n")
  cat("  - Employment vars:", sum(grepl("chom|Emp|agri|indp|pint|empl", outcomes)), "\n")
  cat("  - Education vars:", sum(grepl("^educ", outcomes)), "\n")
  cat("  - Other vars:", sum(!grepl("^pop|poph|popf|chom|Emp|agri|indp|pint|empl|^educ", outcomes)), "\n")
  
  # --------------------------------------------------------------------------
  # 2. LOAD ZRR CONTROLS DATA
  # --------------------------------------------------------------------------
  cat("\n2. Loading ZRR controls data...\n")
  
  data_des_path <- file.path(processed_data_path, "dataDes.RData")
  if (!file.exists(data_des_path)) {
    stop("❌ dataDes.RData does not exist. Please run ZRR data processing first.")
  }
  
  # Load data into local environment
  env <- new.env()
  load(data_des_path, envir = env)
  dfZRRControls <- env$dfZRRControls
  
  cat("✓ Loaded dataDes.RData\n")
  cat("  - Total observations:", nrow(dfZRRControls), "\n")
  cat("  - Available years:", paste(sort(unique(dfZRRControls$year)), collapse = ", "), "\n")
  
  # --------------------------------------------------------------------------
  # 3. FILTER AND SELECT ECONOMIC DATA
  # --------------------------------------------------------------------------
  cat("\n3. Filtering and selecting economic data...\n")
  
  # Define base columns needed for analysis
  base_columns <- c("codecommune", "year", "dep", "reg", "treatment_in_1995")
  
  # Check which columns are available
  available_base <- intersect(base_columns, names(dfZRRControls))
  available_outcomes <- intersect(outcomes, names(dfZRRControls))
  missing_base <- setdiff(base_columns, names(dfZRRControls))
  missing_outcomes <- setdiff(outcomes, names(dfZRRControls))
  
  if (length(missing_base) > 0) {
    warning("Missing base columns: ", paste(missing_base, collapse = ", "))
  }
  if (length(missing_outcomes) > 0) {
    cat("⚠️  Missing outcome variables: ", paste(missing_outcomes, collapse = ", "), "\n")
  }
  
  # Filter data for analysis years and select relevant columns
  df_eco <- dfZRRControls %>%
    filter(year %in% analysis_years) %>%
    select(any_of(c(available_base, available_outcomes)))
  
  cat("✓ Filtered economic data\n")
  cat("  - Observations after filtering:", nrow(df_eco), "\n")
  cat("  - Communes:", length(unique(df_eco$codecommune)), "\n")
  cat("  - Available outcome variables:", length(available_outcomes), "of", length(outcomes), "\n")
  
  # --------------------------------------------------------------------------
  # 4. LOAD AND PROCESS CANTON DATA
  # --------------------------------------------------------------------------
  cat("\n4. Loading and processing canton data...\n")
  
  canton_file <- file.path(raw_data_path, "france1999.dbf")
  if (!file.exists(canton_file)) {
    warning("❌ Canton data file not found: ", canton_file)
    dfCanton <- NULL
  } else {
    dfCanton <- st_read(canton_file, quiet = TRUE) %>%
      mutate(
        codecommune = standardize_commune_codes(paste0(as.character(DEP), as.character(COM))),
        codecanton = paste0(as.character(DEP), as.character(CT))
      ) %>%
      select(codecommune, codecanton)
    
    cat("✓ Loaded canton data\n")
    cat("  - Canton mappings:", nrow(dfCanton), "\n")
    cat("  - Unique cantons:", length(unique(dfCanton$codecanton)), "\n")
  }
  
  # --------------------------------------------------------------------------
  # 5. MERGE WITH CANTON DATA
  # --------------------------------------------------------------------------
  cat("\n5. Merging with canton data...\n")
  
  if (!is.null(dfCanton)) {
    initial_rows <- nrow(df_eco)
    
    # Drop duplicates (keep first row per codecommune) and assert uniqueness
    dfCanton <- dfCanton %>% dplyr::distinct(codecommune, .keep_all = TRUE)
    
    df_eco <- df_eco %>%
      left_join(dfCanton, by = "codecommune", relationship = "many-to-one")
    
    final_rows <- nrow(df_eco)
    cat("✓ Merged with canton data\n")
    cat("  - Observations before merge:", initial_rows, "\n")
    cat("  - Observations after merge:", final_rows, "\n")
    cat("  - Communes with canton info:", sum(!is.na(df_eco$codecanton)), "\n")
  } else {
    cat("⚠️  Skipped canton merge due to missing data\n")
  }
  
  # --------------------------------------------------------------------------
  # 6. LOAD AND MERGE DISTANCE DATA
  # --------------------------------------------------------------------------
  cat("\n6. Loading and merging distance data...\n")
  
  script_sharp_path <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(script_sharp_path)) {
    warning("❌ script_sharp.RData does not exist. Distance data will be missing.")
    dfDistance <- NULL
  } else {
    # Load distance data into local environment
    env_distance <- new.env()
    load(script_sharp_path, envir = env_distance)
    dfDistance <- env_distance$dfDistance
    
    cat("✓ Loaded distance data\n")
    cat("  - Distance observations:", nrow(dfDistance), "\n")
  }
  
  # Merge with distance data
  if (!is.null(dfDistance)) {
    initial_rows <- nrow(df_eco)
    df_eco <- df_eco %>%
      left_join(dfDistance, by = "codecommune", relationship = "many-to-one")
    
    final_rows <- nrow(df_eco)
    cat("✓ Merged with distance data\n")
    cat("  - Observations before merge:", initial_rows, "\n")
    cat("  - Observations after merge:", final_rows, "\n")
    
    if ("distance_to_border" %in% names(df_eco)) {
      cat("  - Communes with distance info:", sum(!is.na(df_eco$distance_to_border)), "\n")
    }
  } else {
    cat("⚠️  Skipped distance merge due to missing data\n")
  }
  
  # --------------------------------------------------------------------------
  # 7. FINAL DATASET SUMMARY
  # --------------------------------------------------------------------------
  cat("\n7. Final Dataset Summary:\n")
  
  final_communes <- length(unique(df_eco$codecommune))
  final_years <- sort(unique(df_eco$year))
  final_observations <- nrow(df_eco)
  final_variables <- ncol(df_eco)
  
  cat("  - Final communes:", final_communes, "\n")
  cat("  - Final years:", paste(final_years, collapse = ", "), "\n")
  cat("  - Final observations:", final_observations, "\n")
  cat("  - Final variables:", final_variables, "\n")
  
  # Treatment distribution
  if ("treatment_in_1995" %in% names(df_eco)) {
    cat("  - Treatment distribution:\n")
    print(table(df_eco$treatment_in_1995, useNA = "ifany"))
  }
  
  # Check data completeness for key variables
  cat("  - Data completeness for key variables:\n")
  key_vars <- intersect(c("treatment_in_1995", "codecanton", "distance_to_border"), names(df_eco))
  for (var in key_vars) {
    complete_pct <- round(100 * sum(!is.na(df_eco[[var]])) / nrow(df_eco), 1)
    cat("    -", var, ":", complete_pct, "%\n")
  }
  
  # --------------------------------------------------------------------------
  # 8. SAVE PROCESSED DATA
  # --------------------------------------------------------------------------
  cat("\n8. Saving processed data...\n")
  
  # Create results list
  results <- list(
    df_eco = df_eco,
    outcomes = outcomes,
    analysis_years = analysis_years,
    processing_info = list(
      communes = final_communes,
      observations = final_observations,
      variables = final_variables,
      processing_date = Sys.Date()
    )
  )
  
  # Save to global environment for compatibility
  assign("outcomes", outcomes, envir = .GlobalEnv)
  
  # Save workspace
  save(df_eco, outcomes, file = file.path(processed_data_path, 'eco_outcomes.RData'))
  
  cat("✓ Saved eco_outcomes.RData\n")
  cat("✓ Created global environment objects: df_eco, outcomes\n")
  
  cat("\n===============================================\n")
  cat("ECONOMIC OUTCOMES PROCESSING COMPLETED SUCCESSFULLY\n")
  cat("===============================================\n")
  
}

# ================================================================================
# EXECUTION 
# ================================================================================

process_eco_outcomes_data(processed_data_path, raw_data_path)
