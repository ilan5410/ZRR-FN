# ==============================================================================
# Evolution of Socioeconomic Variables between 1988 and 2002 with T-Test Results
# ==============================================================================
# This script analyzes the evolution of socioeconomic variables between pre-
# and post-treatment periods (1988 and 2002) using t-tests to assess balance
# and temporal changes in key indicators
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Analyze evolution of socioeconomic variables between 1988 and 2002
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param labels Named vector of variable labels for output
#' @param treatment_year Year of treatment implementation (default: 1995)
#' @param pre_year Pre-treatment comparison year (default: 1988)
#' @param post_year Post-treatment comparison year (default: 2002)
#' @return Balance summary table with t-test results
generate_socioeconomic_evolution_analysis <- function(processed_data_path, path_tables, labels,
                                                      treatment_year = 1995,
                                                      pre_year = 1988,
                                                      post_year = 2002) {
  
  cat("===============================================\n")
  cat("SOCIOECONOMIC VARIABLES EVOLUTION ANALYSIS\n")
  cat("===============================================\n")
  cat("Treatment year:", treatment_year, "\n")
  cat("Pre-treatment period:", pre_year, "\n")
  cat("Post-treatment period:", post_year, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
  data_file <- file.path(processed_data_path, "dataDes.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist")
  }
  
  load(data_file)
  cat("✓ Loaded dataDes.RData\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE AND FILTER DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing and filtering data...\n")
  
  # Variables to exclude from analysis
  exclude_vars <- c("nomcommune", "reg", "nom", "time_since_open", "year_treat", 
                    "FN_log", "treatment", "FN1995", "RPR", "deltaFN", "turnout_2002", 
                    "canton", "popYoungOld", "revenuPerK", "treatment_in_1995", "dep")
  
  # Process and filter data
  df_did <- dfZRRControls %>%
    filter(year %in% c(pre_year, post_year)) %>%
    mutate(
      treated = if_else(year_treat == treatment_year, 1, 0, missing = 0),
      post = if_else(year >= treatment_year, 1, 0),
      did = post * treated
    ) %>%
    select(-any_of(exclude_vars)) %>%
    filter(across(.cols = -c(codecommune, year), .fns = ~ !is.na(.x) & !is.infinite(.x))) %>%
    distinct(codecommune, year, .keep_all = TRUE)
  
  cat("✓ Processed and filtered data\n")
  cat("  - Initial observations:", nrow(df_did), "\n")
  cat("  - Unique communes:", length(unique(df_did$codecommune)), "\n")
  
  # --------------------------------------------------------------------------
  # 3. VALIDATE DATA STRUCTURE
  # --------------------------------------------------------------------------
  cat("\n3. Validating data structure...\n")
  
  # Check if both years are present
  available_years <- unique(df_did$year)
  if (length(available_years) < 2) {
    stop("Data contains only one year (", paste(available_years, collapse = ", "), 
         "); need both ", pre_year, " and ", post_year, " for balance check")
  }
  
  # Check observations per year
  year_counts <- table(df_did$year)
  cat("✓ Data structure validated\n")
  cat("  - Years available:", paste(names(year_counts), collapse = ", "), "\n")
  cat("  - Observations per year:", paste(year_counts, collapse = ", "), "\n")
  
  # --------------------------------------------------------------------------
  # 4. DEFINE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n4. Defining control variables...\n")
  
  # Define control variables (exclude key identifiers and constructed variables)
  controls <- setdiff(names(df_did), c("codecommune", "year", "post", "treated", "did", "FN"))
  vars <- setdiff(controls, "typologie")  # Remove typologie if present
  
  cat("✓ Defined control variables\n")
  cat("  - Total variables for analysis:", length(vars) + 1, "(including FN)\n")  # +1 for FN
  cat("  - Control variables:", length(vars), "\n")
  
  # --------------------------------------------------------------------------
  # 5. DEFINE BALANCE CALCULATION FUNCTION
  # --------------------------------------------------------------------------
  cat("\n5. Defining balance calculation function...\n")
  
  # Function to calculate balance table with t-tests
  calculate_balance <- function(df, grouping_var, target_vars) {
    
    # Calculate means by group
    means <- df %>%
      group_by({{ grouping_var }}) %>%
      summarise(across(all_of(target_vars), mean, na.rm = TRUE), .groups = 'drop') %>%
      pivot_longer(-{{ grouping_var }}, names_to = "variable", values_to = "mean") %>%
      pivot_wider(names_from = {{ grouping_var }}, values_from = mean, names_prefix = "mean_")
    
    # Perform t-tests for each variable
    t_tests <- map_dfr(target_vars, function(var) {
      test <- t.test(df[[var]] ~ df[[rlang::as_name(enquo(grouping_var))]])
      data.frame(
        variable = var,
        difference = test$estimate[2] - test$estimate[1],
        p_value = test$p.value
      )
    })
    
    # Combine results
    summary <- means %>%
      left_join(t_tests, by = "variable") %>%
      mutate(significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      ))
    
    return(summary)
  }
  
  cat("✓ Defined balance calculation function\n")
  
  # --------------------------------------------------------------------------
  # 6. CALCULATE BALANCE STATISTICS
  # --------------------------------------------------------------------------
  cat("\n6. Calculating balance statistics...\n")
  
  # Generate balance summary including FN variable
  analysis_vars <- c("FN", vars)
  balance_summary <- calculate_balance(df_did, post, analysis_vars)
  
  cat("✓ Calculated balance statistics\n")
  cat("  - Variables analyzed:", nrow(balance_summary), "\n")
  cat("  - Significant differences (p<0.05):", sum(balance_summary$p_value < 0.05, na.rm = TRUE), "\n")
  
  # --------------------------------------------------------------------------
  # 7. APPLY VARIABLE LABELS
  # --------------------------------------------------------------------------
  cat("\n7. Applying variable labels...\n")
  
  # Check for labels and apply them
  if (!exists("labels") || !is.vector(labels) || !all(balance_summary$variable %in% names(labels))) {
    warning("Labels not properly defined; using variable names")
    balance_summary$Variable <- balance_summary$variable
    cat("  ⚠ Using variable names (labels not available)\n")
  } else {
    balance_summary$Variable <- labels[balance_summary$variable]
    cat("✓ Applied variable labels\n")
  }
  
  # --------------------------------------------------------------------------
  # 8. FORMAT RESULTS TABLE
  # --------------------------------------------------------------------------
  cat("\n8. Formatting results table...\n")
  
  # Rename and select columns for final output
  balance_summary <- balance_summary %>%
    select(Variable, mean_0, mean_1, difference, significance) %>%
    rename(
      !!paste0(pre_year, " (Mean)") := mean_0,
      !!paste0(post_year, " (Mean)") := mean_1,
      "Difference" = difference,
      "Significance" = significance
    )
  
  cat("✓ Formatted results table\n")
  cat("  - Final table dimensions:", nrow(balance_summary), "x", ncol(balance_summary), "\n")
  
  # --------------------------------------------------------------------------
  # 9. GENERATE LATEX OUTPUT
  # --------------------------------------------------------------------------
  cat("\n9. Generating LaTeX output...\n")
  
  # Create column names for kable
  col_names <- c("Variable", 
                 paste0(pre_year, " (Mean)"), 
                 paste0(post_year, " (Mean)"), 
                 "Difference", 
                 "")
  
  # Generate LaTeX table
  latex_table <- balance_summary %>%
    kable(
      format = "latex",
      caption = paste("Evolution of Socioeconomic Variables between", pre_year, "and", post_year, "with T-Test Results"),
      digits = c(NA, 4, 4, 4, 3, NA),
      booktabs = TRUE,
      escape = FALSE,
      col.names = col_names,
      label = "tab:var-evolution"
    ) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down"),
      full_width = FALSE,
      position = "center"
    ) %>%
    add_footnote(
      paste("Note: Means are calculated for pre-treatment (", pre_year, ") and post-treatment (", 
            post_year, ") periods. Differences are tested using two-sample t-tests. ",
            "Significance levels: *** p<0.001, ** p<0.01, * p<0.05.", sep = ""), 
      notation = "none"
    )
  
  output_file <- file.path(path_tables, "annex_var_evolution.tex")
  cat(latex_table, file = output_file)
  
  cat("✓ Generated LaTeX output\n")
  cat("  - Output file:", output_file, "\n")
  
  # --------------------------------------------------------------------------
  # 10. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n10. Results Summary:\n")
  
  # Summary statistics
  significant_vars <- balance_summary[!is.na(balance_summary$Significance) & 
                                        balance_summary$Significance != "", ]
  
  if (nrow(significant_vars) > 0) {
    cat("  - Significant changes found in", nrow(significant_vars), "variables:\n")
    for (i in 1:min(5, nrow(significant_vars))) {  # Show up to 5 variables
      var_info <- significant_vars[i, ]
      cat("    ", var_info$Variable, " (", var_info$Significance, ")\n")
    }
    if (nrow(significant_vars) > 5) {
      cat("    ... and", nrow(significant_vars) - 5, "more\n")
    }
  } else {
    cat("  - No significant changes found between periods\n")
  }
  
  # Difference statistics
  differences <- balance_summary$Difference[!is.na(balance_summary$Difference)]
  cat("\n  - Change statistics:\n")
  cat("    Mean absolute change:", round(mean(abs(differences)), 4), "\n")
  cat("    Largest increase:", round(max(differences), 4), "\n")
  cat("    Largest decrease:", round(min(differences), 4), "\n")
  
  # Period comparison
  pre_means <- balance_summary[[paste0(pre_year, " (Mean)")]]
  post_means <- balance_summary[[paste0(post_year, " (Mean)")]]
  cat("\n  - Period averages:\n")
  cat("   ", pre_year, "average:", round(mean(pre_means, na.rm = TRUE), 4), "\n")
  cat("   ", post_year, "average:", round(mean(post_means, na.rm = TRUE), 4), "\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("SOCIOECONOMIC EVOLUTION ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
  # Return results
  return(balance_summary)
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_socioeconomic_evolution_analysis(processed_data_path, path_tables, labels)