# ==============================================================================
# Residual Balance Analysis between Control and Treatment Groups with T-Tests
# ==============================================================================
# This script performs residual balance analysis between control and treatment
# groups using propensity score matching and t-tests on residuals conditional
# on department fixed effects and control variables
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate residual balance analysis between control and treatment groups
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param labels Named vector of variable labels for output
#' @param controls Vector of control variable names
#' @param matching_method Method for propensity score matching (default: "nearest")
#' @param matching_distance Distance measure for matching (default: "logit")
#' @param matching_caliper Caliper for matching (default: 0.2)
#' @return Balance summary table with residual t-test results
generate_residual_balance_analysis <- function(processed_data_path, path_tables, labels, controls,
                                               matching_method = "nearest",
                                               matching_distance = "logit",
                                               matching_caliper = 0.2) {
  
  cat("===============================================\n")
  cat("RESIDUAL BALANCE ANALYSIS\n")
  cat("===============================================\n")
  cat("Matching method:", matching_method, "\n")
  cat("Distance measure:", matching_distance, "\n")
  cat("Caliper:", matching_caliper, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
  data_file <- file.path(processed_data_path, "borders_pair.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded borders_pair.RData\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE DATA FOR MATCHING
  # --------------------------------------------------------------------------
  cat("\n2. Preparing data for matching...\n")
  
  # Process and clean data
  df_rct <- dfZRRControls %>%
    select(-x) %>%
    rename(treatmentZRR = z)
  
  cat("✓ Prepared data for matching\n")
  cat("  - Initial observations:", nrow(df_rct), "\n")
  cat("  - Treatment group size:", sum(df_rct$treatmentZRR), "\n")
  cat("  - Control group size:", sum(!df_rct$treatmentZRR), "\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE MATCHING FORMULA
  # --------------------------------------------------------------------------
  cat("\n3. Defining matching formula...\n")
  
  # Define matching variables
  matching_vars <- c("pchom", "FN1988", "delta_pop_1980_1995", "pop", "ratEmp", 
                     "ratForeigners", "asso", "educNoDiplomaPerK", "educSUPPerK", 
                     "educBACPerK", "educCAPBEPPerK", "poph", "popf", "pagri", 
                     "pindp", "ppint", "pempl", "pouvr", "altitude", "superficie", 
                     "min_distance_to_agglo", "logVac") #, "typologie")
  
  # Create matching formula
  matching_formula <- as.formula(paste("treatmentZRR ~", paste(matching_vars, collapse = " + ")))
  
  cat("✓ Defined matching formula\n")
  cat("  - Matching variables:", length(matching_vars), "\n")
  
  # --------------------------------------------------------------------------
  # 4. PERFORM INITIAL PROPENSITY SCORE MATCHING
  # --------------------------------------------------------------------------
  cat("\n4. Performing initial propensity score matching...\n")
  
  # Initial matching without caliper
  ps_model <- matchit(matching_formula, 
                      data = df_rct, 
                      method = matching_method, 
                      distance = matching_distance)
  
  cat("✓ Completed initial matching\n")
  cat("  - Method:", matching_method, "\n")
  cat("  - Distance:", matching_distance, "\n")
  
  # --------------------------------------------------------------------------
  # 5. PERFORM REFINED MATCHING WITH CALIPER
  # --------------------------------------------------------------------------
  cat("\n5. Performing refined matching with caliper...\n")
  
  # Refined matching with caliper
  ps_model_refined <- matchit(matching_formula, 
                              data = df_rct, 
                              method = matching_method, 
                              distance = matching_distance, 
                              caliper = matching_caliper)
  
  # Extract matched data
  df_rct <- match.data(ps_model_refined)
  
  cat("✓ Completed refined matching\n")
  cat("  - Caliper:", matching_caliper, "\n")
  cat("  - Matched observations:", nrow(df_rct), "\n")
  cat("  - Matched treatment units:", sum(df_rct$treatmentZRR), "\n")
  cat("  - Matched control units:", sum(!df_rct$treatmentZRR), "\n")
  
  # --------------------------------------------------------------------------
  # 6. DEFINE RESIDUAL BALANCE CALCULATION FUNCTION
  # --------------------------------------------------------------------------
  cat("\n6. Defining residual balance calculation function...\n")
  
  # Function to calculate mean and t-test of residuals
  calculate_residual_balance <- function(df, grouping_var, target_vars, conditional_on) {
    
    cat("  - Calculating residuals for", length(target_vars), "variables...\n")
    
    # Create a list to store residuals for each target variable
    residuals_list <- list()
    
    # Calculate residuals for each target variable
    for (var in target_vars) {
      formula <- as.formula(paste(var, "~", paste(setdiff(conditional_on, var), collapse = " + ")))
      model <- lm(formula, data = df)
      residuals <- resid(model)
      residuals_list[[var]] <- residuals
      df[[paste0(var, "_resid")]] <- residuals
    }
    
    # Calculate means of residuals by group
    means <- df %>%
      group_by({{ grouping_var }}) %>%
      summarise(across(ends_with("_resid"), mean, na.rm = TRUE), .groups = 'drop') %>%
      pivot_longer(-{{ grouping_var }}, names_to = "variable", values_to = "mean") %>%
      mutate(variable = str_remove(variable, "_resid")) %>%
      pivot_wider(names_from = {{ grouping_var }}, values_from = mean, names_prefix = "mean_")
    
    # Calculate t-tests on residuals
    t_tests <- map_dfr(target_vars, function(var) {
      test <- t.test(df[[paste0(var, "_resid")]] ~ df[[rlang::as_string(rlang::ensym(grouping_var))]])
      data.frame(
        variable = var,
        estimate = test$estimate[2] - test$estimate[1],
        p.value = test$p.value
      )
    })
    
    # Merge means and t-tests
    summary <- means %>%
      left_join(t_tests, by = "variable") %>%
      mutate(significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      ))
    
    return(summary)
  }
  
  cat("✓ Defined residual balance calculation function\n")
  
  # --------------------------------------------------------------------------
  # 7. DEFINE TARGET VARIABLES AND CONDITIONAL SET
  # --------------------------------------------------------------------------
  cat("\n7. Defining target variables and conditional set...\n")
  
  # Define conditional variables (department fixed effects + controls)
  conditional_on <- c("dep", controls)
  
  # Define target variables (exclude typologie from controls)
  target_vars <- setdiff(controls, "typologie")
  
  cat("✓ Defined variable sets\n")
  cat("  - Conditional variables:", length(conditional_on), "\n")
  cat("  - Target variables:", length(target_vars), "\n")
  
  # --------------------------------------------------------------------------
  # 8. CALCULATE RESIDUAL BALANCE SUMMARY
  # --------------------------------------------------------------------------
  cat("\n8. Calculating residual balance summary...\n")
  
  # Calculate balance summary using residuals
  balance_summary <- calculate_residual_balance(df_rct, treatmentZRR, target_vars, conditional_on)
  
  cat("✓ Calculated residual balance summary\n")
  cat("  - Variables analyzed:", nrow(balance_summary), "\n")
  cat("  - Significant differences (p<0.05):", sum(balance_summary$p.value < 0.05, na.rm = TRUE), "\n")
  
  # --------------------------------------------------------------------------
  # 9. FORMAT RESULTS TABLE
  # --------------------------------------------------------------------------
  cat("\n9. Formatting results table...\n")
  
  # Rename columns for better readability
  balance_summary <- balance_summary %>%
    select(variable, `Control (mean)` = mean_FALSE, `Treatment (mean)` = mean_TRUE, 
           `Difference` = estimate, `p-value` = p.value, `Significance` = significance)
  
  # Apply variable labels
  if (!exists("labels") || !is.vector(labels) || !all(balance_summary$variable %in% names(labels))) {
    warning("Labels not properly defined; using variable names")
    cat("  ⚠ Using variable names (labels not available)\n")
  } else {
    balance_summary$variable <- labels[balance_summary$variable]
    cat("✓ Applied variable labels\n")
  }
  
  # Round numerical values
  balance_summary <- balance_summary %>%
    mutate(
      `Control (mean)` = round(`Control (mean)`, 4),
      `Treatment (mean)` = round(`Treatment (mean)`, 4),
      `Difference` = round(`Difference`, 4)
    )
  
  cat("✓ Formatted results table\n")
  cat("  - Final table dimensions:", nrow(balance_summary), "x", ncol(balance_summary), "\n")
  
  # --------------------------------------------------------------------------
  # 10. GENERATE LATEX OUTPUT
  # --------------------------------------------------------------------------
  cat("\n10. Generating LaTeX output...\n")
  
  # Generate LaTeX table using stargazer
  tab <- balance_summary %>%
    dplyr::select(-Difference) %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
    as.data.frame()
  
  stargazer::stargazer(
    tab,
    type = "latex",
    summary = FALSE,
    title = "Comparison of Residual Means between Control and Treatment Groups with T-Test Results, after matching",
    notes = paste(
      "The table displays the means of the residuals of the regression of the variable on the department fixed effects",
      "along with the other set of controls. The right columns show the significance of the t-test to compare both groups",
      "among the border municipalities. The sample corresponds to the matched municipalities.",
      "Significance levels: *** p<0.001, ** p<0.01, * p<0.05."
    ),
    notes.align = "l",
    digits = 2,
    dep.var.labels.include = FALSE,
    rownames = FALSE,
    label = "tab:ttest-border",
    out = file.path(path_tables, "annex_border_pair_ttest.tex")
  )
  
  output_file <- file.path(path_tables, "annex_border_pair_ttest.tex")
  cat("✓ Generated LaTeX output\n")
  cat("  - Output file:", output_file, "\n")
  
  # --------------------------------------------------------------------------
  # 11. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n11. Results Summary:\n")
  
  # Summary statistics
  significant_vars <- balance_summary[!is.na(balance_summary$Significance) & 
                                        balance_summary$Significance != "", ]
  
  if (nrow(significant_vars) > 0) {
    cat("  - Significant residual differences found in", nrow(significant_vars), "variables:\n")
    for (i in 1:min(5, nrow(significant_vars))) {  # Show up to 5 variables
      var_info <- significant_vars[i, ]
      cat("    ", var_info$variable, " (", var_info$Significance, ")\n")
    }
    if (nrow(significant_vars) > 5) {
      cat("    ... and", nrow(significant_vars) - 5, "more\n")
    }
  } else {
    cat("  - No significant residual differences found between groups\n")
  }
  
  
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("RESIDUAL BALANCE ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
  # Return results
  return(balance_summary)
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_residual_balance_analysis(processed_data_path, path_tables, labels, controls)