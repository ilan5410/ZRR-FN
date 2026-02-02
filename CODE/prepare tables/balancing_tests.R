# ==============================================================================
# Balance Tests Analysis - Residual Means Comparison
# ==============================================================================
# This script performs balancing tests between control and treatment groups
# by comparing residual means after controlling for specified covariates
# Uses t-tests to assess statistical significance of differences
# ==============================================================================

# Required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(stargazer)
library(rlang)

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Perform balance tests using residual means comparison
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param conditional_vars Variables to condition on when calculating residuals
#' @param use_border_pairs Whether to use border_pair fixed effects (default: FALSE for computational efficiency)
#' @return Balance test results with means, differences, and p-values
generate_balance_tests <- function(processed_data_path, path_tables, 
                                   controls,
                                   conditional_vars = NULL,
                                   use_border_pairs = FALSE) {
  
  cat("===============================================\n")
  cat("BALANCE TESTS ANALYSIS PIPELINE\n")
  cat("===============================================\n")
  cat("Data file:", "borders_pair.RData", "\n")
  cat("Use border pair FE:", use_border_pairs, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD AND VALIDATE DATA
  # --------------------------------------------------------------------------
  cat("1. Loading and validating data...\n")
  
  # Check if processed data exists
  full_data_path <- file.path(processed_data_path, "borders_pair.RData")
  if (!file.exists(full_data_path)) {
    stop("Error: ", "borders_pair.RData", " does not exist in the specified path")
  }
  
  # Load the data environment
  load(full_data_path)
  cat("✓ Loaded borders_pair.RData", "\n")
  
  # Validate required objects exist
  required_objects <- c("dfZRRControls", "controls")
  missing_objects <- setdiff(required_objects, ls())
  if (length(missing_objects) > 0) {
    stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
  }
  
  # Check for labels object (optional)
  has_labels <- exists("labels")
  if (!has_labels) {
    warning("Labels object not found - will use variable names as labels")
  }
  
  cat("✓ Validated required data objects\n")
  cat("  - dfZRRControls:", nrow(dfZRRControls), "observations\n")
  cat("  - Control variables:", length(controls), "variables\n")
  cat("  - Variable labels available:", has_labels, "\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE DATA FOR BALANCE TESTING
  # --------------------------------------------------------------------------
  cat("\n2. Preparing data for balance testing...\n")
  
  # Prepare the dataset for balance tests
  df_rct <- dfZRRControls %>%
    select(-x) %>%  # Remove running variable
    rename(treatmentZRR = z) %>%
    mutate(
      pop = log(pop),                # Log-transform population
      popDensity = log(popDensity)   # Log-transform population density
    )
  
  cat("✓ Prepared dataset for balance testing\n")
  cat("  - Final observations:", nrow(df_rct), "\n")
  cat("  - Treatment group:", sum(df_rct$treatmentZRR, na.rm = TRUE), "communes\n")
  cat("  - Control group:", sum(!df_rct$treatmentZRR, na.rm = TRUE), "communes\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE TARGET AND CONDITIONING VARIABLES
  # --------------------------------------------------------------------------
  cat("\n3. Defining target and conditioning variables...\n")
  
  # Define target variables for balance testing (exclude treatment and grouping vars)
  excluded_from_targets <- c("treatmentZRR", "border_pair", "typologie")
  target_vars <- setdiff(controls, excluded_from_targets)
  
  # Define conditioning variables
  if (is.null(conditional_vars)) {
    if (use_border_pairs && "border_pair" %in% names(df_rct)) {
      conditional_vars <- c("border_pair", "pagri", "popDensity")
      cat("  ⚠️  Using border_pair fixed effects (computationally intensive)\n")
    } else {
      conditional_vars <- c("dep", "pagri", "popDensity")
      cat("  ✓ Using department fixed effects (more efficient)\n")
    }
  }
  
  cat("✓ Defined variable sets\n")
  cat("  - Target variables for testing:", length(target_vars), "variables\n")
  cat("  - Conditioning variables:", paste(conditional_vars, collapse = ", "), "\n")
  
  # Validate that conditioning variables exist in data
  missing_cond_vars <- setdiff(conditional_vars, names(df_rct))
  if (length(missing_cond_vars) > 0) {
    warning("Missing conditioning variables: ", paste(missing_cond_vars, collapse = ", "))
    conditional_vars <- intersect(conditional_vars, names(df_rct))
  }
  
  # Validate that target variables exist in data
  missing_target_vars <- setdiff(target_vars, names(df_rct))
  if (length(missing_target_vars) > 0) {
    warning("Missing target variables: ", paste(missing_target_vars, collapse = ", "))
    target_vars <- intersect(target_vars, names(df_rct))
  }
  
  cat("  - Final target variables:", length(target_vars), "\n")
  cat("  - Final conditioning variables:", length(conditional_vars), "\n")
  
  # --------------------------------------------------------------------------
  # 4. DEFINE RESIDUAL BALANCE CALCULATION FUNCTION
  # --------------------------------------------------------------------------
  cat("\n4. Defining residual balance calculation function...\n")
  
  # Function to calculate mean and t-test of residuals
  calculate_residual_balance <- function(df, grouping_var, target_vars, conditional_on) {
    cat("    - Calculating residuals for", length(target_vars), "variables:\n")
    
    # Create a list to store residuals for each target variable
    residuals_list <- list()
    
    # Calculate residuals for each target variable
    for (var in target_vars) {
      cat("         - Calculating residuals for", var, "\n")
      # Create formula excluding the target variable from conditioning set
      conditioning_vars_filtered <- setdiff(conditional_on, var)
      
      if (length(conditioning_vars_filtered) == 0) {
        # If no conditioning variables, use intercept-only model
        formula <- as.formula(paste(var, "~ 1"))
      } else {
        formula <- as.formula(paste(var, "~", paste(conditioning_vars_filtered, collapse = " + ")))
      }
      
      # Fit model and extract residuals
      tryCatch({
        model <- lm(formula, data = df)
        residuals <- resid(model)
        residuals_list[[var]] <- residuals
        df[[paste0(var, "_resid")]] <- residuals
      }, error = function(e) {
        warning("Failed to calculate residuals for ", var, ": ", e$message)
        df[[paste0(var, "_resid")]] <- NA
      })
    }
    
    # Calculate means of residuals by group
    cat("    - Calculating group means...\n")
    means <- df %>%
      group_by({{ grouping_var }}) %>%
      summarise(across(ends_with("_resid"), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(-{{ grouping_var }}, names_to = "variable", values_to = "mean") %>%
      mutate(variable = str_remove(variable, "_resid")) %>%
      pivot_wider(names_from = {{ grouping_var }}, values_from = mean, names_prefix = "mean_")
    
    # Calculate t-tests on residuals
    cat("    - Performing t-tests...\n")
    t_tests <- map_dfr(target_vars, function(var) {
      resid_var <- paste0(var, "_resid")
      grouping_var_string <- rlang::as_string(rlang::ensym(grouping_var))
      
      tryCatch({
        # Perform t-test
        test <- t.test(df[[resid_var]] ~ df[[grouping_var_string]])
        
        data.frame(
          variable = var,
          estimate = test$estimate[2] - test$estimate[1],  # Treatment - Control
          p.value = test$p.value,
          statistic = test$statistic,
          conf_low = test$conf.int[1],
          conf_high = test$conf.int[2]
        )
      }, error = function(e) {
        warning("Failed to perform t-test for ", var, ": ", e$message)
        data.frame(
          variable = var,
          estimate = NA,
          p.value = NA,
          statistic = NA,
          conf_low = NA,
          conf_high = NA
        )
      })
    })
    
    # Merge means and t-tests
    summary <- means %>%
      left_join(t_tests, by = "variable") %>%
      mutate(
        significance = case_when(
          is.na(p.value) ~ "",
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
    
    return(summary)
  }
  
  cat("✓ Defined residual balance calculation function\n")
  
  # --------------------------------------------------------------------------
  # 5. CALCULATE BALANCE TESTS
  # --------------------------------------------------------------------------
  cat("\n5. Calculating balance tests...\n")
  
  # Perform the balance analysis
  balance_summary <- calculate_residual_balance(
    df = df_rct, 
    grouping_var = treatmentZRR, 
    target_vars = target_vars, 
    conditional_on = conditional_vars
  )
  
  cat("✓ Completed balance calculations\n")
  cat("  - Variables tested:", nrow(balance_summary), "\n")
  cat("  - Significant differences (p<0.05):", sum(balance_summary$p.value < 0.05, na.rm = TRUE), "\n")
  
  # --------------------------------------------------------------------------
  # 6. FORMAT RESULTS TABLE
  # --------------------------------------------------------------------------
  cat("\n6. Formatting results table...\n")
  
  # Rename and reorder columns for readability
  balance_summary_formatted <- balance_summary %>%
    select(
      variable, 
      `Control (mean)` = mean_FALSE, 
      `Treatment (mean)` = mean_TRUE, 
      `p-value` = p.value
    ) 
  
  balance_summary_formatted <- balance_summary_formatted %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
  
  
  # Apply variable labels if available
  if (has_labels) {
    balance_summary_formatted$variable <- ifelse(
      balance_summary_formatted$variable %in% names(labels),
      labels[balance_summary_formatted$variable],
      balance_summary_formatted$variable
    )
    cat("✓ Applied variable labels\n")
  }
  
  # Round numerical values
  # balance_summary_formatted <- balance_summary_formatted %>%
  #   mutate(
  #     `Control (mean)` = round(`Control (mean)`, 4),
  #     `Treatment (mean)` = round(`Treatment (mean)`, 4),
  #     `Difference` = round(`Difference`, 4),
  #     `p-value` = round(`p-value`, 4)
  #   )
  
  cat("✓ Formatted results table\n")
  
  # --------------------------------------------------------------------------
  # 7. GENERATE OUTPUT TABLE
  # --------------------------------------------------------------------------
  cat("\n7. Generating output table...\n")
  
  # Prepare output file
  output_file <- file.path(path_tables, "balancing_tests.tex")
  
  # Create conditioning description for notes
  conditioning_description <- if (use_border_pairs && "border_pair" %in% conditional_vars) {
    "border-pair fixed effects"
  } else {
    "department fixed effects"
  }
  
  # Format numbers with scientific notation for small values
  format_number <- function(x) {
    if (is.na(x)) return("NA")
    if (abs(x) < 0.001 && x != 0) {
      # Use scientific notation for very small numbers
      formatted <- formatC(x, format = "e", digits = 0)
      # Clean up the scientific notation
      formatted <- gsub("e-0", "e-", formatted)
      formatted <- gsub("e\\+00", "", formatted)
      return(formatted)
    } else {
      # Use fixed notation with 4 decimal places
      return(formatC(x, format = "f", digits = 4))
    }
  }
  
  # Format the output dataframe
  df_out <- balance_summary_formatted %>%
    mutate(
      `Control (mean)` = sapply(`Control (mean)`, format_number),
      `Treatment (mean)` = sapply(`Treatment (mean)`, format_number),
      `p-value` = formatC(`p-value`, format = "f", digits = 4)
    )
  
  # Rename columns to match desired output
  names(df_out) <- c("Variable", "Control", "Treatment", "p-value")
  
  # Create the LaTeX table manually for better control
  latex_lines <- c(
    "\\begin{table}[!htbp]",
    "\\footnotesize",
    "\\centering",
    "  \\caption{Balancing tests for border pairs}",
    "  \\label{tab:ttest-border}",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{@{\\extracolsep{5pt}} p{5.5cm}ccc}",
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    "Variable & Control & Treatment & p-value \\\\",
    "\\hline \\\\[-1.8ex]"
  )
  
  # Add data rows
  for (i in 1:nrow(df_out)) {
    row <- paste(df_out[i, 1], "&", df_out[i, 2], "&", df_out[i, 3], "&", df_out[i, 4], "\\\\")
    latex_lines <- c(latex_lines, row)
  }
  
  # Add closing lines
  latex_lines <- c(
    latex_lines,
    "\\hline \\\\[-1.8ex]",
    "\\end{tabular}",
    "}% end resizebox",
    "\\vspace{0.5em}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    paste0("\\textit{Notes:} The table displays the means of the residuals of the regression of the variable on the ",
           conditioning_description,
           " along with the other set of controls. The right column shows the significance level of the t-test comparing both groups among the border municipalities."),
    "\\end{minipage}",
    "\\end{table}"
  )
  
  # Write to file
  writeLines(latex_lines, output_file)
  
  cat("✓ Generated LaTeX table\n")
  cat("  - Output file:", output_file, "\n")  
  # --------------------------------------------------------------------------
  # 8. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n8. Results Summary:\n")
  
  # Calculate summary statistics
  n_significant_001 <- sum(balance_summary_formatted$`p-value` < 0.001, na.rm = TRUE)
  n_significant_01 <- sum(balance_summary_formatted$`p-value` < 0.01, na.rm = TRUE)
  n_significant_05 <- sum(balance_summary_formatted$`p-value` < 0.05, na.rm = TRUE)
  
  cat("  - Total variables tested:", nrow(balance_summary_formatted), "\n")
  cat("  - Significant at p<0.001:", n_significant_001, "\n")
  cat("  - Significant at p<0.01:", n_significant_01, "\n")
  cat("  - Significant at p<0.05:", n_significant_05, "\n")
  cat("  - Conditioning approach:", conditioning_description, "\n")
  
  # Show variables with largest imbalances
  if (nrow(balance_summary_formatted) > 0) {
    top_imbalances <- balance_summary_formatted %>%
      filter(!is.na(`p-value`)) %>%
      arrange(`p-value`) %>%
      head(3)
    
    cat("  - Top 3 imbalanced variables:\n")
    for (i in 1:min(3, nrow(top_imbalances))) {
      cat("    *", top_imbalances$variable[i], ": p =", 
          round(top_imbalances$`p-value`[i], 4), "\n")
    }
  }
  
  
  cat("✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("BALANCE TESTS ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_balance_tests(processed_data_path, path_tables, controls)
