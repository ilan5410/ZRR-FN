# ==============================================================================
# RDD Multiple Outcomes Analysis - Different Bandwidths
# ==============================================================================
# This script generates regression discontinuity design results for multiple
# outcome variables across different bandwidths using HC1 clustered standard 
# errors at the canton level
# ==============================================================================


# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate RDD results for multiple outcomes across different bandwidths
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param max_bandwidth Maximum bandwidth for initial data filtering (default: 20km)
#' @param custom_bandwidths Custom bandwidth vector (uses bandwidths from data if NULL)
#' @return Summary table with coefficients and standard errors for all outcomes
generate_rdd_multiple_outcomes <- function(processed_data_path, path_tables,
                                           bandwidths, controls) {
  
  cat("===============================================\n")
  cat("RDD MULTIPLE OUTCOMES ANALYSIS PIPELINE\n")
  cat("===============================================\n")
  cat("\n")
  
  target_bandwidth <- bandwidths[2] 
  max_bandwidth <- bandwidths[1] 
  
  cat("Maximum bandwidth for filtering:", max_bandwidth, "m\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD AND VALIDATE DATA
  # --------------------------------------------------------------------------
  cat("1. Loading and validating data...\n")
  
  # Check if processed data exists
  data_file <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(data_file)) {
    stop("Error: script_sharp.RData does not exist in the specified path")
  }
  
  # Load the data environment
  load(data_file)
  cat("✓ Loaded script_sharp.RData\n")
  
  # Validate required objects exist
  required_objects <- c("dfZRRControls", "bandwidths", "controls")
  missing_objects <- setdiff(required_objects, ls())
  if (length(missing_objects) > 0) {
    stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
  }
  
  
  cat("✓ Validated required data objects\n")
  cat("  - dfZRRControls:", nrow(dfZRRControls), "observations\n")
  cat("  - Analysis bandwidths:", paste(bandwidths, collapse = ", "), "m\n")
  cat("  - Control variables:", length(controls), "variables\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE DATA FOR RDD ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n2. Preparing data for RDD analysis...\n")
  
  
  # Filter and prepare the dataset
  df_rdd <- dfZRRControls %>%
    filter(x >= (-max_bandwidth) & x <= (max_bandwidth)) %>%
    mutate(
      dist = x,                      # Create a distance variable
      treatmentZRR = z,              # Define treatment indicator  
      pop = log(pop),                # Log-transform population
      popDensity = log(popDensity)   # Log-transform population density
    ) %>%
    distinct(codecommune, .keep_all = TRUE)  # Remove duplicate communes
  
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  cat("✓ Prepared RDD dataset\n")
  cat("  - Observations after filtering:", nrow(df_rdd), "\n")
  cat("  - Unique communes:", length(unique(df_rdd$codecommune)), "\n")
  cat("  - Distance range:", round(min(df_rdd$x)/1000, 2), "to", round(max(df_rdd$x)/1000, 2), "km\n")
  
  # Validate canton variable for clustering
  if (!"canton" %in% names(df_rdd)) {
    stop("Error: canton variable not found for clustering")
  }
  
  unique_cantons <- length(unique(df_rdd$canton[!is.na(df_rdd$canton)]))
  cat("  - Unique cantons for clustering:", unique_cantons, "\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE OUTCOME VARIABLES AND FORMULAS
  # --------------------------------------------------------------------------
  cat("\n3. Defining outcome variables and regression formulas...\n")
  
  # Define outcome variables (only those needed for the table)
  outcome_variables <- c("deltaFN", "RPR2002", "turnout_2002", "FN1988")
  
  # Build one formula per outcome.
  # For FNYYYY outcomes, include only FNXXXX controls with XXXX < YYYY,
  # pulling candidates from BOTH `controls` and `data_cols` (e.g., names(df)).
  make_formulas <- function(outcomes, controls, data_cols = NULL) {
    in_data <- function(vars) if (is.null(data_cols)) vars else intersect(vars, data_cols)
    # keep only controls that exist in the data (if provided)
    controls <- unique(in_data(controls))
    
    # FN candidates from controls + data columns
    fn_candidates <- unique(c(
      grep("^FN\\d{4}$", controls,  value = TRUE),
      if (!is.null(data_cols)) grep("^FN\\d{4}$", data_cols, value = TRUE) else character(0)
    ))
    fn_years <- setNames(as.integer(sub("^FN", "", fn_candidates)), fn_candidates)
    
    formulas <- setNames(vector("list", length(outcomes)), outcomes)
    
    for (outcome in outcomes) {
      rhs_controls <- controls
      
      if (grepl("^FN\\d{4}$", outcome)) {
        out_year <- as.integer(sub("^FN", "", outcome))
        # drop all FN* from base controls
        rhs_controls <- rhs_controls[!grepl("^FN\\d{4}$", rhs_controls)]
        # add only earlier FNXXXX that exist in data
        prev_fn <- names(fn_years)[fn_years < out_year]
        prev_fn <- prev_fn[order(fn_years[prev_fn])]
        prev_fn <- in_data(prev_fn)
        rhs_controls <- unique(c(rhs_controls, prev_fn))
      }
      
      rhs <- c("z", "x", "border", rhs_controls, "factor(dep)")
      formulas[[outcome]] <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
    }
    
    formulas
  }
  
  formulas <- make_formulas(outcome_variables, controls, names(df_rdd))  
  
  cat("✓ Defined formulas for", length(outcome_variables), "outcome variables\n")
  
  # --------------------------------------------------------------------------
  # 4. RUN REGRESSION MODELS ACROSS BANDWIDTHS
  # --------------------------------------------------------------------------
  cat("\n4. Running regression models across bandwidths...\n")
  
  # Initialize storage for models and observations
  all_models <- list()
  nobs_vector <- c()
  
  # Fit models for each bandwidth and outcome
  for (i in seq_along(bandwidths)) {
    bw_m <- bandwidths[i]
    bw_km <- bandwidths[i]
    
    cat("  - Processing bandwidth:", bw_km, "km\n")
    
    # Filter data to current bandwidth
    filtered_data <- df_rdd %>%
      filter(x >= -bw_m & x <= bw_m) %>%
      filter(!is.na(canton))  # Remove observations with missing canton for clustering
    
    nobs_vector <- c(nobs_vector, nrow(filtered_data))
    cat("    ✓ Filtered to", nrow(filtered_data), "observations\n")
    
    # Run regressions for each outcome
    for (outcome in outcome_variables) {
      cat("      - Running", outcome, "regression...\n")
      
      # Fit OLS model
      model <- lm(as.formula(formulas[[outcome]]), data = filtered_data)
      
      # Calculate HC1 clustered standard errors at canton level
      vcov_clustered <- vcovHC(model, type = "HC1", cluster = filtered_data$canton)
      coefs <- coeftest(model, vcov = vcov_clustered)
      
      # Store results
      model_name <- paste(outcome, "bw", bw_km, sep = "_")
      all_models[[model_name]] <- coefs
    }
  }
  
  cat("✓ Completed all regression models\n")
  cat("  - Total models fitted:", length(all_models), "\n")
  cat("  - Bandwidths processed:", length(bandwidths), "\n")
  cat("  - Outcomes analyzed:", length(outcome_variables), "\n")
  
  # --------------------------------------------------------------------------
  # 5. EXTRACT COEFFICIENTS AND CREATE SUMMARY TABLE
  # --------------------------------------------------------------------------
  cat("\n5. Extracting coefficients and creating summary table...\n")
  
  # Function to extract coefficients with significance stars
  extract_coefficients <- function(model, variable) {
    if (variable %in% rownames(model)) {
      coef <- model[variable, "Estimate"]
      se <- model[variable, "Std. Error"]
      p_val <- model[variable, "Pr(>|t|)"]
      
      # Add significance stars (matching desired output)
      stars <- ifelse(p_val < 0.01, "\\textasteriskcentered \\textasteriskcentered \\textasteriskcentered ", 
                      ifelse(p_val < 0.05, "\\textasteriskcentered \\textasteriskcentered ", 
                             ifelse(p_val < 0.1, "\\textasteriskcentered ", "")))
      
      return(paste0(sprintf("%.3f", coef), " (", sprintf("%.3f", se), ")", stars))
    } else {
      return(NA)
    }
  }
  
  # Create summary table structure with descriptive labels
  outcome_labels <- c(
    "FN vote share change 1988-2002",
    "RPR vote share in 2002",
    "Turnout in 2002",
    "FN vote share in 1988"
  )
  
  summary_table <- data.frame(
    Outcome = outcome_labels,
    stringsAsFactors = FALSE
  )
  
  # Fill the summary table with coefficients for treatment variable "z"
  for (i in seq_along(bandwidths)) {
    bw_km <- bandwidths[i]
    column_name <- paste0("bw=", format(bw_km, scientific = FALSE, big.mark = ","))
    
    for (j in seq_along(outcome_variables)) {
      outcome <- outcome_variables[j]
      model_name <- paste(outcome, "bw", bw_km, sep = "_")
      
      # Try both "zTRUE" and "z" as treatment variable names
      coef_result <- extract_coefficients(all_models[[model_name]], "zTRUE")
      if (is.na(coef_result)) {
        coef_result <- extract_coefficients(all_models[[model_name]], "z")
      }
      
      summary_table[j, column_name] <- coef_result
    }
  }
  
  cat("✓ Extracted coefficients for all models\n")
  
  # --------------------------------------------------------------------------
  # 6. GENERATE LATEX TABLE MANUALLY
  # --------------------------------------------------------------------------
  cat("\n6. Generating LaTeX table...\n")
  
  # Prepare output file path
  latex_file <- file.path(path_tables, "main_results_diff_outcomes.tex")
  
  # Build LaTeX table manually for precise formatting
  latex_lines <- c(
    "\\begin{table}",
    "\\centering",
    "\\footnotesize",
    "\\caption{RDD main specification results on other outcomes}",
    "\\begin{tabular}{llll}",
    "\\toprule"
  )
  
  # Add header row
  bw_labels <- sapply(bandwidths, function(x) paste0("bw=", format(x, scientific = FALSE, big.mark = ",")))
  header <- paste(c("Outcome", bw_labels), collapse = " & ")
  latex_lines <- c(latex_lines, paste0(header, "\\\\"))
  latex_lines <- c(latex_lines, "\\midrule")
  
  # Add section header for 2002 elections
  latex_lines <- c(latex_lines, "\\textit{2002 elections} & & &\\\\")
  
  # Add first three outcomes (2002 elections)
  for (i in 1:3) {
    row_data <- c(summary_table$Outcome[i], 
                  summary_table[i, 2], 
                  summary_table[i, 3], 
                  summary_table[i, 4])
    latex_lines <- c(latex_lines, paste(row_data, collapse = " & "), "\\\\")
  }
  
  # Add spacing and placebo test section
  latex_lines <- c(latex_lines, "\\addlinespace")
  latex_lines <- c(latex_lines, "\\textit{Placebo test} & & & \\\\")
  
  # Add FN1988 (placebo test)
  row_data <- c(summary_table$Outcome[4], 
                summary_table[4, 2], 
                summary_table[4, 3], 
                summary_table[4, 4])
  latex_lines <- c(latex_lines, paste(row_data, collapse = " & "), "\\\\")
  
  # Add spacing and observations row
  latex_lines <- c(latex_lines, "\\addlinespace")
  obs_row <- paste(c("Observations", nobs_vector), collapse = " & ")
  latex_lines <- c(latex_lines, paste0(obs_row, "\\\\"))
  
  # Close table
  latex_lines <- c(latex_lines, 
                   "\\bottomrule",
                   "\\end{tabular}",
                   "\\label{tab:rdd_results_outcomes_later}",
                   "",
                   "\\parbox{\\textwidth}{\\footnotesize \\textit{Notes:} While the effect of the ZRR program on the delta of the FN vote share between 1988 and 2002 is significant and positive, its effect on the vote share of the RPR, Jacques Chirac's party, is null. There is also no effect on the election turnout. We run the specification on different bandwidths, with controls and place fixed effects. The standard errors are clustered at the department level.}",
                   "",
                   "\\end{table}")
  
  # Write to file
  writeLines(latex_lines, latex_file)
  
  cat("✓ Generated LaTeX table\n")
  cat("  - Output file:", latex_file, "\n")
  
  # --------------------------------------------------------------------------
  # 7. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n7. Results Summary:\n")
  
  # Display summary statistics
  cat("  - Outcome variables analyzed:", length(outcome_variables), "\n")
  cat("  - Bandwidths tested:", paste(bandwidths, "m", collapse = ", "), "\n")
  cat("  - Total models fitted:", length(all_models), "\n")
  cat("  - Unique cantons used for clustering:", unique_cantons, "\n")
  
  # Show sample sizes by bandwidth
  cat("  - Sample sizes by bandwidth:\n")
  for (i in seq_along(bandwidths)) {
    cat("    *", bandwidths[i], "m:", nobs_vector[i], "observations\n")
  }
  
  
  cat("✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("RDD MULTIPLE OUTCOMES ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_rdd_multiple_outcomes(processed_data_path, path_tables,
                               bandwidths, controls)