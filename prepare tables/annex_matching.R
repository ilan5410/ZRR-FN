# ==============================================================================
# Border Municipalities Regression Analysis with Propensity Score Matching
# ==============================================================================
# This script performs regression analysis on border municipalities using
# propensity score matching to estimate treatment effects with various
# specifications including fixed effects controls
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate border municipalities regression analysis with matching
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param controls Vector of control variable names
#' @param outcome_var Name of outcome variable (default: "y")
#' @param treatment_var Name of treatment variable (default: "treatmentZRR")
#' @param matching_method Method for propensity score matching (default: "nearest")
#' @param matching_caliper Caliper for matching (default: 0.001)
#' @param dep_var_label Label for dependent variable in output
#' @return List of regression models and summary statistics
generate_border_regression_analysis <- function(processed_data_path, path_tables, controls,
                                                outcome_var = "y",
                                                treatment_var = "treatmentZRR",
                                                matching_method = "nearest",
                                                matching_caliper = 0.001,
                                                dep_var_label = "The vote share for FN in 2002") {
  
  cat("===============================================\n")
  cat("BORDER MUNICIPALITIES REGRESSION ANALYSIS\n")
  cat("===============================================\n")
  cat("Outcome variable:", outcome_var, "\n")
  cat("Treatment variable:", treatment_var, "\n")
  cat("Matching method:", matching_method, "\n")
  cat("Matching caliper:", matching_caliper, "\n")
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
    rename(!!treatment_var := z)
  
  cat("✓ Prepared data for matching\n")
  cat("  - Initial observations:", nrow(df_rct), "\n")
  cat("  - Treatment group size:", sum(df_rct[[treatment_var]]), "\n")
  cat("  - Control group size:", sum(!df_rct[[treatment_var]]), "\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE MATCHING FORMULA AND VARIABLES
  # --------------------------------------------------------------------------
  cat("\n3. Defining matching formula and variables...\n")
  
  # Define matching variables
  matching_vars <- c("pchom", "FN1988", "delta_pop_1980_1995", "pop", "ratEmp", 
                     "ratForeigners", "asso", "educNoDiplomaPerK", "educSUPPerK", 
                     "educBACPerK", "educCAPBEPPerK", "poph", "popf", "pagri", 
                     "pindp", "ppint", "pempl", "pouvr", "altitude", "superficie", 
                     "min_distance_to_agglo", "logVac") #, "typologie")
  
  # Create matching formula
  matching_formula <- as.formula(paste(treatment_var, "~", paste(matching_vars, collapse = " + ")))
  
  cat("✓ Defined matching specifications\n")
  cat("  - Matching variables:", length(matching_vars), "\n")
  cat("  - Formula created with", length(matching_vars), "predictors\n")
  
  # --------------------------------------------------------------------------
  # 4. PERFORM INITIAL PROPENSITY SCORE MATCHING
  # --------------------------------------------------------------------------
  cat("\n4. Performing initial propensity score matching...\n")
  
  # Initial matching without caliper
  ps_model <- matchit(matching_formula, 
                      data = df_rct, 
                      method = matching_method, 
                      distance = "logit")
  
  cat("✓ Completed initial matching\n")
  cat("  - Method:", matching_method, "\n")
  cat("  - Distance: logit\n")
  
  # --------------------------------------------------------------------------
  # 5. PERFORM REFINED MATCHING WITH CALIPER
  # --------------------------------------------------------------------------
  cat("\n5. Performing refined matching with caliper...\n")
  
  # Refined matching with caliper
  ps_model_refined <- matchit(matching_formula, 
                              data = df_rct, 
                              method = matching_method,
                              caliper = matching_caliper)
  
  # Extract matched data
  df_rct <- match.data(ps_model_refined)
  
  cat("✓ Completed refined matching\n")
  cat("  - Caliper:", matching_caliper, "\n")
  cat("  - Final matched observations:", nrow(df_rct), "\n")
  cat("  - Matched treatment units:", sum(df_rct[[treatment_var]]), "\n")
  cat("  - Matched control units:", sum(!df_rct[[treatment_var]]), "\n")
  
  # --------------------------------------------------------------------------
  # 6. VALIDATE DATA FOR REGRESSION ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n6. Validating data for regression analysis...\n")
  
  # Check outcome variable
  if (!outcome_var %in% names(df_rct)) {
    stop("Outcome variable '", outcome_var, "' not found in matched data")
  }
  
  # Check for missing values in key variables
  missing_outcome <- sum(is.na(df_rct[[outcome_var]]))
  missing_treatment <- sum(is.na(df_rct[[treatment_var]]))
  
  cat("✓ Validated data for regression analysis\n")
  cat("  - Outcome variable '", outcome_var, "' available\n")
  cat("  - Missing outcome values:", missing_outcome, "\n")
  cat("  - Missing treatment values:", missing_treatment, "\n")
  
  # --------------------------------------------------------------------------
  # 7. ESTIMATE REGRESSION MODELS
  # --------------------------------------------------------------------------
  cat("\n7. Estimating regression models...\n")
  
  # Model 1: Simple bivariate regression
  cat("  - Estimating Model 1 (bivariate)...\n")
  model1_formula <- as.formula(paste(outcome_var, "~", treatment_var))
  model1 <- lm(model1_formula, data = df_rct)
  
  # Model 2: With controls
  cat("  - Estimating Model 2 (with controls)...\n")
  model2_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", paste(controls, collapse = " + ")))
  model2 <- lm(model2_formula, data = df_rct)
  
  # Model 3: With controls and department fixed effects
  cat("  - Estimating Model 3 (with department FE)...\n")
  model3_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", 
                                     paste(controls, collapse = " + "), "+ factor(dep)"))
  model3 <- lm(model3_formula, data = df_rct)
  
  # Model 4: With controls, department FE, and border pair FE
  cat("  - Estimating Model 4 (with border pair FE)...\n")
  model4_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", 
                                     paste(controls, collapse = " + "), 
                                     "+ factor(border_pair) + factor(dep)"))
  model4 <- lm(model4_formula, data = df_rct)
  
  cat("✓ Estimated all regression models\n")

  # --------------------------------------------------------------------------
  # 8. EXTRACT TREATMENT EFFECTS
  # --------------------------------------------------------------------------
  cat("\n8. Extracting treatment effects...\n")
  
  # Extract treatment coefficients and standard errors
  models <- list(model1, model2, model3, model4)
  treatment_var <- "treatmentZRRTRUE"
  treatment_effects <- sapply(models, function(model) {
    coef(summary(model))[treatment_var, "Estimate"]
  })
  
  treatment_se <- sapply(models, function(model) {
    coef(summary(model))[treatment_var, "Std. Error"]
  })
  
  treatment_pvalues <- sapply(models, function(model) {
    coef(summary(model))[treatment_var, "Pr(>|t|)"]
  })
  
  cat("✓ Extracted treatment effects\n")
  cat("  - Model 1 effect:", round(treatment_effects[1], 4), 
      " (SE:", round(treatment_se[1], 4), ")\n")
  cat("  - Model 2 effect:", round(treatment_effects[2], 4), 
      " (SE:", round(treatment_se[2], 4), ")\n")
  cat("  - Model 3 effect:", round(treatment_effects[3], 4), 
      " (SE:", round(treatment_se[3], 4), ")\n")
  cat("  - Model 4 effect:", round(treatment_effects[4], 4), 
      " (SE:", round(treatment_se[4], 4), ")\n")
  
  # --------------------------------------------------------------------------
  # 9. GENERATE LATEX OUTPUT
  # --------------------------------------------------------------------------
  cat("\n9. Generating LaTeX output...\n")
  
  # Create model list for stargazer
  model_list <- list(model1, model2, model3, model4)
  names(model_list) <- c("(1)", "(2)", "(3)", "(4)")
  
  # Generate stargazer output
  stargazer_output <- stargazer(
    model1, model2, model3, model4,
    type = "latex",
    title = "Border Municipalities: Regression Results (Matching)",
    star.cutoffs = c(0.05, 0.01, 0.001), 
    label = "tab:border-results",
    omit = c("factor\\(dep\\)", "factor\\(border_pair\\)", controls),
    dep.var.labels = dep_var_label,
    add.lines = list(
      c("Controls", "No", "Yes", "Yes", "Yes"),
      c("Department Fixed Effects", "No", "No", "Yes", "Yes"),
      c("Border Pair Fixed Effects", "No", "No", "No", "Yes")
    ),
    digits = 3,
    notes = paste("The table presents the regression results of the effect of the ZRR program on", 
                  tolower(dep_var_label), ", using a sample of matched border municipalities.",
                  "Specification (1) shows the simplest model with no controls.",
                  "Specification (2) includes control variables.",
                  "Specification (3) adds department fixed effects to the model.",
                  "Specification (4) includes border pair fixed effects.",
                  paste("Matching performed using", matching_method, "method with caliper =", matching_caliper, ".")),
    out = file.path(path_tables, "annex_matching.tex")
  )
  
  output_file <- file.path(path_tables, "annex_matching.tex")
  cat("✓ Generated LaTeX output\n")
  cat("  - Output file:", output_file, "\n")
  
  
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("BORDER REGRESSION ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_border_regression_analysis(processed_data_path, path_tables, controls)

