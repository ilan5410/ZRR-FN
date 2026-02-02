# ==============================================================================
# Border Municipalities Regression Analysis - Simple
# ==============================================================================
# This script performs regression analysis on border municipalities to assess
# the effect of ZRR treatment on FN vote share with various specifications
# including robustness checks and placebo tests
# ==============================================================================


# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate border municipalities regression results
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @return List of fitted regression models
generate_border_regression_analysis <- function(processed_data_path, path_tables) {
  
  cat("===============================================\n")
  cat("BORDER MUNICIPALITIES REGRESSION ANALYSIS\n")
  cat("===============================================\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
  data_file <- file.path(processed_data_path, "borders_pair.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist")
  }
  
  load(data_file)
  cat("✓ Loaded borders_pair.RData\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing data...\n")
  
  df_rct <- dfZRRControls %>%
    select(-x) %>%
    rename(treatmentZRR = z) %>%
    mutate(
      pop = log(pop),
      popDensity = log(popDensity)
    )
  
  n_border_pairs <- length(unique(df_rct$border_pair))
  cat("✓ Prepared dataset\n")
  cat("  - Observations:", nrow(df_rct), "\n")
  cat("  - Border pairs:", n_border_pairs, "\n")
  
  # --------------------------------------------------------------------------
  # 3. FIT REGRESSION MODELS
  # --------------------------------------------------------------------------
  cat("\n3. Fitting regression models...\n")
  
  # Model 1: Treatment only
  cat("  - Model 1: Treatment only\n")
  model1 <- lm(y ~ treatmentZRR, data = df_rct)
  
  # Model 2: Treatment + controls
  cat("  - Model 2: Treatment + controls\n")
  formula2 <- as.formula(paste("y ~ treatmentZRR +", paste(controls, collapse = " + ")))
  model2 <- lm(formula2, data = df_rct)
  
  # Model 3: Treatment + controls + same department (filtered data)
  cat("  - Model 3: Treatment + controls + dept FE (same dept only)\n")
  formula3 <- as.formula(paste("y ~ treatmentZRR +", paste(controls, collapse = " + "), "+ same_department"))
  model3 <- lm(formula3, data = df_rct %>% filter(same_department == 1))
  
  # Model 4: Treatment + controls + border pair FE
  cat("  - Model 4: Treatment + controls + border pair FE\n")
  formula4 <- as.formula(paste("y ~ treatmentZRR +", paste(controls, collapse = " + "), " + factor(border_pair)"))
  model4 <- lm(formula4, data = df_rct)
  
  # Model 5: Treatment + controls + same department + border pair FE
  cat("  - Model 5: Treatment + controls + same dept + border pair FE\n")
  formula5 <- as.formula(paste("y ~ treatmentZRR +", paste(controls, collapse = " + "), "+ same_department", " + factor(border_pair)"))
  model5 <- lm(formula5, data = df_rct)
  
  # Model 6: First-difference model
  cat("  - Model 6: First-difference specification\n")
  df_rct_panel <- pdata.frame(df_rct, index = c("border_pair", "year"))
  formula_fd <- as.formula(paste("y ~ treatmentZRR +", paste(controls, collapse = " + ")))
  model6 <- plm(formula_fd, data = df_rct_panel, model = "fd")
  
  # Model 7: Placebo test
  cat("  - Model 7: Placebo test (1988 outcome)\n")
  formula7 <- as.formula(paste("FN1988 ~ treatmentZRR +", paste(controls[!controls %in% c("FN1988")], collapse = " + "), "+ factor(dep)", " + factor(border_pair)"))
  model7 <- lm(formula7, data = df_rct)
  
  cat("✓ Fitted all models\n")
  
  # --------------------------------------------------------------------------
  # 4. GENERATE REGRESSION TABLE
  # --------------------------------------------------------------------------
  cat("\n4. Generating regression table...\n")
  
  output_file <- file.path(path_tables, "border_muni_results.tex")

  keep_vars <- c("treatmentZRRTRUE")

  # Build notes with parbox for proper text wrapping
  notes_text <- paste0(
    "\\parbox{0.95\\linewidth}{\\footnotesize ",
    "The table presents the regression results of the effect of the ZRR program on the vote share for the FN in 2002, ",
    "using a sample of ", n_border_pairs, " pairs of border municipalities. The placebo column uses FN vote share in 1988 as the outcome. ",
    "First-difference specification in column (6) captures change between 1988 and 2002. ",
    "Control variables and fixed effects coefficients not shown for brevity.}"
  )

  stargazer(
    model1, model2, model3, model4, model5, model6, model7,
    type = "latex",
    title = "Regression Results: Effect of ZRR on FN Vote Share in 2002 and Robustness Checks",
    star.cutoffs = c(0.05, 0.01, 0.001),
    label = "tab:border-results",
    keep = keep_vars,
    dep.var.labels.include = FALSE,
    column.labels = c("No Controls", "Controls", "Controls + Dept FE", "Controls + Pair FE", "Dept + Pair FE", "First Diff", "Placebo (1988)"),
    add.lines = list(
      c("Controls", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
      c("Department Fixed Effects", "No", "No", "Yes", "No", "Yes", "/", "Yes"),
      c("Border Pair Fixed Effects", "No", "No", "No", "Yes", "Yes", "/", "Yes")
    ),
    digits = 4,
    font.size = "footnotesize",
    notes = notes_text,
    notes.append = FALSE,
    notes.align = "l",
    out = output_file
  )

  cat("✓ Generated regression table\n")
  cat("  - Output file:", output_file, "\n")

  # Apply formatting fixes for width management - use landscape mode for 7-column table
  format_latex_table(
    tex_file = output_file,
    use_resizebox = TRUE,
    font_size = "footnotesize",
    use_landscape = TRUE,
    notes_width = 0.95
  )
  
  # --------------------------------------------------------------------------
  # 5. SUMMARY RESULTS
  # --------------------------------------------------------------------------
  cat("\n5. Summary of results:\n")
  
  # Store models in list
  models <- list(
    model1 = model1,
    model2 = model2,
    model3 = model3,
    model4 = model4,
    model5 = model5,
    model6 = model6,
    model7 = model7
  )
  
  # Display treatment coefficients
  for (i in 1:7) {
    model_name <- paste0("model", i)
    if (i == 6) {
      # PLM model
      coef_val <- coef(models[[model_name]])["treatmentZRRTRUE"]
    } else {
      # Regular lm models
      coef_val <- coef(models[[model_name]])["treatmentZRRTRUE"]
    }
    cat("  -", model_name, "treatment coefficient:", round(coef_val, 4), "\n")
  }
  
  cat("\n===============================================\n")
  cat("BORDER MUNICIPALITIES ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

# Run the analysis
generate_border_regression_analysis(processed_data_path, path_tables)