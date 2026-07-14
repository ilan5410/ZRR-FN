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
  
  # Model 3: Treatment + controls + department fixed effects (same-department pairs)
  cat("  - Model 3: Treatment + controls + dept FE (same dept only)\n")
  formula3 <- as.formula(paste("y ~ treatmentZRR +", paste(controls, collapse = " + "), "+ factor(dep)"))
  model3 <- lm(formula3, data = df_rct %>% filter(same_department == 1))

  # Model 4: Treatment + controls + border-pair fixed effects. fixest avoids
  # materializing thousands of dummy columns during the full rebuild.
  cat("  - Model 4: Treatment + controls + border pair FE\n")
  formula4 <- as.formula(paste("y ~ treatmentZRR +", paste(controls, collapse = " + "), "| border_pair"))
  model4 <- fixest::feols(formula4, data = df_rct, cluster = ~canton)

  # Model 5: Placebo test with border-pair fixed effects.
  cat("  - Model 5: Placebo test (1988 outcome)\n")
  formula5 <- as.formula(paste("FN1988 ~ treatmentZRR +", paste(controls[!controls %in% "FN1988"], collapse = " + "), "| border_pair"))
  model5 <- fixest::feols(formula5, data = df_rct, cluster = ~canton)
  
  cat("✓ Fitted all models\n")
  
  # --------------------------------------------------------------------------
  # 4. GENERATE REGRESSION TABLE
  # --------------------------------------------------------------------------
  cat("\n4. Generating regression table...\n")
  
  output_file <- file.path(path_tables, "border_muni_results.tex")

  models <- list(model1, model2, model3, model4, model5)
  model_data <- list(df_rct, df_rct, df_rct %>% filter(same_department == 1), df_rct, df_rct)
  treatment_term <- "treatmentZRRTRUE"
  extract_term <- function(model, data) {
    if (inherits(model, "fixest")) {
      result <- fixest::coeftable(model)[treatment_term, , drop = FALSE]
      return(c(estimate = result[1, "Estimate"], se = result[1, "Std. Error"], p = result[1, "Pr(>|t|)"]))
    }
    robust <- lmtest::coeftest(model, vcov. = sandwich::vcovCL(model, cluster = data$canton, type = "HC1"))
    c(estimate = robust[treatment_term, "Estimate"], se = robust[treatment_term, "Std. Error"], p = robust[treatment_term, "Pr(>|t|)"])
  }
  terms <- Map(extract_term, models, model_data)
  stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
  estimates <- vapply(terms, function(x) paste0(sprintf("%.4f", x[["estimate"]]), stars(x[["p"]])), character(1))
  ses <- vapply(terms, function(x) sprintf("(%.4f)", x[["se"]]), character(1))
  observations <- vapply(models, stats::nobs, numeric(1))

  lines <- c(
    "\\begin{table}[!htbp]", "\\centering", "\\footnotesize",
    "\\caption{Border communes: regression results}", "\\label{tab:border-results}",
    "\\begin{tabular}{lccccc}", "\\hline",
    paste0(" & (1) & (2) & (3) & (4) & (5) ", "\\\\"),
    paste("Initial ZRR wave &", paste(estimates, collapse = " & "), "\\\\"),
    paste(" &", paste(ses, collapse = " & "), "\\\\"), "\\hline",
    "\\hline",
    paste0("Outcome & FN 2002 & FN 2002 & FN 2002 & FN 2002 & FN 1988 ", "\\\\"),
    paste0("Controls & No & Yes & Yes & Yes & Yes ", "\\\\"),
    paste0("Same-department pairs & No & No & Yes & No & No ", "\\\\"),
    paste0("Department fixed effects & No & No & Yes & No & No ", "\\\\"),
    paste0("Border-pair fixed effects & No & No & No & Yes & Yes ", "\\\\"),
    paste("Observations &", paste(formatC(observations, format = "d", big.mark = ","), collapse = " & "), "\\\\"),
    "\\hline", "\\end{tabular}",
    paste0("\\parbox{\\textwidth}{\\footnotesize \\textit{Notes:} The all-pair sample contains ", nrow(df_rct),
           " commune-pair observations from ", n_border_pairs, " border pairs. Column (3) restricts to same-department pairs. ",
           "Columns (4)--(5) absorb border-pair fixed effects. Standard errors are HC1 and clustered at the canton level. ",
           "$^{*}$p$<$0.10; $^{**}$p$<$0.05; $^{***}$p$<$0.01.}"), "\\end{table}"
  )
  writeLines(lines, output_file)

  cat("✓ Generated regression table\n")
  cat("  - Output file:", output_file, "\n")

  
  # --------------------------------------------------------------------------
  # 5. SUMMARY RESULTS
  # --------------------------------------------------------------------------
  cat("\n5. Summary of results:\n")
  
  # Store models in list
  for (i in seq_along(terms)) cat("  - Model", i, "treatment coefficient:", round(terms[[i]][["estimate"]], 4), "\n")
  
  cat("\n===============================================\n")
  cat("BORDER MUNICIPALITIES ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

# Run the analysis
generate_border_regression_analysis(processed_data_path, path_tables)
