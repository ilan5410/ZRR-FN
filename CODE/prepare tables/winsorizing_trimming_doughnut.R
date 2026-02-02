# ==============================================================================
# Robustness Analysis - Winsorizing, Trimming, and Doughnut
# ==============================================================================
# This script performs robustness checks using different data treatment methods:
# winsorizing outliers, trimming extreme values, and doughnut hole exclusion
# around the RDD cutoff to test treatment effect sensitivity
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Perform robustness analysis using winsorizing, trimming, and doughnut methods
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param bandwidth Maximum bandwidth for data filtering (default: 20km)
#' @param winsor_probs Winsorizing quantile limits (default: c(0.01, 0.99))
#' @param trim_probs Trimming quantile limits (default: c(0.01, 0.99))
#' @param doughnut_radius Radius for doughnut hole exclusion (default: 5km)
#' @return List of fitted models for different robustness approaches
generate_robustness_analysis <- function(processed_data_path, path_tables,
                                         bandwidth = 20, 
                                         winsor_probs = c(0.01, 0.99),
                                         trim_probs = c(0.01, 0.99), 
                                         doughnut_radius = 5) {
  
  cat("===============================================\n")
  cat("ROBUSTNESS ANALYSIS - WTD METHODS\n")
  cat("===============================================\n")
  cat("Bandwidth:", bandwidth, "km\n")
  cat("Winsorizing limits:", paste(winsor_probs * 100, collapse = "%-", sep = ""), "%\n")
  cat("Trimming limits:", paste(trim_probs * 100, collapse = "%-", sep = ""), "%\n")
  cat("Doughnut radius:", doughnut_radius, "km\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
  data_file <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist")
  }
  
  load(data_file)
  cat("✓ Loaded script_sharp.RData\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE BASE DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing base data...\n")
  
  # Convert bandwidth to meters for filtering
  bandwidth_m <- bandwidth * 1000
  
  # Filter and prepare the dataset
  df_rdd <- dfZRRControls %>%
    filter(x >= (-bandwidth_m) & x <= (bandwidth_m)) %>%
    mutate(
      dist = x,                      # Create a distance variable
      treatmentZRR = z,              # Define treatment indicator
      pop = log(pop),                # Log-transform population
      popDensity = log(popDensity)   # Log-transform population density
    ) %>%
    distinct(codecommune, .keep_all = TRUE)  # Remove duplicate communes
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  # Convert distance from meters to kilometers
  df_rdd$x <- df_rdd$x / 1000
  
  cat("✓ Prepared base dataset\n")
  cat("  - Initial observations:", nrow(df_rdd), "\n")
  cat("  - Distance range:", round(min(df_rdd$x), 2), "to", round(max(df_rdd$x), 2), "km\n")
  
  # Define regression formula
  formula <- as.formula(paste("FN2002 ~ z + x +", paste(controls, collapse = " + "), "+ factor(dep)"))
  cat("  - Regression formula defined\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE DATA TREATMENT FUNCTIONS
  # --------------------------------------------------------------------------
  cat("\n3. Defining data treatment functions...\n")
  
  # Winsorizing function
  winsorize <- function(x, probs = winsor_probs) {
    if (all(is.na(x))) return(x)
    limits <- quantile(x, probs = probs, na.rm = TRUE)
    x[x < limits[1]] <- limits[1]
    x[x > limits[2]] <- limits[2]
    return(x)
  }
  
  # Trimming function
  trim <- function(x, probs = trim_probs) {
    if (all(is.na(x))) return(x)
    limits <- quantile(x, probs = probs, na.rm = TRUE)
    x[x < limits[1] | x > limits[2]] <- NA
    return(x)
  }
  
  cat("✓ Defined data treatment functions\n")
  
  # --------------------------------------------------------------------------
  # 4. APPLY WINSORIZING
  # --------------------------------------------------------------------------
  cat("\n4. Applying winsorizing method...\n")
  
  df_winsorized <- df_rdd %>%
    mutate(across(where(is.numeric), ~ winsorize(.)))
  
  model_winsorized <- lm(formula, data = df_winsorized)
  
  cat("✓ Fitted winsorized model\n")
  cat("  - Observations:", nobs(model_winsorized), "\n")
  
  # --------------------------------------------------------------------------
  # 5. APPLY TRIMMING
  # --------------------------------------------------------------------------
  cat("\n5. Applying trimming method...\n")
  
  df_trimmed <- df_rdd %>%
    mutate(across(where(is.numeric), ~ trim(.))) %>%
    drop_na()
  
  model_trimmed <- lm(formula, data = df_trimmed)
  
  cat("✓ Fitted trimmed model\n")
  cat("  - Observations after trimming:", nobs(model_trimmed), "\n")
  cat("  - Observations removed:", nrow(df_rdd) - nobs(model_trimmed), "\n")
  
  # --------------------------------------------------------------------------
  # 6. APPLY DOUGHNUT APPROACH
  # --------------------------------------------------------------------------
  cat("\n6. Applying doughnut approach...\n")
  
  df_doughnut <- df_rdd %>%
    filter(abs(x) > doughnut_radius)
  
  model_doughnut <- lm(formula, data = df_doughnut)
  
  cat("✓ Fitted doughnut model\n")
  cat("  - Observations after doughnut exclusion:", nobs(model_doughnut), "\n")
  cat("  - Observations in doughnut hole:", nrow(df_rdd) - nobs(model_doughnut), "\n")
  
  # --------------------------------------------------------------------------
  # 7. PREPARE MODEL SUMMARY
  # --------------------------------------------------------------------------
  cat("\n7. Preparing model summary...\n")
  
  # Store models and names
  models <- list(model_winsorized, model_trimmed, model_doughnut)
  model_names <- c("Winsorized", "Trimmed", "Doughnut")
  
  # Function to check if terms are included in the formula
  has_fe <- function(model, term) {
    term %in% attr(model$terms, "term.labels")
  }
  
  # Prepare fixed effects indicators
  fe_indicators <- list(
    "Controls" = rep("Yes", length(models)),
    "Department fixed effects" = sapply(models, function(m) ifelse(has_fe(m, "factor(dep)"), "Yes", "No")),
    "Region fixed effects" = sapply(models, function(m) ifelse(has_fe(m, "factor(reg)"), "Yes", "No"))
  )
  
  cat("✓ Prepared model summary information\n")
  
  # --------------------------------------------------------------------------
  # 8. GENERATE REGRESSION TABLE
  # --------------------------------------------------------------------------
  cat("\n8. Generating regression table...\n")
  
  output_file <- file.path(path_tables, "winsorizing_trimming_doughnut.tex")
  
  # Create descriptive notes
  winsor_desc <- paste0("Winsorizing: outliers replaced with ", winsor_probs[1]*100, "th and ", winsor_probs[2]*100, "th percentile values")
  trim_desc <- paste0("Trimming: observations outside ", trim_probs[1]*100, "th and ", trim_probs[2]*100, "th percentiles removed")
  doughnut_desc <- paste0("Doughnut: observations within ", doughnut_radius, "km of the frontier border removed")
  
  # Build notes with parbox for proper text wrapping
  notes_text <- paste0("\\parbox{0.9\\textwidth}{\\footnotesize ",
                       winsor_desc, ". ", trim_desc, ". ", doughnut_desc, ". ",
                       "All specifications include control variables and department fixed effects. ",
                       "Standard errors are heteroskedasticity-robust.}")

  stargazer(
    models,
    type = "latex",
    title = "Winsorizing, trimming and doughnut: Estimation of Treatment Effect",
    column.labels = model_names,
    keep = c("z", "x"),  # Only show treatment and running variable
    add.lines = list(
      c("Controls", fe_indicators$Controls),
      c("Department fixed effects", fe_indicators$`Department fixed effects`),
      c("Region fixed effects", fe_indicators$`Region fixed effects`)
    ),
    omit.stat = c("LL", "ser", "f"),
    label = "tab:robustness-wtd",
    star.cutoffs = c(0.05, 0.01, 0.001),
    digits = 4,
    font.size = "footnotesize",
    notes = notes_text,
    notes.append = FALSE,
    notes.align = "l",
    out = output_file
  )

  cat("✓ Generated regression table\n")
  cat("  - Output file:", output_file, "\n")

  # Apply formatting fixes for width management
  format_latex_table(
    tex_file = output_file,
    use_resizebox = TRUE,
    font_size = "footnotesize",
    use_landscape = FALSE,
    notes_width = 0.9
  )
  
  # --------------------------------------------------------------------------
  # 9. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n9. Results Summary:\n")
  
  # Extract treatment coefficients
  treatment_coefs <- sapply(models, function(m) coef(m)["zTRUE"])
  treatment_ses <- sapply(models, function(m) summary(m)$coefficients["zTRUE", "Std. Error"])
  
  # Display results
  for (i in seq_along(models)) {
    cat("  -", model_names[i], ":\n")
    cat("    Treatment coefficient:", round(treatment_coefs[i], 4), "\n")
    cat("    Standard error:", round(treatment_ses[i], 4), "\n")
    cat("    Observations:", nobs(models[[i]]), "\n")
  }
  
  # Compare sample sizes
  cat("\n  Sample size comparison:\n")
  cat("    Original data:", nrow(df_rdd), "\n")
  cat("    Winsorized:", nobs(model_winsorized), "(", round((nobs(model_winsorized)/nrow(df_rdd))*100, 1), "%)\n")
  cat("    Trimmed:", nobs(model_trimmed), "(", round((nobs(model_trimmed)/nrow(df_rdd))*100, 1), "%)\n")
  cat("    Doughnut:", nobs(model_doughnut), "(", round((nobs(model_doughnut)/nrow(df_rdd))*100, 1), "%)\n")
  
  
  cat("✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("ROBUSTNESS ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_robustness_analysis(processed_data_path, path_tables)

