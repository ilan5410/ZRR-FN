# ==============================================================================
# ZRR Effect on Absolute Number of Votes (log) for FN
# ==============================================================================
# This script analyzes the effect of the ZRR program on the absolute number
# of votes for the Front National (FN) party using RDD methodology with
# different bandwidths and clustered standard errors
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Analyze ZRR effect on absolute FN votes with different bandwidths
#' @param processed_data_path Path to processed data directory
#' @param raw_data_path Path to raw data directory (for canton shapefile)
#' @param path_tables Path to output tables directory
#' @param controls Vector of control variable names
#' @param outcome_var Outcome variable name (default: "FN2002")
#' @param turnout_var Turnout variable name (default: "turnout_2002")
#' @return List of fitted models for different bandwidths
generate_zrr_voting_analysis <- function(processed_data_path, raw_data_path, path_tables, controls,
                                         outcome_var = "FN2002",
                                         turnout_var = "turnout_2002") {
  
  cat("===============================================\n")
  cat("ZRR EFFECT ON ABSOLUTE FN VOTES ANALYSIS\n")
  cat("===============================================\n")
  cat("Outcome variable:", outcome_var, "\n")
  cat("Bandwidths:", paste(bandwidths/1000, collapse = ", "), "km\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD MAIN DATA
  # --------------------------------------------------------------------------
  cat("1. Loading main data...\n")
  
  data_file <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist")
  }
  
  load(data_file)
  cat("✓ Loaded script_sharp.RData\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE BASE DATASET
  # --------------------------------------------------------------------------
  cat("\n2. Preparing base dataset...\n")
  
  # Use maximum bandwidth for initial filtering
  max_bandwidth <- max(bandwidths)
  
  # Filter and prepare the main dataset
  df_rdd <- dfZRRControls %>%
    filter(x >= ( - max_bandwidth) & x <= ( + max_bandwidth)) %>%
    mutate(
      dist = x,                     # Create distance variable
      treatmentZRR = z,             # Define treatment indicator
      pop = log(pop)                # Log-transform population
    ) %>%
    distinct(codecommune, .keep_all = TRUE)  # Remove duplicate communes
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  cat("✓ Prepared base dataset\n")
  cat("  - Initial observations:", nrow(df_rdd), "\n")
  cat("  - Distance range:", round(min(df_rdd$x)/1000, 2), "to", round(max(df_rdd$x)/1000, 2), "km\n")
  
  
  cat("  - Observations with canton info:", sum(!is.na(df_rdd$canton)), "\n")
  
  # --------------------------------------------------------------------------
  # 4. CREATE ABSOLUTE VOTE VARIABLE
  # --------------------------------------------------------------------------
  cat("\n4. Creating absolute vote variable...\n")
  
  # Create absolute vote count (log-transformed)
  outcome_abs <- paste0(outcome_var, "abs")
  df_rdd[[outcome_abs]] <- log(df_rdd[[outcome_var]] * exp(df_rdd$pop) * df_rdd[[turnout_var]] + 0.0001)
  
  cat("✓ Created absolute vote variable:", outcome_abs, "\n")
  cat("  - Mean absolute votes (log):", round(mean(df_rdd[[outcome_abs]], na.rm = TRUE), 3), "\n")
  
  # --------------------------------------------------------------------------
  # 5. DEFINE REGRESSION FORMULA
  # --------------------------------------------------------------------------
  cat("\n5. Defining regression formula...\n")
  
  # Create regression formula
  formula <- as.formula(paste(outcome_abs, "~ z + x +", 
                              paste(controls, collapse = " + "), 
                              "+ factor(dep)"))
  
  cat("✓ Defined regression formula\n")
  cat("  - Controls included:", length(controls), "\n")
  cat("  - Fixed effects: Department\n")
  
  # --------------------------------------------------------------------------
  # 6. FIT MODELS FOR DIFFERENT BANDWIDTHS
  # --------------------------------------------------------------------------
  cat("\n6. Fitting models for different bandwidths...\n")
  
  models <- list()
  clustered_se <- list()
  model_names <- character(length(bandwidths))
  
  for (i in seq_along(bandwidths)) {
    bw <- bandwidths[i]
    cat("  - Processing bandwidth:", bw/1000, "km\n")
    
    # Filter data by bandwidth
    data_filtered <- filter(df_rdd, x >= -bw & x <= bw)
    
    # Fit model
    model <- lm(formula, data = data_filtered)
    
    # Calculate clustered standard errors
    cluster_se <- coeftest(model, 
                           vcov = vcovHC(model, type = "HC1", 
                                         cluster = "group", 
                                         cluster.id = data_filtered$canton))
    
    # Store results
    model_name <- paste0("Bandwidth = ", bw/1000, " km")
    models[[model_name]] <- model
    clustered_se[[model_name]] <- cluster_se
    model_names[i] <- model_name
    
    cat("    ✓ Fitted model with", nobs(model), "observations\n")
  }
  
  cat("✓ Completed", length(models), "model estimations\n")
  
  # --------------------------------------------------------------------------
  # 7. GENERATE MODEL SUMMARY TABLE
  # --------------------------------------------------------------------------
  cat("\n7. Generating model summary table...\n")
  
  # Create modelsummary output
  summary_output <- modelsummary(
    models, 
    vcov = clustered_se,
    estimate = "{estimate} ({std.error}){stars}",
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    notes = c("Standard errors are clustered at the canton level"),
    statistic = c(),
    gof_omit = "IC|Log|Adj|p\\.value|statistic|se_type|Std.Errors|RMSE",
    output = "kableExtra"
  ) %>%
    row_spec(2, background = "#F5ABEA") %>%  # Highlight treatment row
    kable_styling()
  
  cat("✓ Generated model summary table\n")
  
  # --------------------------------------------------------------------------
  # 8. PREPARE STARGAZER OUTPUT
  # --------------------------------------------------------------------------
  cat("\n8. Preparing publication-ready table...\n")
  
  # Function to check fixed effects inclusion
  has_fe <- function(model, term) {
    term %in% attr(model$terms, "term.labels")
  }
  
  # Create stargazer output
  output_file <- file.path(path_tables, "absolute_vote.tex")
  
  stargazer(
    models, 
    type = "latex", 
    title = "ZRR effect on absolute number of votes (log) for FN, different bandwidths",
    column.labels = model_names, 
    covariate.labels = c("Treatment ZRR", "Distance to Frontier"), 
    omit = c("factor\\(canton\\)", "factor\\(dep\\)", "factor\\(reg\\)", controls), 
    add.lines = list(
      c("Controls", rep("Yes", length(models))),
      c("Department fixed effects", sapply(models, function(m) ifelse(has_fe(m, "factor(dep)"), "Yes", "No"))),
      c("Region fixed effects", sapply(models, function(m) ifelse(has_fe(m, "factor(reg)"), "Yes", "No")))
    ), 
    omit.stat = c("LL", "ser", "f"), 
    star.cutoffs = c(0.05, 0.01, 0.001), 
    notes = "Standard errors are clustered at the canton level",
    label = "tab:absolute-vote",
    out = output_file
  )
  
  cat("✓ Generated publication-ready table\n")
  cat("  - Output file:", output_file, "\n")
  
  # --------------------------------------------------------------------------
  # 9. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n9. Results Summary:\n")
  
  # Extract treatment coefficients and significance
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    
    # Get treatment coefficient
    treatment_coef <- coef(model)["zTRUE"]
    treatment_se <- summary(model)$coefficients["zTRUE", "Std. Error"]
    treatment_p <- summary(model)$coefficients["zTRUE", "Pr(>|t|)"]
    
    # Determine significance
    sig_level <- ifelse(treatment_p < 0.001, "***",
                        ifelse(treatment_p < 0.01, "**",
                               ifelse(treatment_p < 0.05, "*", "")))
    
    cat("  -", model_name, ":\n")
    cat("    Treatment coefficient:", round(treatment_coef, 4), sig_level, "\n")
    cat("    Standard error:", round(treatment_se, 4), "\n")
    cat("    P-value:", round(treatment_p, 4), "\n")
    cat("    Observations:", nobs(model), "\n")
  }
  
  # Overall summary
  cat("\n  - Treatment effect summary:\n")
  treatment_coefs <- sapply(models, function(m) coef(m)["zTRUE"])
  cat("    Mean effect across bandwidths:", round(mean(treatment_coefs), 4), "\n")
  cat("    Range of effects:", round(min(treatment_coefs), 4), "to", round(max(treatment_coefs), 4), "\n")
  
  # Sample size comparison
  sample_sizes <- sapply(models, nobs)
  cat("    Sample sizes:", paste(sample_sizes, collapse = ", "), "\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("ZRR VOTING ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_zrr_voting_analysis(processed_data_path, raw_data_path, path_tables, controls)