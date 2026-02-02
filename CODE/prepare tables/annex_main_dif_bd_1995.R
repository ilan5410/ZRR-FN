# ==============================================================================
# Annex: RDD Main Results Analysis FN1995 - Different Specifications 
# ==============================================================================
# This script generates regression discontinuity design results with different
# specifications using HC1 clustered standard errors at the canton level
# Bandwidth: 10km analysis with various control variable combinations
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate RDD main results with different specifications
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param max_bandwidth Maximum bandwidth for data filtering (default: from bandwidths[2])
#' @return List of regression models with clustered standard errors
generate_rdd_main_results <- function(processed_data_path, path_tables,
                                      bandwidths, controls) {
  
  cat("===============================================\n")
  cat("RDD MAIN RESULTS ANALYSIS PIPELINE\n")
  cat("===============================================\n")
  target_bandwidth <- bandwidths[2] 
  max_bandwidth <- bandwidths[1] 
  
  cat("Target bandwidth:", target_bandwidth, "m\n")
  if (!is.null(max_bandwidth)) {
    cat("Maximum bandwidth for filtering:", max_bandwidth, "km\n")
  }
  cat("\n")
  
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
  cat("  - Available bandwidths:", paste(bandwidths, collapse = ", "), "\n")
  cat("  - Control variables:", length(controls), "variables\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE DATA FOR RDD ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n2. Preparing data for RDD analysis...\n")
  
  # Set bandwidth (use provided max_bandwidth or default from bandwidths)
  b <- ifelse(!is.null(max_bandwidth), max_bandwidth, bandwidths[2])
  max_filter <- ifelse(!is.null(max_bandwidth), max_bandwidth, max(bandwidths))
  
  # Filter and prepare the dataset
  df_rdd <- dfZRRControls %>%
    filter(x >= (-max_filter) & x <= (max_filter)) %>%
    mutate(
      dist = x,                     # Create a distance variable
      treatmentZRR = z,            # Define treatment indicator
      pop = log(pop),              # Log-transform population
      popDensity = log(popDensity) # Log-transform population density
    ) %>%
    distinct(codecommune, .keep_all = TRUE)  # Remove duplicate communes
  
  cat("✓ Prepared RDD dataset\n")
  cat("  - Observations after filtering:", nrow(df_rdd), "\n")
  cat("  - Unique communes:", length(unique(df_rdd$codecommune)), "\n")
  cat("  - Distance range:", round(min(df_rdd$x), 2), "to", round(max(df_rdd$x), 2), "\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE CONTROL VARIABLE SPECIFICATIONS
  # --------------------------------------------------------------------------
  cat("\n3. Defining control variable specifications...\n")
  
  # Define the groups of control variables for different specifications
  control_groups <- list(
    group1 = list(
      vars = c("x"), 
      dep = FALSE, 
      controls = FALSE,
      description = "Distance only"
    ),
    group2 = list(
      vars = c("x", "pop", "superficie"), 
      dep = FALSE, 
      controls = FALSE,
      description = "Distance + basic demographics"
    ),
    group3 = list(
      vars = c("x", "pop", "superficie", "dep"), 
      dep = TRUE, 
      controls = FALSE,
      description = "Distance + demographics + dept FE"
    ),
    group4 = list(
      vars = c("x", "pop", "superficie", setdiff(controls, c("pop", "superficie"))), 
      dep = FALSE, 
      controls = TRUE,
      description = "Distance + demographics + all controls"
    ),
    group6 = list(
      vars = c("x", "pop", "superficie", setdiff(controls, c("pop", "superficie")), "dep"), 
      dep = TRUE, 
      controls = TRUE,
      description = "Distance + demographics + all controls + dept FE"
    )
  )
  
  cat("✓ Defined", length(control_groups), "control variable specifications\n")
  for (i in seq_along(control_groups)) {
    cat("  -", names(control_groups)[i], ":", control_groups[[i]]$description, "\n")
  }
  
  # --------------------------------------------------------------------------
  # 4. FILTER DATA TO TARGET BANDWIDTH
  # --------------------------------------------------------------------------
  cat("\n4. Filtering data to target bandwidth...\n")
  
  # Filter to the specific bandwidth for analysis
  df_filtered <- df_rdd %>%
    filter(x >= -target_bandwidth & x <= target_bandwidth) %>% 
    mutate(x = x / 10000)  # Scale distance variable
  
  df_filtered <- clean_data_variables(df_filtered, names(df_filtered))
  
  cat("✓ Filtered data to", target_bandwidth, "m bandwidth\n")
  cat("  - Final observations:", nrow(df_filtered), "\n")
  cat("  - Treatment group:", sum(df_filtered$treatmentZRR, na.rm = TRUE), "communes\n")
  cat("  - Control group:", sum(!df_filtered$treatmentZRR, na.rm = TRUE), "communes\n")
  
  # Validate canton variable for clustering
  if (!"canton" %in% names(df_filtered)) {
    stop("Error: canton variable not found for clustering")
  }
  
  unique_cantons <- length(unique(df_filtered$canton[!is.na(df_filtered$canton)]))
  cat("  - Unique cantons for clustering:", unique_cantons, "\n")
  
  # --------------------------------------------------------------------------
  # 5. RUN REGRESSION MODELS
  # --------------------------------------------------------------------------
  cat("\n5. Running regression models with clustered standard errors...\n")
  
  # Initialize lists to store models and robust results
  models <- list()
  robust_models <- list()
  
  # Run regressions for each specification
  for (i in seq_along(control_groups)) {
    group_name <- names(control_groups)[i]
    control_vars <- control_groups[[i]]$vars
    
    cat("  - Running", group_name, "specification...\n")
    
    # Create formula
    formula <- as.formula(paste("FN1995 ~ treatmentZRR +", paste(control_vars, collapse = " + ")))
    
    # Fit OLS model
    model <- lm(formula, data = df_filtered)
    models[[group_name]] <- model
    
    # Calculate HC1 clustered standard errors at canton level
    # Remove observations with missing canton values for clustering
    model_data <- df_filtered[!is.na(df_filtered$canton), ]
    model_clustered <- lm(formula, data = model_data)
    
    # Compute clustered standard errors
    vcov_clustered <- vcovHC(model_clustered, type = "HC1", cluster = model_data$canton)
    robust_models[[group_name]] <- coeftest(model_clustered, vcov = vcov_clustered)
    
    cat("    ✓ Completed", group_name, "\n")
  }
  
  cat("✓ Completed all", length(models), "regression specifications\n")
  
  # --------------------------------------------------------------------------
  # 6. PREPARE FIXED EFFECTS AND CONTROLS INDICATORS
  # --------------------------------------------------------------------------
  cat("\n6. Preparing table formatting indicators...\n")
  
  # Create lines indicating the inclusion of fixed effects and controls
  fe_lines <- list(
    "Controls" = sapply(control_groups, function(x) ifelse(x$controls, "True", "False")),
    "Dept FE" = sapply(control_groups, function(x) ifelse(x$dep, "True", "False"))
  )
  
  cat("✓ Prepared fixed effects and controls indicators\n")
  
  # --------------------------------------------------------------------------
  # 7. GENERATE REGRESSION TABLE
  # --------------------------------------------------------------------------
  cat("\n7. Generating regression table...\n")
  
  # Prepare output file path
  output_file <- file.path(path_tables, "annex_main_dif_bd_1995.tex")
  
  # Generate the regression table using stargazer with clustered standard errors
  table_output <- stargazer(
    models, 
    se = lapply(robust_models, function(x) x[, "Std. Error"]),
    type = "latex",
    title = paste("Main results when bandwidth is", target_bandwidth, "km, different specifications"),
    dep.var.labels = "FN Vote Share 1995",
    covariate.labels = c("ZRR Treatment", "Distance"),
    omit = c(setdiff(controls, "superficie"), "dep"),  # Omit control variables from display
    omit.stat = c("adj.rsq", "ser", "f"),
    add.lines = list(
      c("Controls", fe_lines$Controls),
      c("Dept FE", fe_lines$`Dept FE`)
    ),
    star.cutoffs = c(0.05, 0.01, 0.001), 
    column.sep.width = "3pt",
    float = FALSE,
    notes = paste("Standard errors clustered at canton level using HC1 estimator.",
                  "Sample restricted to municipalities located", target_bandwidth, 
                  "km at most from the program frontier."),
    notes.align = "l",
    table.placement = "H",
    label = "tab:rdd_results_diffbandwidth-1995",
    out = output_file
  )
  

  cat("✓ Generated regression table\n")
  cat("  - Output file:", output_file, "\n")
  
  # --------------------------------------------------------------------------
  # 8. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n8. Results Summary:\n")
  
  # Display key statistics for each model
  for (i in seq_along(models)) {
    model_name <- names(models)[i]
    model <- models[[i]]
    robust_result <- robust_models[[i]]
    
    treatment_coef <- coef(model)["treatmentZRRTRUE"]
    treatment_se <- robust_result["treatmentZRRTRUE", "Std. Error"]
    treatment_pval <- robust_result["treatmentZRRTRUE", "Pr(>|t|)"]
    
    cat("  -", model_name, ":\n")
    cat("    Treatment coefficient:", round(treatment_coef, 4), "\n")
    cat("    Clustered SE:", round(treatment_se, 4), "\n")
    cat("    P-value:", round(treatment_pval, 4), "\n")
    cat("    Observations:", nobs(model), "\n")
    cat("    R-squared:", round(summary(model)$r.squared, 4), "\n")
  }
  
  
  cat("✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("RDD MAIN RESULTS ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_rdd_main_results(processed_data_path, path_tables, bandwidths, controls)

