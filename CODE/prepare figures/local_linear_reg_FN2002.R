# ==============================================================================
# Local Linear Regression Analysis with Varying Bandwidth (2002 FN Vote Share)
# ==============================================================================
# This script performs local linear regression analysis with varying bandwidths
# to analyze the treatment effect on FN vote share in 2002 elections across
# French municipalities, with comprehensive bandwidth sensitivity analysis
# ==============================================================================

#' Clean data by removing rows with NA, Inf, or -Inf values
#' @param df Data frame to clean
#' @param variables Character vector of variable names to check
#' @return Cleaned data frame
clean_data_variables <- function(df, variables) {
  cat("Cleaning data for", length(variables), "variables...\n")
  initial_rows <- nrow(df)
  
  for (var in variables) {
    if (var %in% names(df)) {
      # Remove NA values
      df <- df[!is.na(df[[var]]), ]
      
      # Remove Inf and -Inf values (only for numeric variables)
      if (is.numeric(df[[var]])) {
        df <- df[!is.infinite(df[[var]]), ]
      }
    } else {
      warning(paste("Variable", var, "not found in dataset"))
    }
  }
  
  final_rows <- nrow(df)
  cat("Removed", initial_rows - final_rows, "rows with missing/invalid values\n")
  cat("Final dataset:", final_rows, "observations\n")
  
  return(df)
}

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate local linear regression analysis with varying bandwidth
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 8)
#' @param figure_height Height of output figure in inches (default: 6)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param bandwidth_range Vector of bandwidths to test (default: seq(1000, 30000, by = 1000))
#' @param distance_limits Vector of distance limits for initial filtering (default: c(-20000, 20000))
#' @param outcome_variable Name of outcome variable to analyze (default: "FN2002")
#' @param controls Vector of control variable names to include in regression
#' @return List containing plot object, regression results, and bandwidth sensitivity data
generate_local_linear_regression_analysis <- function(processed_data_path, path_figures,
                                                      figure_width = 8,
                                                      figure_height = 6,
                                                      figure_dpi = 300,
                                                      bandwidth_range = seq(1000, 30000, by = 1000),
                                                      distance_limits = c(-20000, 20000),
                                                      outcome_variable = "FN2002") {
  
  cat("===============================================\n")
  cat("LOCAL LINEAR REGRESSION ANALYSIS\n")
  cat("===============================================\n")
  cat("Outcome variable:", outcome_variable, "\n")
  cat("Distance limits:", paste(distance_limits, collapse = " to "), "\n")
  cat("Bandwidth range:", min(bandwidth_range), "to", max(bandwidth_range), "by", unique(diff(bandwidth_range))[1], "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD AND VALIDATE DATA
  # --------------------------------------------------------------------------
  cat("1. Loading and validating data...\n")
  
  data_file <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded script_sharp.RData\n")
  
  # Check if required datasets exist
  if (!exists("dfZRRControls")) {
    stop("Required dataset 'dfZRRControls' not found in loaded environment")
  }
  
  if (!exists("controls") && is.null(controls)) {
    stop("Control variables not specified and 'controls' object not found in environment")
  }
  
  # Use controls from environment if not provided
  if (is.null(controls) && exists("controls")) {
    controls <- get("controls")
  }
  
  cat("  - Dataset 'dfZRRControls' available with", nrow(dfZRRControls), "observations\n")
  cat("  - Control variables:", length(controls), "variables\n")
  cat("  - Controls:", paste(head(controls, 3), collapse = ", "), 
      if(length(controls) > 3) "..." else "", "\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE RDD DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing RDD data...\n")
  
  # Prepare the RDD dataset
  df_rdd <- dfZRRControls %>%
    filter(x >= distance_limits[1] & x <= distance_limits[2]) %>%
    mutate(
      dist = x,                     # Create a distance variable
      treatmentZRR = z,             # Define treatment indicator
      pop = log(pop),               # Log-transform population
      popDensity = log(popDensity)  # Log-transform population density
    ) %>%
    distinct(codecommune, .keep_all = TRUE)  # Remove duplicate communes
  
  cat("✓ Prepared RDD dataset\n")
  cat("  - Observations after filtering:", nrow(df_rdd), "\n")
  cat("  - Distance range:", min(df_rdd$x), "to", max(df_rdd$x), "\n")
  cat("  - Treatment observations:", sum(df_rdd$treatmentZRR, na.rm = TRUE), "\n")
  cat("  - Control observations:", sum(!df_rdd$treatmentZRR, na.rm = TRUE), "\n")
  
  # Validate outcome variable
  if (!outcome_variable %in% names(df_rdd)) {
    stop("Outcome variable '", outcome_variable, "' not found in dataset")
  }
  
  outcome_na <- sum(is.na(df_rdd[[outcome_variable]]))
  if (outcome_na > 0) {
    cat("  - Warning:", outcome_na, "missing values in outcome variable\n")
  }
  
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  # --------------------------------------------------------------------------
  # 3. CONSTRUCT REGRESSION FORMULA
  # --------------------------------------------------------------------------
  cat("\n3. Constructing regression formula...\n")
  
  # Build the regression formula
  formula_components <- c("z", "x", controls, "factor(dep)")
  formula_string <- paste(outcome_variable, "~", paste(formula_components, collapse = " + "))
  formula <- as.formula(formula_string)
  
  cat("✓ Constructed regression formula\n")
  cat("  - Formula:", deparse(formula), "\n")
  cat("  - Total predictors:", length(formula_components), "\n")
  
  # --------------------------------------------------------------------------
  # 4. INITIALIZE BANDWIDTH ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n4. Initializing bandwidth analysis...\n")
  
  n_bandwidths <- length(bandwidth_range)
  
  # Initialize vectors to store results
  coef_zTRUE <- numeric(n_bandwidths)
  se_zTRUE <- numeric(n_bandwidths)
  n_obs <- integer(n_bandwidths)
  
  cat("✓ Initialized bandwidth analysis\n")
  cat("  - Number of bandwidths to test:", n_bandwidths, "\n")
  cat("  - Bandwidth sequence:", paste(head(bandwidth_range, 3), collapse = ", "), 
      if(n_bandwidths > 3) "..." else "", "\n")
  
  # --------------------------------------------------------------------------
  # 5. PERFORM BANDWIDTH SENSITIVITY ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n5. Performing bandwidth sensitivity analysis...\n")
  
  # Progress tracking
  progress_intervals <- seq(1, n_bandwidths, length.out = min(10, n_bandwidths))
  
  # Loop over bandwidths
  for (i in seq_along(bandwidth_range)) {
    bw <- bandwidth_range[i]
    
    # Progress reporting
    if (i %in% round(progress_intervals)) {
      cat("  - Processing bandwidth", bw, "(", i, "/", n_bandwidths, ")\n")
    }
    
    # Filter the data based on the current bandwidth
    df_filtered <- filter(df_rdd, x >= -bw & x <= bw)
    n_obs[i] <- nrow(df_filtered)
    
    # Skip if insufficient observations
    if (n_obs[i] < 50) {  # Minimum threshold for reliable estimation
      coef_zTRUE[i] <- NA_real_
      se_zTRUE[i] <- NA_real_
      next
    }
    
    # Estimate the model
    tryCatch({
      model <- lm(formula, data = df_filtered)
      
      # Calculate cluster-robust standard errors
      clustered_se <- vcovCL(model, cluster = df_filtered$canton, type = "HC1")
      
      # Store the coefficient of zTRUE and its standard error
      if ("zTRUE" %in% rownames(summary(model)$coefficients)) {
        coef_zTRUE[i] <- summary(model)$coefficients["zTRUE", "Estimate"]
        se_zTRUE[i] <- sqrt(diag(clustered_se))["zTRUE"]
      } else {
        coef_zTRUE[i] <- NA_real_
        se_zTRUE[i] <- NA_real_
      }
    }, error = function(e) {
      cat("    Warning: Model estimation failed for bandwidth", bw, "\n")
      coef_zTRUE[i] <<- NA_real_
      se_zTRUE[i] <<- NA_real_
    })
  }
  
  cat("✓ Completed bandwidth sensitivity analysis\n")
  cat("  - Successfully estimated models:", sum(!is.na(coef_zTRUE)), "/", n_bandwidths, "\n")
  cat("  - Failed estimations:", sum(is.na(coef_zTRUE)), "\n")
  
  # --------------------------------------------------------------------------
  # 6. PREPARE RESULTS DATA
  # --------------------------------------------------------------------------
  cat("\n6. Preparing results data...\n")
  
  # Create results data frame
  results_df <- data.frame(
    Bandwidth = bandwidth_range,
    Coefficient = coef_zTRUE,
    SE = se_zTRUE,
    N_Obs = n_obs,
    CI_Lower = coef_zTRUE - 1.96 * se_zTRUE,
    CI_Upper = coef_zTRUE + 1.96 * se_zTRUE
  ) %>%
    mutate(
      Significant = abs(Coefficient / SE) > 1.96,
      Significant = ifelse(is.na(Significant), FALSE, Significant)
    )
  
  cat("✓ Prepared results data\n")
  cat("  - Valid coefficient estimates:", sum(!is.na(results_df$Coefficient)), "\n")
  cat("  - Statistically significant results:", sum(results_df$Significant, na.rm = TRUE), "\n")
  
  # Calculate summary statistics
  coef_summary <- results_df %>%
    filter(!is.na(Coefficient)) %>%
    summarize(
      mean_coef = mean(Coefficient),
      sd_coef = sd(Coefficient),
      min_coef = min(Coefficient),
      max_coef = max(Coefficient),
      mean_se = mean(SE, na.rm = TRUE)
    )
  
  cat("  - Coefficient range:", sprintf("%.4f", coef_summary$min_coef), "to", sprintf("%.4f", coef_summary$max_coef), "\n")
  cat("  - Mean coefficient:", sprintf("%.4f", coef_summary$mean_coef), "(SD:", sprintf("%.4f", coef_summary$sd_coef), ")\n")
  
  # --------------------------------------------------------------------------
  # 7. CREATE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n7. Creating visualization...\n")
  
  # Create the main plot
  plot <- ggplot(results_df, aes(x = Bandwidth, y = Coefficient)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 2, alpha = 0.8) +
    geom_errorbar(
      aes(ymin = CI_Lower, ymax = CI_Upper), 
      width = max(bandwidth_range) * 0.01, 
      alpha = 0.5
    ) +
    geom_smooth(
      method = "lm", 
      formula = y ~ poly(x, 1), 
      se = FALSE, 
      color = "red", 
      linetype = "dashed",
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.7) +
    labs(
      # title = paste("Local Linear Regression: Effect of ZRR Treatment on", outcome_variable),
      # subtitle = "Bandwidth Sensitivity Analysis with 95% Confidence Intervals",
      x = "Bandwidth",
      y = "Estimated Coefficient (LATE)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  
  cat("✓ Created visualization\n")
  cat("  - Plot includes", sum(!is.na(results_df$Coefficient)), "data points\n")
  cat("  - Bandwidth range displayed:", min(bandwidth_range), "to", max(bandwidth_range), "\n")
  
  # --------------------------------------------------------------------------
  # 8. PERFORM TREND ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n8. Performing trend analysis...\n")
  
  # Fit trend line to coefficients
  valid_results <- results_df %>% filter(!is.na(Coefficient))
  
  if (nrow(valid_results) > 2) {
    trend_model <- lm(Coefficient ~ Bandwidth, data = valid_results)
    trend_slope <- coef(trend_model)[2]
    trend_pvalue <- summary(trend_model)$coefficients[2, "Pr(>|t|)"]
    trend_r2 <- summary(trend_model)$r.squared
    
    cat("✓ Performed trend analysis\n")
    cat("  - Trend slope:", sprintf("%.6f", trend_slope), "coefficient units per bandwidth unit\n")
    cat("  - Trend p-value:", sprintf("%.4f", trend_pvalue), "\n")
    cat("  - Trend R-squared:", sprintf("%.4f", trend_r2), "\n")
  } else {
    cat("  - Insufficient data points for trend analysis\n")
    trend_slope <- NA_real_
    trend_pvalue <- NA_real_
    trend_r2 <- NA_real_
  }
  
  # --------------------------------------------------------------------------
  # 9. SAVE VISUALIZATION AND RESULTS
  # --------------------------------------------------------------------------
  cat("\n9. Saving visualization and results...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Save the plot
  output_file <- file.path(path_figures, paste0("local_linear_reg_", outcome_variable, ".png"))
  ggsave(
    filename = output_file,
    plot = plot,
    width = figure_width,
    height = figure_height,
    dpi = figure_dpi,
    bg = "white"
  )
  
  cat("✓ Saved visualization\n")
  cat("  - Output file:", output_file, "\n")
  cat("  - Dimensions:", figure_width, "x", figure_height, "inches\n")
  cat("  - Resolution:", figure_dpi, "DPI\n")
  
  # --------------------------------------------------------------------------
  # 10. COMPILE ANALYSIS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n10. Compiling analysis summary...\n")
  
  # Create comprehensive results list
  analysis_results <- list(
    plot = plot,
    results_data = results_df,
    summary_statistics = coef_summary,
    trend_analysis = list(
      slope = trend_slope,
      p_value = trend_pvalue,
      r_squared = trend_r2
    ),
    analysis_parameters = list(
      outcome_variable = outcome_variable,
      bandwidth_range = bandwidth_range,
      distance_limits = distance_limits,
      n_bandwidths = n_bandwidths,
      successful_estimations = sum(!is.na(coef_zTRUE)),
      total_observations = nrow(df_rdd)
    )
  )
  
  cat("✓ Compiled analysis summary\n")
  cat("  - Results object contains:", length(analysis_results), "components\n")
  
  # Final summary
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("LOCAL LINEAR REGRESSION ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  cat("Key findings:\n")
  cat("- Coefficient range:", sprintf("%.4f", coef_summary$min_coef), "to", sprintf("%.4f", coef_summary$max_coef), "\n")
  cat("- Mean treatment effect:", sprintf("%.4f", coef_summary$mean_coef), "\n")
  cat("- Significant results:", sum(results_df$Significant, na.rm = TRUE), "/", sum(!is.na(results_df$Coefficient)), "\n")
  if (!is.na(trend_slope)) {
    cat("- Bandwidth sensitivity trend:", ifelse(abs(trend_slope) < 0.00001, "Stable", "Variable"), "\n")
  }
  cat("\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_local_linear_regression_analysis(processed_data_path, path_figures)