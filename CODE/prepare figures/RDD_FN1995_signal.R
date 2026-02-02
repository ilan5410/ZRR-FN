# ==============================================================================
# RDD Signal Test Analysis - FN Vote Share 1995
# ==============================================================================
# This script performs a regression discontinuity design (RDD) placebo test
# analyzing Front National (FN) vote share in 1995 around the ZRR program
# frontier, using binned scatter plots 
# ==============================================================================


# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate RDD placebo test analysis for FN vote share in 1988
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in pixels (default: 800)
#' @param figure_height Height of output figure in pixels (default: 900)
#' @param rdd_bandwidth RDD bandwidth around threshold in meters (default: 20000)
#' @param analysis_bandwidth Analysis bandwidth for regression in meters (default: 10000)
#' @param threshold RDD threshold value (default: 0)
#' @param outcome_variable Outcome variable for analysis (default: "FN1995")
#' @param num_bins Number of bins for scatter plot (default: 15)
#' @return List containing plot, regression results, and analysis statistics
generate_rdd_placebo_analysis <- function(processed_data_path, path_figures,
                                          figure_width = 8,
                                          figure_height = 8,
                                          outcome_variable = "FN1995",
                                          num_bins = 15) {
  
  threshold = 0
  analysis_bandwidth = 10000
  rdd_bandwidth = 20000
  cat("===============================================\n")
  cat("RDD PLACEBO TEST ANALYSIS - FN 1988\n")
  cat("===============================================\n")
  cat("Outcome variable:", outcome_variable, "\n")
  cat("RDD bandwidth:", scales::comma(rdd_bandwidth), "meters\n")
  cat("Analysis bandwidth:", scales::comma(analysis_bandwidth), "meters\n")
  cat("Threshold:", threshold, "\n")
  cat("Number of bins:", num_bins, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "pixels\n")
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
  
  # Check if required data exists
  if (!exists("dfZRRControls")) {
    stop("Required dataset 'dfZRRControls' not found in loaded environment")
  }
  
  cat("  - Dataset 'dfZRRControls' available with", nrow(dfZRRControls), "observations\n")
  
  # Verify required variables exist
  required_vars <- c("x", "z", outcome_variable, "codecommune", "pop")
  missing_vars <- setdiff(required_vars, names(dfZRRControls))
  
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  cat("✓ Validated required variables\n")
  
  # --------------------------------------------------------------------------
  # 2. DEFINE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n2. Defining control variables...\n")
  
  # Define comprehensive set of control variables
  controls_to_use <- controls
  
  # Verify control variables exist
  available_controls <- intersect(controls_to_use, names(dfZRRControls))
  missing_controls <- setdiff(controls_to_use, names(dfZRRControls))
  
  if (length(missing_controls) > 0) {
    warning("Missing control variables: ", paste(missing_controls, collapse = ", "))
  }
  
  controls_to_use <- available_controls
  
  cat("✓ Defined control variables\n")
  cat("  - Available controls:", length(controls_to_use), "\n")
  cat("  - Missing controls:", length(missing_controls), "\n")
  
  # --------------------------------------------------------------------------
  # 3. PREPARE RDD DATASET
  # --------------------------------------------------------------------------
  cat("\n3. Preparing RDD dataset...\n")
  
  # Filter data within RDD bandwidth and prepare variables
  df_rdd <- dfZRRControls %>%
    filter(
      x >= threshold - rdd_bandwidth,
      x <= threshold + rdd_bandwidth
    ) %>%
    mutate(
      dist = x,
      treatmentZRR = z,
      pop = log(pop)
    ) %>%
    # Remove duplicates, keeping first instance per municipality
    group_by(codecommune) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  cat("✓ Prepared RDD dataset\n")
  cat("  - Observations within bandwidth:", nrow(df_rdd), "\n")
  cat("  - Bandwidth range:", scales::comma(threshold - rdd_bandwidth), "to", scales::comma(threshold + rdd_bandwidth), "meters\n")
  
  # Check data availability around threshold
  treatment_distribution <- df_rdd %>%
    mutate(side = ifelse(x <= threshold, "Treatment Side (≤0)", "Control Side (>0)")) %>%
    group_by(side, treatmentZRR) %>%
    summarise(count = n(), .groups = 'drop')
  
  cat("  - Data distribution around threshold:\n")
  for (i in 1:nrow(treatment_distribution)) {
    cat("    ", treatment_distribution$side[i], "- Treatment", treatment_distribution$treatmentZRR[i], ":", 
        treatment_distribution$count[i], "municipalities\n")
  }
  
  # --------------------------------------------------------------------------
  # 4. FILTER DATA FOR REGRESSION ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n4. Filtering data for regression analysis...\n")
  
  # Apply analysis bandwidth (narrower than RDD bandwidth)
  df_filtered <- df_rdd %>%
    filter(
      x >= -analysis_bandwidth,
      x <= analysis_bandwidth
    ) %>%
    # Remove observations with missing outcome or key variables
    filter(
      !is.na(.data[[outcome_variable]]),
      !is.na(x),
      !is.na(z)
    )
  
  cat("✓ Filtered data for regression analysis\n")
  cat("  - Analysis bandwidth:", scales::comma(-analysis_bandwidth), "to", scales::comma(analysis_bandwidth), "meters\n")
  cat("  - Final observations for analysis:", nrow(df_filtered), "\n")
  
  
  # --------------------------------------------------------------------------
  # 5. FIT REGRESSION MODEL AND EXTRACT RESIDUALS
  # --------------------------------------------------------------------------
  cat("\n5. Fitting regression model and extracting residuals...\n")
  
  # Define regression formula with controls and department fixed effects
  conditional_on <- c(controls_to_use, "dep")
  
  
  df_filtered <- df_filtered %>%
    select(all_of(conditional_on), outcome_variable, "canton", "x")
  
  df_filtered <- clean_data_variables(df_filtered, c(outcome_variable, conditional_on, "x"))
  
  formula_text <- paste(outcome_variable, "~", paste(conditional_on, collapse = " + "))
  regression_formula <- as.formula(formula_text)
  
  cat("  - Regression formula:", formula_text, "\n")
  
  # Fit regression model
  tryCatch({
    model <- lm(regression_formula, data = df_filtered)
    cat("✓ Fitted regression model successfully\n")
  }, error = function(e) {
    stop("Error fitting regression model: ", e$message)
  })
  
  # Extract residuals
  df_filtered$residuals <- residuals(model)
  
  # Model diagnostics
  model_summary <- summary(model)
  cat("  - Model diagnostics:\n")
  cat("    R-squared:", sprintf("%.4f", model_summary$r.squared), "\n")
  cat("    Adjusted R-squared:", sprintf("%.4f", model_summary$adj.r.squared), "\n")
  cat("    Residual standard error:", sprintf("%.4f", model_summary$sigma), "\n")
  cat("    Observations:", nrow(model$model), "\n")
  


  # --------------------------------------------------------------------------
  # 8. PREPARE BINNED DATA FOR VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n8. Preparing binned data for visualization...\n")
  
  # Create binned data for scatter plot
  df_binned <- df_filtered %>%
    mutate(bin = ntile(x, num_bins)) %>%
    group_by(bin) %>%
    mutate(
      bin_center = mean(x, na.rm = TRUE),
      group = ifelse(mean(x, na.rm = TRUE) <= threshold, "Inside Program", "Outside Program")
    ) %>%
    summarize(
      bin_center = mean(x, na.rm = TRUE),
      residuals_mean = mean(residuals, na.rm = TRUE),
      residuals_se = sd(residuals, na.rm = TRUE) / sqrt(n()),
      group = first(group),
      count = n(),
      .groups = "drop"
    )
  
  cat("✓ Prepared binned data\n")
  cat("  - Number of bins:", num_bins, "\n")
  cat("  - Bins on treatment side:", sum(df_binned$group == "Inside Program"), "\n")
  cat("  - Bins on control side:", sum(df_binned$group == "Outside Program"), "\n")
  
  # Check bin distribution
  bin_stats <- df_binned %>%
    group_by(group) %>%
    summarise(
      mean_count = mean(count),
      total_obs = sum(count),
      .groups = 'drop'
    )
  
  cat("  - Bin statistics:\n")
  for (i in 1:nrow(bin_stats)) {
    cat("    ", bin_stats$group[i], ": mean bin size =", sprintf("%.1f", bin_stats$mean_count[i]), 
        ", total observations =", bin_stats$total_obs[i], "\n")
  }
  
  # --------------------------------------------------------------------------
  # 9. CREATE RDD VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n9. Creating RDD visualization...\n")
  
  # Create RDD plot with binned scatter and fitted lines
  rdd_plot <- ggplot(df_binned, aes(x = bin_center, y = residuals_mean)) +
    # Add binned scatter points
    geom_point(aes(shape = group), size = 2, color = "black") +
    # Add fitted lines on both sides of threshold
    geom_smooth(
      data = df_filtered, 
      aes(x = x, y = residuals, group = ifelse(x <= threshold, "Inside Program", "Outside Program")),
      method = "lm", 
      formula = y ~ x, 
      size = 1, 
      color = "black",
      se = TRUE
    ) +
    # Add vertical line at threshold
    geom_vline(xintercept = threshold, color = "black", linewidth = 0.8) +
    # Formatting
    scale_x_continuous(
      labels = function(x) scales::comma(x, scale = 1e-3, suffix = "k"),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_y_continuous(
      labels = function(x) sprintf("%.3f", x),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_shape_manual(values = c("Inside Program" = 16, "Outside Program" = 17)) +
    labs(
      shape = "Program Status"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) 
  
  cat("✓ Created RDD visualization\n")
  
  # --------------------------------------------------------------------------
  # 10. CREATE COMBINED PLOT WITH AXIS LABELS
  # --------------------------------------------------------------------------
  cat("\n10. Creating combined plot with axis labels...\n")
  
  # Create combined plot with proper axis labels
  combined_plot <- gridExtra::grid.arrange(
    rdd_plot, 
    ncol = 1,
    bottom = grid::textGrob(
      "Distance to Program Frontier (km)", 
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    ),
    left = grid::textGrob(
      "Residuals", 
      rot = 90, 
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    ),
    # top = grid::textGrob(
    #   paste0("Regression Discontinuity Analysis: ", outcome_variable),
    #   gp = grid::gpar(fontsize = 16, fontface = "bold")
    # )
  )
  
  cat("✓ Created combined plot with axis labels\n")
  
  # --------------------------------------------------------------------------
  # 11. CALCULATE ADDITIONAL DIAGNOSTICS
  # --------------------------------------------------------------------------
  cat("\n11. Calculating additional diagnostics...\n")
  
  # McCrary density test for manipulation
  density_test <- tryCatch({
    # Simple density comparison around threshold
    left_density <- sum(df_filtered$x > -1000 & df_filtered$x <= 0)
    right_density <- sum(df_filtered$x > 0 & df_filtered$x <= 1000)
    density_ratio <- right_density / left_density
    list(left = left_density, right = right_density, ratio = density_ratio)
  }, error = function(e) {
    list(left = NA, right = NA, ratio = NA)
  })
  

  cat("✓ Calculated additional diagnostics\n")
  cat("  - Density test results:\n")
  cat("    Left side (≤0):", density_test$left, "observations\n")
  cat("    Right side (>0):", density_test$right, "observations\n")
  cat("    Density ratio:", sprintf("%.3f", density_test$ratio), "\n")
  

  # --------------------------------------------------------------------------
  # 12. SAVE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n12. Saving visualization...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Define output file path
  output_file <- file.path(path_figures, "RDD_FN1995_signal.png")
  
  # Save the plot 
  ggsave(
    filename = output_file,
    plot = combined_plot,
    width = figure_width,
    height = figure_height,
    dpi = 300,
    bg = "white"
  )
  
  cat("✓ Saved visualization\n")
  cat("  - Output file:", output_file, "\n")
  cat("  - Dimensions:", figure_width, "x", figure_height, "pixels\n")
  
  # --------------------------------------------------------------------------
  # 13. COMPILE COMPREHENSIVE RESULTS
  # --------------------------------------------------------------------------
  cat("\n13. Compiling comprehensive results...\n")
  
  # Compile comprehensive results
  analysis_results <- list(
    plot = combined_plot,
    individual_plot = rdd_plot,
    regression_results = list(
      model = model,
      model_summary = model_summary
    ),
    diagnostics = list(
      density_test = density_test,
      bin_statistics = bin_stats
    ),
    processed_data = list(
      rdd_data = df_rdd,
      analysis_data = df_filtered,
      binned_data = df_binned
    ),
    metadata = list(
      outcome_variable = outcome_variable,
      rdd_bandwidth = rdd_bandwidth,
      analysis_bandwidth = analysis_bandwidth,
      threshold = threshold,
      num_bins = num_bins,
      controls_used = controls,
      output_file = output_file,
      creation_date = Sys.time()
    )
  )
  
  cat("✓ Compiled comprehensive results\n")
  cat("  - Result components:", length(analysis_results), "\n")
  
  # --------------------------------------------------------------------------
  # 14. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n14. Results Summary:\n")
  
  cat("  - Analysis Overview:\n")
  cat("    Outcome variable:", outcome_variable, "\n")
  cat("    RDD bandwidth:", scales::comma(rdd_bandwidth), "meters\n")
  cat("    Analysis bandwidth:", scales::comma(analysis_bandwidth), "meters\n")
  cat("    Binning resolution:", num_bins, "bins\n")
  
  cat("\n  - Data Coverage:\n")
  cat("    Total municipalities in bandwidth:", nrow(df_rdd), "\n")
  cat("    Analysis sample:", nrow(df_filtered), "\n")


  cat("\n  - Model Performance:\n")
  cat("    R-squared:", sprintf("%.4f", model_summary$r.squared), "\n")
  cat("    Controls included:", length(controls), "\n")
  
  cat("\n  - Output:\n")
  cat("    Visualization saved to:", basename(output_file), "\n")
  cat("    Plot type: RDD binned scatter with fitted lines\n")
  
  cat("\n✓ RDD signal test analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("RDD SIGNAL ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

# Generate RDD placebo test analysis
generate_rdd_placebo_analysis(processed_data_path, path_figures)