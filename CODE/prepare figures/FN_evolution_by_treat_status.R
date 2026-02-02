# ==============================================================================
# FN Vote Share Evolution by Treatment Status Analysis
# ==============================================================================
# This script analyzes the evolution of Front National (FN) vote share between
# 1988 and 2002, comparing municipalities by ZRR treatment status with and
# without controls, using binned scatter plots to examine treatment effects
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate FN vote share evolution comparison by treatment status
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 12)
#' @param figure_height Height of output figure in inches (default: 12)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param baseline_year Baseline year for comparison (default: 1988)
#' @param comparison_year Comparison year for evolution analysis (default: 2002)
#' @param treatment_year ZRR treatment implementation year (default: 1995)
#' @param num_bins Number of bins for binned scatter plots (default: 50)
#' @param baseline_threshold Maximum baseline vote share threshold for filtering (default: 0.3)
#' @return List containing combined plot, individual plots, and analysis statistics
generate_fn_evolution_analysis <- function(processed_data_path, path_figures,
                                           figure_width = 12,
                                           figure_height = 12,
                                           figure_dpi = 300,
                                           baseline_year = 1988,
                                           comparison_year = 2002,
                                           treatment_year = 1995,
                                           num_bins = 50,
                                           baseline_threshold = 0.3) {
  
  cat("===============================================\n")
  cat("FN VOTE SHARE EVOLUTION ANALYSIS\n")
  cat("===============================================\n")
  cat("Baseline year:", baseline_year, "\n")
  cat("Comparison year:", comparison_year, "\n")
  cat("Treatment year:", treatment_year, "\n")
  cat("Number of bins:", num_bins, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("Baseline threshold:", baseline_threshold, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD AND VALIDATE DATA
  # --------------------------------------------------------------------------
  cat("1. Loading and validating data...\n")
  
  data_file <- file.path(processed_data_path, "dataDes.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded dataDes.RData\n")
  
  # Check if required data exists
  if (!exists("dfZRRControls")) {
    stop("Required dataset 'dfZRRControls' not found in loaded environment")
  }
  
  cat("  - Dataset 'dfZRRControls' available with", nrow(dfZRRControls), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2. DEFINE HELPER FUNCTIONS
  # --------------------------------------------------------------------------
  cat("\n2. Defining helper functions...\n")
  
  #' Clean dataframe by removing NA and infinite values
  #' @param df Input dataframe
  #' @param vars Variables to clean
  #' @return Cleaned dataframe
  clean_df <- function(df, vars) {
    initial_rows <- nrow(df)
    for (var in vars) {
      df <- df[!is.na(df[[var]]) & !is.infinite(df[[var]]), ]
    }
    final_rows <- nrow(df)
    cat("    Cleaned", length(vars), "variables:", initial_rows, "->", final_rows, "rows\n")
    return(df)
  }
  
  cat("✓ Defined helper functions\n")
  
  # --------------------------------------------------------------------------
  # 3. PREPARE BASE DATASET
  # --------------------------------------------------------------------------
  cat("\n3. Preparing base dataset...\n")
  
  # Create treatment and time variables
  dfZRRControls <- dfZRRControls %>%
    mutate(
      time_since_open = year - year_treat,
      treated = ifelse(year_treat == treatment_year, 1, 0),
      post = ifelse(year >= treatment_year, 1, 0),
      did = post * treated,
      logpop = log(pop)
    ) 
  
  cat("✓ Created treatment and time variables\n")
  cat("  - Added time_since_open, treated, post, did, logpop variables\n")
  
  controls_to_use <- setdiff(controls, c("revenuPerK", "popYoungOld", "FN1988"))
  cat("  - ", length(controls_to_use), "control variables for regression analysis\n")
  
  # --------------------------------------------------------------------------
  # 4. PREPARE DATA FOR PANEL A (NO CONTROLS)
  # --------------------------------------------------------------------------
  cat("\n4. Preparing data for Panel A (no controls)...\n")
  
  # Filter data for Panel A analysis
  dfSpe <- dfZRRControls %>% 
    filter(
      year_treat > 0, 
      year %in% c(baseline_year, comparison_year), 
      !is.na(FN), 
      !is.na(treatment)
    ) 
  
  cat("  - Filtered to municipalities with treatment and required years\n")
  cat("  - Initial observations:", nrow(dfSpe), "\n")
  
  # Clean Panel A data
  variables_to_clean <- c("codecommune", "FN", "year", controls_to_use, "post", "treated", "year_treat", "time_since_open", "dep")
  dfSpe <- dfSpe %>% 
    select(all_of(variables_to_clean)) %>%
    distinct()
  
  dfSpe <- clean_df(dfSpe, variables_to_clean)
  
  cat("✓ Prepared Panel A dataset\n")
  cat("  - Final observations:", nrow(dfSpe), "\n")
  cat("  - Variables included:", length(variables_to_clean), "\n")
  
  # --------------------------------------------------------------------------
  # 5. CREATE PANEL A PLOT DATA (NO CONTROLS)
  # --------------------------------------------------------------------------
  cat("\n5. Creating Panel A plot data (no controls)...\n")
  
  # Reshape data for Panel A visualization
  plot_df_a <- dfSpe %>%
    filter(year %in% c(baseline_year, comparison_year)) %>%
    select(codecommune, FN, year, year_treat) %>%
    pivot_wider(names_from = year, values_from = FN, values_fn = mean) %>%
    rename(
      FN1988 = !!paste0(baseline_year), 
      FN2002 = !!paste0(comparison_year)
    ) %>%
    drop_na(FN1988, FN2002) %>%
    mutate(zrr_1995 = factor(
      year_treat == treatment_year, 
      levels = c(TRUE, FALSE), 
      labels = c(as.character(treatment_year), "after 2004")
    ))
  
  cat("  - Reshaped data to wide format\n")
  cat("  - Municipalities with both years:", nrow(plot_df_a), "\n")
  
  # Create binned summary for Panel A
  # df_binned_summary_a <- plot_df_a %>%
  #   filter(FN1988 < baseline_threshold) %>%
  #   mutate(bin = ntile(FN1988, num_bins)) %>%
  #   group_by(bin, zrr_1995) %>%
  #   summarize(
  #     mean_FN1988 = mean(FN1988, na.rm = TRUE),
  #     mean_FN2002 = mean(FN2002, na.rm = TRUE),
  #     count = n(),
  #     .groups = "drop"
  #   )
  
  # Create bins with equal number of observations within each bin
  df_binned_summary_a <- plot_df_a %>%
    filter(FN1988 < baseline_threshold) %>%
    group_by(zrr_1995) %>%
    mutate(bin = ntile(FN1988, num_bins)) %>%   # equal obs within each treatment group
    ungroup() %>%
    group_by(bin, zrr_1995) %>%
    summarize(
      mean_FN1988 = mean(FN1988, na.rm = TRUE),
      mean_FN2002 = mean(FN2002, na.rm = TRUE),
      count       = n(),
      .groups = "drop"
    )
  
  
  cat("✓ Created Panel A binned data\n")
  cat("  - Applied baseline threshold (<", baseline_threshold, ")\n")
  cat("  - Binned observations:", nrow(df_binned_summary_a), "\n")
  
  # Calculate Panel A statistics
  panel_a_stats <- plot_df_a %>%
    filter(FN1988 < baseline_threshold) %>%
    group_by(zrr_1995) %>%
    summarise(
      n_municipalities = n(),
      mean_change = mean(FN2002 - FN1988, na.rm = TRUE),
      sd_change = sd(FN2002 - FN1988, na.rm = TRUE),
      correlation = cor(FN1988, FN2002, use = "complete.obs"),
      .groups = 'drop'
    )
  
  cat("  - Panel A statistics calculated by treatment group\n")
  
  # --------------------------------------------------------------------------
  # 6. CREATE PANEL A VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n6. Creating Panel A visualization...\n")
  
  plot1 <- ggplot(df_binned_summary_a, aes(x = mean_FN1988, y = mean_FN2002, shape = zrr_1995, color = zrr_1995)) +
    geom_point(size = 1.5) +
    scale_shape_manual(values = c("1995" = 17, "after 2004" = 15)) +
    scale_color_manual(values = c("1995" = "black", "after 2004" = "grey")) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = paste0("Vote share for FN in ", baseline_year, " (binned)"),
      y = paste0("Vote share for FN in ", comparison_year),
      title = "Panel A: no control",
      shape = "ZRR Year",
      color = "ZRR Year"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  cat("✓ Created Panel A visualization\n")
  
  # --------------------------------------------------------------------------
  # 7. PREPARE DATA FOR PANEL B (WITH CONTROLS)
  # --------------------------------------------------------------------------
  cat("\n7. Preparing data for Panel B (with controls)...\n")
  
  # Filter and prepare data for Panel B with controls
  df_filtered <- dfZRRControls %>%
    filter(year %in% c(baseline_year, comparison_year)) %>%
    mutate(
      treated = ifelse(year_treat == treatment_year, 1, 0),
      post = ifelse(year >= treatment_year, 1, 0)
    ) %>%
    select(codecommune, FN, post, treated, year_treat, time_since_open, dep, all_of(controls_to_use)) %>%
    drop_na() %>%
    arrange(codecommune)
  
  df_filtered <- clean_df(df_filtered, setdiff(variables_to_clean, "year"))
  
  cat("  - Filtered and cleaned control variables data\n")
  cat("  - Initial Panel B observations:", nrow(df_filtered), "\n")
  
  # Keep only communes with both years
  communes_keep <- df_filtered %>% 
    count(codecommune) %>% 
    filter(n == 2) %>% 
    pull(codecommune)
  
  df_filtered <- df_filtered %>% 
    filter(codecommune %in% communes_keep) %>%
    group_by(codecommune, year_treat, post) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  cat("  - Balanced panel: municipalities with both years:", length(communes_keep), "\n")
  
  # --------------------------------------------------------------------------
  # 8. CALCULATE RESIDUALS FOR PANEL B
  # --------------------------------------------------------------------------
  cat("\n8. Calculating residuals for Panel B...\n")
  
  # Reshape data for regression analysis
  df_spread <- df_filtered %>%
    pivot_wider(names_from = post, values_from = c(FN, all_of(controls_to_use))) %>%
    ungroup() %>%
    mutate(zrr_1995 = factor(
      year_treat == treatment_year, 
      levels = c(TRUE, FALSE), 
      labels = c(as.character(treatment_year), "after 2004")
    ))
  
  cat("  - Reshaped data for regression analysis\n")
  
  # Prepare regression datasets
  df_spread <- inner_join(
    clean_df(
      df_spread %>% select(c("codecommune", "FN_0", "year_treat", paste0(controls_to_use, "_0"), "dep", "zrr_1995")),
      c("codecommune", "FN_0", paste0(controls_to_use, "_0"), "dep", "zrr_1995")
    ),
    clean_df(
      df_spread %>% select(c("codecommune", "FN_1", "dep")),
      c("codecommune", "FN_1", "dep")
    ), 
    by = c("codecommune", "dep")
  )
  
  cat("  - Merged pre- and post-treatment data\n")
  cat("  - Final regression dataset:", nrow(df_spread), "municipalities\n")
  
  # Run regressions to obtain residuals
  regression_formula_0 <- as.formula(paste("FN_0 ~", paste(paste0(controls_to_use, "_0"), collapse = " + "), "+ dep"))
  regression_formula_1 <- as.formula(paste("FN_1 ~", paste(paste0(controls_to_use, "_0"), collapse = " + "), "+ dep"))
  
  regression_0 <- lm(regression_formula_0, data = df_spread)
  regression_1 <- lm(regression_formula_1, data = df_spread)
  
  cat("  - Fitted regression models for residual calculation\n")
  
  # Extract residuals
  df_spread <- df_spread %>%
    mutate(
      residuals_0 = residuals(regression_0),
      residuals_1 = residuals(regression_1)
    )
  
  cat("✓ Calculated residuals for Panel B\n")
  cat("  - Regression R² (baseline):", sprintf("%.3f", summary(regression_0)$r.squared), "\n")
  cat("  - Regression R² (comparison):", sprintf("%.3f", summary(regression_1)$r.squared), "\n")
  
  # --------------------------------------------------------------------------
  # 9. CREATE PANEL B PLOT DATA (WITH CONTROLS)
  # --------------------------------------------------------------------------
  cat("\n9. Creating Panel B plot data (with controls)...\n")
  
  # Create binned summary for Panel B using residuals
  # df_binned_summary_b <- df_spread %>%
  #   mutate(bin = ntile(residuals_0, num_bins)) %>%
  #   group_by(bin, zrr_1995) %>%
  #   summarize(
  #     mean_FN1988 = mean(residuals_0, na.rm = TRUE),
  #     mean_FN2002 = mean(residuals_1, na.rm = TRUE),
  #     count = n(),
  #     .groups = "drop"
  #   )
  
  df_binned_summary_b <- df_spread %>%
    group_by(zrr_1995) %>%
    mutate(bin = ntile(residuals_0, num_bins)) %>%   # equal obs within each treatment group
    ungroup() %>%
    group_by(bin, zrr_1995) %>%
    summarize(
      mean_FN1988 = mean(residuals_0, na.rm = TRUE),
      mean_FN2002 = mean(residuals_1, na.rm = TRUE),
      count       = n(),
      .groups = "drop"
    )
  
  
  cat("✓ Created Panel B binned data\n")
  cat("  - Binned observations:", nrow(df_binned_summary_b), "\n")
  
  # Calculate Panel B statistics
  panel_b_stats <- df_spread %>%
    group_by(zrr_1995) %>%
    summarise(
      n_municipalities = n(),
      mean_change = mean(residuals_1 - residuals_0, na.rm = TRUE),
      sd_change = sd(residuals_1 - residuals_0, na.rm = TRUE),
      correlation = cor(residuals_0, residuals_1, use = "complete.obs"),
      .groups = 'drop'
    )
  
  cat("  - Panel B statistics calculated by treatment group\n")
  
  # --------------------------------------------------------------------------
  # 10. CREATE PANEL B VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n10. Creating Panel B visualization...\n")
  
  plot2 <- ggplot(df_binned_summary_b, aes(x = mean_FN1988, y = mean_FN2002, shape = zrr_1995, color = zrr_1995)) +
    geom_point(size = 1.5) +
    scale_shape_manual(values = c("1995" = 17, "after 2004" = 15)) +
    scale_color_manual(values = c("1995" = "black", "after 2004" = "grey")) +
    scale_x_continuous(labels = function(x) sprintf("%.2f", x)) +
    scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
    labs(
      x = paste0("Vote share for FN in ", baseline_year, " (residuals)"),
      y = paste0("Vote share for FN in ", comparison_year, " (residuals)"),
      title = "Panel B: with controls",
      shape = "ZRR Year",
      color = "ZRR Year"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  cat("✓ Created Panel B visualization\n")
  
  # --------------------------------------------------------------------------
  # 11. CREATE COMBINED VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n11. Creating combined visualization...\n")
  
  # Combine plots using patchwork
  combined_plot <- plot1 / plot2
  
  cat("✓ Created combined visualization\n")
  cat("  - Layout: 2 panels (vertical arrangement)\n")
  
  # --------------------------------------------------------------------------
  # 12. CALCULATE COMPREHENSIVE STATISTICS
  # --------------------------------------------------------------------------
  cat("\n12. Calculating comprehensive statistics...\n")
  
  # Combine statistics from both panels
  comprehensive_stats <- list(
    panel_a = panel_a_stats,
    panel_b = panel_b_stats,
    regression_summaries = list(
      baseline_model = summary(regression_0),
      comparison_model = summary(regression_1)
    ),
    data_coverage = list(
      panel_a_municipalities = nrow(plot_df_a),
      panel_b_municipalities = nrow(df_spread),
      baseline_year = baseline_year,
      comparison_year = comparison_year,
      treatment_year = treatment_year
    )
  )
  
  cat("✓ Calculated comprehensive statistics\n")
  
  # Display key statistics
  cat("  - Panel A treatment effects:\n")
  for (i in 1:nrow(panel_a_stats)) {
    group <- panel_a_stats$zrr_1995[i]
    change <- panel_a_stats$mean_change[i]
    cat("    ", group, ": mean change =", sprintf("%.3f", change), "\n")
  }
  
  cat("  - Panel B treatment effects (residuals):\n")
  for (i in 1:nrow(panel_b_stats)) {
    group <- panel_b_stats$zrr_1995[i]
    change <- panel_b_stats$mean_change[i]
    cat("    ", group, ": mean change =", sprintf("%.3f", change), "\n")
  }
  
  # --------------------------------------------------------------------------
  # 13. SAVE COMBINED VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n13. Saving combined visualization...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Define output file path
  output_file <- file.path(path_figures, "FN_evolution_by_treat_status.png")
  
  # Save the combined plot
  ggsave(
    filename = output_file,
    plot = combined_plot,
    width = figure_width,
    height = figure_height,
    dpi = figure_dpi,
    bg = "white"
  )
  
  cat("✓ Saved combined visualization\n")
  cat("  - Output file:", output_file, "\n")
  cat("  - Dimensions:", figure_width, "x", figure_height, "inches\n")
  cat("  - Resolution:", figure_dpi, "DPI\n")
  
  # --------------------------------------------------------------------------
  # 14. COMPILE COMPREHENSIVE RESULTS
  # --------------------------------------------------------------------------
  cat("\n14. Compiling comprehensive results...\n")
  
  # Create comprehensive results object
  analysis_results <- list(
    combined_plot = combined_plot,
    individual_plots = list(
      panel_a_no_controls = plot1,
      panel_b_with_controls = plot2
    ),
    statistics = comprehensive_stats,
    processed_data = list(
      panel_a_data = plot_df_a,
      panel_b_data = df_spread,
      binned_data_a = df_binned_summary_a,
      binned_data_b = df_binned_summary_b
    ),
    metadata = list(
      baseline_year = baseline_year,
      comparison_year = comparison_year,
      treatment_year = treatment_year,
      num_bins = num_bins,
      baseline_threshold = baseline_threshold,
      controls_used = controls,
      output_file = output_file,
      creation_date = Sys.time()
    )
  )
  
  cat("✓ Compiled comprehensive results\n")
  cat("  - Result components:", length(analysis_results), "\n")
  cat("  - Both panels available for further analysis\n")
  
  # --------------------------------------------------------------------------
  # 15. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n15. Results Summary:\n")
  
  cat("  - Analysis Overview:\n")
  cat("    Evolution period:", baseline_year, "→", comparison_year, "\n")
  cat("    Treatment year:", treatment_year, "\n")
  cat("    Baseline threshold:", baseline_threshold, "\n")
  cat("    Binning resolution:", num_bins, "bins\n")
  
  cat("\n  - Data Coverage:\n")
  cat("    Panel A municipalities:", nrow(plot_df_a), "\n")
  cat("    Panel B municipalities:", nrow(df_spread), "\n")
  cat("    Control variables used:", length(controls), "\n")
  
  cat("\n  - Model Performance:\n")
  cat("    Baseline regression R²:", sprintf("%.3f", summary(regression_0)$r.squared), "\n")
  cat("    Comparison regression R²:", sprintf("%.3f", summary(regression_1)$r.squared), "\n")
  
  cat("\n  - Output:\n")
  cat("    Combined visualization saved to:", basename(output_file), "\n")
  cat("    Panel arrangement: 2 panels (vertical)\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("FN EVOLUTION ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

# Generate FN vote share evolution analysis
generate_fn_evolution_analysis(processed_data_path, path_figures)