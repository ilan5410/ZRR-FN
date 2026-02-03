# ==============================================================================
# FN Vote Share vs Population Analysis - Temporal Evolution
# ==============================================================================
# This script analyzes the relationship between municipality population size
# and Front National (FN) vote share across multiple election years, examining
# how demographic patterns influence electoral support through binned scatter
# plots and density overlays for treated municipalities
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate FN vote share vs population temporal analysis
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 10)
#' @param figure_height Height of output figure in inches (default: 8)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param analysis_years Vector of years to analyze (default: c("1988", "1995", "2002", "2007", "2017", "2022"))
#' @param population_variable Variable name for population data (default: "logpop")
#' @param num_bins Number of bins for scatter plot (default: 50)
#' @param density_scale_factor Scale factor for density visualization (default: 0.35)
#' @param min_population_threshold Minimum population threshold for analysis (default: 10000000)
#' @return List containing combined plot, individual plots, and correlation statistics
generate_fn_population_analysis <- function(processed_data_path, path_figures,
                                            figure_width = 10,
                                            figure_height = 8,
                                            figure_dpi = 300,
                                            analysis_years = c("1988", "1995", "2002", "2007", "2017", "2022"),
                                            population_variable = "logpop",
                                            num_bins = 50,
                                            density_scale_factor = 0.35,
                                            min_population_threshold = 10000000
) {
  
  cat("===============================================\n")
  cat("FN VOTE SHARE vs POPULATION ANALYSIS\n")
  cat("===============================================\n")
  cat("Analysis years:", paste(analysis_years, collapse = ", "), "\n")
  cat("Population variable:", population_variable, "\n")
  cat("Number of bins:", num_bins, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("Density scale factor:", density_scale_factor, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD FN GROWTH DATA
  # --------------------------------------------------------------------------
  cat("1. Loading FN growth data...\n")
  
  data_file <- file.path(processed_data_path, "FN_growth.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded FN_growth.RData\n")
  
  # Check if required data exists
  if (!exists("dfZRRControls")) {
    stop("Required dataset 'dfZRRControls' not found in loaded environment")
  }
  
  cat("  - Dataset 'dfZRRControls' available with", nrow(dfZRRControls), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE POPULATION VARIABLE
  # --------------------------------------------------------------------------
  cat("\n2. Preparing population variable...\n")
  
  # Create log population variable if it doesn't exist
  if (population_variable == "logpop" && !"logpop" %in% names(dfZRRControls)) {
    dfZRRControls[["logpop"]] <- log(dfZRRControls[["pop"]])
    cat("  - Created log population variable\n")
  }
  
  # Verify required variables exist
  required_vars <- c(population_variable, "FN", "pop", "treatment", "codecommune", "year")
  missing_vars <- setdiff(required_vars, names(dfZRRControls))
  
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  cat("✓ Prepared population variable\n")
  cat("  - Population variable (", population_variable, ") available\n")
  cat("  - All required variables present\n")
  
  # Check population variable statistics
  pop_stats <- dfZRRControls %>%
    filter(!is.na(.data[[population_variable]]), !is.infinite(.data[[population_variable]])) %>%
    summarise(
      n_obs = n(),
      min_val = min(.data[[population_variable]], na.rm = TRUE),
      max_val = max(.data[[population_variable]], na.rm = TRUE),
      mean_val = mean(.data[[population_variable]], na.rm = TRUE),
      median_val = median(.data[[population_variable]], na.rm = TRUE)
    )
  
  cat("  - Population variable statistics:\n")
  cat("    Observations:", pop_stats$n_obs, "\n")
  cat("    Range:", sprintf("%.2f", pop_stats$min_val), "to", sprintf("%.2f", pop_stats$max_val), "\n")
  cat("    Mean:", sprintf("%.2f", pop_stats$mean_val), "\n")
  cat("    Median:", sprintf("%.2f", pop_stats$median_val), "\n")
  
  # --------------------------------------------------------------------------
  # 3. VALIDATE ANALYSIS YEARS DATA AVAILABILITY
  # --------------------------------------------------------------------------
  cat("\n3. Validating analysis years data availability...\n")
  
  # Check data availability for each year
  year_availability <- dfZRRControls %>%
    filter(year %in% analysis_years) %>%
    group_by(year) %>%
    summarise(
      n_municipalities = n_distinct(codecommune),
      n_observations = n(),
      complete_cases = sum(!is.na(FN) & !is.na(.data[[population_variable]]) & 
                             !is.infinite(.data[[population_variable]])),
      mean_fn = mean(FN, na.rm = TRUE),
      mean_pop = mean(.data[[population_variable]], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(year)
  
  cat("✓ Validated analysis years\n")
  cat("  - Data availability by year:\n")
  for (i in 1:nrow(year_availability)) {
    year <- year_availability$year[i]
    cat("    ", year, ": ", 
        year_availability$complete_cases[i], " complete cases, ",
        "FN mean = ", sprintf("%.1f%%", year_availability$mean_fn[i] * 100), "\n")
  }
  
  # Filter to years with sufficient data
  sufficient_years <- year_availability$year[year_availability$complete_cases >= 100]
  if (length(sufficient_years) < length(analysis_years)) {
    warning("Some years have insufficient data: ", 
            paste(setdiff(analysis_years, sufficient_years), collapse = ", "))
  }
  
  final_years <- intersect(analysis_years, sufficient_years)
  cat("  - Final analysis years:", paste(final_years, collapse = ", "), "\n")
  
  # --------------------------------------------------------------------------
  # 4. DEFINE PLOT CREATION HELPER FUNCTION
  # --------------------------------------------------------------------------
  cat("\n4. Defining plot creation functions...\n")
  
  #' Create FN vs population plots for multiple years
  #' @param dfReg Input regression dataframe
  #' @param x_var Variable name for x-axis (population)
  #' @param var_lab Variable label for x-axis
  #' @param years Vector of years to analyze
  #' @param num_bins Number of bins for binned scatter plot
  #' @param density_scale Scale factor for density overlay
  #' @return List containing individual plots and summary statistics
  create_fn_population_plots <- function(dfReg, x_var, var_lab, years, num_bins, density_scale) {
    
    cat("    - Processing", length(years), "years for FN vs", x_var, "analysis\n")
    
    # --------------------------------------------------------------------------
    # 4a. FILTER AND PREPARE DATA
    # --------------------------------------------------------------------------
    cat("      Filtering and preparing data...\n")
    
    # Filter and select relevant variables
    dfReg_filtered <- dfReg %>%
      filter(year %in% years) %>%
      select(codecommune, year, x = !!sym(x_var), FN, pop, treatment) %>%
      group_by(codecommune, year) %>%
      filter(n() == 1) %>%  # Remove duplicates
      ungroup() %>%
      filter(!is.na(x), !is.na(FN), !is.infinite(x), !is.infinite(FN))
    
    cat("        Filtered data:", nrow(dfReg_filtered), "observations\n")
    
    # Reshape to wide format for cross-year analysis
    dfReg_wide <- dfReg_filtered %>%
      pivot_wider(
        names_from = year, 
        values_from = c(x, FN, pop, treatment), 
        names_sep = ""
      ) %>%
      filter_all(all_vars(!is.infinite(.)))
    
    cat("        Wide format data:", nrow(dfReg_wide), "municipalities\n")
    
    # --------------------------------------------------------------------------
    # 4b. GENERATE INDIVIDUAL YEAR PLOTS
    # --------------------------------------------------------------------------
    cat("      Generating individual year plots...\n")
    
    panel_labels <- paste0("Panel ", LETTERS[1:length(years)], ": ")
    plots_list <- list()
    correlation_stats <- data.frame()
    
    for (i in seq_along(years)) {
      year <- years[i]
      cat("        Processing year", year, "...\n")
      
      # Define variable names for current year
      xVar <- paste0("x", year)
      yVar <- paste0("FN", year)
      popVar <- paste0("pop", year)
      treatVar <- paste0("treatment", year)
      
      # Prepare data for current year
      df_plot <- dfReg_wide %>%
        filter(!is.na(!!sym(xVar)), !is.na(!!sym(yVar)))
      
      # Calculate correlation and basic statistics
      correlation <- cor(df_plot[[xVar]], df_plot[[yVar]], 
                         use = "complete.obs", method = "pearson")
      obs_count <- nrow(df_plot)
      
      cat("          Correlation:", sprintf("%.3f", correlation), "\n")
      cat("          Observations:", obs_count, "\n")
      
      # Store correlation statistics
      correlation_stats <- rbind(correlation_stats, data.frame(
        year = year,
        correlation = correlation,
        observations = obs_count,
        mean_x = mean(df_plot[[xVar]], na.rm = TRUE),
        mean_y = mean(df_plot[[yVar]], na.rm = TRUE),
        sd_x = sd(df_plot[[xVar]], na.rm = TRUE),
        sd_y = sd(df_plot[[yVar]], na.rm = TRUE)
      ))
      
      # --------------------------------------------------------------------------
      # 4c. CREATE BINNED SCATTER DATA
      # --------------------------------------------------------------------------
      
      # Create bins for scatter plot
      bin_var <- paste0(xVar, "_bins")
      df_binned <- df_plot %>%
        mutate(!!bin_var := cut(!!sym(xVar), breaks = num_bins, labels = FALSE)) %>%
        group_by(!!sym(bin_var)) %>%
        summarise(
          bin_center = mean(!!sym(xVar), na.rm = TRUE),
          mean_y = mean(!!sym(yVar), na.rm = TRUE),
          count = n(),
          se_y = sd(!!sym(yVar), na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        ) %>%
        filter(!is.na(bin_center), !is.na(mean_y))
      
      cat("          Binned data points:", nrow(df_binned), "\n")
      
      # --------------------------------------------------------------------------
      # 4d. PREPARE DENSITY OVERLAY FOR TREATED UNITS
      # --------------------------------------------------------------------------
      
      # Filter treated municipalities
      treated_df <- dfReg_wide %>%
        filter(!is.na(!!sym(xVar)), !!sym(treatVar) == TRUE)
      
      cat("          Treated municipalities:", nrow(treated_df), "\n")
      
      # Compute density for treated units
      if (nrow(treated_df) > 10) {  # Only if sufficient treated units
        density_data <- density(treated_df[[xVar]], na.rm = TRUE)
        density_df <- data.frame(
          x = density_data$x, 
          y = density_data$y * density_scale / max(density_data$y)
        )
      } else {
        density_df <- data.frame(x = numeric(0), y = numeric(0))
      }
      
      # --------------------------------------------------------------------------
      # 4e. CREATE PLOT WITH ENHANCED AESTHETICS
      # --------------------------------------------------------------------------
      
      # Define axis breaks and labels for log scale
      if (x_var == "logpop") {
        breaks <- log(c(100, 1000, 10000, 100000, 1000000))
        labels <- scales::comma(c(0.1, 1, 10, 100, 1000))
      } else {
        breaks <- scales::pretty_breaks(n = 6)(df_plot[[xVar]])
        labels <- scales::comma(breaks)
      }
      
      # Create the plot
      p <- ggplot(df_binned, aes(x = bin_center, y = mean_y)) +
        # Add density overlay if available
        {if (nrow(density_df) > 0) {
          list(
            geom_area(data = density_df, aes(x = x, y = y), 
                      inherit.aes = FALSE, fill = "red", alpha = 0.1),
            geom_line(data = density_df, aes(x = x, y = y), 
                      inherit.aes = FALSE, color = "red", alpha = 0.4, linewidth = 0.5)
          )
        }} +
        # Add binned scatter points
        geom_point(alpha = 0.6, size = 1.2, color = "black") +
        # Add polynomial trend line
        geom_smooth(
          method = "lm", 
          formula = y ~ poly(x, 2), 
          se = FALSE, 
          color = "blue", 
          linewidth = 0.8,
          alpha = 0.8
        ) +
        # Formatting and labels
        scale_y_continuous(
          limits = c(0, density_scale),
          labels = scales::percent_format(accuracy = 1)
        ) +
        scale_x_continuous(
          breaks = breaks,
          labels = labels
        ) +
        labs(title = paste0(panel_labels[i], year)) +
        # Add correlation and observation count annotations
        annotate("text", x = Inf, y = Inf, 
                 label = paste0("r = ", sprintf("%.2f", correlation)),
                 hjust = 1.1, vjust = 2.0, size = 3, color = "black", fontface = "bold") +
        annotate("text", x = Inf, y = Inf, 
                 label = paste0("n = ", scales::comma(obs_count)),
                 hjust = 1.1, vjust = 3.5, size = 3, color = "black") +
        # Theme and styling
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
          axis.text = element_text(size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.3, colour = scales::alpha("black", 0.7)),
          plot.margin = ggplot2::margin(10, 10, 10, 10)
        )
      
      plots_list[[i]] <- p
      cat("          ✓ Created plot for", year, "\n")
    }

    # Remove any NULL entries from failed plots
    plots_list <- Filter(Negate(is.null), plots_list)

    cat("      ✓ Generated", length(plots_list), "individual plots\n")
    
    return(list(
      plots = plots_list,
      correlation_stats = correlation_stats,
      processed_data = dfReg_wide
    ))
  }
  
  cat("✓ Defined plot creation functions\n")
  
  # --------------------------------------------------------------------------
  # 5. GENERATE PLOTS AND STATISTICS
  # --------------------------------------------------------------------------
  cat("\n5. Generating plots and calculating statistics...\n")
  
  plot_results <- create_fn_population_plots(
    dfReg = dfZRRControls,
    x_var = population_variable,
    var_lab = "Population size (log-scale), in 1,000",
    years = final_years,
    num_bins = num_bins,
    density_scale = density_scale_factor
  )
  
  plots_list <- plot_results$plots
  correlation_stats <- plot_results$correlation_stats
  
  cat("✓ Generated plots and statistics\n")
  cat("  - Individual plots created:", length(plots_list), "\n")
  cat("  - Correlation statistics calculated for", nrow(correlation_stats), "years\n")
  
  # --------------------------------------------------------------------------
  # 6. ANALYZE CORRELATION PATTERNS ACROSS TIME
  # --------------------------------------------------------------------------
  cat("\n6. Analyzing correlation patterns across time...\n")
  
  # Calculate temporal trends in correlation
  if (nrow(correlation_stats) > 1) {
    correlation_trend <- lm(correlation ~ as.numeric(year), data = correlation_stats)
    trend_slope <- coef(correlation_trend)[2]
    trend_p_value <- summary(correlation_trend)$coefficients[2, 4]
  } else {
    trend_slope <- NA
    trend_p_value <- NA
  }
  
  # Summary statistics
  correlation_summary <- correlation_stats %>%
    summarise(
      mean_correlation = mean(correlation, na.rm = TRUE),
      sd_correlation = sd(correlation, na.rm = TRUE),
      min_correlation = min(correlation, na.rm = TRUE),
      max_correlation = max(correlation, na.rm = TRUE),
      trend_slope = trend_slope,
      trend_p_value = trend_p_value,
      .groups = 'drop'
    )
  
  cat("✓ Analyzed correlation patterns\n")
  cat("  - Correlation statistics:\n")
  cat("    Mean correlation:", sprintf("%.3f", correlation_summary$mean_correlation), "\n")
  cat("    Correlation range:", sprintf("%.3f", correlation_summary$min_correlation), 
      "to", sprintf("%.3f", correlation_summary$max_correlation), "\n")
  
  if (!is.na(trend_slope)) {
    cat("    Temporal trend slope:", sprintf("%.4f", trend_slope), "\n")
    cat("    Trend significance (p-value):", sprintf("%.3f", trend_p_value), "\n")
  }
  
  # Display year-by-year correlations
  cat("  - Year-by-year correlations:\n")
  for (i in 1:nrow(correlation_stats)) {
    year <- correlation_stats$year[i]
    corr <- correlation_stats$correlation[i]
    obs <- correlation_stats$observations[i]
    cat("    ", year, ": r =", sprintf("%.3f", corr), "(n =", scales::comma(obs), ")\n")
  }
  
  # --------------------------------------------------------------------------
  # 7. CREATE COMBINED VISUALIZATION LAYOUT
  # --------------------------------------------------------------------------
  cat("\n7. Creating combined visualization layout...\n")
  
  # Filter out NULL plots
  plots_list <- Filter(Negate(is.null), plots_list)

  # Determine optimal grid layout
  n_plots <- length(plots_list)

  if (n_plots == 0) {
    stop("No valid plots were generated. Check data availability for analysis years.")
  }

  n_cols <- ceiling(sqrt(n_plots))
  n_rows <- ceiling(n_plots / n_cols)

  cat("  - Grid layout:", n_rows, "rows ×", n_cols, "columns\n")

  # Create combined plot with professional layout
  combined_plot <- gridExtra::grid.arrange(
    gridExtra::arrangeGrob(
      grobs = plots_list,
      nrow = n_rows,
      ncol = n_cols
    ),
    bottom = grid::textGrob(
      "Population size (log-scale), in 1,000", 
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    ),
    left = grid::textGrob(
      "FN Vote Share", 
      rot = 90, 
      gp = grid::gpar(fontsize = 14, fontface = "bold")
    ),
    # top = grid::textGrob(
    #   "Front National Vote Share vs Population Size Over Time",
    #   gp = grid::gpar(fontsize = 16, fontface = "bold")
    # )
  )
  
  cat("✓ Created combined visualization\n")
  cat("  - Layout: ", n_rows, "×", n_cols, "grid\n")
  cat("  - Individual panels:", length(plots_list), "\n")
  
  # --------------------------------------------------------------------------
  # 8. CALCULATE ADDITIONAL SUMMARY STATISTICS
  # --------------------------------------------------------------------------
  cat("\n8. Calculating additional summary statistics...\n")
  
  # Treatment distribution analysis
  treatment_stats <- dfZRRControls %>%
    filter(year %in% final_years, !is.na(treatment)) %>%
    group_by(year) %>%
    summarise(
      total_municipalities = n(),
      treated_municipalities = sum(treatment == TRUE, na.rm = TRUE),
      treatment_rate = mean(treatment == TRUE, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Population distribution analysis
  population_distribution <- dfZRRControls %>%
    filter(year %in% final_years, !is.na(.data[[population_variable]])) %>%
    group_by(year) %>%
    summarise(
      q25 = quantile(.data[[population_variable]], 0.25, na.rm = TRUE),
      median = median(.data[[population_variable]], na.rm = TRUE),
      q75 = quantile(.data[[population_variable]], 0.75, na.rm = TRUE),
      mean = mean(.data[[population_variable]], na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("✓ Calculated additional statistics\n")
  cat("  - Treatment rates by year:\n")
  for (i in 1:nrow(treatment_stats)) {
    year <- treatment_stats$year[i]
    rate <- treatment_stats$treatment_rate[i]
    treated <- treatment_stats$treated_municipalities[i]
    total <- treatment_stats$total_municipalities[i]
    cat("    ", year, ":", sprintf("%.1f%%", rate * 100), 
        "(", treated, "/", total, "municipalities )\n")
  }
  
  # --------------------------------------------------------------------------
  # 9. SAVE COMBINED VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n9. Saving combined visualization...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Define output file path
  output_file <- file.path(path_figures, "FN_versus_pop.png")
  
  # Save the combined plot
  ggsave(
    filename = output_file,
    plot = combined_plot,
    device = "png",
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
  # 10. COMPILE COMPREHENSIVE ANALYSIS RESULTS
  # --------------------------------------------------------------------------
  cat("\n10. Compiling comprehensive analysis results...\n")
  
  # Create comprehensive results object
  analysis_results <- list(
    combined_plot = combined_plot,
    individual_plots = plots_list,
    statistics = list(
      correlations = correlation_stats,
      correlation_summary = correlation_summary,
      treatment_stats = treatment_stats,
      population_distribution = population_distribution,
      year_availability = year_availability
    ),
    processed_data = plot_results$processed_data,
    metadata = list(
      analysis_years = final_years,
      population_variable = population_variable,
      num_bins = num_bins,
      density_scale_factor = density_scale_factor,
      output_file = output_file,
      creation_date = Sys.time()
    )
  )
  
  cat("✓ Compiled comprehensive results\n")
  cat("  - Result components:", length(analysis_results), "\n")
  cat("  - Statistical summaries included\n")
  cat("  - Individual plots available for further analysis\n")
  
  # --------------------------------------------------------------------------
  # 11. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n11. Results Summary:\n")
  
  cat("  - Analysis Overview:\n")
  cat("    Years analyzed:", paste(final_years, collapse = ", "), "\n")
  cat("    Population variable:", population_variable, "\n")
  cat("    Binning resolution:", num_bins, "bins\n")
  cat("    Total municipalities processed:", nrow(plot_results$processed_data), "\n")
  
  cat("\n  - Correlation Analysis:\n")
  strongest_year <- correlation_stats$year[which.max(abs(correlation_stats$correlation))]
  strongest_corr <- correlation_stats$correlation[which.max(abs(correlation_stats$correlation))]
  cat("    Strongest correlation:", strongest_year, "(r =", sprintf("%.3f", strongest_corr), ")\n")
  cat("    Average correlation:", sprintf("%.3f", correlation_summary$mean_correlation), "\n")
  
  if (!is.na(trend_slope)) {
    trend_direction <- ifelse(trend_slope > 0, "increasing", "decreasing")
    cat("    Temporal trend:", trend_direction, "(slope =", sprintf("%.4f", trend_slope), ")\n")
  }
  
  cat("\n  - Treatment Coverage:\n")
  avg_treatment_rate <- mean(treatment_stats$treatment_rate, na.rm = TRUE)
  total_treated <- sum(treatment_stats$treated_municipalities, na.rm = TRUE)
  cat("    Average treatment rate:", sprintf("%.1f%%", avg_treatment_rate * 100), "\n")
  cat("    Total treated municipalities:", total_treated, "\n")
  
  cat("\n  - Output:\n")
  cat("    Combined visualization saved to:", basename(output_file), "\n")
  cat("    Panel layout:", n_rows, "rows ×", n_cols, "columns\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("FN vs POPULATION ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

# Generate FN vote share vs population temporal analysis
generate_fn_population_analysis(processed_data_path, path_figures)