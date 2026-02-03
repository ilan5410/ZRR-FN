# ==============================================================================
# FN Support Growth Analysis - Locality Characteristics Over Time
# ==============================================================================
# This script analyzes the nonparametric effects of various locality 
# characteristics on Front National (FN) support over time, using 2002 as 
# reference year to examine how demographic and economic factors influence
# electoral patterns across multiple election cycles
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate FN support growth analysis by locality characteristics
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 10)
#' @param figure_height Height of output figure in inches (default: 8)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param reference_year Year to use as reference for analysis (default: 2002)
#' @param eu_election_years Vector of European election years (default: c(2017, 1994, 1999, 2004, 2014, 2019))
#' @param analysis_variables List of variables to analyze with panel titles
#' @return List containing combined plot, individual plots, and model results
generate_fn_growth_analysis <- function(processed_data_path, path_figures,
                                        figure_width = 10,
                                        figure_height = 8,
                                        figure_dpi = 300,
                                        reference_year = 2002,
                                        eu_election_years = c(2017, 1994, 1999, 2004, 2014, 2019),
                                        analysis_variables = list(
                                          "pop" = "Panel A: population size",
                                          "educNoDiplomaPerK" = "Panel B: proportion of no diploma",
                                          "ratEmp" = "Panel C: proportion of employed",
                                          "pouvr" = "Panel D: proportion of manual workers",
                                          "min_distance_to_agglo" = "Panel E: distance to closest agglomeration"
                                        )
) {
  
  cat("===============================================\n")
  cat("FN SUPPORT GROWTH ANALYSIS - LOCALITY CHARACTERISTICS\n")
  cat("===============================================\n")
  cat("Reference year:", reference_year, "\n")
  cat("Number of variables to analyze:", length(analysis_variables), "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("EU election years:", paste(eu_election_years, collapse = ", "), "\n")
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
  
  # Verify required variables exist
  missing_vars <- setdiff(names(analysis_variables), names(dfZRRControls))
  if (length(missing_vars) > 0) {
    warning("Missing variables in dataset: ", paste(missing_vars, collapse = ", "))
    analysis_variables <- analysis_variables[!names(analysis_variables) %in% missing_vars]
  }
  
  cat("  - Variables to analyze:", length(analysis_variables), "\n")
  for (var in names(analysis_variables)) {
    cat("    ", var, ":", analysis_variables[[var]], "\n")
  }
  
  # --------------------------------------------------------------------------
  # 2. EXAMINE DATA STRUCTURE AND TEMPORAL COVERAGE
  # --------------------------------------------------------------------------
  cat("\n2. Examining data structure and temporal coverage...\n")
  
  # Analyze temporal structure
  temporal_summary <- dfZRRControls %>%
    group_by(year) %>%
    summarise(
      n_municipalities = n_distinct(codecommune),
      n_observations = n(),
      mean_fn_support = mean(FN, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(year)
  
  cat("✓ Analyzed temporal structure\n")
  cat("  - Years covered:", nrow(temporal_summary), "\n")
  cat("  - Year range:", min(temporal_summary$year), "to", max(temporal_summary$year), "\n")
  cat("  - Reference year present:", reference_year %in% temporal_summary$year, "\n")
  
  cat("  - Temporal coverage summary:\n")
  for (i in 1:nrow(temporal_summary)) {
    year <- temporal_summary$year[i]
    is_ref <- ifelse(year == reference_year, " (Reference)", "")
    is_eu <- ifelse(year %in% eu_election_years, " (EU)", "")
    cat("    ", year, is_ref, is_eu, ": ", 
        temporal_summary$n_municipalities[i], " municipalities, ",
        "FN support = ", sprintf("%.1f%%", temporal_summary$mean_fn_support[i] * 100), "\n")
  }
  
  # --------------------------------------------------------------------------
  # 3. DEFINE MODEL ESTIMATION AND PLOTTING HELPER FUNCTION
  # --------------------------------------------------------------------------
  cat("\n3. Defining model estimation and plotting functions...\n")
  
  #' Create regression model and coefficient plot for specified variable
  #' @param df Input dataframe with panel data
  #' @param x Variable name to analyze
  #' @param ref_year Reference year for model
  #' @param title Plot title
  #' @param eu_years Vector of EU election years
  #' @return ggplot object with coefficient estimates over time
  create_locality_model_plot <- function(df, x, ref_year, title, eu_years) {
    
    cat("    - Processing variable:", x, "\n")
    
    # Validate variable exists and has data
    if (!x %in% names(df)) {
      stop("Variable '", x, "' not found in dataset")
    }
    
    # Create working copy and add variable of interest
    df_work <- df
    df_work$x <- df_work[[x]]
    
    # Check for missing values in the variable
    missing_count <- sum(is.na(df_work$x))
    cat("      Missing values in", x, ":", missing_count, "\n")
    
    # Create baseline dataframe for reference year
    dfX <- df_work %>%
      filter(year == ref_year) %>%
      select(codecommune, x) %>%
      distinct(codecommune, .keep_all = TRUE)
    
    cat("      Reference year municipalities:", nrow(dfX), "\n")
    
    # Prepare data for model estimation
    df_model <- df_work %>%
      select(-x) %>%
      left_join(dfX, by = "codecommune") %>%
      filter(!is.na(x), !is.na(FN)) %>%  # Remove missing values
      select(codecommune, year, FN, reg, x) %>%
      mutate(
        year = factor(as.numeric(year)),
        EU = ifelse(year %in% eu_years, 1, 0)
      )
    
    cat("      Final model data:", nrow(df_model), "observations\n")
    
    # Set reference year for factor
    df_model$year <- relevel(df_model$year, ref = as.character(ref_year))
    
    # Define and estimate model
    model_formula <- FN ~ factor(reg):factor(year) + x:factor(year) + factor(EU)
    
    cat("      Estimating regression model...\n")
    model <- lm(model_formula, data = df_model)
    model_summary <- summary(model)
    
    # Extract model diagnostics
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    n_obs <- nrow(model_summary$residuals)
    
    cat("      Model diagnostics:\n")
    cat("        R-squared:", sprintf("%.3f", r_squared), "\n")
    cat("        Adjusted R-squared:", sprintf("%.3f", adj_r_squared), "\n")
    cat("        Observations:", n_obs, "\n")
    
    # Extract interaction coefficients and standard errors
    years_in_data <- setdiff(levels(df_model$year), as.character(ref_year))
    
    coefficients_df <- numeric(length(years_in_data))
    sd_df <- numeric(length(years_in_data))
    
    for (i in seq_along(years_in_data)) {
      coef_name <- paste0("factor(year)", years_in_data[i], ":x")
      if (coef_name %in% rownames(model_summary$coefficients)) {
        coefficients_df[i] <- model_summary$coefficients[coef_name, "Estimate"]
        sd_df[i] <- model_summary$coefficients[coef_name, "Std. Error"]
      } else {
        coefficients_df[i] <- NA
        sd_df[i] <- NA
      }
    }
    
    # Insert reference year (coefficient = 0)
    years_numeric <- as.numeric(years_in_data)
    ref_position <- sum(years_numeric < ref_year) + 1
    
    years_final <- append(years_numeric, ref_year, ref_position - 1)
    coefficients_final <- append(coefficients_df, 0, ref_position - 1)
    sd_final <- append(sd_df, 0, ref_position - 1)
    
    # Create plotting dataframe
    plot_data <- data.frame(
      year = years_final,
      estimate = coefficients_final,
      sd = sd_final,
      lower_ci = coefficients_final - sd_final,
      upper_ci = coefficients_final + sd_final,
      is_reference = (years_final == ref_year),
      is_eu = years_final %in% eu_years
    ) %>%
      arrange(year) %>%
      filter(!is.na(estimate))
    
    cat("      Plot data points:", nrow(plot_data), "\n")
    
    # Generate coefficient plot
    plot <- ggplot(plot_data, aes(x = year, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "solid", colour = scales::alpha("grey70", 0.7)) +
      geom_vline(xintercept = ref_year, linetype = "dashed", colour = scales::alpha("black", 0.8)) +
      geom_line(color = "black", size = 0.8) +
      geom_errorbar(
        aes(ymin = lower_ci, ymax = upper_ci), 
        width = 0.8, 
        color = "black",
        size = 0.5
      ) +
      geom_point(
        aes(
          color = is_reference,
          shape = is_eu,
          size = is_reference
        )
      ) +
      scale_color_manual(
        values = c("FALSE" = "black", "TRUE" = "red"),
        guide = "none"
      ) +
      scale_shape_manual(
        values = c("FALSE" = 16, "TRUE" = 17),
        guide = "none"
      ) +
      scale_size_manual(
        values = c("FALSE" = 2, "TRUE" = 3),
        guide = "none"
      ) +
      scale_x_continuous(
        breaks = plot_data$year, 
        labels = plot_data$year
      ) +
      labs(title = title) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.3, colour = scales::alpha("grey70", 0.7)),
        panel.grid.major.y = element_line(size = 0.3, colour = scales::alpha("grey70", 0.7)),
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )
    
    return(list(
      plot = plot,
      model = model,
      model_summary = model_summary,
      plot_data = plot_data,
      diagnostics = list(
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        n_obs = n_obs
      )
    ))
  }
  
  cat("✓ Defined model estimation and plotting functions\n")
  
  # --------------------------------------------------------------------------
  # 4. VALIDATE REFERENCE YEAR DATA AVAILABILITY
  # --------------------------------------------------------------------------
  cat("\n4. Validating reference year data availability...\n")
  
  # Check reference year data
  ref_year_data <- dfZRRControls %>%
    filter(year == reference_year)
  
  if (nrow(ref_year_data) == 0) {
    stop("No data available for reference year: ", reference_year)
  }
  
  cat("✓ Validated reference year data\n")
  cat("  - Reference year (", reference_year, ") observations:", nrow(ref_year_data), "\n")
  cat("  - Unique municipalities in reference year:", n_distinct(ref_year_data$codecommune), "\n")
  
  # Check variable availability in reference year
  ref_year_var_summary <- ref_year_data %>%
    select(all_of(names(analysis_variables))) %>%
    summarise_all(~sum(!is.na(.))) %>%
    gather(variable, non_missing_count)
  
  cat("  - Variable availability in reference year:\n")
  for (i in 1:nrow(ref_year_var_summary)) {
    var <- ref_year_var_summary$variable[i]
    count <- ref_year_var_summary$non_missing_count[i]
    percentage <- (count / nrow(ref_year_data)) * 100
    cat("    ", var, ":", count, "observations (", sprintf("%.1f%%", percentage), ")\n")
  }
  
  # --------------------------------------------------------------------------
  # 5. ESTIMATE MODELS AND GENERATE PLOTS FOR ALL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n5. Estimating models and generating plots for all variables...\n")
  
  # Generate plots and models for each variable
  model_results <- list()
  plots_list <- list()
  
  for (var_name in names(analysis_variables)) {
    cat("  Processing", var_name, "...\n")
    
    result <- create_locality_model_plot(
      df = dfZRRControls,
      x = var_name,
      ref_year = reference_year,
      title = analysis_variables[[var_name]],
      eu_years = eu_election_years
    )
    
    model_results[[var_name]] <- result
    plots_list[[var_name]] <- result$plot
    
    cat("    ✓ Completed", var_name, "\n")
  }
  
  cat("✓ Generated all individual plots and models\n")
  cat("  - Total plots created:", length(plots_list), "\n")
  
  # --------------------------------------------------------------------------
  # 6. CALCULATE CROSS-VARIABLE MODEL DIAGNOSTICS
  # --------------------------------------------------------------------------
  cat("\n6. Calculating cross-variable model diagnostics...\n")
  
  # Compile model diagnostics
  diagnostics_summary <- data.frame(
    variable = names(model_results),
    r_squared = sapply(model_results, function(x) x$diagnostics$r_squared),
    stringsAsFactors = FALSE
  )
  
  cat("✓ Calculated model diagnostics\n")
  cat("  - Model performance summary:\n")
  for (i in 1:nrow(diagnostics_summary)) {
    cat("    ", diagnostics_summary$variable[i], ":\n")
    cat("      R²:", sprintf("%.3f", diagnostics_summary$r_squared[i]), "\n")
  }
  
  # --------------------------------------------------------------------------
  # 7. ANALYZE COEFFICIENT PATTERNS ACROSS TIME
  # --------------------------------------------------------------------------
  cat("\n7. Analyzing coefficient patterns across time...\n")
  
  # Extract coefficient evolution for analysis
  coefficient_evolution <- data.frame()
  
  for (var_name in names(model_results)) {
    var_data <- model_results[[var_name]]$plot_data %>%
      mutate(variable = var_name) %>%
      select(variable, year, estimate, sd, is_reference, is_eu)
    
    coefficient_evolution <- rbind(coefficient_evolution, var_data)
  }
  
  # Calculate temporal patterns
  temporal_patterns <- coefficient_evolution %>%
    filter(!is_reference) %>%
    group_by(variable) %>%
    summarise(
      mean_coefficient = mean(estimate, na.rm = TRUE),
      max_coefficient = max(estimate, na.rm = TRUE),
      min_coefficient = min(estimate, na.rm = TRUE),
      coefficient_range = max(estimate, na.rm = TRUE) - min(estimate, na.rm = TRUE),
      trend_slope = if(n() > 1) coef(lm(estimate ~ year, data = cur_data()))[2] else NA,
      .groups = 'drop'
    )
  
  cat("✓ Analyzed coefficient patterns\n")
  cat("  - Temporal evolution summary:\n")
  for (i in 1:nrow(temporal_patterns)) {
    var <- temporal_patterns$variable[i]
    cat("    ", var, ":\n")
    cat("      Coefficient range:", sprintf("%.4f", temporal_patterns$coefficient_range[i]), "\n")
    cat("      Time trend:", sprintf("%.6f", temporal_patterns$trend_slope[i]), "\n")
  }
  
  # --------------------------------------------------------------------------
  # 8. CREATE COMBINED VISUALIZATION LAYOUT
  # --------------------------------------------------------------------------
  cat("\n8. Creating combined visualization layout...\n")
  
  # Arrange plots in grid layout
  combined_plot <- gridExtra::grid.arrange(
    grobs = plots_list,
    ncol = 2,
    nrow = ceiling(length(plots_list) / 2),
    left = grid::textGrob(
      "Coefficient Estimates", 
      gp = grid::gpar(fontsize = 15, fontface = "bold"), 
      rot = 90
    ),
    bottom = grid::textGrob(
      "Years", 
      gp = grid::gpar(fontsize = 15, fontface = "bold")
    )
    # top = grid::textGrob(
    #   paste("Nonparametric Effect of Locality Characteristics on FN Support Over Time\n(Reference Year:", reference_year, ")"),
    #   gp = grid::gpar(fontsize = 14, fontface = "bold")
    # )
  )
  
  cat("✓ Created combined visualization\n")
  cat("  - Layout: 2 columns,", ceiling(length(plots_list) / 2), "rows\n")
  cat("  - Individual plots:", length(plots_list), "\n")
  
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
  output_file <- file.path(path_figures, "FN_growth.png")
  
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
    model_results = model_results,
    diagnostics = list(
      model_performance = diagnostics_summary,
      temporal_patterns = temporal_patterns,
      coefficient_evolution = coefficient_evolution
    ),
    data_summary = list(
      temporal_coverage = temporal_summary,
      reference_year_summary = ref_year_var_summary,
      total_observations = nrow(dfZRRControls)
    ),
    metadata = list(
      reference_year = reference_year,
      eu_election_years = eu_election_years,
      variables_analyzed = names(analysis_variables),
      output_file = output_file,
      creation_date = Sys.time()
    )
  )
  
  cat("✓ Compiled comprehensive results\n")
  cat("  - Result components:", length(analysis_results), "\n")
  cat("  - Individual model results available\n")
  cat("  - Temporal patterns and diagnostics included\n")
  
  # --------------------------------------------------------------------------
  # 11. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n11. Results Summary:\n")
  
  cat("  - Analysis Overview:\n")
  cat("    Reference year:", reference_year, "\n")
  cat("    Variables analyzed:", length(analysis_variables), "\n")
  cat("    Time period covered:", min(temporal_summary$year), "-", max(temporal_summary$year), "\n")
  cat("    Total observations processed:", sum(temporal_summary$n_observations), "\n")
  
  cat("\n  - Temporal Effects:\n")
  strongest_trend <- temporal_patterns$variable[which.max(abs(temporal_patterns$trend_slope))]
  cat("    Strongest temporal trend:", strongest_trend, "\n")
  cat("    Average coefficient range:", sprintf("%.4f", mean(temporal_patterns$coefficient_range)), "\n")
  
  cat("\n  - Output:\n")
  cat("    Combined visualization saved to:", basename(output_file), "\n")
  cat("    Figure layout:", ceiling(length(plots_list) / 2), "rows ×", 2, "columns\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("FN GROWTH ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

# Generate FN support growth analysis
generate_fn_growth_analysis(processed_data_path, path_figures)