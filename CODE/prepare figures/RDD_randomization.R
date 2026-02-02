# ==============================================================================
# RDD Randomization Test Analysis (Treatment Effect Validation)
# ==============================================================================
# This script performs randomization tests for RDD analysis by comparing observed
# treatment effects against permutation distributions using randomly generated
# borders to validate the identification strategy and test for spurious effects
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate RDD randomization test analysis with permutation distributions
#' @param processed_data_path Path to processed data directory
#' @param raw_data_path Path to raw data directory
#' @param path_figures Path to output figures directory
#' @param figure_width_per_col Width per column in inches (default: 3.2)
#' @param figure_height_per_row Height per row in inches (default: 2.8)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param bandwidths Vector of bandwidths to test (default: c(20, 10, 5))
#' @param n_permutations Number of permutation iterations (default: 100)
#' @param outcome_variable Name of outcome variable to analyze (default: "FN2002")
#' @param controls Vector of control variable names to include in regression
#' @param hist_bins Number of histogram bins for distribution plots (default: 30)
#' @return List containing plots, results data, and statistical tests
generate_rdd_randomization_analysis <- function(processed_data_path, raw_data_path, path_figures,
                                                figure_width_per_col = 3.2,
                                                figure_height_per_row = 2.8,
                                                figure_dpi = 300,
                                                n_permutations = 100,
                                                outcome_variable = "FN2002",
                                                hist_bins = 30) {
  
  cat("===============================================\n")
  cat("RDD RANDOMIZATION TEST ANALYSIS\n")
  cat("===============================================\n")
  cat("Outcome variable:", outcome_variable, "\n")
  cat("Bandwidths tested:", paste(bandwidths, collapse = ", "), "\n")
  cat("Number of permutations:", n_permutations, "\n")
  cat("Histogram bins:", hist_bins, "\n")
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
  
  
  # Use controls from environment if not provided
  if (is.null(controls) && exists("controls")) {
    controls <- get("controls")
  } else if (is.null(controls)) {
    stop("Control variables not specified and 'controls' object not found in environment")
  }
  
  cat("  - Dataset 'dfZRRControls' available with", nrow(dfZRRControls), "observations\n")
  cat("  - Control variables:", length(controls), "variables\n")
  
  # --------------------------------------------------------------------------
  # 2. DEFINE HELPER FUNCTIONS
  # --------------------------------------------------------------------------
  cat("\n2. Defining helper functions...\n")
  
  # Helper function for significance stars
  add_stars <- function(coef, se) {
    t_value <- abs(coef / se)
    dplyr::case_when(
      t_value > 2.58 ~ "***",
      t_value > 1.96 ~ "**",
      t_value > 1.65 ~ "*",
      TRUE ~ ""
    )
  }
  
  # Function to prepare RDD data
  prepare_rdd_data <- function(df, bandwidth) {
    df %>%
      filter(x >= ( - bandwidth), x <= ( + bandwidth)) %>%
      mutate(
        dist = x,
        treatmentZRR = z,
        pop = log(pop)
      ) %>%
      distinct(codecommune, .keep_all = TRUE) 
  }
  
  # Function to run model for a given bandwidth
  run_model <- function(data, bw, formula) {
    model_data <- filter(data, x >= -bw, x <= bw)
    model <- lm(formula, data = model_data)
    coef_z <- coef(model)[grepl("^z", names(coef(model)))]
    se_z <- sqrt(diag(vcovHC(model, type = "HC1", cluster = "group", 
                             cluster.id = model_data$canton))[grepl("^z", names(coef(model)))])
    
    return(data.frame(
      Bandwidth = paste0("Bandwidth = ", bw), 
      Coefficient = as.numeric(coef_z), 
      Clustered_SE = as.numeric(se_z)
    ))
    
  }
  
  cat("✓ Defined helper functions\n")
  cat("  - add_stars: Significance level formatting\n")
  cat("  - prepare_rdd_data: Data preprocessing\n")
  cat("  - run_model: Model estimation with error handling\n")
  
  
  # --------------------------------------------------------------------------
  # 4. CONSTRUCT REGRESSION FORMULA
  # --------------------------------------------------------------------------
  cat("\n4. Constructing regression formula...\n")
  
  # Build the regression formula
  formula_components <- c("z", "x", controls, "factor(dep)")
  formula_string <- paste(outcome_variable, "~", paste(formula_components, collapse = " + "))
  formula <- as.formula(formula_string)
  
  cat("✓ Constructed regression formula\n")
  cat("  - Formula:", deparse(formula), "\n")
  cat("  - Predictors:", length(formula_components), "\n")
  
  # --------------------------------------------------------------------------
  # 5. PROCESS OBSERVED DATA
  # --------------------------------------------------------------------------
  cat("\n5. Processing observed data...\n")
  
  # Initialize results storage
  results_list <- list()
  
  # Prepare observed data
  observed_data <- prepare_rdd_data(dfZRRControls, 20000)
  observed_data <- clean_data_variables(observed_data, names(observed_data))
  
  # Run models for each bandwidth on observed data
  observed_results <- bind_rows(lapply(bandwidths, function(bw) {
    run_model(observed_data, bw, formula)
  }))
  
  results_list[["Observed"]] <- observed_results
  
  cat("✓ Processed observed data\n")
  cat("  - Observations in 20km bandwidth:", nrow(observed_data), "\n")
  cat("  - Models estimated for bandwidths:", paste(bandwidths, collapse = ", "), "\n")
  
  # Display observed results
  cat("  - Observed treatment effects:\n")
  for (i in 1:nrow(observed_results)) {
    bw_result <- observed_results[i, ]
    if (!is.na(bw_result$Coefficient)) {
      stars <- add_stars(bw_result$Coefficient, bw_result$Clustered_SE)
      cat("    ", bw_result$Bandwidth, ": ", sprintf("%.4f", bw_result$Coefficient), 
          " (SE: ", sprintf("%.4f", bw_result$Clustered_SE), ")", stars, "\n")
    } else {
      cat("    ", bw_result$Bandwidth, ": Failed to estimate\n")
    }
  }
  
  # --------------------------------------------------------------------------
  # PERFORM PERMUTATION TESTS
  # --------------------------------------------------------------------------
  cat("\n6. Performing permutation tests...\n")
  
  # Check if permutation data directory exists
  perm_data_dir <- file.path(processed_data_path, "dataGeoRDD_canton_random")
  if (!dir.exists(perm_data_dir)) {
    stop("Permutation data directory not found: ", perm_data_dir)
  }
  
  # Initialize progress bar
  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      total = n_permutations, 
      format = "[:bar] :percent :current/:total (:eta)"
    )
  } else {
    pb <- NULL
    cat("  - Progress package not available, running without progress bar\n")
  }
  
  # Loop over permutation files
  successful_perms <- 0
  failed_perms <- 0
  
  for (i in 0:(n_permutations-1)) {
    perm_file <- file.path(perm_data_dir, paste0("dataGeoRDD_canton_random", i, ".xlsx"))
    
    if (!file.exists(perm_file)) {
      cat("    Warning: Permutation file", i, "not found\n")
      failed_perms <- failed_perms + 1
      if (!is.null(pb)) pb$tick()
      next
    }
    
    
    # Read permutation distance data
    dfDistance <- readxl::read_excel(perm_file) %>%
      mutate(
        codecommune = sub("^0+", "", as.character(codecommune)),
        x = distance_to_border,
        z = ifelse(x < 0, 1, 0)
      ) %>%
      select(codecommune, x, z)
    
    # Merge with main data
    df_rdd <- dfZRRControls %>%
      select(-x, -z, -distance_to_border, -treatment, -border) %>%
      distinct(codecommune, .keep_all = TRUE) %>%
      mutate(codecommune = sub("^0+", "", as.character(codecommune))) %>%
      left_join(dfDistance, by = "codecommune") %>%
      prepare_rdd_data(20000)
    
    df_rdd <- suppressWarnings(
      suppressMessages(
        {
          tmp <- capture.output(
            result <- clean_data_variables(df_rdd, names(df_rdd))
          )
          result
        }
      )
    )
    
    
    # Run models for each bandwidth
    perm_results <- bind_rows(lapply(bandwidths, function(bw) {
      run_model(df_rdd, bw, formula)
    }))
    
    results_list[[paste0("Iteration_", i)]] <- perm_results
    successful_perms <- successful_perms + 1
    
    if (!is.null(pb)) pb$tick()
  }
  
  cat("✓ Completed permutation tests\n")
  cat("  - Successful permutations:", successful_perms, "\n")
  cat("  - Failed permutations:", failed_perms, "\n")
  cat("  - Success rate:", sprintf("%.1f%%", (successful_perms/n_permutations)*100), "\n")
  
  # --------------------------------------------------------------------------
  # 7. COMBINE AND ANALYZE RESULTS
  # --------------------------------------------------------------------------
  cat("\n7. Combining and analyzing results...\n")
  
  # Combine all results
  final_results <- bind_rows(lapply(names(results_list), function(name) {
    mutate(results_list[[name]], Iteration = name)
  }))
  
  # Calculate permutation test statistics
  perm_stats <- final_results %>%
    filter(Iteration != "Observed", !is.na(Coefficient)) %>%
    group_by(Bandwidth) %>%
    summarize(
      n_perms = n(),
      mean_coef = mean(Coefficient),
      sd_coef = sd(Coefficient),
      min_coef = min(Coefficient),
      max_coef = max(Coefficient),
      .groups = 'drop'
    )
  
  # Calculate p-values for observed effects
  observed_pvalues <- final_results %>%
    filter(Iteration == "Observed") %>%
    left_join(
      final_results %>%
        filter(Iteration != "Observed", !is.na(Coefficient)) %>%
        group_by(Bandwidth) %>%
        summarize(
          perm_coeffs = list(Coefficient),
          .groups = 'drop'
        ),
      by = "Bandwidth"
    ) %>%
    mutate(
      p_value_two_sided = map2_dbl(Coefficient, perm_coeffs, function(obs, perms) {
        if (is.na(obs) || length(perms) == 0) return(NA_real_)
        mean(abs(perms) >= abs(obs), na.rm = TRUE)
      })
    ) %>%
    select(Bandwidth, Coefficient, p_value_two_sided)
  
  cat("✓ Analyzed permutation results\n")
  cat("  - Total result combinations:", nrow(final_results), "\n")
  cat("  - Valid permutation coefficients per bandwidth:\n")
  for (i in 1:nrow(perm_stats)) {
    cat("    ", perm_stats$Bandwidth[i], ":", perm_stats$n_perms[i], "valid permutations\n")
  }
  
  # --------------------------------------------------------------------------
  # 8. CREATE PUBLICATION-FRIENDLY THEME
  # --------------------------------------------------------------------------
  cat("\n8. Creating visualization theme...\n")
  
  theme_paper <- function(base_size = 11, base_family = NULL){
    theme_minimal(base_size = base_size, base_family = base_family) +
      theme(
        plot.title         = element_text(face = "bold", margin = ggplot2::margin(b = 6)),
        plot.subtitle      = element_text(margin = ggplot2::margin(b = 8)),
        axis.title.x       = element_text(margin = ggplot2::margin(t = 6)),
        axis.title.y       = element_text(margin = ggplot2::margin(r = 6)),
        axis.text          = element_text(color = "grey20"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
        axis.ticks         = element_line(linewidth = 0.3, color = "grey60"),
        axis.ticks.length  = grid::unit(2.5, "pt"),
        plot.margin        = ggplot2::margin(6, 10, 6, 6),
        strip.text         = element_text(face = "bold")
      )
  }
  
  cat("✓ Created publication-friendly theme\n")
  
  # --------------------------------------------------------------------------
  # 9. BUILD INDIVIDUAL PLOTS
  # --------------------------------------------------------------------------
  cat("\n9. Building individual plots...\n")
  
  plot_list <- list()
  
  for (x in bandwidths) {
    cat("  - Creating plot for bandwidth", x, "\n")
    
    # Permutation draws (null distribution)
    b_plot <- final_results %>%
      filter(Bandwidth == paste0("Bandwidth = ", x),
             Iteration != "Observed",
             !is.na(Coefficient))
    
    # Observed coefficient
    b_plot_observed <- final_results %>%
      filter(Bandwidth == paste0("Bandwidth = ", x),
             Iteration == "Observed") %>%
      pull(Coefficient)
    b_plot_observed <- if (length(b_plot_observed) > 0) as.numeric(b_plot_observed[1]) else NA_real_
    
    # Get p-value for this bandwidth
    p_val <- observed_pvalues %>%
      filter(Bandwidth == paste0("Bandwidth = ", x)) %>%
      pull(p_value_two_sided)
    p_val <- if (length(p_val) > 0) p_val[1] else NA_real_
    
    if (nrow(b_plot) == 0) {
      cat("    Warning: No valid permutation data for bandwidth", x, "\n")
      next
    }
    
    # Calculate histogram max for label placement
    tmp_build <- ggplot_build(
      ggplot(b_plot, aes(x = Coefficient)) + geom_histogram(bins = hist_bins)
    )
    max_count <- if (length(tmp_build$data) > 0) max(tmp_build$data[[1]]$count, na.rm = TRUE) else 0
    y_lab <- max_count * 1.08
    
    # Set x-axis limits to include observed value
    x_range <- range(c(b_plot$Coefficient, b_plot_observed), na.rm = TRUE)
    pad <- diff(range(pretty(x_range)))/40
    x_limits <- c(min(x_range) - pad, max(x_range) + pad)
    
    # Create base plot
    p <- ggplot(b_plot, aes(x = Coefficient)) +
      geom_histogram(bins = hist_bins, fill = "grey80", color = "grey30", 
                     linewidth = 0.25, boundary = 0) +
      scale_x_continuous(limits = x_limits, breaks = pretty_breaks(5)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(title = paste0("Bandwidth = ", x),
           x = "Coefficient",
           y = "Count") +
      coord_cartesian(clip = "off") +
      theme_paper()
    
    # Add observed value line and annotation
    if (!is.na(b_plot_observed)) {
      label_text <- paste0("Observed: ", sprintf("%.3f", b_plot_observed))
      if (!is.na(p_val)) {
        label_text <- paste0(label_text, "\np = ", sprintf("%.3f", p_val))
      }
      
      p <- p +
        geom_vline(xintercept = b_plot_observed, color = "red",
                   linewidth = 0.6, linetype = "dashed") +
        annotate("label",
                 x = b_plot_observed, y = y_lab,
                 label = label_text,
                 vjust = 0, hjust = -0.05,
                 size = 3.1, label.size = 0, fill = "white", color = "grey20") +
        expand_limits(y = y_lab * 1.05)
    }
    
    plot_list[[paste0("bw_", x)]] <- p
  }
  
  cat("✓ Built", length(plot_list), "individual plots\n")
  
  # --------------------------------------------------------------------------
  # 10. ARRANGE AND SAVE COMBINED PLOT
  # --------------------------------------------------------------------------
  cat("\n10. Arranging and saving combined plot...\n")
  
  if (length(plot_list) == 0) {
    stop("No plots were successfully created")
  }
  
  # Calculate grid dimensions
  n_plots <- length(plot_list)
  ncol <- min(3, n_plots)  # Maximum 3 columns for readability
  nrow <- ceiling(n_plots / ncol)
  
  # Arrange plots
  combined_plot <- patchwork::wrap_plots(plotlist = plot_list, nrow = nrow, ncol = ncol) +
    patchwork::plot_annotation(
      # title = "Permutation Distributions of Treatment Effects by Bandwidth",
      # subtitle = paste0("RDD Randomization Test for ", outcome_variable, " (", successful_perms, " permutations)"),
      theme = theme_paper()
    )
  
  # Calculate output dimensions
  width_in  <- figure_width_per_col * ncol
  height_in <- figure_height_per_row * nrow
  
  # Create output directory if needed
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Save plot
  output_file <- file.path(path_figures, "RDD_randomization.png")
  ggsave(filename = output_file, plot = combined_plot,
         width = width_in, height = height_in, dpi = figure_dpi, bg = "white")
  
  cat("✓ Saved combined plot\n")
  cat("  - Output file:", output_file, "\n")
  cat("  - Dimensions:", width_in, "x", height_in, "inches\n")
  cat("  - Grid layout:", nrow, "rows x", ncol, "columns\n")
  cat("  - Resolution:", figure_dpi, "DPI\n")
  
  # --------------------------------------------------------------------------
  # 11. COMPILE FINAL RESULTS
  # --------------------------------------------------------------------------
  cat("\n11. Compiling final results...\n")
  
  # Create comprehensive results object
  analysis_results <- list(
    plots = list(
      individual_plots = plot_list,
      combined_plot = combined_plot
    ),
    results_data = list(
      all_results = final_results,
      observed_results = observed_results,
      permutation_stats = perm_stats,
      p_values = observed_pvalues
    ),
    analysis_parameters = list(
      bandwidths = bandwidths,
      n_permutations = n_permutations,
      successful_permutations = successful_perms,
      outcome_variable = outcome_variable,
      hist_bins = hist_bins
    ),
    summary_statistics = list(
      total_results = nrow(final_results),
      valid_permutations_per_bw = perm_stats$n_perms,
      success_rate = (successful_perms/n_permutations)*100
    )
  )
  
  cat("✓ Compiled final results\n")
  cat("  - Results object contains:", length(analysis_results), "main components\n")
  
  # --------------------------------------------------------------------------
  # 12. DISPLAY FINAL SUMMARY
  # --------------------------------------------------------------------------
  cat("\n12. Final summary of randomization test...\n")
  
  cat("✓ RDD Randomization Test completed successfully\n")
  cat("\n===============================================\n")
  cat("RDD RANDOMIZATION TEST ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  cat("Key findings:\n")
  
  # Display observed vs permutation results
  for (i in 1:nrow(observed_pvalues)) {
    bw_data <- observed_pvalues[i, ]
    if (!is.na(bw_data$Coefficient) && !is.na(bw_data$p_value_two_sided)) {
      significance <- ifelse(bw_data$p_value_two_sided < 0.05, "SIGNIFICANT", "Not significant")
      cat("- ", bw_data$Bandwidth, ": Coef = ", sprintf("%.4f", bw_data$Coefficient), 
          ", p = ", sprintf("%.3f", bw_data$p_value_two_sided), " (", significance, ")\n")
    }
  }
  
  cat("- Success rate:", sprintf("%.1f%%", (successful_perms/n_permutations)*100), 
      "(", successful_perms, "/", n_permutations, "permutations)\n")
  cat("\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_rdd_randomization_analysis(processed_data_path, raw_data_path, path_figures)