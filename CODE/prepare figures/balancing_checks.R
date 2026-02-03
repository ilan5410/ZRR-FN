# ==============================================================================
# RDD Balance Tests Analysis
# ==============================================================================
# This script performs comprehensive balance tests for regression discontinuity
# design (RDD) by testing treatment effects on predetermined characteristics
# around the ZRR program frontier using normalized control variables
# ==============================================================================


# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate comprehensive RDD balance tests analysis
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param rdd_bandwidth RDD bandwidth around threshold in meters (default: 20000)
#' @param analysis_bandwidth Analysis bandwidth for regression in meters (default: 10000)
#' @param threshold RDD threshold value (default: 0)
#' @param num_bins Number of bins for scatter plots (default: 15)
#' @param plots_per_row Number of plots per row in output (default: 3)
#' @param figure_height_per_row Height per row in inches (default: 2.5)
#' @param figure_width Width of output figure in inches (default: 8.5)
#' @param figure_dpi DPI resolution for output figures (default: 300)
#' @return List containing plots, balance test statistics, and analysis results
generate_rdd_balance_analysis <- function(processed_data_path, path_figures,
                                          rdd_bandwidth = 20000,
                                          analysis_bandwidth = 10000,
                                          num_bins = 15,
                                          plots_per_row = 3,
                                          figure_height_per_row = 2.5,
                                          figure_width = 8.5,
                                          figure_dpi = 300) {
  
  cat("===============================================\n")
  cat("RDD BALANCE TESTS ANALYSIS\n")
  cat("===============================================\n")
  cat("RDD bandwidth:", scales::comma(rdd_bandwidth), "meters\n")
  cat("Analysis bandwidth:", scales::comma(analysis_bandwidth), "meters\n")
  cat("Threshold:", 0, "\n")
  cat("Number of bins:", num_bins, "\n")
  cat("Plots per row:", plots_per_row, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height_per_row, "inches per row @", figure_dpi, "DPI\n")
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
  required_vars <- c("x", "z", "codecommune", "pop")
  missing_vars <- setdiff(required_vars, names(dfZRRControls))
  
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  cat("✓ Validated required variables\n")
  
  # --------------------------------------------------------------------------
  # 2. DEFINE AND VALIDATE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n2. Defining and validating control variables...\n")
  
  # Define comprehensive set of control variables
  controls_to_use <- setdiff(controls, c("FN1988"))
  
  # Verify control variables exist and filter out missing ones
  available_controls <- intersect(controls_to_use, names(dfZRRControls))
  missing_controls <- setdiff(controls_to_use, names(dfZRRControls))
  
  if (length(missing_controls) > 0) {
    warning("Missing control variables: ", paste(missing_controls, collapse = ", "))
  }
  
  controls_clean <- available_controls
  
  
  cat("✓ Defined and validated control variables\n")
  cat("  - Available controls:", length(controls_clean), "\n")
  cat("  - Missing controls:", length(missing_controls), "\n")
  
  # --------------------------------------------------------------------------
  # 3. PREPARE RDD DATASET
  # --------------------------------------------------------------------------
  cat("\n3. Preparing RDD dataset...\n")
  
  # Define bandwidth bounds
  lower_bound <-  - rdd_bandwidth
  upper_bound <-  + rdd_bandwidth
  
  # Filter data within RDD bandwidth and prepare variables
  df_rdd <- dfZRRControls %>%
    filter(x >= lower_bound, x <= upper_bound) %>%
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
  cat("  - Bandwidth range:", scales::comma(lower_bound), "to", scales::comma(upper_bound), "meters\n")
  
  # --------------------------------------------------------------------------
  # 4. SPLIT CONTROLS INTO GROUPS FOR SEPARATE PLOTS
  # --------------------------------------------------------------------------
  cat("\n4. Splitting controls into groups for visualization...\n")
  
  # Remove problematic variables and split controls
  excluded_vars <- c("border", "FN1988", "typologie")
  controls_clean <- setdiff(controls_clean, excluded_vars)
  
  # Split controls into two balanced parts
  controls_part1 <- controls_clean[1:ceiling(length(controls_clean)/2)]
  controls_part2 <- controls_clean[(ceiling(length(controls_clean)/2) + 1):length(controls_clean)]
  
  cat("✓ Split controls into groups\n")
  cat("  - Excluded variables:", paste(intersect(excluded_vars, controls), collapse = ", "), "\n")
  cat("  - Part 1 controls:", length(controls_part1), "\n")
  cat("  - Part 2 controls:", length(controls_part2), "\n")
  
  # Display control groups
  cat("  - Part 1 variables:", paste(controls_part1, collapse = ", "), "\n")
  cat("  - Part 2 variables:", paste(controls_part2, collapse = ", "), "\n")
  
  # --------------------------------------------------------------------------
  # 5. NORMALIZE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n5. Normalizing control variables...\n")
  
  # Normalize variables for better comparison across different scales
  df_rdd_nor <- clean_data_variables(df_rdd, names(df_rdd))
  
  # Normalize Part 1 controls
  if (length(controls_part1) > 0) {
    for (var in controls_part1) {
      if (var %in% names(df_rdd_nor) & !(var %in% c("pop", "asso"))) {
        df_rdd_nor[[var]] <- as.vector(scale(df_rdd_nor[[var]]))
      }
    }
    cat("  - Normalized Part 1 controls:", length(controls_part1), "\n")
  }
  
  # Normalize Part 2 controls
  if (length(controls_part2) > 0) {
    for (var in controls_part2) {
      if (var %in% names(df_rdd_nor) & !(var %in% c("pop", "asso"))) {
        df_rdd_nor[[var]] <- as.vector(scale(df_rdd_nor[[var]]))
      }
    }
    cat("  - Normalized Part 2 controls:", length(controls_part2), "\n")
  }
  
  cat("✓ Normalized control variables\n")
  cat("  - All variables now have mean ≈ 0 and SD ≈ 1\n")
  
  # --------------------------------------------------------------------------
  # 6. DEFINE BALANCE TEST PLOTTING FUNCTION
  # --------------------------------------------------------------------------
  cat("\n6. Defining balance test plotting function...\n")
  
  
  conditional_on <- c(controls, "dep")
  
  create_balance_plots <- function(controls_subset, file_name, b, df_data) {
    plots <- list()
    df_filtered <- filter(df_rdd_nor, x >= -b & x <= b)
    
    
    for (y in controls_subset) {
      # Fit regression model and extract residuals
      formula <- as.formula(paste(y, "~", paste(setdiff(conditional_on, y), collapse = " + ")))
      
      df_reg <- df_filtered %>% 
        select(y, all_of(conditional_on), "x")
      
      model <- lm(formula, data = df_reg)
      df_reg$residuals <- residuals(model)
      


      # Binning step 
      df_bins <- df_reg %>% 
        mutate(bin = ntile(x, 15)) %>%
        group_by(bin) %>%
        mutate(
          bin_center = mean(x, na.rm = TRUE),
          group = ifelse(mean(x, na.rm = TRUE) <= 0, "Inside Program", "Outside Program")
        ) %>%
        summarise(
          bin_center = mean(x, na.rm = TRUE),
          residuals_mean = mean(residuals, na.rm = TRUE),
          se = sd(residuals, na.rm = TRUE) / sqrt(n()),  # <- SE of the mean
          group = first(group),
          .groups = "drop"
        )
      
      
      # Get variable label (wrap for better display)
      var_label <- if (y %in% names(labels)) {
        stringr::str_wrap(labels[y], width = 25)
      } else {
        stringr::str_wrap(y, width = 25)
      }
      
      # with bin-level standard deviation
      
      # p <- ggplot(df_bins, aes(x = bin_center, y = residuals_mean)) +
      #   geom_point(aes(shape = group), size = 2, color = "black") +
      #   geom_errorbar(aes(ymin = residuals_mean - 1.96 * se,
      #                     ymax = residuals_mean + 1.96 * se),
      #                 width = 500, color = "gray40") +
      #   geom_smooth(
      #     data = df_reg,
      #     aes(x = x, y = residuals, group = ifelse(x <= 0, "Inside Program", "Outside Program")),
      #     method = "lm",
      #     formula = y ~ poly(x, 1),
      #     size = 0.5,
      #     color = "black",
      #     se = TRUE   # shaded confidence interval for regression line
      #   )
      
      p <- ggplot(df_bins, aes(x = bin_center, y = residuals_mean)) +
        geom_point(aes(shape = group), size = 2, color = "black") +
        geom_smooth(
          data = df_reg, 
          aes(x = x, y = residuals, 
              group = ifelse(x <= 0, "Inside Program", "Outside Program")),
          method = "lm", 
          formula = y ~ poly(x, 1), 
          size = 0.5, 
          color = "black",
          se = TRUE,                   # <- turn on confidence bands
          fill = "gray70",             # <- shaded ribbon color
          alpha = 0.3                  # <- transparency of ribbon
        ) +
        geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = 0.7) +
        scale_x_continuous(
          labels = function(x) scales::comma(x, scale = 1e-3, suffix = "k")
        ) +
        scale_y_continuous(
          labels = function(x) sprintf("%.2f", x)
        ) +
        scale_shape_manual(values = c("Inside Program" = 16, "Outside Program" = 17)) +
        labs(title = var_label) +
        theme_minimal(base_family = "serif") +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 11, hjust = 0.5, face = "bold", 
                                    margin = ggplot2::margin(b = 8)),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 9, color = "black"),
          panel.grid.major = element_line(color = "gray90", size = 0.3),
          panel.grid.minor = element_blank(),
          plot.margin = ggplot2::margin(10, 10, 10, 10)
        )
      
      
      plots[[y]] <- p
    }
    
    # Combine plots into one figure with adjusted layout
    png(file_name, width = 8.5 * 300, height = 2.5 * 300 * ceiling(length(controls_subset) / 3), res = 300)
    
    # combined_plot <- grid.arrange(
    #   grobs = plots, 
    #   ncol = 3, 
    #   padding = unit(0.5, "cm"),
    #   bottom = textGrob("Distance to Program Frontier", gp = gpar(fontsize = 14, fontfamily = "serif")),
    #   left = textGrob("Residuals", rot = 90, gp = gpar(fontsize = 14, fontfamily = "serif")),
    #   top = textGrob("", gp = gpar(fontsize = 16, fontfamily = "serif")),
    #   widths = unit(rep(2.5, 3), "in"),
    #   heights = unit(rep(2.5, ceiling(length(controls_subset) / 3)), "in")
    # )
    # 
    
    combined_plot <- gridExtra::grid.arrange(
      grobs = plots,
      ncol = plots_per_row,
      padding = unit(0.3, "cm"),
      bottom = grid::textGrob(
        "Distance to Program Frontier (km)", 
        gp = grid::gpar(fontsize = 12, fontfamily = "serif", fontface = "bold")
      ),
      left = grid::textGrob(
        "Residuals (Standardized)", 
        rot = 90, 
        gp = grid::gpar(fontsize = 12, fontfamily = "serif", fontface = "bold")
      )
    # top = grid::textGrob(
      #   "Balance Tests: Predetermined Characteristics",
      #   gp = grid::gpar(fontsize = 14, fontfamily = "serif", fontface = "bold")
      # )
    )
    
    
    dev.off()
    cat("      ✓ Saved combined plot to:", basename(file_name), "\n")
    
    return(list(
      individual_plots = plots,
      combined_plot = combined_plot,
      output_file = file_name
    ))
    
  }
  
  cat("✓ Defined balance test plotting function\n")
  
  # --------------------------------------------------------------------------
  # 7. CREATE BALANCE PLOTS FOR PART 1
  # --------------------------------------------------------------------------
  cat("\n7. Creating balance plots for Part 1...\n")
  
  part1_results <- create_balance_plots(
    controls_subset = controls_part1,
    file_name = file.path(path_figures, "balancing_checks_1.png"),
    b = analysis_bandwidth,
    df_data = df_rdd_nor
  )
  
  cat("✓ Created Part 1 balance plots\n")
  
  
  # --------------------------------------------------------------------------
  # 8. CREATE BALANCE PLOTS FOR PART 2
  # --------------------------------------------------------------------------
  cat("\n8. Creating balance plots for Part 2...\n")
  
  part2_results <- create_balance_plots(
    controls_subset = controls_part2,
    file_name = file.path(path_figures, "balancing_checks_2.png"),
    b = analysis_bandwidth,
    df_data = df_rdd_nor
  )
  
  

  # --------------------------------------------------------------------------
  # 12. DISPLAY FINAL SUMMARY
  # --------------------------------------------------------------------------
  cat("\n12. Final Summary:\n")

  cat("  - Analysis Overview:\n")
  cat("    RDD bandwidth:", scales::comma(rdd_bandwidth), "meters\n")
  cat("    Analysis bandwidth:", scales::comma(analysis_bandwidth), "meters\n")
  cat("    Variables tested:", length(c(controls_part1, controls_part2)), "\n")

  cat("\n  - Data Coverage:\n")
  cat("    Total municipalities in RDD sample:", nrow(df_rdd), "\n")
  cat("    Analysis sample size:", sum(df_rdd_nor$x >= -analysis_bandwidth & df_rdd_nor$x <= analysis_bandwidth), "\n")


  cat("\n  - Output Files:\n")
  if (!is.null(part1_results)) {
    cat("    Part 1 plots:", basename(part1_results$output_file), "\n")
  }
  if (!is.null(part2_results)) {
    cat("    Part 2 plots:", basename(part2_results$output_file), "\n")
  }
  
  cat("\n✓ RDD balance tests analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("RDD BALANCE TESTS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

# Generate RDD balance tests analysis
generate_rdd_balance_analysis(processed_data_path, path_figures)