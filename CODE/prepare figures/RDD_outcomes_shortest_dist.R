# ==============================================================================
# RDD main outcomes
# ==============================================================================
# This script performs a regression discontinuity design (RDD) 
# analyzing several outcomes around the ZRR program
# frontier, using binned scatter plots and clustered standard errors
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
#' @param cluster_variable Variable for clustered standard errors (default: "canton")
#' @return List containing plots, balance test statistics, and analysis results
generate_rdd_balance_analysis <- function(processed_data_path, path_figures,
                                          rdd_bandwidth = 20000,
                                          analysis_bandwidth = 10000,
                                          threshold = 0,
                                          num_bins = 15,
                                          plots_per_row = 3,
                                          figure_height_per_row = 2.5,
                                          figure_width = 8.5,
                                          figure_dpi = 300,
                                          cluster_variable = "canton",
                                          outcomes = c("FN2002", "RPR2002", "turnout_2002", 
                                                       "FN2007", "FN2012", "FN2017", "FN2022")
) {
  
  cat("===============================================\n")
  cat("RDD ANALYSIS\n")
  cat("===============================================\n")
  cat("RDD bandwidth:", scales::comma(rdd_bandwidth), "meters\n")
  cat("Analysis bandwidth:", scales::comma(analysis_bandwidth), "meters\n")
  cat("Threshold:", threshold, "\n")
  cat("Number of bins:", num_bins, "\n")
  cat("Plots per row:", plots_per_row, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height_per_row, "inches per row @", figure_dpi, "DPI\n")
  cat("Cluster variable:", cluster_variable, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD AND VALIDATE DATA
  # --------------------------------------------------------------------------
  cat("1. Loading and validating data...\n")
  
  data_file <- file.path(processed_data_path, "script_sharp_noEpicenter.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded script_sharp_noEpicenter.RData\n")
  
  # Check if required data exists
  if (!exists("dfZRRControls")) {
    stop("Required dataset 'dfZRRControls' not found in loaded environment")
  }
  
  cat("  - Dataset 'dfZRRControls' available with", nrow(dfZRRControls), "observations\n")
  
  # Verify required variables exist
  required_vars <- c("x", "z", "codecommune", "pop", cluster_variable)
  missing_vars <- setdiff(required_vars, names(dfZRRControls))
  
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  cat("✓ Validated required variables\n")
  
  # --------------------------------------------------------------------------
  # 2. DEFINE AND VALIDATE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  cat("\n2. Defining and validating control variables...\n")
  
  
  # Verify control variables exist and filter out missing ones
  available_controls <- intersect(controls, names(dfZRRControls))
  missing_controls <- setdiff(controls, names(dfZRRControls))
  
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
  lower_bound <- threshold - rdd_bandwidth
  upper_bound <- threshold + rdd_bandwidth
  
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
  
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  cat("✓ Prepared RDD dataset\n")
  cat("  - Observations within bandwidth:", nrow(df_rdd), "\n")
  cat("  - Bandwidth range:", scales::comma(lower_bound), "to", scales::comma(upper_bound), "meters\n")
  
  
  # # --------------------------------------------------------------------------
  # # NORMALIZE CONTROL VARIABLES
  # # --------------------------------------------------------------------------
  # cat("\n5. Normalizing control variables...\n")
  # 
  # # Normalize variables for better comparison across different scales
  # df_rdd_nor <- clean_data_variables(df_rdd, names(df_rdd))
  # 
  # # Normalize outcomes
  #   for (var in outcomes) {
  #     cat(var)
  #     df_rdd_nor[[var]] <- as.vector(scale(df_rdd_nor[[var]]))
  #   }
  #   cat("  - Normalized outcomes:", length(outcomes), "\n")
  # 
  # 
  # 
  # cat("✓ Normalized control variables\n")
  # cat("  - All variables now have mean ≈ 0 and SD ≈ 1\n")
  
  # --------------------------------------------------------------------------
  # DEFINE BALANCE TEST PLOTTING FUNCTION
  # --------------------------------------------------------------------------
  cat("\nDefining balance test plotting function...\n")
  
  
  conditional_on <- c(controls, "dep")
  
  create_balance_plots <- function(controls_subset, file_name, b, df_data) {
    plots <- list()
    balance_stats <- data.frame()
    df_filtered <- filter(df_data, x >= -b & x <= b)
    
    
    for (y in controls_subset) {
      # Fit regression model and extract residuals
      formula <- as.formula(paste(y, "~", paste(setdiff(conditional_on, y), collapse = " + ")))
      
      if (y %in% c("FN2007")) {
        formula <- as.formula(paste(y, "~", paste(c(setdiff(conditional_on, y), "FN2002"), collapse = " + ")))
      }
      
      if (y %in% c("FN2012")) {
        formula <- as.formula(paste(y, "~", paste(c(setdiff(conditional_on, y), "FN2002", "FN2007"), collapse = " + ")))
      }
      
      if (y %in% c("FN2017")) {
        formula <- as.formula(paste(y, "~", paste(c(setdiff(conditional_on, y), "FN2002", "FN2007", "FN2012"), collapse = " + ")))
      }
      
      if (y %in% c("FN2022")) {
        formula <- as.formula(paste(y, "~", paste(c(setdiff(conditional_on, y), "FN2002", "FN2007", "FN2012", "FN2017"), collapse = " + ")))
      }
      
      
      df_reg <- df_filtered %>% 
        select(y, all_of(conditional_on), "canton", "x", "FN2002", "FN2007",  "FN2012",  "FN2017",  "FN2022")
      
      model <- lm(formula, data = df_reg)
      df_reg$residuals <- residuals(model)
      
      # Compute clustered standard errors at the "canton" level
      clustered_se <- vcovCL(model, cluster = df_reg$canton, type = "HC1")
      
      # # Extract coefficient, clustered standard error, and p-value for `zTRUE`
      # z_coeff <- summary(model)$coefficients["zTRUE", "Estimate"]
      # z_se <- sqrt(diag(clustered_se))["zTRUE"]
      # z_pvalue <- coeftest(model, vcov = clustered_se)["zTRUE", "Pr(>|t|)"]
      # 
      # # Determine significance stars based on p-value
      # significance <- ifelse(z_pvalue < 0.001, "***", 
      #                        ifelse(z_pvalue < 0.01, "**", 
      #                               ifelse(z_pvalue < 0.05, "*", "")))
      # 
      # # Store statistics
      # balance_stats <- rbind(balance_stats, data.frame(
      #   variable = y,
      #   coefficient = z_coeff,
      #   std_error = z_se,
      #   p_value = z_pvalue,
      #   significance = significance,
      #   stringsAsFactors = FALSE
      # ))
      # 
      # cat("        Coef:", sprintf("%.4f", z_coeff), 
      #     ", SE:", sprintf("%.4f", z_se), 
      #     ", p:", sprintf("%.4f", z_pvalue), 
      #     significance, "\n")
      # 
      # 
      # # Format the text for annotation
      # annotation_text <- paste0("Coef. Treatment: ", round(z_coeff, 3), " (", round(z_se, 3), ") ", significance)
      
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
      
      
      p <- ggplot(df_bins, aes(x = bin_center, y = residuals_mean)) +
        geom_point(aes(shape = group), size = 2, color = "black") +
        geom_smooth(
          data = df_reg,
          aes(x = x, y = residuals,
              group = ifelse(x <= threshold, "Inside Program", "Outside Program")),
          method = "lm",
          formula = y ~ poly(x, 1),
          size = 0.5,
          color = "black",
          se = TRUE,                   # <- turn on confidence bands
          fill = "gray70",             # <- shaded ribbon color
          alpha = 0.3                  # <- transparency of ribbon
        ) +
        geom_vline(xintercept = threshold, color = "black", linetype = "dashed", alpha = 0.7) +
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
      ),
      # top = grid::textGrob(
      #   "Balance Tests: Outcomes",
      #   gp = grid::gpar(fontsize = 14, fontfamily = "serif", fontface = "bold")
      # )
    )
    
    
    dev.off()
    cat("      ✓ Saved combined plot to:", basename(file_name), "\n")
    
    # return(list(
    #   individual_plots = plots,
    #   combined_plot = combined_plot,
    #   balance_statistics = balance_stats,
    #   output_file = file_name
    # ))
    
  }
  
  cat("✓ Defined balance test plotting function\n")
  
  # --------------------------------------------------------------------------
  # CREATE BALANCE PLOTS FOR OUTCOMES
  # --------------------------------------------------------------------------
  cat("\nCreating balance plots for outcomes...\n")
  
  results <- create_balance_plots(
    controls_subset = outcomes,
    file_name = paste0(path_figures, "RDD_outcomes_shortest_dist_", analysis_bandwidth, ".png"),
    b = analysis_bandwidth,
    df_data = df_rdd
  )
  
  
  
  cat("\n✓ RDD balance tests analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("RDD BALANCE TESTS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

# Generate RDD balance tests analysis
generate_rdd_balance_analysis(processed_data_path, path_figures, analysis_bandwidth = 10000)
generate_rdd_balance_analysis(processed_data_path, path_figures, analysis_bandwidth = 20000)