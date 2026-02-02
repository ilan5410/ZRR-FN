# ==============================================================================
# RDD Later Elections Analysis - Panel Results Across Time
# ==============================================================================
# This script analyzes the persistence of ZRR treatment effects across multiple
# election years (2002-2022) using regression discontinuity design with
# HC1 clustered standard errors at the canton level
# Multiple bandwidths: 20km, 10km, and 5km analysis
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate RDD results across election years with multiple bandwidths
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param controls Vector of control variables
#' @return Data frame with results and saves visualization
generate_rdd_later_elections <- function(processed_data_path, path_figures, controls) {
  
  cat("===============================================\n")
  cat("RDD LATER ELECTIONS ANALYSIS PIPELINE\n")
  cat("===============================================\n")
  cat("Analysis years: 2002, 2007, 2012, 2017, 2022\n")
  cat("Bandwidths: 20km, 10km, 5km\n")
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
  required_objects <- c("dfZRRControls")
  missing_objects <- setdiff(required_objects, ls())
  if (length(missing_objects) > 0) {
    stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
  }
  
  cat("✓ Validated required data objects\n")
  cat("  - dfZRRControls:", nrow(dfZRRControls), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE DATA FOR RDD ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n2. Preparing data for RDD analysis...\n")
  
  # Filter and prepare the dataset
  df_rdd <- dfZRRControls %>%
    filter(x >= (-20000) & x <= (20000)) %>%
    mutate(
      dist = x,                     # Create a distance variable
      treatmentZRR = z,            # Define treatment indicator
      pop = log(pop),              # Log-transform population
      popDensity = log(popDensity) # Log-transform population density
    ) %>%
    distinct(codecommune, .keep_all = TRUE)  # Remove duplicate communes
  
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  cat("✓ Prepared RDD dataset\n")
  cat("  - Observations after filtering:", nrow(df_rdd), "\n")
  cat("  - Unique communes:", length(unique(df_rdd$codecommune)), "\n")
  cat("  - Distance range:", round(min(df_rdd$x), 2), "to", round(max(df_rdd$x), 2), "\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE ANALYSIS PARAMETERS
  # --------------------------------------------------------------------------
  cat("\n3. Defining analysis parameters...\n")
  
  # Define conditional variables
  conditional_on <- c(controls, "dep")
  
  # Initialize results data frame
  results <- data.frame(Year = c(2002, 2007, 2012, 2017, 2022))
  outputs <- c("FN2002", "FN2007", "FN2012", "FN2017", "FN2022")
  bw <- c(20000, 10000, 5000)
  
  cat("✓ Defined analysis parameters\n")
  cat("  - Election years:", paste(results$Year, collapse = ", "), "\n")
  cat("  - Outcome variables:", paste(outputs, collapse = ", "), "\n")
  cat("  - Bandwidths (m):", paste(bw, collapse = ", "), "\n")
  cat("  - Conditional variables:", length(conditional_on), "variables\n")
  
  # --------------------------------------------------------------------------
  # 4. RUN REGRESSION MODELS FOR EACH YEAR AND BANDWIDTH
  # --------------------------------------------------------------------------
  cat("\n4. Running regression models across years and bandwidths...\n")
  
  # Regression loop
  for (i in seq_along(outputs)) {
    y <- outputs[i]
    year <- results$Year[i]
    
    cat("  - Processing", year, "election...\n")
    
    # Define previous FN variables as controls
    previous_controls <- if (i > 1) paste(outputs[1:(i-1)], collapse = " + ") else ""
    
    # Build formula with conditional_on and previous_controls
    formula_str <- paste(y, "~ z + x + ", paste(conditional_on, collapse = " + "))
    if (nchar(previous_controls) > 0) {
      formula_str <- paste(formula_str, " + ", previous_controls)
    }
    formula <- as.formula(formula_str)
    
    cat("    Formula:", formula_str, "\n")
    
    # Bandwidth 20000
    cat("    - Bandwidth 20km...\n")
    df_filtered <- filter(df_rdd, x >= -20000 & x <= 20000)
    model <- lm(formula, data = df_filtered)
    cluster_se <- coeftest(model, vcov = vcovHC(model, type = "HC1", 
                                                cluster = "group", 
                                                cluster.id = df_rdd$canton))
    results$Est_20000[i] <- cluster_se["zTRUE", "Estimate"]
    results$SE_20000[i] <- cluster_se["zTRUE", "Std. Error"]
    results$Pval_20000[i] <- cluster_se["zTRUE", "Pr(>|t|)"]
    
    # Bandwidth 10000
    cat("    - Bandwidth 10km...\n")
    df_filtered <- filter(df_rdd, x >= -10000 & x <= 10000)
    model <- lm(formula, data = df_filtered)
    cluster_se <- coeftest(model, vcov = vcovHC(model, type = "HC1", 
                                                cluster = "group", 
                                                cluster.id = df_rdd$canton))
    results$Est_10000[i] <- cluster_se["zTRUE", "Estimate"]
    results$SE_10000[i] <- cluster_se["zTRUE", "Std. Error"]
    results$Pval_10000[i] <- cluster_se["zTRUE", "Pr(>|t|)"]
    
    # Bandwidth 5000
    cat("    - Bandwidth 5km...\n")
    df_filtered <- filter(df_rdd, x >= -5000 & x <= 5000)
    model <- lm(formula, data = df_filtered)
    cluster_se <- coeftest(model, vcov = vcovHC(model, type = "HC1", 
                                                cluster = "group", 
                                                cluster.id = df_rdd$canton))
    results$Est_5000[i] <- cluster_se["zTRUE", "Estimate"]
    results$SE_5000[i] <- cluster_se["zTRUE", "Std. Error"]
    results$Pval_5000[i] <- cluster_se["zTRUE", "Pr(>|t|)"]
    
    cat("    ✓ Completed", year, "\n")
  }
  
  cat("✓ Completed all", length(outputs), "election year regressions\n")
  
  # --------------------------------------------------------------------------
  # 5. ADD SIGNIFICANCE INDICATORS
  # --------------------------------------------------------------------------
  cat("\n5. Adding significance indicators...\n")
  
  # Add significance stars
  results <- results %>%
    mutate(
      Sig_20000 = ifelse(Pval_20000 < 0.001, "***", 
                         ifelse(Pval_20000 < 0.01, "**", 
                                ifelse(Pval_20000 < 0.05, "*", ""))),
      Sig_10000 = ifelse(Pval_10000 < 0.001, "***", 
                         ifelse(Pval_10000 < 0.01, "**", 
                                ifelse(Pval_10000 < 0.05, "*", ""))),
      Sig_5000 = ifelse(Pval_5000 < 0.001, "***", 
                        ifelse(Pval_5000 < 0.01, "**", 
                               ifelse(Pval_5000 < 0.05, "*", "")))
    )
  
  cat("✓ Added significance stars\n")
  
  # --------------------------------------------------------------------------
  # 6. RESHAPE DATA FOR VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n6. Reshaping data for visualization...\n")
  
  # Reshape to long format
  df_long <- results %>%
    pivot_longer(cols = matches("Est_|SE_"), 
                 names_to = c("Metric", "Bandwidth"), 
                 names_pattern = "(Est|SE)_(\\d+)", 
                 values_to = "Value") %>%
    pivot_wider(names_from = Metric, values_from = Value) %>%
    left_join(
      results %>%
        pivot_longer(cols = matches("Sig_"), 
                     names_to = c("Metric", "Bandwidth"), 
                     names_pattern = "(Sig)_(\\d+)", 
                     values_to = "Sig") %>%
        select(Year, Bandwidth, Sig),
      by = c("Year", "Bandwidth")
    ) %>%
    mutate(Bandwidth = paste("Bandwidth", Bandwidth))
  
  cat("✓ Reshaped data to long format\n")
  cat("  - Total observations:", nrow(df_long), "\n")
  
  # --------------------------------------------------------------------------
  # 7. CREATE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n7. Creating visualization...\n")
  
  # Create the plot
  p <- ggplot(df_long, aes(x = Year, y = Est, group = Bandwidth, color = Bandwidth)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymin = Est - SE, ymax = Est + SE), width = 0.2, linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      x = "Election Year",
      y = "Coefficient Estimate",
      color = "Bandwidth",
      caption = "Note: Error bars represent standard errors clustered at the canton level.\nControls include FN shares in previous election results."
    ) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0)
    ) +
    scale_x_continuous(breaks = c(2002, 2007, 2012, 2017, 2022)) +
    scale_color_manual(values = c("Bandwidth 5000" = "#1b9e77", 
                                  "Bandwidth 10000" = "#d95f02", 
                                  "Bandwidth 20000" = "#7570b3"))
  
  cat("✓ Created visualization\n")
  
  # --------------------------------------------------------------------------
  # 8. EXPORT RESULTS
  # --------------------------------------------------------------------------
  cat("\n8. Exporting results...\n")
  
  # Export the combined plot
  file_path <- file.path(path_figures, "later_elections.png")
  ggsave(file_path, p, width = 10, height = 8)
  
  cat("✓ Exported visualization\n")
  cat("  - Output file:", file_path, "\n")
  
  # --------------------------------------------------------------------------
  # 9. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n9. Results Summary:\n")
  
  # Display key statistics for each year and bandwidth
  for (i in seq_along(results$Year)) {
    year <- results$Year[i]
    cat("  -", year, "election:\n")
    
    cat("    20km bandwidth: Est =", round(results$Est_20000[i], 4), 
        ", SE =", round(results$SE_20000[i], 4), 
        ", p =", round(results$Pval_20000[i], 4), 
        results$Sig_20000[i], "\n")
    
    cat("    10km bandwidth: Est =", round(results$Est_10000[i], 4), 
        ", SE =", round(results$SE_10000[i], 4), 
        ", p =", round(results$Pval_10000[i], 4), 
        results$Sig_10000[i], "\n")
    
    cat("    5km bandwidth: Est =", round(results$Est_5000[i], 4), 
        ", SE =", round(results$SE_5000[i], 4), 
        ", p =", round(results$Pval_5000[i], 4), 
        results$Sig_5000[i], "\n")
  }
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("RDD LATER ELECTIONS ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_rdd_later_elections(processed_data_path, path_figures, controls)
