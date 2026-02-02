# ==============================================================================
# Evolution of FN Vote Share Analysis (1988-2022) - Presidential/Legislative Elections
# ==============================================================================
# This script analyzes and visualizes the evolution of Front National (FN) vote
# share across French municipalities from 1988 to 2022, comparing presidential
# and legislative elections with comprehensive data processing and visualization
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate FN vote share evolution analysis and visualization
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 8)
#' @param figure_height Height of output figure in inches (default: 6)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param presidential_years Vector of presidential election years to analyze
#' @param legislative_data Tibble with legislative election data (year, mean_vote_share)
#' @return List containing plot object, summary data, and statistics
generate_fn_voteshare_evolution_analysis <- function(processed_data_path, path_figures,
                                                     figure_width = 8,
                                                     figure_height = 6,
                                                     figure_dpi = 300,
                                                     presidential_years = c(1988, 1995, 2002, 2007, 2012, 2017, 2022),
                                                     legislative_data = NULL) {
  
  cat("===============================================\n")
  cat("FN VOTE SHARE EVOLUTION ANALYSIS\n")
  cat("===============================================\n")
  cat("Analysis period: 1988-2022\n")
  cat("Presidential election years:", paste(presidential_years, collapse = ", "), "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
  data_file <- file.path(processed_data_path, "main.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded main.RData\n")
  
  # Check if required data exists
  if (!exists("dfZRR_raw")) {
    stop("Required dataset 'dfZRR_raw' not found in loaded environment")
  }
  
  cat("  - Dataset 'dfZRR_raw' available with", nrow(dfZRR_raw), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE PRESIDENTIAL ELECTION DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing presidential election data...\n")
  
  # Filter and select presidential vote share data
  df_pres <- dfZRR_raw %>%
    filter(year == 2005) %>%  # Use 2005 as reference year for static data
    select(codecommune, FN1988, FN1995, FN2002, FN2007, FN2012, FN2017, FN2022) %>%
    distinct() %>%
    mutate(across(everything(), as.numeric))
  
  cat("✓ Extracted presidential election data\n")
  cat("  - Unique communes:", nrow(df_pres), "\n")
  cat("  - Presidential election variables:", sum(startsWith(names(df_pres), "FN")), "\n")
  
  # Reshape to long format
  df_pres_long <- df_pres %>%
    pivot_longer(cols = starts_with("FN"), names_to = "year", values_to = "vote_share") %>%
    mutate(year = as.numeric(gsub("FN", "", year)))
  
  cat("  - Reshaped to long format:", nrow(df_pres_long), "observations\n")
  
  # --------------------------------------------------------------------------
  # 3. CALCULATE PRESIDENTIAL SUMMARY STATISTICS
  # --------------------------------------------------------------------------
  cat("\n3. Calculating presidential summary statistics...\n")
  
  # Calculate summary statistics by year
  df_pres_summary <- df_pres_long %>%
    group_by(year) %>%
    summarize(
      mean_vote_share = mean(vote_share, na.rm = TRUE),
      sd_vote_share = sd(vote_share, na.rm = TRUE),
      median_vote_share = median(vote_share, na.rm = TRUE),
      min_vote_share = min(vote_share, na.rm = TRUE),
      max_vote_share = max(vote_share, na.rm = TRUE),
      n_obs = sum(!is.na(vote_share)),
      .groups = 'drop'
    ) %>%
    mutate(type = "Presidential")
  
  cat("✓ Calculated presidential summary statistics\n")
  cat("  - Years covered:", paste(sort(unique(df_pres_summary$year)), collapse = ", "), "\n")
  cat("  - Average observations per year:", round(mean(df_pres_summary$n_obs)), "\n")
  
  # Display key presidential statistics
  cat("  - Presidential vote share evolution:\n")
  for (i in 1:nrow(df_pres_summary)) {
    year_data <- df_pres_summary[i, ]
    cat("    ", year_data$year, ": ", 
        sprintf("%.1f%%", year_data$mean_vote_share * 100), 
        " (SD: ", sprintf("%.1f%%", year_data$sd_vote_share * 100), ")\n")
  }
  
  # --------------------------------------------------------------------------
  # 4. PREPARE LEGISLATIVE ELECTION DATA
  # --------------------------------------------------------------------------
  cat("\n4. Preparing legislative election data...\n")
  
  # Use provided legislative data or default values
  if (is.null(legislative_data)) {
    cat("  - Using default legislative election data\n")
    df_leg_summary <- tibble(
      year = c(1988, 1993, 1997, 2002, 2007, 2012, 2017, 2022),
      mean_vote_share = c(0.097, 0.124, 0.150, 0.113, 0.043, 0.136, 0.132, 0.187),
      sd_vote_share = NA_real_,
      median_vote_share = NA_real_,
      min_vote_share = NA_real_,
      max_vote_share = NA_real_,
      n_obs = NA_integer_,
      type = "Legislative"
    )
  } else {
    cat("  - Using provided legislative election data\n")
    df_leg_summary <- legislative_data %>%
      mutate(
        sd_vote_share = NA_real_,
        median_vote_share = NA_real_,
        min_vote_share = NA_real_,
        max_vote_share = NA_real_,
        n_obs = NA_integer_,
        type = "Legislative"
      )
  }
  
  cat("✓ Prepared legislative election data\n")
  cat("  - Legislative election years:", paste(sort(df_leg_summary$year), collapse = ", "), "\n")
  
  # Display legislative statistics
  cat("  - Legislative vote share values:\n")
  for (i in 1:nrow(df_leg_summary)) {
    year_data <- df_leg_summary[i, ]
    cat("    ", year_data$year, ": ", 
        sprintf("%.1f%%", year_data$mean_vote_share * 100), "\n")
  }
  
  # --------------------------------------------------------------------------
  # 5. COMBINE AND VALIDATE DATASETS
  # --------------------------------------------------------------------------
  cat("\n5. Combining and validating datasets...\n")
  
  # Combine presidential and legislative data
  df_summary_combined <- bind_rows(df_pres_summary, df_leg_summary) %>%
    arrange(year, type)
  
  # Validate combined dataset
  total_observations <- nrow(df_summary_combined)
  years_covered <- length(unique(df_summary_combined$year))
  election_types <- unique(df_summary_combined$type)
  
  cat("✓ Combined datasets successfully\n")
  cat("  - Total observations:", total_observations, "\n")
  cat("  - Years covered:", years_covered, "\n")
  cat("  - Election types:", paste(election_types, collapse = ", "), "\n")
  
  # Check for data quality issues
  missing_values <- sum(is.na(df_summary_combined$mean_vote_share))
  if (missing_values > 0) {
    warning("Found ", missing_values, " missing vote share values")
  }
  
  # --------------------------------------------------------------------------
  # 6. CALCULATE COMPARATIVE STATISTICS
  # --------------------------------------------------------------------------
  cat("\n6. Calculating comparative statistics...\n")
  
  # Calculate election type comparisons
  type_stats <- df_summary_combined %>%
    group_by(type) %>%
    summarize(
      mean_across_years = mean(mean_vote_share, na.rm = TRUE),
      sd_across_years = sd(mean_vote_share, na.rm = TRUE),
      min_vote_share = min(mean_vote_share, na.rm = TRUE),
      max_vote_share = max(mean_vote_share, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("✓ Calculated comparative statistics\n")
  cat("  - Presidential elections average:", sprintf("%.1f%%", type_stats$mean_across_years[type_stats$type == "Presidential"] * 100), "\n")
  cat("  - Legislative elections average:", sprintf("%.1f%%", type_stats$mean_across_years[type_stats$type == "Legislative"] * 100), "\n")
  
  # Calculate year-over-year changes
  yearly_changes <- df_summary_combined %>%
    arrange(type, year) %>%
    group_by(type) %>%
    mutate(
      vote_change = mean_vote_share - lag(mean_vote_share),
      percent_change = (vote_change / lag(mean_vote_share)) * 100
    ) %>%
    filter(!is.na(vote_change))
  
  cat("  - Calculated year-over-year changes for trend analysis\n")
  
  # --------------------------------------------------------------------------
  # 7. CREATE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n7. Creating visualization...\n")
  
  # Define color and aesthetic parameters
  colors <- c("Presidential" = "blue", "Legislative" = "red")
  
  # Create the main plot
  plot <- ggplot(df_summary_combined, aes(x = year, y = mean_vote_share, color = type, linetype = type)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = sort(unique(df_summary_combined$year)),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, max(df_summary_combined$mean_vote_share, na.rm = TRUE) * 1.1)
    ) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = c("Presidential" = "solid", "Legislative" = "dashed")) +
    labs(
      # title = "Evolution of FN Vote Share (1988–2022)",
      # subtitle = "Presidential and Legislative Elections in France",
      x = "Year",
      y = "Mean Vote Share (%)",
      color = "Election Type",
      linetype = "Election Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  
  cat("✓ Created visualization\n")
  cat("  - Plot includes", nrow(df_summary_combined), "data points\n")
  cat("  - Time range: ", min(df_summary_combined$year), "-", max(df_summary_combined$year), "\n")
  
  # --------------------------------------------------------------------------
  # 8. ADD TREND ANALYSIS ELEMENTS
  # --------------------------------------------------------------------------
  cat("\n8. Adding trend analysis elements...\n")
  
  # Calculate overall trends
  pres_trend <- df_summary_combined %>%
    filter(type == "Presidential") %>%
    lm(mean_vote_share ~ year, data = .)
  
  leg_trend <- df_summary_combined %>%
    filter(type == "Legislative") %>%
    lm(mean_vote_share ~ year, data = .)
  
  pres_slope <- coef(pres_trend)[2] * 100  # Convert to percentage points per year
  leg_slope <- coef(leg_trend)[2] * 100
  
  cat("✓ Calculated trend analysis\n")
  cat("  - Presidential trend:", sprintf("%.3f", pres_slope), "percentage points per year\n")
  cat("  - Legislative trend:", sprintf("%.3f", leg_slope), "percentage points per year\n")
  
  # --------------------------------------------------------------------------
  # 9. SAVE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n9. Saving visualization...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Save the plot
  output_file <- file.path(path_figures, "evolution_FN_voteshare.png")
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
  
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("FN VOTE SHARE EVOLUTION ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_fn_voteshare_evolution_analysis(processed_data_path, path_figures)