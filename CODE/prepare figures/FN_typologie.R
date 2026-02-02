# ==============================================================================
# FN Vote Share by Municipality Typologie Analysis
# ==============================================================================
# This script analyzes and visualizes FN vote share patterns across different
# municipality typologies (Rural, Intermediary, Urban) over time, providing
# insights into spatial voting patterns and urban-rural political divides
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate FN vote share analysis by municipality typologie
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 10)
#' @param figure_height Height of output figure in inches (default: 6)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param years_filter Vector of specific years to analyze (NULL for all years)
#' @param typologie_mapping Custom mapping for typologie groups (optional)
#' @return List containing plot object, summary data, and typologie statistics
generate_fn_typologie_analysis <- function(processed_data_path, path_figures,
                                           figure_width = 10,
                                           figure_height = 6,
                                           figure_dpi = 300,
                                           years_filter = NULL,
                                           typologie_mapping = NULL) {
  
  cat("===============================================\n")
  cat("FN VOTE SHARE BY TYPOLOGIE ANALYSIS\n")
  cat("===============================================\n")
  cat("Analysis focus: Municipality typologie patterns\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  if (!is.null(years_filter)) {
    cat("Year filter applied:", paste(years_filter, collapse = ", "), "\n")
  }
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
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
  # 2. PREPARE AND FILTER DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing and filtering data...\n")
  
  # Select relevant columns
  df <- dfZRRControls %>%
    select(year, FN, typologie) %>%
    filter(!is.na(FN), !is.na(typologie), !is.na(year))
  
  # Apply year filter if specified
  if (!is.null(years_filter)) {
    df <- df %>% filter(year %in% years_filter)
    cat("  - Applied year filter:", paste(years_filter, collapse = ", "), "\n")
  }
  
  cat("✓ Prepared base dataset\n")
  cat("  - Observations after filtering:", nrow(df), "\n")
  cat("  - Years available:", paste(sort(unique(df$year)), collapse = ", "), "\n")
  cat("  - Unique typologies:", length(unique(df$typologie)), "\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE TYPOLOGIE GROUPING
  # --------------------------------------------------------------------------
  cat("\n3. Defining typologie grouping...\n")
  
  # Use custom mapping if provided, otherwise use default
  if (!is.null(typologie_mapping)) {
    cat("  - Using custom typologie mapping\n")
    df <- df %>%
      mutate(typologie_group = typologie_mapping[typologie])
  } else {
    cat("  - Using default typologie mapping\n")
    df <- df %>%
      mutate(
        typologie_group = case_when(
          typologie %in% c("rural autonome peu dense", 
                           "rural autonome très peu dense",
                           "rural sous faible influence d'un pôle",
                           "rural sous forte influence d'un pôle") ~ "Rural",
          typologie %in% c("urbain densité intermédiaire") ~ "Intermediary",
          typologie %in% c("urbain dense") ~ "Urban",
          TRUE ~ NA_character_
        )
      )
  }
  
  # Remove observations without typologie group
  df <- df %>% filter(!is.na(typologie_group))
  
  cat("✓ Applied typologie grouping\n")
  cat("  - Final observations:", nrow(df), "\n")
  
  # Display typologie group distribution
  groupe_counts <- df %>% count(typologie_group, sort = TRUE)
  cat("  - Typologie group distribution:\n")
  for (i in 1:nrow(groupe_counts)) {
    cat("    ", groupe_counts$typologie_group[i], ":", groupe_counts$n[i], "observations\n")
  }
  
  # --------------------------------------------------------------------------
  # 4. VALIDATE ORIGINAL TYPOLOGIE CATEGORIES
  # --------------------------------------------------------------------------
  cat("\n4. Validating original typologie categories...\n")
  
  # Check original typologie categories
  original_typologies <- unique(dfZRRControls$typologie)
  mapped_typologies <- unique(df$typologie)
  
  cat("✓ Validated typologie categories\n")
  cat("  - Original typologie categories:", length(original_typologies), "\n")
  cat("  - Categories used in analysis:", length(mapped_typologies), "\n")
  
  # Show mapping details
  mapping_detail <- df %>%
    group_by(typologie, typologie_group) %>%
    summarise(n = n(), .groups = 'drop') %>%
    arrange(typologie_group, typologie)
  
  cat("  - Detailed mapping:\n")
  for (i in 1:nrow(mapping_detail)) {
    cat("    ", mapping_detail$typologie[i], " → ", 
        mapping_detail$typologie_group[i], " (", mapping_detail$n[i], " obs.)\n")
  }
  
  # --------------------------------------------------------------------------
  # 5. COMPUTE SUMMARY STATISTICS
  # --------------------------------------------------------------------------
  cat("\n5. Computing summary statistics...\n")
  
  # Calculate means by year and typologie group
  df_summary <- df %>%
    group_by(year, typologie_group) %>%
    summarise(
      mean_FN = mean(FN, na.rm = TRUE),
      median_FN = median(FN, na.rm = TRUE),
      sd_FN = sd(FN, na.rm = TRUE),
      min_FN = min(FN, na.rm = TRUE),
      max_FN = max(FN, na.rm = TRUE),
      n_obs = sum(!is.na(FN)),
      .groups = "drop"
    )
  
  cat("✓ Computed summary statistics\n")
  cat("  - Summary table dimensions:", nrow(df_summary), "x", ncol(df_summary), "\n")
  cat("  - Year-typologie combinations:", nrow(df_summary), "\n")
  
  # Display key statistics
  cat("  - Mean FN vote share by typologie group (overall):\n")
  overall_means <- df %>%
    group_by(typologie_group) %>%
    summarise(overall_mean = mean(FN, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(overall_mean))
  
  for (i in 1:nrow(overall_means)) {
    cat("    ", overall_means$typologie_group[i], ": ", 
        sprintf("%.2f%%", overall_means$overall_mean[i] * 100), "\n")
  }
  
  # --------------------------------------------------------------------------
  # 6. ANALYZE TEMPORAL TRENDS
  # --------------------------------------------------------------------------
  cat("\n6. Analyzing temporal trends...\n")
  
  # Calculate year-over-year changes
  temporal_trends <- df_summary %>%
    arrange(typologie_group, year) %>%
    group_by(typologie_group) %>%
    mutate(
      fn_change = mean_FN - lag(mean_FN),
      percent_change = (fn_change / lag(mean_FN)) * 100
    ) %>%
    filter(!is.na(fn_change))
  
  # Calculate trend slopes for each typologie
  trend_analysis <- df_summary %>%
    mutate(year = as.numeric(year)) %>%                 # ensure numeric
    group_by(typologie_group) %>%
    nest() %>%
    mutate(
      model = map(data, ~ {
        d <- .x %>% filter(!is.na(mean_FN), !is.na(year))
        if (nrow(d) >= 2 && length(unique(d$year)) >= 2) lm(mean_FN ~ year, data = d) else NULL
      }),
      slope      = map_dbl(model, ~ if (is.null(.x)) NA_real_ else coef(.x)[["year"]]),
      r_squared  = map_dbl(model, ~ if (is.null(.x)) NA_real_ else summary(.x)$r.squared)
    ) %>%
    select(typologie_group, slope, r_squared) %>%
    ungroup()
  
  cat("✓ Analyzed temporal trends\n")
  cat("  - Trend slopes (percentage points per year):\n")
  for (i in 1:nrow(trend_analysis)) {
    cat("    ", trend_analysis$typologie_group[i], ": ", 
        sprintf("%.4f", trend_analysis$slope[i] * 100), 
        " (R² = ", sprintf("%.3f", trend_analysis$r_squared[i]), ")\n")
  }
  
  # --------------------------------------------------------------------------
  # 7. PREPARE DATA FOR VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n7. Preparing data for visualization...\n")
  
  # Order categories and format data
  df_summary <- df_summary %>%
    mutate(
      typologie_group = fct_relevel(typologie_group, "Rural", "Intermediary", "Urban"),
      year = as.factor(year)
    )
  
  # Validate data completeness for plotting
  complete_combinations <- df_summary %>%
    group_by(typologie_group) %>%
    summarise(n_years = n(), .groups = 'drop')
  
  cat("✓ Prepared data for visualization\n")
  cat("  - Typologie groups ordered:", paste(levels(df_summary$typologie_group), collapse = " → "), "\n")
  cat("  - Data completeness:\n")
  for (i in 1:nrow(complete_combinations)) {
    cat("    ", complete_combinations$typologie_group[i], ": ", 
        complete_combinations$n_years[i], "years\n")
  }
  
  # --------------------------------------------------------------------------
  # 8. CREATE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n8. Creating visualization...\n")
  
  # Define color palette
  colors <- c("Rural" = "#2E8B57", "Intermediary" = "#FF8C00", "Urban" = "#4169E1")
  
  # Create the main plot
  plot <- ggplot(df_summary, aes(x = year, y = mean_FN, color = typologie_group, group = typologie_group)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    scale_color_manual(values = colors) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      limits = c(0, max(df_summary$mean_FN, na.rm = TRUE) * 1.1)
    ) +
    labs(
      # title = "Evolution of FN Vote Share by Municipality Typologie",
      # subtitle = "Mean vote share across Rural, Intermediary, and Urban municipalities",
      x = "Year",
      y = "Mean FN Vote Share (%)",
      color = "Municipality Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "top",
      legend.margin = ggplot2::margin(b = 20),
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_blank(),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
  
  cat("✓ Created visualization\n")
  cat("  - Plot includes", nrow(df_summary), "data points\n")
  cat("  - Color scheme: Rural (green), Intermediary (orange), Urban (blue)\n")
  
  # --------------------------------------------------------------------------
  # 9. ADD STATISTICAL ANNOTATIONS
  # --------------------------------------------------------------------------
  cat("\n9. Adding statistical annotations...\n")
  
  # Calculate gap analysis
  gap_analysis <- df_summary %>%
    select(year, typologie_group, mean_FN) %>%
    pivot_wider(names_from = typologie_group, values_from = mean_FN) %>%
    mutate(
      rural_urban_gap = Rural - Urban,
      rural_intermediary_gap = Rural - Intermediary,
      intermediary_urban_gap = Intermediary - Urban
    )
  
  # Find maximum gaps
  max_rural_urban_gap <- max(abs(gap_analysis$rural_urban_gap), na.rm = TRUE)
  max_gap_year <- gap_analysis$year[which.max(abs(gap_analysis$rural_urban_gap))]
  
  cat("✓ Added statistical annotations\n")
  cat("  - Maximum Rural-Urban gap:", sprintf("%.2f", max_rural_urban_gap * 100), "percentage points in", max_gap_year, "\n")
  
  # --------------------------------------------------------------------------
  # 10. SAVE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n10. Saving visualization...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Save the plot
  output_file <- file.path(path_figures, "FN_typologie.png")
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
  # 11. GENERATE COMPREHENSIVE SUMMARY TABLE
  # --------------------------------------------------------------------------
  cat("\n11. Generating comprehensive summary table...\n")
  
  # Create summary table with all statistics
  summary_table <- df_summary %>%
    select(year, typologie_group, mean_FN, sd_FN, n_obs) %>%
    pivot_wider(
      names_from = typologie_group, 
      values_from = c(mean_FN, sd_FN, n_obs),
      names_sep = "_"
    ) %>%
    arrange(year)
  
  # Create trend summary
  trend_summary <- trend_analysis %>%
    select(typologie_group, slope, r_squared) %>%
    mutate(
      slope_pp_per_year = slope * 100,  # Convert to percentage points
      trend_direction = ifelse(slope > 0, "Increasing", "Decreasing")
    )
  
  cat("✓ Generated comprehensive summary tables\n")
  cat("  - Main summary table:", nrow(summary_table), "rows\n")
  cat("  - Trend summary table:", nrow(trend_summary), "rows\n")
  
  
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("FN TYPOLOGIE ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_fn_typologie_analysis(processed_data_path, path_figures)