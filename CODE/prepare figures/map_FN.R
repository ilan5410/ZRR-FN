# ==============================================================================
# French Election Map Analysis - FN Vote Share Visualization
# ==============================================================================
# This script creates comparative spatial visualizations of Front National (FN)
# vote shares in the first round of the 1988 and 2002 presidential elections,
# showing geographic patterns and temporal evolution across French municipalities
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate FN vote share comparative map analysis
#' @param processed_data_path Path to processed data directory
#' @param raw_data_path Path to raw data directory containing shapefiles
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 10)
#' @param figure_height Height of output figure in inches (default: 16)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param election_year_1 First election year for comparison (default: 1988)
#' @param election_year_2 Second election year for comparison (default: 2002)
#' @param vote_scale_max Maximum value for vote share scale (default: 0.5)
#' @return List containing combined plot, individual maps, and electoral statistics
generate_fn_vote_map_analysis <- function(processed_data_path, raw_data_path, path_figures,
                                          figure_width = 10,
                                          figure_height = 16,
                                          figure_dpi = 300,
                                          election_year_1 = 1988,
                                          election_year_2 = 2002,
                                          vote_scale_max = 0.5
) {
  
  cat("===============================================\n")
  cat("FRENCH ELECTION MAP ANALYSIS - FN VOTE SHARE\n")
  cat("===============================================\n")
  shapefile_name <- "communes-20220101-shp"
  cat("Election years for comparison:", election_year_1, "vs", election_year_2, "\n")
  cat("Vote share scale range: 0 to", vote_scale_max, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("Shapefile directory:", shapefile_name, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD ELECTION DATA
  # --------------------------------------------------------------------------
  cat("1. Loading election data...\n")
  
  data_file <- file.path(processed_data_path, "dataDes.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded dataDes.RData\n")
  
  # Check if required data exists
  if (!exists("dfZRR")) {
    stop("Required dataset 'dfZRR' not found in loaded environment")
  }
  
  cat("  - Dataset 'dfZRR' available with", nrow(dfZRR), "observations\n")
  
  # Verify required vote share variables exist
  fn_var_1 <- paste0("FN", election_year_1)
  fn_var_2 <- paste0("FN", election_year_2)
  
  if (!fn_var_1 %in% names(dfZRR) || !fn_var_2 %in% names(dfZRR)) {
    stop("Required FN vote share variables not found: ", fn_var_1, " and/or ", fn_var_2)
  }
  
  cat("  - Found vote share variables:", fn_var_1, "and", fn_var_2, "\n")
  
  # --------------------------------------------------------------------------
  # 2. LOAD AND PREPARE SHAPEFILE DATA
  # --------------------------------------------------------------------------
  cat("\n2. Loading and preparing shapefile data...\n")
  
  # Construct shapefile path
  shapefile_path <- file.path(raw_data_path, shapefile_name, paste0(gsub("-shp$", "", shapefile_name), ".shp"))
  
  if (!file.exists(shapefile_path)) {
    stop("Shapefile not found: ", shapefile_path)
  }
  
  # Load shapefile
  dfShape <- st_read(shapefile_path, quiet = TRUE) %>%
    select(geometry, insee) %>%
    mutate(
      codecommune = as.character(insee),
      insee = sub("^0+", "", as.character(insee))
    ) %>%
    select(geometry, insee)
  
  cat("✓ Loaded shapefile data\n")
  cat("  - Shapefile path:", shapefile_path, "\n")
  cat("  - Geometric features loaded:", nrow(dfShape), "\n")
  
  # --------------------------------------------------------------------------
  # 3. DEFINE DATA PREPROCESSING HELPER FUNCTION
  # --------------------------------------------------------------------------
  cat("\n3. Defining data preprocessing functions...\n")
  
  #' Preprocess election data for mapping
  #' @param df Election data dataframe
  #' @param dfShape Shapefile dataframe
  #' @param var Variable name to extract
  #' @return Preprocessed spatial dataframe
  preprocess_electoral_data <- function(df, dfShape, var) {
    
    cat("    - Processing variable:", var, "\n")
    
    # Prepare election data
    df_processed <- df %>%
      rename(insee = codecommune) %>%
      select(all_of(var), insee) %>%
      mutate(insee = sub("^0+", "", as.character(insee)))
    
    cat("      Election data prepared:", nrow(df_processed), "municipalities\n")
    
    # Merge with geographic data
    merged_df <- df_processed %>%
      inner_join(dfShape, by = "insee") %>%
      select(insee, all_of(var), geometry) %>%
      mutate_at(vars(one_of(var)), as.numeric) %>%
      filter_all(all_vars(!is.na(.))) %>%
      filter_if(is.numeric, all_vars(is.finite(.)))
    
    cat("      Final merged data:", nrow(merged_df), "municipalities\n")
    
    return(merged_df)
  }
  
  cat("✓ Defined preprocessing functions\n")
  
  # --------------------------------------------------------------------------
  # 4. PREPROCESS ELECTION DATA FOR BOTH YEARS
  # --------------------------------------------------------------------------
  cat("\n4. Preprocessing election data for both years...\n")
  
  # Process 1988 election data
  cat("  Processing", election_year_1, "election data:\n")
  df1988 <- preprocess_electoral_data(dfZRR, dfShape, fn_var_1)
  colnames(df1988)[colnames(df1988) == fn_var_1] <- "FN"
  
  # Process 2002 election data
  cat("  Processing", election_year_2, "election data:\n")
  df2002 <- preprocess_electoral_data(dfZRR, dfShape, fn_var_2)
  colnames(df2002)[colnames(df2002) == fn_var_2] <- "FN"
  
  cat("✓ Preprocessed election data for both years\n")
  cat("  -", election_year_1, "municipalities with valid data:", nrow(df1988), "\n")
  cat("  -", election_year_2, "municipalities with valid data:", nrow(df2002), "\n")
  
  # --------------------------------------------------------------------------
  # 5. CREATE COMBINED DATASET FOR SCALE HARMONIZATION
  # --------------------------------------------------------------------------
  cat("\n5. Creating combined dataset for scale harmonization...\n")
  
  # Combine datasets to determine common scale
  combined_df <- rbind(
    df1988 %>% mutate(year = election_year_1), 
    df2002 %>% mutate(year = election_year_2)
  )
  
  # Calculate vote share statistics
  vote_stats <- combined_df %>%
    st_drop_geometry() %>%
    summarise(
      min_vote = min(FN, na.rm = TRUE),
      max_vote = max(FN, na.rm = TRUE),
      mean_vote = mean(FN, na.rm = TRUE),
      median_vote = median(FN, na.rm = TRUE),
      q75_vote = quantile(FN, 0.75, na.rm = TRUE),
      q95_vote = quantile(FN, 0.95, na.rm = TRUE)
    )
  
  cat("✓ Created combined dataset\n")
  cat("  - Total observations:", nrow(combined_df), "\n")
  cat("  - Vote share statistics:\n")
  cat("    Min:", sprintf("%.3f", vote_stats$min_vote), "\n")
  cat("    Max:", sprintf("%.3f", vote_stats$max_vote), "\n")
  cat("    Mean:", sprintf("%.3f", vote_stats$mean_vote), "\n")
  cat("    Median:", sprintf("%.3f", vote_stats$median_vote), "\n")
  cat("    75th percentile:", sprintf("%.3f", vote_stats$q75_vote), "\n")
  cat("    95th percentile:", sprintf("%.3f", vote_stats$q95_vote), "\n")
  
  # Define common scale limits
  common_scale_limits <- c(0, vote_scale_max)
  cat("  - Using common scale limits:", common_scale_limits[1], "to", common_scale_limits[2], "\n")
  
  # --------------------------------------------------------------------------
  # 6. CALCULATE YEAR-SPECIFIC ELECTORAL STATISTICS
  # --------------------------------------------------------------------------
  cat("\n6. Calculating year-specific electoral statistics...\n")
  
  # Statistics for 1988
  stats_1988 <- df1988 %>%
    st_drop_geometry() %>%
    summarise(
      year = election_year_1,
      municipalities = n(),
      mean_vote = mean(FN, na.rm = TRUE),
      median_vote = median(FN, na.rm = TRUE),
      max_vote = max(FN, na.rm = TRUE),
      above_10pct = sum(FN > 0.1, na.rm = TRUE),
      above_20pct = sum(FN > 0.2, na.rm = TRUE)
    )
  
  # Statistics for 2002
  stats_2002 <- df2002 %>%
    st_drop_geometry() %>%
    summarise(
      year = election_year_2,
      municipalities = n(),
      mean_vote = mean(FN, na.rm = TRUE),
      median_vote = median(FN, na.rm = TRUE),
      max_vote = max(FN, na.rm = TRUE),
      above_10pct = sum(FN > 0.1, na.rm = TRUE),
      above_20pct = sum(FN > 0.2, na.rm = TRUE)
    )
  
  electoral_stats <- rbind(stats_1988, stats_2002)
  
  cat("✓ Calculated year-specific statistics\n")
  cat("  - Electoral performance summary:\n")
  for (i in 1:nrow(electoral_stats)) {
    year <- electoral_stats$year[i]
    cat("    ", year, ":\n")
    cat("      Municipalities:", electoral_stats$municipalities[i], "\n")
    cat("      Mean vote share:", sprintf("%.1f%%", electoral_stats$mean_vote[i] * 100), "\n")
    cat("      Max vote share:", sprintf("%.1f%%", electoral_stats$max_vote[i] * 100), "\n")
    cat("      Above 10%:", electoral_stats$above_10pct[i], "municipalities\n")
    cat("      Above 20%:", electoral_stats$above_20pct[i], "municipalities\n")
  }
  
  # --------------------------------------------------------------------------
  # 7. DEFINE MAP CREATION HELPER FUNCTION
  # --------------------------------------------------------------------------
  cat("\n7. Defining map creation functions...\n")
  
  #' Create standardized map plot for FN vote share
  #' @param data Spatial dataframe with vote share data
  #' @param var Variable name for vote share
  #' @param title Plot title
  #' @param scale_limits Common scale limits for comparison
  #' @return ggplot map object
  create_fn_vote_map <- function(data, var, title, scale_limits) {
    
    ggplot(data) +
      geom_sf(aes_string(geometry = "geometry", fill = var), color = NA, size = 0) +
      scale_fill_gradient(
        name = "Share of votes",
        na.value = "white",
        low = "white",
        high = "black",
        limits = scale_limits,
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(title = title) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = ggplot2::margin(b = 10)),
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.6, "cm"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )
  }
  
  cat("✓ Defined map creation functions\n")
  
  # --------------------------------------------------------------------------
  # 8. CREATE INDIVIDUAL MAPS
  # --------------------------------------------------------------------------
  cat("\n8. Creating individual maps...\n")
  
  # Create map for 1988
  p1 <- create_fn_vote_map(
    data = df1988,
    var = "FN",
    title = "",
    scale_limits = common_scale_limits
  )
  
  # Create map for 2002
  p2 <- create_fn_vote_map(
    data = df2002,
    var = "FN",
    title = "",
    scale_limits = common_scale_limits
  )
  
  cat("✓ Created individual maps\n")
  cat("  - Map 1 (", election_year_1, "):", nrow(df1988), "municipalities visualized\n")
  cat("  - Map 2 (", election_year_2, "):", nrow(df2002), "municipalities visualized\n")
  
  # --------------------------------------------------------------------------
  # 9. COMBINE MAPS INTO COMPARATIVE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n9. Combining maps into comparative visualization...\n")
  
  # Create panel labels
  panel_label_1 <- paste("Panel A: FN vote share in", election_year_1)
  panel_label_2 <- paste("Panel B: FN vote share in", election_year_2)
  
  # Combine the plots into one figure
  combined_plot <- cowplot::plot_grid(
    p1, p2,
    ncol = 1,
    labels = c(panel_label_1, panel_label_2),
    label_size = 12,
    label_fontface = "bold",
    label_x = 0.02,
    label_y = 0.98,
    hjust = 0,
    vjust = 1
  )
  
  cat("✓ Combined maps into comparative visualization\n")
  cat("  - Layout: 2 panels arranged vertically\n")
  cat("  - Panel A:", election_year_1, "election results\n")
  cat("  - Panel B:", election_year_2, "election results\n")
  
  # --------------------------------------------------------------------------
  # 10. CALCULATE TEMPORAL CHANGE ANALYSIS
  # --------------------------------------------------------------------------
  cat("\n10. Calculating temporal change analysis...\n")
  
  # Find common municipalities between both years
  common_municipalities <- intersect(df1988$insee, df2002$insee)
  
  if (length(common_municipalities) > 0) {
    # Create change analysis dataset
    change_analysis <- df1988 %>%
      st_drop_geometry() %>%
      filter(insee %in% common_municipalities) %>%
      select(insee, vote_1988 = FN) %>%
      inner_join(
        df2002 %>% 
          st_drop_geometry() %>%
          select(insee, vote_2002 = FN),
        by = "insee"
      ) %>%
      mutate(
        vote_change = vote_2002 - vote_1988,
        vote_change_pct = (vote_2002 - vote_1988) / vote_1988 * 100
      )
    
    # Calculate change statistics
    change_stats <- change_analysis %>%
      summarise(
        n_municipalities = n(),
        mean_change = mean(vote_change, na.rm = TRUE),
        median_change = median(vote_change, na.rm = TRUE),
        increased = sum(vote_change > 0, na.rm = TRUE),
        decreased = sum(vote_change < 0, na.rm = TRUE),
        no_change = sum(vote_change == 0, na.rm = TRUE)
      )
    
    cat("✓ Calculated temporal change analysis\n")
    cat("  - Common municipalities between years:", length(common_municipalities), "\n")
    cat("  - Vote share evolution:\n")
    cat("    Mean change:", sprintf("%.1f", change_stats$mean_change * 100), "percentage points\n")
    cat("    Municipalities with increase:", change_stats$increased, "\n")
    cat("    Municipalities with decrease:", change_stats$decreased, "\n")
    cat("    Municipalities with no change:", change_stats$no_change, "\n")
    
  } else {
    cat("  - No common municipalities found between years\n")
    change_stats <- NULL
  }
  
  # --------------------------------------------------------------------------
  # 11. SAVE COMPARATIVE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n11. Saving comparative visualization...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Define output file path
  output_file <- file.path(path_figures, "map_FN.png")
  
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
  
  cat("✓ Saved comparative visualization\n")
  cat("  - Output file:", output_file, "\n")
  cat("  - Dimensions:", figure_width, "x", figure_height, "inches\n")
  cat("  - Resolution:", figure_dpi, "DPI\n")
  
  # --------------------------------------------------------------------------
  # 12. COMPILE ANALYSIS RESULTS
  # --------------------------------------------------------------------------
  cat("\n12. Compiling analysis results...\n")
  
  # Create comprehensive results object
  analysis_results <- list(
    combined_plot = combined_plot,
    individual_maps = list(
      map_1988 = p1,
      map_2002 = p2
    ),
    electoral_data = list(
      data_1988 = df1988,
      data_2002 = df2002,
      combined_data = combined_df
    ),
    statistics = list(
      overall_stats = vote_stats,
      yearly_stats = electoral_stats,
      change_analysis = if(exists("change_stats")) change_stats else NULL
    ),
    metadata = list(
      election_years = c(election_year_1, election_year_2),
      scale_limits = common_scale_limits,
      output_file = output_file,
      creation_date = Sys.time()
    )
  )
  
  cat("✓ Compiled analysis results\n")
  cat("  - Result components:", length(analysis_results), "\n")
  cat("  - Individual maps available for further analysis\n")
  cat("  - Statistical summaries included\n")
  
  # --------------------------------------------------------------------------
  # 13. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n13. Results Summary:\n")
  
  cat("  - FN Electoral Performance Comparison:\n")
  cat("    ", election_year_1, "average:", sprintf("%.1f%%", stats_1988$mean_vote * 100), 
      "(", stats_1988$municipalities, "municipalities)\n")
  cat("    ", election_year_2, "average:", sprintf("%.1f%%", stats_2002$mean_vote * 100), 
      "(", stats_2002$municipalities, "municipalities)\n")
  
  if (!is.null(change_stats)) {
    cat("    Average change:", sprintf("%.1f", change_stats$mean_change * 100), "percentage points\n")
  }
  
  cat("\n  - Geographic Coverage:\n")
  cat("    Total municipalities visualized:", nrow(combined_df), "\n")
  cat("    Vote share scale: 0% to", sprintf("%.0f%%", vote_scale_max * 100), "\n")
  cat("    Highest vote share observed:", sprintf("%.1f%%", vote_stats$max_vote * 100), "\n")
  
  cat("\n  - Output:\n")
  cat("    Comparative map saved to:", basename(output_file), "\n")
  cat("    Figure dimensions:", figure_width, "×", figure_height, "inches\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("FRENCH ELECTION MAP ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_fn_vote_map_analysis(processed_data_path, raw_data_path, path_figures)