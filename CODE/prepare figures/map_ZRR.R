# ==============================================================================
# ZRR Program Municipality Map Analysis
# ==============================================================================
# This script creates a comprehensive spatial visualization of municipalities
# participating in the ZRR (Zone de Revitalisation Rurale) program, showing
# treatment timing and geographical distribution across France
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate ZRR program municipality map analysis
#' @param processed_data_path Path to processed data directory
#' @param raw_data_path Path to raw data directory containing shapefiles
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 10)
#' @param figure_height Height of output figure in inches (default: 8)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param reference_year Year to use for treatment status (default: 1988)
#' @param treatment_year Main treatment year to highlight (default: 1995)
#' @return List containing map plot, spatial data, and treatment statistics
generate_zrr_map_analysis <- function(processed_data_path, raw_data_path, path_figures,
                                      figure_width = 10,
                                      figure_height = 8,
                                      figure_dpi = 300,
                                      reference_year = 1988,
                                      treatment_year = 1995
) {
  
  cat("===============================================\n")
  cat("ZRR PROGRAM MUNICIPALITY MAP ANALYSIS\n")
  cat("===============================================\n")
  shapefile_name = "communes-20220101-shp"
  cat("Reference year:", reference_year, "\n")
  cat("Main treatment year:", treatment_year, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("Shapefile directory:", shapefile_name, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD TREATMENT DATA
  # --------------------------------------------------------------------------
  cat("1. Loading treatment data...\n")
  
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
  # 2. PREPARE TREATMENT STATUS DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing treatment status data...\n")
  
  # Filter and prepare treatment data
  dfMap <- dfZRRControls %>%
    filter(year == reference_year) %>%
    mutate(
      treatment_status = case_when(
        year_treat == treatment_year ~ paste0("treat_", treatment_year),
        year_treat > treatment_year ~ "after 2004",
        year_treat == 0 ~ "never treated",
        TRUE ~ "other"
      )
    ) %>%
    select(codecommune, treatment_status, year_treat, treatment)
  
  cat("✓ Prepared treatment status data\n")
  cat("  - Municipalities after filtering:", nrow(dfMap), "\n")
  
  # Display treatment status distribution
  treatment_counts <- dfMap %>% count(treatment_status, sort = TRUE)
  cat("  - Treatment status distribution:\n")
  for (i in 1:nrow(treatment_counts)) {
    cat("    ", treatment_counts$treatment_status[i], ":", treatment_counts$n[i], "municipalities\n")
  }
  
  # --------------------------------------------------------------------------
  # 3. LOAD AND PREPARE SHAPEFILE DATA
  # --------------------------------------------------------------------------
  cat("\n3. Loading and preparing shapefile data...\n")
  
  # Construct shapefile path
  shapefile_path <- file.path(raw_data_path, shapefile_name, paste0(gsub("-shp$", "", shapefile_name), ".shp"))
  
  if (!file.exists(shapefile_path)) {
    stop("Shapefile not found: ", shapefile_path)
  }
  
  # Load shapefile
  dfShape <- st_read(shapefile_path, quiet = TRUE) %>%
    select(geometry, insee) %>%
    mutate(codecommune = sub("^0+", "", as.character(insee))) %>%
    select(-insee)
  
  cat("✓ Loaded shapefile data\n")
  cat("  - Shapefile path:", shapefile_path, "\n")
  cat("  - Geometric features loaded:", nrow(dfShape), "\n")
  
  # --------------------------------------------------------------------------
  # 4. MERGE TREATMENT AND GEOGRAPHIC DATA
  # --------------------------------------------------------------------------
  cat("\n4. Merging treatment and geographic data...\n")
  
  # Merge datasets
  dfMap <- merge(dfMap, dfShape, by = "codecommune", all.y = TRUE)
  dfMap <- st_as_sf(dfMap)
  
  cat("✓ Merged datasets\n")
  cat("  - Total features after merge:", nrow(dfMap), "\n")
  
  # Check merge success
  matched_municipalities <- sum(!is.na(dfMap$treatment_status))
  unmatched_municipalities <- sum(is.na(dfMap$treatment_status))
  
  cat("  - Successfully matched municipalities:", matched_municipalities, "\n")
  cat("  - Unmatched municipalities:", unmatched_municipalities, "\n")
  
  # --------------------------------------------------------------------------
  # 5. FILTER AND CLEAN GEOGRAPHIC DATA
  # --------------------------------------------------------------------------
  cat("\n5. Filtering and cleaning geographic data...\n")
  
  # Filter to metropolitan France (exclude overseas territories)
  dfMap <- dfMap %>%
    filter(as.numeric(codecommune) <= 96000) %>%
    mutate(treatment_status = replace_na(treatment_status, "never treated"))
  
  cat("✓ Filtered and cleaned geographic data\n")
  cat("  - Municipalities after geographic filtering:", nrow(dfMap), "\n")
  
  # Final treatment status distribution
  final_treatment_counts <- dfMap %>%
    st_drop_geometry() %>%
    count(treatment_status, sort = TRUE)
  
  cat("  - Final treatment status distribution:\n")
  for (i in 1:nrow(final_treatment_counts)) {
    cat("    ", final_treatment_counts$treatment_status[i], ":", 
        final_treatment_counts$n[i], "municipalities (", 
        sprintf("%.1f%%", (final_treatment_counts$n[i] / nrow(dfMap)) * 100), ")\n")
  }
  
  # --------------------------------------------------------------------------
  # 6. VALIDATE GEOGRAPHIC DATA INTEGRITY
  # --------------------------------------------------------------------------
  cat("\n6. Validating geographic data integrity...\n")
  
  # Check for invalid geometries
  invalid_geoms <- sum(!st_is_valid(dfMap))
  empty_geoms <- sum(st_is_empty(dfMap))
  
  cat("✓ Validated geographic data\n")
  cat("  - Invalid geometries:", invalid_geoms, "\n")
  cat("  - Empty geometries:", empty_geoms, "\n")
  
  # Calculate geographic extent
  bbox <- st_bbox(dfMap)
  cat("  - Geographic extent:\n")
  cat("    Longitude: ", sprintf("%.2f", bbox["xmin"]), " to ", sprintf("%.2f", bbox["xmax"]), "\n")
  cat("    Latitude: ", sprintf("%.2f", bbox["ymin"]), " to ", sprintf("%.2f", bbox["ymax"]), "\n")
  
  # --------------------------------------------------------------------------
  # 7. CALCULATE SPATIAL STATISTICS
  # --------------------------------------------------------------------------
  cat("\n7. Calculating spatial statistics...\n")
  
  # Calculate areas (in km²) - approximate for visualization purposes
  dfMap <- dfMap %>%
    mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)
  
  # Treatment status area analysis
  area_by_treatment <- dfMap %>%
    st_drop_geometry() %>%
    group_by(treatment_status) %>%
    summarise(
      total_area_km2 = sum(area_km2, na.rm = TRUE),
      mean_area_km2 = mean(area_km2, na.rm = TRUE),
      median_area_km2 = median(area_km2, na.rm = TRUE),
      n_municipalities = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_area_km2))
  
  cat("✓ Calculated spatial statistics\n")
  cat("  - Area coverage by treatment status:\n")
  for (i in 1:nrow(area_by_treatment)) {
    cat("    ", area_by_treatment$treatment_status[i], ": ", 
        sprintf("%.0f", area_by_treatment$total_area_km2[i]), " km² (", 
        area_by_treatment$n_municipalities[i], " municipalities)\n")
  }
  
  # --------------------------------------------------------------------------
  # 8. DEFINE MAP AESTHETICS
  # --------------------------------------------------------------------------
  cat("\n8. Defining map aesthetics...\n")
  
  # Define color scheme
  color_scheme <- setNames(
    c("#000000", "#4A4A4A", "#CCCCCC"),          # the values
    c(paste0("treat_", treatment_year),"after 2004", "never treated")  # the names
  )
  
  
  # Define labels
  
  label_scheme <- setNames(
    c(paste("Treatment in", treatment_year), "Treatment after 2004", "Never Treated"),          # the values
    c(paste0("treat_", treatment_year),"after 2004", "never treated")  # the names
  )
  
  cat("✓ Defined map aesthetics\n")
  cat("  - Color scheme: Black (", treatment_year, "), Dark Grey (after 2004), Light Grey (never)\n")
  
  # --------------------------------------------------------------------------
  # 9. CREATE MAP VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n9. Creating map visualization...\n")
  
  # Create the main map
  p <- ggplot(dfMap) +
    geom_sf(aes(fill = treatment_status), color = NA, size = 0) +
    scale_fill_manual(
      values = color_scheme,
      labels = label_scheme,
      name = "Treatment Status"
    ) +
    labs(
      # title = "ZRR Program Treatment Status of French Municipalities",
      # subtitle = paste("Geographic distribution of treatment timing (Reference year:", reference_year, ")"),
      caption = "Source: ZRR program data and INSEE municipality boundaries"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = ggplot2::margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey40", margin = ggplot2::margin(b = 15)),
      plot.caption = element_text(hjust = 0.5, size = 9, color = "grey50", margin = ggplot2::margin(t = 10)),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.8, "cm"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
  
  cat("✓ Created map visualization\n")
  cat("  - Map includes", nrow(dfMap), "municipalities\n")
  cat("  - Treatment categories:", length(unique(dfMap$treatment_status)), "\n")
  
  # --------------------------------------------------------------------------
  # 10. ADD MAP ENHANCEMENTS
  # --------------------------------------------------------------------------
  cat("\n10. Adding map enhancements...\n")
  
  # Calculate treatment concentration for potential hotspot analysis
  treatment_summary <- dfMap %>%
    st_drop_geometry() %>%
    filter(treatment_status != "never treated") %>%
    summarise(
      treated_municipalities = n(),
      treated_percentage = (n() / nrow(dfMap)) * 100
    )
  
  cat("✓ Added map enhancements\n")
  cat("  - Total treated municipalities:", treatment_summary$treated_municipalities, "\n")
  cat("  - Treated percentage:", sprintf("%.1f%%", treatment_summary$treated_percentage), "\n")
  
  # --------------------------------------------------------------------------
  # 11. SAVE MAP VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n11. Saving map visualization...\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Save the map
  output_file <- file.path(path_figures, "map_ZRR.png")
  ggsave(
    filename = output_file,
    plot = p,
    device = "png",
    width = figure_width,
    height = figure_height,
    dpi = figure_dpi,
    bg = "white"
  )
  
  cat("✓ Saved map visualization\n")
  cat("  - Output file:", output_file, "\n")
  cat("  - Dimensions:", figure_width, "x", figure_height, "inches\n")
  cat("  - Resolution:", figure_dpi, "DPI\n")
  
  # --------------------------------------------------------------------------
  # 12. GENERATE SPATIAL SUMMARY STATISTICS
  # --------------------------------------------------------------------------
  cat("\n12. Generating spatial summary statistics...\n")
  
  # Create comprehensive spatial summary
  spatial_summary <- list(
    treatment_distribution = final_treatment_counts,
    area_analysis = area_by_treatment,
    geographic_extent = list(
      longitude_range = c(bbox["xmin"], bbox["xmax"]),
      latitude_range = c(bbox["ymin"], bbox["ymax"]),
      total_area_km2 = sum(dfMap$area_km2, na.rm = TRUE)
    ),
    data_quality = list(
      total_municipalities = nrow(dfMap),
      matched_municipalities = matched_municipalities,
      invalid_geometries = invalid_geoms,
      empty_geometries = empty_geoms
    )
  )
  
  cat("✓ Generated spatial summary statistics\n")
  cat("  - Total geographic coverage:", sprintf("%.0f", spatial_summary$geographic_extent$total_area_km2), "km²\n")
  
  # --------------------------------------------------------------------------
  # 13. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n13. Results Summary:\n")
  
  # Treatment effectiveness summary
  main_treatment_count <- final_treatment_counts$n[final_treatment_counts$treatment_status == paste0("treat_", treatment_year)]
  later_treatment_count <- final_treatment_counts$n[final_treatment_counts$treatment_status == "after 2004"]
  never_treated_count <- final_treatment_counts$n[final_treatment_counts$treatment_status == "never treated"]
  
  cat("  - ZRR Program Coverage:\n")
  cat("    Main treatment (", treatment_year, "):", main_treatment_count, "municipalities (", 
      sprintf("%.1f%%", (main_treatment_count / nrow(dfMap)) * 100), ")\n")
  cat("    Later treatment (after 2004):", later_treatment_count, "municipalities (", 
      sprintf("%.1f%%", (later_treatment_count / nrow(dfMap)) * 100), ")\n")
  cat("    Never treated:", never_treated_count, "municipalities (", 
      sprintf("%.1f%%", (never_treated_count / nrow(dfMap)) * 100), ")\n")
  
  # Geographic distribution
  largest_treatment_area <- area_by_treatment$treatment_status[1]
  cat("\n  - Geographic Distribution:\n")
  cat("    Largest area coverage:", largest_treatment_area, "\n")
  cat("    Average municipality size:", sprintf("%.1f", mean(dfMap$area_km2, na.rm = TRUE)), "km²\n")
  
  # Data completeness
  completeness_rate <- (matched_municipalities / nrow(dfMap)) * 100
  cat("\n  - Data Quality:\n")
  cat("    Geographic matching rate:", sprintf("%.1f%%", completeness_rate), "\n")
  cat("    Shapefile features processed:", nrow(dfShape), "\n")
  cat("    Final map features:", nrow(dfMap), "\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("ZRR MAP ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_zrr_map_analysis(processed_data_path, raw_data_path, path_figures)
