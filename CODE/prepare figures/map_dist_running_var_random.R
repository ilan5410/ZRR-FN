# ==============================================================================
# Map + Distribution of the Running Variable (RDD Frontiers)
# ==============================================================================
# This function builds (i) a choropleth with distance-to-frontier categories and
# treatment boundary overlay, and (ii) a histogram of the running variable.
# It saves a combined figure to `path_figures`.
# ==============================================================================

#' Generate map and distribution of the running variable
#' @param processed_data_path Path to processed data directory (ending with '/')
#' @param path_figures Path to output figures directory (ending with '/')
#' @param figure_width Width of output figure in inches (default: 10)
#' @param figure_height Height of output figure in inches (default: 12)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param bandwidth Half-window around the threshold in meters (default: 20000)
#' @param threshold Threshold for the running variable (default: 0)
#' @return NULL (no return; saves figure to disk)
generate_running_variable_map <- function(processed_data_path,
                                          path_figures,
                                          figure_width = 10,
                                          figure_height = 12,
                                          figure_dpi = 300,
                                          bandwidth = 20000) {
  threshold = 0
  cat("===============================================\n")
  cat("MAP + RUNNING VARIABLE DISTRIBUTION (RDD)\n")
  cat("===============================================\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("RDD window: [", threshold - bandwidth, ", ", threshold + bandwidth, "] (meters)\n\n", sep = "")
  
  # --------------------------------------------------------------------------
  # LOAD DATA ENV
  # --------------------------------------------------------------------------
  cat("1. Loading data environment...\n")
  rdata_file <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(rdata_file)) {
    stop("Data environment does not exist: ", rdata_file)
  }
  load(rdata_file)
  cat("✓ Loaded:", basename(rdata_file), "\n")
  
  
  # --------------------------------------------------------------------------
  # 2. LOAD AND PREPARE SHAPEFILE DATA
  # --------------------------------------------------------------------------
  cat("\n2. Loading and preparing shapefile data...\n")
  
  # Construct shapefile path
  shapefile_name <- "communes-20220101-shp"
  shapefile_path <- file.path(raw_data_path, shapefile_name, paste0(gsub("-shp$", "", shapefile_name), ".shp"))
  
  if (!file.exists(shapefile_path)) {
    stop("Shapefile not found: ", shapefile_path)
  }
  
  # Load shapefile
  dfShape <- st_read(shapefile_path, quiet = TRUE) %>%
    select(geometry, insee) %>%
    mutate(
      codecommune = as.character(insee),
      codecommune = sub("^0+", "", as.character(codecommune))
    ) %>%
    select(geometry, codecommune)
  
  cat("✓ Loaded shapefile data\n")
  cat("  - Shapefile path:", shapefile_path, "\n")
  cat("  - Geometric features loaded:", nrow(dfShape), "\n")
  
  
  # --------------------------------------------------------------------------
  # SHAPE & MERGE PREP
  # --------------------------------------------------------------------------
  
  # Sanity checks
  if (!inherits(dfShape, "sf")) stop("'dfShape' must be an sf object")
  if (!all(c("codecommune", "geometry") %in% names(dfShape))) {
    stop("'dfShape' must contain 'codecommune' and 'geometry'")
  }
  
  dfMap <- read_excel(paste0(processed_data_path, "dataGeoRDD_canton_random/dataGeoRDD_canton_random1.xlsx")) %>%
    mutate(
      codecommune = sub("^0+", "", as.character(codecommune)),
      x = distance_to_border,
      z = ifelse(x < 0, 1, 0),
      treatment = z,
      distance_to_border = x
    ) %>%
    select(codecommune, treatment, distance_to_border)
  
  n_before <- nrow(dfMap)
  dfMap <- dfMap %>% right_join(dfShape, by = "codecommune")
  n_after <- nrow(dfMap)
  cat("  - Merge rows before/after:", n_before, "→", n_after, "\n")
  
  # Coerce to sf
  dfMap <- st_as_sf(dfMap)
  cat("  - dfMap is sf:", inherits(dfMap, "sf"), "\n")
  
  # --------------------------------------------------------------------------
  # GEOMETRY OPS: TREATMENT UNION
  # --------------------------------------------------------------------------
  cat("\n3. Building treatment boundary overlay...\n")
  treatment_areas <- dfMap %>% filter(treatment == 1 & !st_is_empty(geometry))
  if (nrow(treatment_areas) == 0) stop("No treated geometries found for overlay.")
  treatment_union <- suppressWarnings(st_union(treatment_areas$geometry))
  treatment_union_sf <- st_sf(geometry = st_sfc(treatment_union, crs = st_crs(dfMap)))
  cat("  - Treatment polygons:", nrow(treatment_areas), " | Union: 1 feature\n")
  
  # --------------------------------------------------------------------------
  # CATEGORIZE DISTANCE-TO-BORDER
  # --------------------------------------------------------------------------
  cat("\n4. Categorizing distance to frontier...\n")
  # Use an explicit 'Missing' category instead of imputing a sentinel
  cuts <- c(-60000, -5000, 5000, 60000, Inf)
  labels <- c("-60 – -5", "-5 – 5", "5 – 60", "Above 60")
  dfMap <- dfMap %>%
    mutate(
      distance_cat = case_when(
        is.na(distance_to_border) ~ "Missing",
        TRUE ~ as.character(cut(distance_to_border, breaks = cuts, labels = labels, include.lowest = TRUE))
      ),
      distance_cat = factor(distance_cat, levels = c("-60 – -5", "-5 – 5", "5 – 60", "Above 60", "Missing"))
    ) %>%
    filter(distance_cat != "Missing")
  
  
  # --------------------------------------------------------------------------
  # BUILD MAP
  # --------------------------------------------------------------------------
  cat("\n5. Creating map layer...\n")
  # Grayscale palette (including 'Missing')
  fill_vals <- c("black", "gray30", "gray60", "gray85", "white")
  names(fill_vals) <- c("-60 – -5", "-5 – 5", "5 – 60", "Above 60", "Missing")
  
  map_plot <- ggplot(dfMap) +
    geom_sf(aes(geometry = geometry, fill = distance_cat), color = NA) +
    scale_fill_manual(values = fill_vals, drop = FALSE, name = "Distance Category (km)") +
    geom_sf(data = treatment_union_sf, fill = NA, color = "black", linewidth = 0.2) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9)
    )
  cat("✓ Map prepared\n")
  
  # --------------------------------------------------------------------------
  # BUILD HISTOGRAM (WITHIN RDD WINDOW)
  # --------------------------------------------------------------------------
  cat("\n6. Preparing histogram data...\n")
  lower_bound <-  - bandwidth
  upper_bound <-  + bandwidth
  
  df_rdd <- dfMap %>%
    mutate(x = distance_to_border) %>%
    filter(x >= lower_bound, x <= upper_bound) %>%
    arrange(codecommune) %>%
    distinct(codecommune, .keep_all = TRUE) %>%
    mutate(
      dist = x,
      # group by sign of distance to match your legend text
      dist_group = if_else(dist < 0, "Below 0: In program", "Above 0: Not in program")
    )
  
  cat("  - RDD rows:", nrow(df_rdd), " | Window:", lower_bound, "to", upper_bound, "(m)\n")
  
  p_dis <- ggplot(df_rdd, aes(x = dist, fill = dist_group)) +
    geom_histogram(color = "white", boundary = 0, bins = 100) +
    scale_fill_manual(values = c("Below 0: In program" = "gray20",
                                 "Above 0: Not in program" = "gray80")) +
    labs(x = "Distance to program frontier (meters)", 
         y = "Number of localities") +
    theme_minimal() +
    theme(
      legend.position = c(0.8, 0.8),
      legend.title = element_blank(),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.box.background = element_rect(color = "black")
    )
  cat("✓ Histogram prepared (100 bins, boundary at 0)\n")
  
  # --------------------------------------------------------------------------
  # COMBINE & SAVE
  # --------------------------------------------------------------------------
  cat("\n7. Combining panels and saving...\n")
  combined_plot <- map_plot / p_dis + patchwork::plot_layout(ncol = 1)
  
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  out_file <- file.path(path_figures, "map_dist_running_var_random.png")
  ggsave(
    filename = out_file,
    plot = combined_plot,
    device = "png",
    width = figure_width,
    height = figure_height,
    dpi = figure_dpi,
    bg = "white"
  )
  
  cat("✓ Saved figure\n")
  cat("  - File:", out_file, "\n")
  cat("  - Size:", figure_width, "x", figure_height, "in @", figure_dpi, "DPI\n")
  
  cat("\n✓ Completed successfully\n")
  cat("\n===============================================\n")
  cat("RUNNING VARIABLE MAP + DISTRIBUTION COMPLETED\n")
  cat("===============================================\n")
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_running_variable_map(processed_data_path, path_figures)
