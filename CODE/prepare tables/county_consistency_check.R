# ==============================================================================
# Checks that no different treatment status within county
# ==============================================================================
# 
# ==============================================================================

# ==============================================================================
# MAIN PROCESSING FUNCTION
# ==============================================================================


check_canton_treatment_overlap <- function(raw_data_path, baseline_year = 1995) {
  cat("===============================================\n")
  cat("CANTON × TREATMENT CONSISTENCY CHECK\n")
  cat("===============================================\n")
  cat("Baseline year:", baseline_year, "\n\n")
  
  # ------------------------------------------------------------------------
  # 1) LOAD DATA (with checks)
  # ------------------------------------------------------------------------
  zrr_path <- file.path(raw_data_path, "ZRR.csv")
  canton_dbf <- file.path(raw_data_path, "france1999.dbf")
  
  if (!file.exists(zrr_path)) stop("❌ Missing file: ", zrr_path)
  if (!file.exists(canton_dbf)) stop("❌ Missing file: ", canton_dbf)
  
  cat("1. Loading source files...\n")
  dfZRR_raw <- read.csv(zrr_path)
  cat("   ✓ ZRR.csv loaded. Rows:", nrow(dfZRR_raw), "\n")
  
  df_canton <- sf::st_read(canton_dbf, quiet = TRUE) %>%
    mutate(
      codecommune = paste0(as.character(DEP), as.character(COM)),  # NOTE: pad COM if needed
      codecanton  = paste0(as.character(DEP), as.character(CT))
    ) %>%
    dplyr::select(codecommune, codecanton, NCC, DEP, REG) %>%
    dplyr::distinct(codecommune, .keep_all = TRUE)  # ensure one row per commune
  
  cat("   ✓ france1999.dbf loaded. Rows:", nrow(df_canton), 
      " | Unique communes:", dplyr::n_distinct(df_canton$codecommune), "\n\n")
  
  # ------------------------------------------------------------------------
  # 2) BUILD BASELINE DATA (JOIN + FILTER)
  # ------------------------------------------------------------------------
  cat("2. Joining ZRR with canton mapping and filtering baseline year...\n")
  df_base <- dfZRR_raw %>%
    dplyr::left_join(df_canton, by = "codecommune", relationship = "many-to-one") %>%
    dplyr::filter(year == baseline_year)
  
  cat("   ✓ Baseline rows:", nrow(df_base), 
      " | Cantons in baseline:", dplyr::n_distinct(df_base$codecanton), "\n\n")
  
  # ------------------------------------------------------------------------
  # 3) CANTON-LEVEL CONSISTENCY CHECK
  # ------------------------------------------------------------------------
  cat("3. Checking within-canton treatment consistency...\n")
  canton_check <- df_base %>%
    dplyr::rename(z = treatment) %>%
    dplyr::filter(!is.na(codecanton)) %>%
    dplyr::group_by(codecanton) %>%
    dplyr::summarise(
      unique_z_values = dplyr::n_distinct(z, na.rm = TRUE),
      n_communes      = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(unique_z_values > 1) %>%
    dplyr::arrange(codecanton)
  
  if (nrow(canton_check) == 0) {
    cat("   ✓ No overlap: all communes within each canton are consistently treated or not treated.\n\n")
  } else {
    cat("   ⚠️  Overlap detected in", nrow(canton_check), "cantons.\n\n")
  }
  
  # ------------------------------------------------------------------------
  # 4) LIST PROBLEMATIC MUNICIPALITIES (DETAIL)
  # ------------------------------------------------------------------------
  cat("4. Listing municipalities in problematic cantons...\n")
  problematic_municipalities <- df_base %>%
    dplyr::filter(codecanton %in% canton_check$codecanton) %>%
    dplyr::select(codecommune, codecanton, treatment, DEP, REG) %>%
    dplyr::arrange(REG, DEP, codecanton, codecommune)
  
  cat("   ✓ Problematic municipalities:", nrow(problematic_municipalities), "\n\n")
  
  # ------------------------------------------------------------------------
  # 5) SUMMARY + REFERENCE
  # ------------------------------------------------------------------------
  cat("5. Summary:\n")
  cat("   - Total cantons (baseline):", dplyr::n_distinct(df_base$codecanton), "\n")
  cat("   - Cantons with overlap:", nrow(canton_check), "\n")
  cat("   - Municipalities in overlapping cantons:", nrow(problematic_municipalities), "\n")
  cat("   - Note: Some overlaps correspond to legal exceptions.\n")
  cat("     Ref: https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000000191822 (search for \"seules les communes de\").\n")
  cat("===============================================\n")
  
  # Return structured results
  list(
    baseline = df_base,
    canton_overlap = canton_check,
    problematic_municipalities = problematic_municipalities
  )
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

check_canton_treatment_overlap(raw_data_path, baseline_year = 1995) 

