# ================================================================================
# SUMMARY STATISTICS BY BANDWIDTH
# ================================================================================

build_bandwidth_summaries <- function(processed_data_path,
                                      out_tex = file.path(path_tables, "summary_stats_bandwidth.tex")) {
  cat("===============================================\n")
  cat("SUMMARY STATISTICS FOR DIFFERENT BANDWIDTHS\n")
  cat("===============================================\n")
  cat("Bandwidths (meters):", paste(bandwidths, collapse = ", "), "\n\n")
  
  # --------------------------------------------------------------------------
  # 0) LOAD DATA + SANITY CHECKS
  # --------------------------------------------------------------------------
  sharp_path <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(sharp_path)) stop("❌ script_sharp.RData not found at: ", sharp_path)
  
  env_sharp <- new.env()
  load(sharp_path, envir = env_sharp)
  
  if (!exists("dfZRRControls", envir = env_sharp))
    stop("❌ dfZRRControls not found in script_sharp.RData")
  
  dfZRRControls <- get("dfZRRControls", envir = env_sharp)
  
  # clean controls set
  controls_clean <- unique(setdiff(c(controls, "FN1988"), "typologie"))
  controls_clean <- intersect(controls_clean, names(dfZRRControls))  # keep only existing vars
  
  req_cols <- c("x", "z", "pop")
  missing_req <- setdiff(req_cols, names(dfZRRControls))
  if (length(missing_req)) stop("❌ Missing required columns in dfZRRControls: ", paste(missing_req, collapse = ", "))
  
  dfZRRControls <- clean_data_variables(dfZRRControls, controls_clean)
  
  cat("✓ Data loaded. Observations:", nrow(dfZRRControls), 
      "| Controls used:", length(controls_clean), "\n\n")
  
  # --------------------------------------------------------------------------
  # 1) HELPERS
  # --------------------------------------------------------------------------
  # choose available canton column (prefer 'canton', then 'codecanton'); optional 'dep'
  pick_canton_col <- function(df) {
    if ("canton" %in% names(df)) return("canton")
    if ("codecanton" %in% names(df)) return("codecanton")
    return(NA_character_)
  }
  
  # Panel-level stats inside a dataframe (for one z-group inside a bandwidth)
  compute_panel_stats <- function(df) {
    if (nrow(df) == 0L) {
      tibble::tibble(
        `Number of cantons`           = NA_real_,
        `Average communes per canton` = NA_real_,
        `Average pop per canton`      = NA_real_,
        `Average SD of pop`           = NA_real_,
        `Average Min of pop`          = NA_real_,
        `Average Max of pop`          = NA_real_
      )
    } else {
      cant_col <- pick_canton_col(df)
      dep_col  <- if ("dep" %in% names(df)) "dep" else NA_character_
      
      # canton identifier
      if (is.na(cant_col)) {
        nb_canton <- NA_real_
        groups <- df %>% dplyr::mutate(..grp.. = 1L) %>% dplyr::group_by(..grp..)
      } else {
        if (!is.na(dep_col)) {
          nb_canton <- df %>% dplyr::distinct(.data[[cant_col]], .data[[dep_col]]) %>% nrow()
          groups <- df %>% dplyr::group_by(.data[[cant_col]])
        } else {
          nb_canton <- df %>% dplyr::distinct(.data[[cant_col]]) %>% nrow()
          groups <- df %>% dplyr::group_by(.data[[cant_col]])
        }
      }
      
      nb_communes <- nrow(df)
      denom <- ifelse(is.na(nb_canton) || nb_canton == 0, NA_real_, nb_canton)
      
      pop_by_canton <- groups %>%
        dplyr::summarise(
          sd_pop  = stats::sd(pop, na.rm = TRUE),
          min_pop = min(pop, na.rm = TRUE),
          max_pop = max(pop, na.rm = TRUE),
          .groups = "drop"
        )
      
      tibble::tibble(
        `Number of cantons`           = nb_canton,
        `Average communes per canton` = nb_communes / denom,
        `Average pop per canton`      = sum(df$pop, na.rm = TRUE) / denom,
        `Average SD of pop`           = mean(pop_by_canton$sd_pop,  na.rm = TRUE),
        `Average Min of pop`          = mean(pop_by_canton$min_pop, na.rm = TRUE),
        `Average Max of pop`          = mean(pop_by_canton$max_pop, na.rm = TRUE)
      )
    }
  }
  
  # Controls stats (means only) for a z-group
  compute_controls_stats <- function(df, controls) {
    if (nrow(df) == 0L) {
      tibble::tibble(Variable = controls, Mean = NA_real_)
    } else {
      tibble::tibble(
        Variable = controls,
        Mean = vapply(controls, function(v) mean(df[[v]], na.rm = TRUE), numeric(1))
      )
    }
  }
  
  # Build a single bandwidth block (wide columns for Control/Treatment)
  compute_bandwidth_block <- function(b, df, controls, labels) {
    # subset by distance
    sub <- df %>% dplyr::filter(.data$x >= -b, .data$x <= b)
    
    # split by z
    df0 <- sub %>% dplyr::filter(.data$z == 0)
    df1 <- sub %>% dplyr::filter(.data$z == 1)
    
    # --- Panel section ---
    p0 <- compute_panel_stats(df0)
    p1 <- compute_panel_stats(df1)
    
    panel_tbl <- tibble::tibble(
      Statistic = names(p0),
      Control   = as.numeric(p0[1, ]),
      Treatment = as.numeric(p1[1, ])
    )
    
    # --- Controls section ---
    c0 <- compute_controls_stats(df0, controls_clean)
    c1 <- compute_controls_stats(df1, controls_clean)
    
    # attach labels
    c0$Statistic <- labels[c0$Variable]; c0$Statistic[is.na(c0$Statistic)] <- c0$Variable
    c1$Statistic <- labels[c1$Variable]; c1$Statistic[is.na(c1$Statistic)] <- c1$Variable
    
    controls_tbl <- dplyr::left_join(
      c0 %>% dplyr::select(Statistic, Control = Mean),
      c1 %>% dplyr::select(Statistic, Treatment = Mean),
      by = "Statistic"
    )
    
    # combine and suffix columns with bandwidth
    out <- dplyr::bind_rows(panel_tbl, controls_tbl) %>%
      dplyr::mutate(
        dplyr::across(c(Control, Treatment), as.numeric)
      )
    
    names(out)[names(out) == "Control"]   <- paste0("Control_", b)
    names(out)[names(out) == "Treatment"] <- paste0("Treatment_", b)
    out
  }
  
  # --------------------------------------------------------------------------
  # 2) COMPUTE ALL BANDWIDTHS
  # --------------------------------------------------------------------------
  cat("2) Computing statistics by bandwidth...\n")
  blocks <- lapply(bandwidths, 
                   compute_bandwidth_block, 
                   df = dfZRRControls, 
                   controls = controls_clean, 
                   labels = labels)
  
  # align by "Statistic" via full joins (safer than cbind)
  results_out <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Statistic"), blocks) %>%
    dplyr::distinct(Statistic, .keep_all = TRUE)
  
  cat("✓ Computed and aligned across bandwidths. Rows:", nrow(results_out), "\n\n")
  
  # --------------------------------------------------------------------------
  # 3) OPTIONAL ORDERING (if you keep your labels_ordered vector)
  # --------------------------------------------------------------------------
  # labels_ordered <- c(
  #   "Number of cantons","Average communes per canton","Average pop per canton",
  #   "Average SD of pop","Average Min of pop","Average Max of pop",
  #   "Vote share for FN in 1988","Unemployed (%)","In the labor force (%)",
  #   "Agriculture (%)","Independant (%)","Intermediate occupations (%)","Clerical (%)","Manual (%)",
  #   "Population","Foreigners (%)","Ages 20-40 (%), men","Ages 20-40 (%), women",
  #   "Vacant housing (%)","Population change in p.p. 1980-1990","Population density",
  #   "OPI per 1,000 inhabitants","No diploma (%)","Academic (%)","Highschool (%)","Technical (%)",
  #   "Altitude","Distance to closest agglomeration in meters (log)","Area in km2 (log)"
  # )
  # results_out <- results_out %>%
  #   dplyr::mutate(Statistic = factor(Statistic, levels = labels_ordered)) %>%
  #   dplyr::arrange(Statistic) %>%
  #   dplyr::mutate(Statistic = as.character(Statistic))
  
  # --------------------------------------------------------------------------
  # 4) RENDER LATEX (KABLEEXTRA) - FORMATTED LIKE TARGET TABLE
  # --------------------------------------------------------------------------
  cat("4) Rendering LaTeX (kableExtra)...\n")
  
  # Prepare columns in the desired order
  value_cols <- as.vector(rbind(paste0("Control_", bandwidths), paste0("Treatment_", bandwidths)))
  keep_cols  <- c("Statistic", value_cols)
  
  # Round to 2 decimals
  results_print <- results_out[, keep_cols, drop = FALSE]
  results_print <- results_print %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~round(.x, 2))
    )
  
  # Identify section breaks
  panel_vars <- c("Number of cantons", "Average communes per canton", "Average pop per canton",
                  "Average SD of pop", "Average Min of pop", "Average Max of pop")
  section_labels <- c("Counties", "Municipalities", "Election", "Employment", 
                      "Demographics", "Education", "Geography")
  
  # Create a mapping of which rows belong to which section
  results_print <- results_print %>%
    dplyr::mutate(
      Section = dplyr::case_when(
        Statistic %in% panel_vars[1] ~ "Counties",
        Statistic %in% panel_vars[2:6] ~ "Municipalities",
        grepl("FN.*1988|Vote share", Statistic) ~ "Election",
        grepl("Unemployed|labor force|Agriculture|Independant|Intermediate|Clerical|Manual|Labor force", Statistic) ~ "Employment",
        grepl("Population|Foreigners|Ages|Vacant|OPI|Age ratio", Statistic) ~ "Demographics",
        grepl("diploma|Academic|Highschool|Technical", Statistic) ~ "Education",
        grepl("Altitude|Distance|Area|Fences|Vines", Statistic) ~ "Geography",
        TRUE ~ "Other"
      )
    )
  
  # Use kableExtra for better control

  latex_table <- results_print %>%
    dplyr::select(-Section) %>%
    kbl(format = "latex",
        booktabs = TRUE,
        col.names = c("Statistic", rep(c("C", "T"), length(bandwidths))),
        caption = "Summary Statistics for Different Bandwidths",
        label = "summary_stats_combined",
        align = c("l", rep("c", length(value_cols))),
        linesep = "") %>%
    kable_styling(latex_options = c("hold_position", "scale_down"),
                  font_size = 9) %>%
    # Add bandwidth header
    add_header_above(c(" " = 1, 
                       setNames(rep(2, length(bandwidths)), 
                                paste0(format(bandwidths, big.mark = ",")))),
                     bold = TRUE) %>%
    add_header_above(c(" " = 1, "Bandwidths" = length(value_cols)), bold = TRUE) %>%
    # Add section headers with banding
    pack_rows("Counties", 
              which(results_print$Statistic == panel_vars[1]),
              which(results_print$Statistic == panel_vars[1]),
              italic = TRUE, bold = FALSE, hline_after = FALSE) %>%
    pack_rows("Municipalities",
              which(results_print$Statistic == panel_vars[2]),
              which(results_print$Statistic == panel_vars[6]),
              italic = TRUE, bold = FALSE) %>%
    # Add other sections dynamically
    {
      tbl <- .
      for (sec in c("Election", "Employment", "Demographics", "Education", "Geography")) {
        sec_rows <- which(results_print$Section == sec)
        if (length(sec_rows) > 0) {
          tbl <- pack_rows(tbl, sec, min(sec_rows), max(sec_rows), 
                           italic = TRUE, bold = FALSE)
        }
      }
      tbl
    } %>%
    footnote(general = "The table displays the main summary statistics of the demographic distributions of the sample as well as the summary statistics of the main controls for each bandwidth, with separate columns for the Control (C) and Treatment (T) groups.",
             general_title = "Notes:",
             footnote_as_chunk = TRUE,
             threeparttable = TRUE)
  
  # Save to file
  writeLines(latex_table, out_tex)
  
  cat("✓ Saved LaTeX to:", out_tex, "\n")
  
  cat("✓ Saved alternative LaTeX (stargazer)\n")
  cat("\n===============================================\n")
  cat("DONE\n")
  cat("===============================================\n")  
}


# -------------------------------------------------------------------------------
# RUN
# -------------------------------------------------------------------------------

build_bandwidth_summaries(processed_data_path,
                          out_tex = file.path(path_tables, "summary_stats_bandwidth.tex"))
