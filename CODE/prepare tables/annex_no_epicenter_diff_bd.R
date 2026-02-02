# ================================================================================
# TABLE — MAIN RESULTS (Different Bandwidths) § shortest distance
# ================================================================================

build_main_results_by_bandwidth <- function(processed_data_path,
                                            outcome   = "FN2002") {
  
  cat("===============================================\n")
  cat("TABLE Annex: MAIN RESULTS (DIFFERENT BANDWIDTHS)\n")
  cat("===============================================\n")
  cat("Outcome   :", outcome, "\n")
  cat("Bandwidths:", paste(bandwidths, collapse = ", "), "meters\n\n")
  
  # --------------------------------------------------------------------------
  # 1) LOAD DATA & BASIC CHECKS
  # --------------------------------------------------------------------------
  sharp_path <- file.path(processed_data_path, "script_sharp_noEpicenter.RData")
  if (!file.exists(sharp_path)) stop("❌ script_sharp_noEpicenter.RData not found at: ", sharp_path)
  
  env <- new.env()
  load(sharp_path, envir = env)
  if (!exists("dfZRRControls", envir = env)) stop("❌ dfZRRControls not found in script_sharp_noEpicenter.RData")
  df0 <- get("dfZRRControls", envir = env)
  if (!exists("controls", inherits = FALSE)) controls <- get("controls", envir = env)
  
  max_bw <- max(bandwidths)
  cat("✓ Data loaded. Rows:", nrow(df0), " | Max bandwidth:", max_bw, "m\n")
  
  # Required vars
  needed <- c("x", "z", "pop", "popDensity", "dep", "codecommune")
  miss <- setdiff(needed, names(df0))
  if (length(miss)) stop("❌ Missing required variables: ", paste(miss, collapse = ", "))
  
  # Controls present?
  controls <- intersect(controls, names(df0))
  cat("✓ Controls used (", length(controls), "): ", paste(controls, collapse = ", "), "\n\n", sep = "")
  
  # --------------------------------------------------------------------------
  # 2) PREP RDD FRAME (ONCE)
  # --------------------------------------------------------------------------
  cat("2) Preparing RDD analysis frame...\n")
  
  safe_log <- function(x) { out <- ifelse(x > 0, log(x), NA_real_); out[is.infinite(out)] <- NA_real_; out }
  
  df_rdd <- df0 %>%
    dplyr::filter(.data$x >= -max_bw, .data$x <= max_bw) %>%
    dplyr::mutate(
      dist       = .data$x,
      x_km10     = .data$x / 10000,              # distance in 10km
      treatmentZRR = .data$z,
      pop        = safe_log(.data$pop),          # log pop, safe
      popDensity = safe_log(.data$popDensity)    # log density, safe
    ) %>%
    dplyr::distinct(codecommune, .keep_all = TRUE)
  
  # choose canton identifier for clustering
  cluster_var <- dplyr::case_when(
    "canton"     %in% names(df_rdd) ~ "canton",
    "codecanton" %in% names(df_rdd) ~ "codecanton",
    TRUE ~ NA_character_
  )
  if (is.na(cluster_var)) stop("❌ No canton/codecanton column found for clustering SEs.")
  
  # drop rows missing outcome, cluster id, or key RHS
  keep_cols <- unique(c(outcome, "z", "x_km10", "dep", cluster_var, controls))
  df_rdd <- df_rdd %>% dplyr::filter(!if_any(dplyr::all_of(c(outcome, cluster_var, "z", "x_km10")), is.na))
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  cat("✓ RDD frame ready. Rows:", nrow(df_rdd), 
      " | Unique cantons:", dplyr::n_distinct(df_rdd[[cluster_var]]), "\n\n")
  
  # --------------------------------------------------------------------------
  # 3) FIT MODELS BY BANDWIDTH (HC1 clustered by canton)
  # --------------------------------------------------------------------------
  cat("3) Fitting models by bandwidth...\n")
  
  # Build formula text
  rhs <- c("z", "x_km10", controls, "factor(dep)")
  fml <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  
  # Per-bandwidth fit helper
  fit_one <- function(b) {
    dat <- df_rdd %>% dplyr::filter(.data$x >= -b, .data$x <= b)
    # Drop unused dep levels within bandwidth
    dat$dep <- droplevels(as.factor(dat$dep))
    mod <- stats::lm(fml, data = dat)
    # HC1 clustered by canton
    vc  <- sandwich::vcovCL(mod, cluster = dat[[cluster_var]], type = "HC1")
    list(model = mod, vcov = vc, n = stats::nobs(mod), bw = b)
  }
  
  fits <- lapply(bandwidths, fit_one)
  models <- lapply(fits, `[[`, "model")
  vcovs  <- lapply(fits, `[[`, "vcov")
  names(models) <- paste0("Bandwidth = ", bandwidths)
  
  cat("✓ Models fit: ", paste(names(models), collapse = " | "), "\n\n", sep = "")
  
  # --------------------------------------------------------------------------
  # 4) RENDER TABLE (modelsummary → LaTeX)
  # --------------------------------------------------------------------------
  cat("4) Rendering LaTeX table with modelsummary...\n")
  
  # Pretty labels for key terms
  coef_map <- c(
    "zTRUE"        = "Treatment ZRR",
    "x_km10"   = "Distance to Frontier (10 km)"
  )
  
  coef_keep <- c("zTRUE", "x_km10")
  modelsummary(models, coef_keep = coef_keep, 
               coef_map = coef_map)
  
  # Build LaTeX table and save
  out_path <- file.path(path_tables, "annex_no_epicenter_diff_bd.tex")
  
  note_text <- "Standard errors clustered at the canton level (HC1)."
  
  
  ms <- modelsummary::modelsummary(
    models,
    vcov      = vcovs,
    estimate  = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    coef_keep = coef_keep,          # keep only z and x_km10
    coef_map  = coef_map,           # pretty labels
    gof_omit  = "IC|Log|Adj|p\\.value|statistic|se_type|Std|RMSE",
    stars     = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    fmt       = 4,
    output    = "kableExtra"
  ) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::footnote(
      general = note_text,
      threeparttable = TRUE
    )
  
  
  kableExtra::save_kable(ms, out_path)
  cat("✓ Saved LaTeX table to: ", out_path, "\n", sep = "")
  
  cat("\n===============================================\n")
  cat("DONE\n")
  cat("===============================================\n")
  
  invisible(list(models = models, vcov = vcovs, formula = fml, cluster = cluster_var))
}

# ==============================================================================
# EXECUTION
# ==============================================================================

build_main_results_by_bandwidth(processed_data_path, outcome = "FN2002")
