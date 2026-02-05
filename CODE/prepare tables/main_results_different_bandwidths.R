

build_main_results_by_bandwidth <- function(processed_data_path,
                                            outcome = "FN2002") {
  
  cat("===============================================\n")
  cat("TABLE 4: MAIN RESULTS (DIFFERENT BANDWIDTHS)\n")
  cat("===============================================\n")
  cat("Outcome   :", outcome, "\n")
  cat("Bandwidths:", paste(bandwidths, collapse = ", "), "meters\n\n")
  
  # --------------------------------------------------------------------------
  # 1) LOAD DATA & BASIC CHECKS
  # --------------------------------------------------------------------------
  sharp_path <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(sharp_path)) stop("❌ script_sharp.RData not found at: ", sharp_path)
  
  env <- new.env()
  load(sharp_path, envir = env)
  if (!exists("dfZRRControls", envir = env)) stop("❌ dfZRRControls not found in script_sharp.RData")
  df0 <- get("dfZRRControls", envir = env)
  
  # Get controls from environment or set default
  if (exists("controls", envir = env)) {
    controls <- get("controls", envir = env)
  } else {
    controls <- character(0)
    warning("No 'controls' variable found in data; proceeding without controls")
  }
  
  max_bw <- max(bandwidths)
  cat("✓ Data loaded. Rows:", nrow(df0), " | Max bandwidth:", max_bw, "m\n")
  
  needed <- c("x", "z", "pop", "popDensity", "dep", "codecommune")
  miss <- setdiff(needed, names(df0))
  if (length(miss)) stop("❌ Missing required variables: ", paste(miss, collapse = ", "))
  
  controls <- intersect(controls, names(df0))
  cat("✓ Controls used (", length(controls), "): ", paste(controls, collapse = ", "), "\n\n", sep = "")
  
  # --------------------------------------------------------------------------
  # 2) PREP RDD FRAME (ONCE)
  # --------------------------------------------------------------------------
  cat("2) Preparing RDD analysis frame...\n")
  
  safe_log <- function(x) { 
    out <- ifelse(x > 0, log(x), NA_real_)
    out[is.infinite(out)] <- NA_real_
    out 
  }
  
  df_rdd <- df0 %>%
    dplyr::filter(.data$x >= -max_bw, .data$x <= max_bw) %>%
    dplyr::mutate(
      dist       = .data$x,
      x_km10     = .data$x / 10000,
      treatmentZRR = as.integer(.data$z),
      pop        = safe_log(.data$pop),
      popDensity = safe_log(.data$popDensity)
    ) %>%
    dplyr::distinct(codecommune, .keep_all = TRUE)
  
  df_rdd <- clean_data_variables(df_rdd, names(df_rdd))
  
  cluster_var <- dplyr::case_when(
    "canton"     %in% names(df_rdd) ~ "canton",
    "codecanton" %in% names(df_rdd) ~ "codecanton",
    TRUE ~ NA_character_
  )
  if (is.na(cluster_var)) stop("❌ No canton/codecanton column found for clustering SEs.")
  
  keep_cols <- unique(c(outcome, "treatmentZRR", "x_km10", "dep", cluster_var, controls))
  df_rdd <- df_rdd %>% dplyr::filter(!if_any(dplyr::all_of(c(outcome, cluster_var, "treatmentZRR", "x_km10")), is.na))
  
  cat("✓ RDD frame ready. Rows:", nrow(df_rdd), 
      " | Unique cantons:", dplyr::n_distinct(df_rdd[[cluster_var]]), "\n\n")
  
  # --------------------------------------------------------------------------
  # 3) FIT MODELS BY BANDWIDTH (HC1 clustered by canton)
  # --------------------------------------------------------------------------
  cat("3) Fitting models by bandwidth...\n")
  
  rhs <- c("treatmentZRR", "x_km10", "treatmentZRR:x_km10", controls, "factor(dep)")
  fml <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  
  fit_one <- function(b) {
    dat <- df_rdd %>% dplyr::filter(abs(.data$dist) <= b)
    dat$dep <- droplevels(as.factor(dat$dep))
    mod <- stats::lm(fml, data = dat)
    vc  <- sandwich::vcovCL(mod, cluster = dat[[cluster_var]], type = "HC1")
    list(model = mod, vcov = vc, n = stats::nobs(mod), bw = b)
  }
  
  fits <- lapply(bandwidths, fit_one)
  models <- lapply(fits, `[[`, "model")
  vcovs  <- lapply(fits, `[[`, "vcov")
  names(models) <- paste0(format(bandwidths, big.mark = ","))
  
  cat("✓ Models fit: ", paste(names(models), collapse = " | "), "\n\n", sep = "")
  
  # --------------------------------------------------------------------------
  # 4) RENDER TABLE (Manual LaTeX construction)
  # --------------------------------------------------------------------------
  cat("4) Building LaTeX table manually...\n")
  
  # Extract coefficients and standard errors for each model
  extract_coef_info <- function(model, vcov_mat) {
    coefs <- coef(model)
    ses <- sqrt(diag(vcov_mat))
    pvals <- 2 * pt(abs(coefs / ses), df = model$df.residual, lower.tail = FALSE)
    
    # Add stars based on p-values
    stars <- ifelse(pvals < 0.001, "***",
                    ifelse(pvals < 0.01, "**",
                           ifelse(pvals < 0.05, "*", "")))
    
    list(coef = coefs, se = ses, stars = stars, 
         nobs = nobs(model), r2 = summary(model)$r.squared)
  }
  
  # Extract info for all models
  model_info <- mapply(extract_coef_info, models, vcovs, SIMPLIFY = FALSE)
  
  # Format numbers
  fmt_coef <- function(x, star = "") {
    sign <- ifelse(x < 0, "$-$", "")
    sprintf("%s%.4f%s", sign, abs(x), star)
  }
  
  fmt_se <- function(x) {
    sprintf("(%.4f)", x)
  }
  
  fmt_int <- function(x) {
    format(round(x), big.mark = ",")
  }
  
  fmt_r2 <- function(x) {
    sprintf("%.3f", x)
  }
  
  # Build coefficient rows
  coef_vars <- c("treatmentZRR", "x_km10", "treatmentZRR:x_km10")
  coef_labels <- c("ZRR", "Distance to Frontier (km)", "Treatment $\\times$ Distance")
  
  coef_rows <- character()
  for (i in seq_along(coef_vars)) {
    var <- coef_vars[i]
    label <- coef_labels[i]
    
    # Coefficient row
    coef_line <- paste0(label, " & ")
    coef_vals <- sapply(model_info, function(m) {
      if (var %in% names(m$coef)) {
        fmt_coef(m$coef[var], m$stars[var])
      } else {
        ""
      }
    })
    coef_line <- paste0(coef_line, paste(coef_vals, collapse = " & "), " \\\\")
    
    # SE row
    se_line <- "  & "
    se_vals <- sapply(model_info, function(m) {
      if (var %in% names(m$se)) {
        fmt_se(m$se[var])
      } else {
        ""
      }
    })
    se_line <- paste0(se_line, paste(se_vals, collapse = " & "), " \\\\")
    
    # Add spacing after each variable
    coef_rows <- c(coef_rows, coef_line, se_line, "  & & & \\\\")
  }
  
  # Remove last extra spacing line
  coef_rows <- coef_rows[-length(coef_rows)]
  
  # Build GOF rows
  obs_line <- paste0("Observations & ", 
                     paste(sapply(model_info, function(m) fmt_int(m$nobs)), collapse = " & "),
                     " \\\\")
  r2_line <- paste0("R$^{2}$ & ", 
                    paste(sapply(model_info, function(m) fmt_r2(m$r2)), collapse = " & "),
                    " \\\\")
  
  if (!dir.exists(path_tables)) dir.create(path_tables, recursive = TRUE)
  out_path <- file.path(path_tables, "main_results_different_bandwidths.tex")
  
  note_text <- paste0(
    "$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001. ",
    "Distance to frontier is defined as the distance between the locality centroid and the closest point on the frontier. ",
    "The regressions include controls and department fixed effects. ",
    "Standard errors are clustered at the county level."
  )
  
  # Build column headers
  col_names <- paste0("Bandwidth = ", format(bandwidths, big.mark = ","))
  col_numbers <- paste0("(", seq_along(bandwidths), ")")
  
  # Assemble complete table
  latex_table <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\footnotesize",
    "\\caption{Main results, different bandwidths}",
    "\\label{tab:rdd_results_diffbandwidth}",
    "\\begin{threeparttable}",
    paste0("\\begin{tabular}{@{\\extracolsep{2pt}}l", paste(rep("c", length(bandwidths)), collapse = ""), "}"),
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    paste0(" & \\multicolumn{", length(bandwidths), "}{c}{\\textit{Dependent variable: Vote Share for FN in 2002}} \\\\"),
    paste0("\\cline{2-", length(bandwidths) + 1, "}"),
    paste0(" & ", paste(col_names, collapse = " & "), " \\\\"),
    paste0("\\\\[-1.8ex] & ", paste(col_numbers, collapse = " & "), "\\\\"),
    "\\hline \\\\[-1.8ex]",
    coef_rows,
    "\\hline \\\\[-1.8ex]",
    obs_line,
    r2_line,
    "\\hline",
    "\\hline \\\\[-1.8ex]",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "  \\footnotesize",
    paste0("  \\item \\textit{Notes:} ", note_text),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  
  # Write to file
  writeLines(latex_table, out_path)
  
  if (file.exists(out_path) && file.size(out_path) > 0) {
    cat("✓ LaTeX table successfully saved to: ", out_path, "\n")
    cat("File size: ", file.size(out_path), " bytes\n\n")
  } else {
    stop("❌ LaTeX file creation failed.")
  }
  
  cat("===============================================\n")
  cat("DONE\n")
  cat("===============================================\n")
  
  invisible(list(models = models, vcov = vcovs, formula = fml, cluster = cluster_var))
}


# ==============================================================================
# EXECUTION
# ==============================================================================


build_main_results_by_bandwidth(processed_data_path, outcome = "FN2002")
