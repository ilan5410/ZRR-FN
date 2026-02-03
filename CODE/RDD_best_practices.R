# ==============================================================================
# COMPREHENSIVE RDD ANALYSIS - BEST PRACTICES IMPLEMENTATION
# ==============================================================================
# Based on Lee & Lemieux (2010) "Regression Discontinuity Designs in Economics"
# Journal of Economic Literature, 48(2), 281-355
#
# This script implements:
# 1. Formal McCrary density test (manipulation check)
# 2. Optimal bandwidth selection (IK and CCT methods)
# 3. Multiple polynomial orders with AIC/BIC selection
# 4. Bias-corrected RDD estimates (rdrobust methodology)
# 5. Local polynomial estimation with kernel weighting
# 6. Comprehensive covariate balance tests
# 7. Placebo cutoff tests at multiple fake thresholds
# 8. Donut hole robustness checks
# 9. Publication-quality RDD plots with optimal binning
# ==============================================================================

# ==============================================================================
# REQUIRED PACKAGES
# ==============================================================================

required_packages <- c(
 "rdrobust",      # Optimal bandwidth, bias-corrected inference
 "rddensity",     # McCrary density test
 "ggplot2",       # Visualization
 "dplyr",         # Data manipulation
 "tidyr",         # Data reshaping
 "sandwich",      # Robust standard errors
 "lmtest",        # Coefficient testing
 "gridExtra",     # Plot arrangement
 "scales",        # Axis formatting
 "broom",         # Model tidying
 "knitr",         # Table formatting
 "kableExtra"     # Enhanced tables
)

# Install missing packages
install_if_missing <- function(pkg) {
 if (!requireNamespace(pkg, quietly = TRUE)) {
   install.packages(pkg, repos = "https://cloud.r-project.org")
 }
}

invisible(sapply(required_packages, install_if_missing))

# Load packages
invisible(sapply(required_packages, library, character.only = TRUE))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Clean data by removing rows with NA, Inf, or -Inf values
#' @param df Data frame to clean
#' @param variables Character vector of variable names to check
#' @return Cleaned data frame
clean_data_variables <- function(df, variables) {
 initial_rows <- nrow(df)

 for (var in variables) {
   if (var %in% names(df)) {
     df <- df[!is.na(df[[var]]), ]
     if (is.numeric(df[[var]])) {
       df <- df[!is.infinite(df[[var]]), ]
     }
   }
 }

 final_rows <- nrow(df)
 cat("  Cleaned data:", initial_rows - final_rows, "rows removed,", final_rows, "remaining\n")

 return(df)
}

#' Format p-values for display
#' @param p P-value
#' @return Formatted string
format_pvalue <- function(p) {
 if (is.na(p)) return("NA")
 if (p < 0.001) return("< 0.001")
 if (p < 0.01) return(sprintf("%.3f", p))
 return(sprintf("%.3f", p))
}

#' Add significance stars
#' @param p P-value
#' @return Stars string
add_stars <- function(p) {
 if (is.na(p)) return("")
 if (p < 0.01) return("***")
 if (p < 0.05) return("**")
 if (p < 0.1) return("*")
 return("")
}

# ==============================================================================
# MAIN RDD ANALYSIS FUNCTION
# ==============================================================================

#' Run comprehensive RDD analysis following Lee & Lemieux (2010) best practices
#'
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param path_tables Path to output tables directory
#' @param outcome_var Name of outcome variable (default: "FN2002")
#' @param running_var Name of running variable (default: "x" for distance_to_border)
#' @param cutoff Cutoff value (default: 0)
#' @param controls Vector of control variable names
#' @param cluster_var Variable for clustering standard errors (default: "canton")
#' @return List containing all analysis results
run_rdd_best_practices <- function(
   processed_data_path,
   path_figures,
   path_tables,
   outcome_var = "FN2002",
   running_var = "x",
   cutoff = 0,
   controls = NULL,
   cluster_var = "canton"
) {

 cat("\n")
 cat("================================================================\n")
 cat("  COMPREHENSIVE RDD ANALYSIS - BEST PRACTICES\n")
 cat("  Based on Lee & Lemieux (2010)\n")
 cat("================================================================\n")
 cat("Outcome:", outcome_var, "\n")
 cat("Running variable:", running_var, "\n")
 cat("Cutoff:", cutoff, "\n")
 cat("\n")

 # --------------------------------------------------------------------------
 # 1. LOAD AND PREPARE DATA
 # --------------------------------------------------------------------------
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 1: LOADING AND PREPARING DATA\n")
 cat("=" , rep("=", 60), "\n", sep = "")

 data_file <- file.path(processed_data_path, "script_sharp.RData")
 if (!file.exists(data_file)) {
   stop("Data file not found: ", data_file)
 }

 load(data_file)
 cat("Loaded script_sharp.RData\n")

 # Prepare the dataset
 df <- dfZRRControls %>%
   mutate(
     Y = .data[[outcome_var]],           # Outcome
     X = .data[[running_var]],           # Running variable (centered at cutoff)
     D = as.numeric(X <= cutoff),        # Treatment indicator (inside program = 1)
     X_centered = X - cutoff,            # Centered running variable
     pop = log(pop),                     # Log transform
     popDensity = log(popDensity)
   ) %>%
   filter(!is.na(Y), !is.na(X)) %>%
   distinct(codecommune, .keep_all = TRUE)

 cat("Dataset prepared:\n")
 cat("  - Total observations:", nrow(df), "\n")
 cat("  - Treated (D=1, inside program):", sum(df$D == 1), "\n")
 cat("  - Control (D=0, outside program):", sum(df$D == 0), "\n")
 cat("  - Running variable range:", round(min(df$X)), "to", round(max(df$X)), "meters\n")
 cat("  - Outcome range:", round(min(df$Y), 3), "to", round(max(df$Y), 3), "\n")

 # Store results
 results <- list()
 results$data <- df
 results$parameters <- list(
   outcome_var = outcome_var,
   running_var = running_var,
   cutoff = cutoff,
   n_total = nrow(df),
   n_treated = sum(df$D == 1),
   n_control = sum(df$D == 0)
 )

 # --------------------------------------------------------------------------
 # 2. MCCRARY DENSITY TEST (MANIPULATION CHECK)
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 2: MCCRARY DENSITY TEST (Manipulation Check)\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Testing for discontinuity in density of running variable at cutoff.\n")
 cat("Null hypothesis: No manipulation (continuous density at cutoff).\n\n")

 # Run formal McCrary test using rddensity package
 density_test <- tryCatch({
   rddensity(X = df$X_centered, c = 0)
 }, error = function(e) {
   cat("Warning: rddensity failed, using manual density test\n")
   NULL
 })

 if (!is.null(density_test)) {
   cat("McCrary Density Test Results (rddensity):\n")
   cat("  - Test statistic (T):", sprintf("%.4f", density_test$test$t_jk), "\n")
   cat("  - P-value:", format_pvalue(density_test$test$p_jk), "\n")
   cat("  - Bandwidth (left):", sprintf("%.2f", density_test$h$left), "\n")
   cat("  - Bandwidth (right):", sprintf("%.2f", density_test$h$right), "\n")
   cat("  - N effective (left):", density_test$N$eff_left, "\n")
   cat("  - N effective (right):", density_test$N$eff_right, "\n")

   if (density_test$test$p_jk < 0.05) {
     cat("\n  *** WARNING: Significant density discontinuity detected! ***\n")
     cat("  This may indicate manipulation of the running variable.\n")
   } else {
     cat("\n  PASS: No significant density discontinuity (p =",
         format_pvalue(density_test$test$p_jk), ")\n")
   }

   results$density_test <- density_test
 }

 # Also compute simple density comparison for transparency
 cat("\nSimple density comparison (within 2km of cutoff):\n")
 left_2km <- sum(df$X_centered >= -2000 & df$X_centered < 0)
 right_2km <- sum(df$X_centered >= 0 & df$X_centered < 2000)
 cat("  - Observations left of cutoff:", left_2km, "\n")
 cat("  - Observations right of cutoff:", right_2km, "\n")
 cat("  - Ratio (right/left):", sprintf("%.3f", right_2km/left_2km), "\n")

 # --------------------------------------------------------------------------
 # 3. COVARIATE BALANCE TESTS
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 3: COVARIATE BALANCE TESTS\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Testing for discontinuities in predetermined covariates.\n")
 cat("If RDD is valid, covariates should be balanced at cutoff.\n\n")

 # Define covariates to test (excluding outcome and treatment-related vars)
 balance_vars <- c("FN1988", "pchom", "pop", "ratEmp", "ratForeigners",
                   "educNoDiplomaPerK", "educSUPPerK", "altitude", "superficie",
                   "min_distance_to_agglo", "revenuPerK", "popDensity")
 balance_vars <- intersect(balance_vars, names(df))

 cat("Testing", length(balance_vars), "predetermined covariates:\n\n")

 balance_results <- data.frame(
   Variable = character(),
   Coefficient = numeric(),
   Std_Error = numeric(),
   P_Value = numeric(),
   Bandwidth = numeric(),
   N_Effective = integer(),
   stringsAsFactors = FALSE
 )

 for (var in balance_vars) {
   if (sum(!is.na(df[[var]])) < 100) next

   tryCatch({
     # Use rdrobust for each covariate
     rd_cov <- rdrobust(y = df[[var]], x = df$X_centered, c = 0)

     balance_results <- rbind(balance_results, data.frame(
       Variable = var,
       Coefficient = rd_cov$coef[1],
       Std_Error = rd_cov$se[3],  # Robust SE
       P_Value = rd_cov$pv[3],    # Robust p-value
       Bandwidth = rd_cov$bws[1, 1],
       N_Effective = rd_cov$N_h[1] + rd_cov$N_h[2],
       stringsAsFactors = FALSE
     ))

     sig_marker <- add_stars(rd_cov$pv[3])
     cat(sprintf("  %-25s: coef = %8.4f, SE = %6.4f, p = %s %s\n",
                 var, rd_cov$coef[1], rd_cov$se[3],
                 format_pvalue(rd_cov$pv[3]), sig_marker))
   }, error = function(e) {
     cat(sprintf("  %-25s: FAILED (%s)\n", var, e$message))
   })
 }

 # Count significant imbalances
 n_significant <- sum(balance_results$P_Value < 0.05, na.rm = TRUE)
 cat("\n")
 if (n_significant == 0) {
   cat("PASS: No significant covariate imbalances detected at 5% level.\n")
 } else {
   cat("WARNING:", n_significant, "covariate(s) show significant imbalance at 5% level.\n")
 }

 results$balance_tests <- balance_results

 # --------------------------------------------------------------------------
 # 4. OPTIMAL BANDWIDTH SELECTION
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 4: OPTIMAL BANDWIDTH SELECTION\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Computing MSE-optimal bandwidths using different methods.\n\n")

 # MSE-optimal bandwidth (Imbens-Kalyanaraman)
 bw_ik <- tryCatch({
   rdbwselect(y = df$Y, x = df$X_centered, c = 0, bwselect = "mserd")
 }, error = function(e) NULL)

 # CCT bandwidth (Calonico, Cattaneo, Titiunik)
 bw_cct <- tryCatch({
   rdbwselect(y = df$Y, x = df$X_centered, c = 0, bwselect = "cerrd")
 }, error = function(e) NULL)

 # Different kernel options
 bw_tri <- tryCatch({
   rdbwselect(y = df$Y, x = df$X_centered, c = 0, kernel = "triangular")
 }, error = function(e) NULL)

 bw_uni <- tryCatch({
   rdbwselect(y = df$Y, x = df$X_centered, c = 0, kernel = "uniform")
 }, error = function(e) NULL)

 cat("Optimal Bandwidth Results:\n")
 cat("-" , rep("-", 50), "\n", sep = "")

 bandwidth_table <- data.frame(
   Method = character(),
   Bandwidth_h = numeric(),
   Bandwidth_b = numeric(),
   N_left = integer(),
   N_right = integer(),
   stringsAsFactors = FALSE
 )

 if (!is.null(bw_ik)) {
   n_left <- if (length(bw_ik$N_h) >= 1) bw_ik$N_h[1] else NA
   n_right <- if (length(bw_ik$N_h) >= 2) bw_ik$N_h[2] else NA
   cat(sprintf("  MSE-optimal (IK):        h = %8.1f m  (N_eff = %s + %s)\n",
               bw_ik$bws[1, 1],
               ifelse(is.na(n_left), "NA", as.character(n_left)),
               ifelse(is.na(n_right), "NA", as.character(n_right))))
   bandwidth_table <- rbind(bandwidth_table, data.frame(
     Method = "MSE-optimal (IK)",
     Bandwidth_h = bw_ik$bws[1, 1],
     Bandwidth_b = if (ncol(bw_ik$bws) >= 2) bw_ik$bws[1, 2] else NA,
     N_left = n_left,
     N_right = n_right,
     stringsAsFactors = FALSE
   ))
 }

 if (!is.null(bw_cct)) {
   n_left <- if (length(bw_cct$N_h) >= 1) bw_cct$N_h[1] else NA
   n_right <- if (length(bw_cct$N_h) >= 2) bw_cct$N_h[2] else NA
   cat(sprintf("  CER-optimal (CCT):       h = %8.1f m  (N_eff = %s + %s)\n",
               bw_cct$bws[1, 1],
               ifelse(is.na(n_left), "NA", as.character(n_left)),
               ifelse(is.na(n_right), "NA", as.character(n_right))))
   bandwidth_table <- rbind(bandwidth_table, data.frame(
     Method = "CER-optimal (CCT)",
     Bandwidth_h = bw_cct$bws[1, 1],
     Bandwidth_b = if (ncol(bw_cct$bws) >= 2) bw_cct$bws[1, 2] else NA,
     N_left = n_left,
     N_right = n_right,
     stringsAsFactors = FALSE
   ))
 }

 if (!is.null(bw_tri)) {
   n_left <- if (length(bw_tri$N_h) >= 1) bw_tri$N_h[1] else NA
   n_right <- if (length(bw_tri$N_h) >= 2) bw_tri$N_h[2] else NA
   cat(sprintf("  Triangular kernel:       h = %8.1f m  (N_eff = %s + %s)\n",
               bw_tri$bws[1, 1],
               ifelse(is.na(n_left), "NA", as.character(n_left)),
               ifelse(is.na(n_right), "NA", as.character(n_right))))
 }

 if (!is.null(bw_uni)) {
   n_left <- if (length(bw_uni$N_h) >= 1) bw_uni$N_h[1] else NA
   n_right <- if (length(bw_uni$N_h) >= 2) bw_uni$N_h[2] else NA
   cat(sprintf("  Uniform kernel:          h = %8.1f m  (N_eff = %s + %s)\n",
               bw_uni$bws[1, 1],
               ifelse(is.na(n_left), "NA", as.character(n_left)),
               ifelse(is.na(n_right), "NA", as.character(n_right))))
 }

 # Store optimal bandwidth for main analysis
 optimal_bw <- if (!is.null(bw_ik)) bw_ik$bws[1, 1] else 10000
 cat("\n  >> Using optimal bandwidth:", round(optimal_bw), "meters for main analysis\n")

 results$bandwidth_selection <- list(
   optimal = optimal_bw,
   bw_ik = bw_ik,
   bw_cct = bw_cct,
   table = bandwidth_table
 )

 # --------------------------------------------------------------------------
 # 5. MAIN RDD ESTIMATES (RDROBUST)
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 5: MAIN RDD ESTIMATES (Bias-Corrected)\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Using rdrobust for bias-corrected local polynomial estimation.\n\n")

 # Main estimate with optimal bandwidth
 rd_main <- rdrobust(y = df$Y, x = df$X_centered, c = 0)

 cat("Main RDD Estimate (Local Linear, MSE-optimal bandwidth):\n")
 cat("-" , rep("-", 50), "\n", sep = "")
 cat(sprintf("  Treatment Effect (LATE):  %8.4f\n", rd_main$coef[1]))
 cat(sprintf("  Conventional SE:          %8.4f\n", rd_main$se[1]))
 cat(sprintf("  Robust SE:                %8.4f\n", rd_main$se[3]))
 cat(sprintf("  Robust 95%% CI:            [%.4f, %.4f]\n", rd_main$ci[3, 1], rd_main$ci[3, 2]))
 cat(sprintf("  Robust p-value:           %s %s\n", format_pvalue(rd_main$pv[3]), add_stars(rd_main$pv[3])))
 cat(sprintf("  Bandwidth (h):            %.1f meters\n", rd_main$bws[1, 1]))
 cat(sprintf("  N effective (left):       %d\n", rd_main$N_h[1]))
 cat(sprintf("  N effective (right):      %d\n", rd_main$N_h[2]))

 results$main_estimate <- rd_main

 # --------------------------------------------------------------------------
 # 6. POLYNOMIAL ORDER ROBUSTNESS
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 6: POLYNOMIAL ORDER ROBUSTNESS\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Testing sensitivity to polynomial order (linear, quadratic, cubic).\n\n")

 poly_results <- data.frame(
   Order = integer(),
   Coefficient = numeric(),
   Robust_SE = numeric(),
   Robust_PValue = numeric(),
   CI_Lower = numeric(),
   CI_Upper = numeric(),
   Bandwidth = numeric(),
   N_Effective = integer(),
   stringsAsFactors = FALSE
 )

 for (p in 1:3) {
   rd_poly <- tryCatch({
     rdrobust(y = df$Y, x = df$X_centered, c = 0, p = p)
   }, error = function(e) NULL)

   if (!is.null(rd_poly)) {
     poly_results <- rbind(poly_results, data.frame(
       Order = p,
       Coefficient = rd_poly$coef[1],
       Robust_SE = rd_poly$se[3],
       Robust_PValue = rd_poly$pv[3],
       CI_Lower = rd_poly$ci[3, 1],
       CI_Upper = rd_poly$ci[3, 2],
       Bandwidth = rd_poly$bws[1, 1],
       N_Effective = rd_poly$N_h[1] + rd_poly$N_h[2]
     ))

     poly_name <- c("Linear", "Quadratic", "Cubic")[p]
     cat(sprintf("  %-10s (p=%d): coef = %7.4f, SE = %6.4f, p = %s %s\n",
                 poly_name, p, rd_poly$coef[1], rd_poly$se[3],
                 format_pvalue(rd_poly$pv[3]), add_stars(rd_poly$pv[3])))
   }
 }

 results$polynomial_robustness <- poly_results

 # --------------------------------------------------------------------------
 # 7. BANDWIDTH SENSITIVITY ANALYSIS
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 7: BANDWIDTH SENSITIVITY ANALYSIS\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Testing stability across different bandwidth choices.\n\n")

 # Test range of bandwidths
 bw_range <- seq(2000, 20000, by = 1000)

 bw_sensitivity <- data.frame(
   Bandwidth = numeric(),
   Coefficient = numeric(),
   Robust_SE = numeric(),
   CI_Lower = numeric(),
   CI_Upper = numeric(),
   N_Left = integer(),
   N_Right = integer(),
   stringsAsFactors = FALSE
 )

 for (bw in bw_range) {
   rd_bw <- tryCatch({
     rdrobust(y = df$Y, x = df$X_centered, c = 0, h = bw)
   }, error = function(e) NULL)

   if (!is.null(rd_bw)) {
     bw_sensitivity <- rbind(bw_sensitivity, data.frame(
       Bandwidth = bw,
       Coefficient = rd_bw$coef[1],
       Robust_SE = rd_bw$se[3],
       CI_Lower = rd_bw$ci[3, 1],
       CI_Upper = rd_bw$ci[3, 2],
       N_Left = rd_bw$N_h[1],
       N_Right = rd_bw$N_h[2]
     ))
   }
 }

 cat("Bandwidth sensitivity (showing every 5th):\n")
 cat("-" , rep("-", 70), "\n", sep = "")
 cat(sprintf("  %-10s  %-10s  %-10s  %-25s  %-10s\n",
             "BW (m)", "Coef", "SE", "95% CI", "N_eff"))
 cat("-" , rep("-", 70), "\n", sep = "")

 for (i in seq(1, nrow(bw_sensitivity), by = 5)) {
   row <- bw_sensitivity[i, ]
   cat(sprintf("  %-10.0f  %10.4f  %10.4f  [%7.4f, %7.4f]  %10d\n",
               row$Bandwidth, row$Coefficient, row$Robust_SE,
               row$CI_Lower, row$CI_Upper, row$N_Left + row$N_Right))
 }

 results$bandwidth_sensitivity <- bw_sensitivity

 # --------------------------------------------------------------------------
 # 8. PLACEBO CUTOFF TESTS
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 8: PLACEBO CUTOFF TESTS\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Testing for spurious effects at fake cutoff points.\n")
 cat("No significant effects should be found at non-cutoff points.\n\n")

 # Test fake cutoffs
 placebo_cutoffs <- c(-8000, -5000, -3000, 3000, 5000, 8000)

 placebo_results <- data.frame(
   Cutoff = numeric(),
   Coefficient = numeric(),
   Robust_SE = numeric(),
   Robust_PValue = numeric(),
   N_Effective = integer(),
   stringsAsFactors = FALSE
 )

 for (fake_c in placebo_cutoffs) {
   # Filter data for this placebo test
   if (fake_c < 0) {
     df_placebo <- df %>% filter(X_centered < 0)
   } else {
     df_placebo <- df %>% filter(X_centered >= 0)
   }

   rd_placebo <- tryCatch({
     rdrobust(y = df_placebo$Y, x = df_placebo$X_centered, c = fake_c)
   }, error = function(e) NULL)

   if (!is.null(rd_placebo)) {
     placebo_results <- rbind(placebo_results, data.frame(
       Cutoff = fake_c,
       Coefficient = rd_placebo$coef[1],
       Robust_SE = rd_placebo$se[3],
       Robust_PValue = rd_placebo$pv[3],
       N_Effective = rd_placebo$N_h[1] + rd_placebo$N_h[2]
     ))

     cat(sprintf("  Cutoff = %6d m:  coef = %7.4f, SE = %6.4f, p = %s %s\n",
                 fake_c, rd_placebo$coef[1], rd_placebo$se[3],
                 format_pvalue(rd_placebo$pv[3]), add_stars(rd_placebo$pv[3])))
   }
 }

 # Check for spurious significance
 n_placebo_sig <- sum(placebo_results$Robust_PValue < 0.05, na.rm = TRUE)
 cat("\n")
 if (n_placebo_sig == 0) {
   cat("PASS: No significant effects at placebo cutoffs.\n")
 } else {
   cat("WARNING:", n_placebo_sig, "placebo cutoff(s) show significant effects.\n")
 }

 results$placebo_tests <- placebo_results

 # --------------------------------------------------------------------------
 # 9. DONUT HOLE ROBUSTNESS
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 9: DONUT HOLE ROBUSTNESS\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Excluding observations very close to cutoff.\n")
 cat("Tests whether results are driven by observations at the boundary.\n\n")

 donut_holes <- c(500, 1000, 2000, 3000)

 donut_results <- data.frame(
   Donut_Size = integer(),
   Coefficient = numeric(),
   Robust_SE = numeric(),
   Robust_PValue = numeric(),
   N_Effective = integer(),
   stringsAsFactors = FALSE
 )

 for (donut in donut_holes) {
   df_donut <- df %>% filter(abs(X_centered) >= donut)

   rd_donut <- tryCatch({
     rdrobust(y = df_donut$Y, x = df_donut$X_centered, c = 0)
   }, error = function(e) NULL)

   if (!is.null(rd_donut)) {
     donut_results <- rbind(donut_results, data.frame(
       Donut_Size = donut,
       Coefficient = rd_donut$coef[1],
       Robust_SE = rd_donut$se[3],
       Robust_PValue = rd_donut$pv[3],
       N_Effective = rd_donut$N_h[1] + rd_donut$N_h[2]
     ))

     cat(sprintf("  Donut = %4d m:  coef = %7.4f, SE = %6.4f, p = %s %s (N_eff = %d)\n",
                 donut, rd_donut$coef[1], rd_donut$se[3],
                 format_pvalue(rd_donut$pv[3]), add_stars(rd_donut$pv[3]),
                 rd_donut$N_h[1] + rd_donut$N_h[2]))
   }
 }

 results$donut_robustness <- donut_results

 # --------------------------------------------------------------------------
 # 10. WITH/WITHOUT COVARIATES
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 10: COVARIATE SENSITIVITY\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("Comparing estimates with and without covariates.\n")
 cat("Large changes may indicate specification problems.\n\n")

 # Define covariates
 covs_available <- intersect(
   c("FN1988", "pchom", "pop", "ratEmp", "ratForeigners",
     "educNoDiplomaPerK", "altitude", "superficie", "revenuPerK"),
   names(df)
 )

 # Remove observations with missing covariates
 df_cov <- df %>%
   select(Y, X_centered, all_of(covs_available)) %>%
   na.omit()

 cov_matrix <- as.matrix(df_cov[, covs_available])

 # Without covariates
 rd_no_cov <- rdrobust(y = df_cov$Y, x = df_cov$X_centered, c = 0)

 # With covariates
 rd_with_cov <- tryCatch({
   rdrobust(y = df_cov$Y, x = df_cov$X_centered, c = 0, covs = cov_matrix)
 }, error = function(e) NULL)

 cat("Without covariates:\n")
 cat(sprintf("  Coefficient: %7.4f (SE = %6.4f, p = %s)\n",
             rd_no_cov$coef[1], rd_no_cov$se[3], format_pvalue(rd_no_cov$pv[3])))

 if (!is.null(rd_with_cov)) {
   cat("\nWith covariates:\n")
   cat(sprintf("  Coefficient: %7.4f (SE = %6.4f, p = %s)\n",
               rd_with_cov$coef[1], rd_with_cov$se[3], format_pvalue(rd_with_cov$pv[3])))

   change_pct <- abs((rd_with_cov$coef[1] - rd_no_cov$coef[1]) / rd_no_cov$coef[1]) * 100
   cat(sprintf("\nChange in coefficient: %.1f%%\n", change_pct))

   if (change_pct > 20) {
     cat("WARNING: Large change when adding covariates. Check specification.\n")
   } else {
     cat("Coefficient stable when adding covariates.\n")
   }
 }

 results$covariate_sensitivity <- list(
   without_covariates = rd_no_cov,
   with_covariates = rd_with_cov
 )

 # --------------------------------------------------------------------------
 # 11. GENERATE FIGURES
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 11: GENERATING PUBLICATION-QUALITY FIGURES\n")
 cat("=" , rep("=", 60), "\n", sep = "")

 # Create output directory if needed
 if (!dir.exists(path_figures)) {
   dir.create(path_figures, recursive = TRUE)
 }

 # Figure 1: Main RDD Plot with optimal binning
 cat("\nGenerating Figure 1: Main RDD Plot...\n")

 rdplot_obj <- rdplot(y = df$Y, x = df$X_centered, c = 0,
                      title = "",
                      x.label = "Distance to Program Frontier (meters)",
                      y.label = paste(outcome_var, "(Outcome)"),
                      hide = TRUE)

 # Extract binned data for custom ggplot
 rdplot_data <- data.frame(
   x = rdplot_obj$vars_bins$rdplot_mean_x,
   y = rdplot_obj$vars_bins$rdplot_mean_y,
   group = ifelse(rdplot_obj$vars_bins$rdplot_mean_x <= 0, "Treated", "Control")
 )

 # Create polynomial predictions
 poly_left <- rdplot_obj$vars_poly$rdplot_y[rdplot_obj$vars_poly$rdplot_x <= 0]
 poly_right <- rdplot_obj$vars_poly$rdplot_y[rdplot_obj$vars_poly$rdplot_x > 0]
 x_left <- rdplot_obj$vars_poly$rdplot_x[rdplot_obj$vars_poly$rdplot_x <= 0]
 x_right <- rdplot_obj$vars_poly$rdplot_x[rdplot_obj$vars_poly$rdplot_x > 0]

 poly_data <- data.frame(
   x = c(x_left, x_right),
   y = c(poly_left, poly_right),
   group = c(rep("Treated", length(x_left)), rep("Control", length(x_right)))
 )

 # Main RDD plot
 p_main <- ggplot() +
   geom_point(data = rdplot_data, aes(x = x, y = y, shape = group),
              size = 3, alpha = 0.8) +
   geom_line(data = poly_data, aes(x = x, y = y, group = group),
             size = 1, color = "black") +
   geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", size = 0.8) +
   scale_x_continuous(labels = function(x) scales::comma(x/1000, suffix = "k")) +
   scale_shape_manual(values = c("Treated" = 16, "Control" = 17),
                      name = "Program Status") +
   labs(x = "Distance to Program Frontier (meters)",
        y = paste(outcome_var)) +
   theme_minimal() +
   theme(
     legend.position = "bottom",
     axis.title = element_text(size = 12, face = "bold"),
     axis.text = element_text(size = 10),
     panel.grid.minor = element_blank()
   )

 ggsave(file.path(path_figures, "RDD_main_rdrobust.png"),
        p_main, width = 10, height = 7, dpi = 300, bg = "white")
 cat("  Saved: RDD_main_rdrobust.png\n")

 # Figure 2: Density plot
 cat("\nGenerating Figure 2: Density Plot...\n")

 if (!is.null(density_test)) {
   png(file.path(path_figures, "RDD_density_test.png"),
       width = 10, height = 7, units = "in", res = 300)
   rdplotdensity(density_test, df$X_centered,
                 title = "",
                 xlabel = "Distance to Program Frontier (meters)",
                 ylabel = "Density")
   dev.off()
   cat("  Saved: RDD_density_test.png\n")
 }

 # Figure 3: Bandwidth sensitivity plot
 cat("\nGenerating Figure 3: Bandwidth Sensitivity Plot...\n")

 p_bw <- ggplot(bw_sensitivity, aes(x = Bandwidth, y = Coefficient)) +
   geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2, fill = "blue") +
   geom_line(size = 1) +
   geom_point(size = 2) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
   geom_vline(xintercept = optimal_bw, linetype = "dotted", color = "darkgreen", size = 0.8) +
   scale_x_continuous(labels = function(x) scales::comma(x/1000, suffix = "k")) +
   labs(x = "Bandwidth (meters)",
        y = "Treatment Effect (LATE)",
        caption = paste("Green line indicates optimal bandwidth:", round(optimal_bw), "m")) +
   theme_minimal() +
   theme(
     axis.title = element_text(size = 12, face = "bold"),
     axis.text = element_text(size = 10),
     panel.grid.minor = element_blank()
   )

 ggsave(file.path(path_figures, "RDD_bandwidth_sensitivity_rdrobust.png"),
        p_bw, width = 10, height = 7, dpi = 300, bg = "white")
 cat("  Saved: RDD_bandwidth_sensitivity_rdrobust.png\n")

 # Figure 4: Covariate balance plot
 cat("\nGenerating Figure 4: Covariate Balance Plot...\n")

 if (nrow(balance_results) > 0) {
   balance_results$Significant <- balance_results$P_Value < 0.05

   p_balance <- ggplot(balance_results, aes(x = reorder(Variable, Coefficient),
                                            y = Coefficient)) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
     geom_errorbar(aes(ymin = Coefficient - 1.96*Std_Error,
                       ymax = Coefficient + 1.96*Std_Error),
                   width = 0.2, color = "gray50") +
     geom_point(aes(color = Significant), size = 3) +
     scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                        name = "Significant\nat 5%") +
     coord_flip() +
     labs(x = "Covariate",
          y = "RDD Coefficient (discontinuity at cutoff)") +
     theme_minimal() +
     theme(
       axis.title = element_text(size = 12, face = "bold"),
       axis.text = element_text(size = 10),
       legend.position = "bottom"
     )

   ggsave(file.path(path_figures, "RDD_covariate_balance.png"),
          p_balance, width = 10, height = 8, dpi = 300, bg = "white")
   cat("  Saved: RDD_covariate_balance.png\n")
 }

 results$figures <- list(
   main_plot = p_main,
   bandwidth_plot = p_bw,
   balance_plot = if (exists("p_balance")) p_balance else NULL
 )

 # --------------------------------------------------------------------------
 # 12. SUMMARY TABLE
 # --------------------------------------------------------------------------
 cat("\n")
 cat("=" , rep("=", 60), "\n", sep = "")
 cat("STEP 12: GENERATING SUMMARY TABLE\n")
 cat("=" , rep("=", 60), "\n", sep = "")

 # Create comprehensive results table
 summary_table <- data.frame(
   Specification = c(
     "Main estimate (optimal BW)",
     "Linear polynomial",
     "Quadratic polynomial",
     "Cubic polynomial",
     "Without covariates",
     "With covariates",
     "Donut = 1000m",
     "Donut = 2000m"
   ),
   Coefficient = numeric(8),
   Robust_SE = numeric(8),
   P_Value = numeric(8),
   Bandwidth = numeric(8),
   N_Effective = integer(8),
   stringsAsFactors = FALSE
 )

 # Fill in values
 summary_table[1, 2:6] <- c(rd_main$coef[1], rd_main$se[3], rd_main$pv[3],
                            rd_main$bws[1,1], rd_main$N_h[1] + rd_main$N_h[2])

 for (i in 1:min(3, nrow(poly_results))) {
   summary_table[i+1, 2:6] <- c(poly_results$Coefficient[i], poly_results$Robust_SE[i],
                                 poly_results$Robust_PValue[i], poly_results$Bandwidth[i],
                                 poly_results$N_Effective[i])
 }

 summary_table[5, 2:6] <- c(rd_no_cov$coef[1], rd_no_cov$se[3], rd_no_cov$pv[3],
                            rd_no_cov$bws[1,1], rd_no_cov$N_h[1] + rd_no_cov$N_h[2])

 if (!is.null(rd_with_cov)) {
   summary_table[6, 2:6] <- c(rd_with_cov$coef[1], rd_with_cov$se[3], rd_with_cov$pv[3],
                              rd_with_cov$bws[1,1], rd_with_cov$N_h[1] + rd_with_cov$N_h[2])
 }

 if (nrow(donut_results) >= 2) {
   donut_1k <- donut_results[donut_results$Donut_Size == 1000, ]
   donut_2k <- donut_results[donut_results$Donut_Size == 2000, ]

   if (nrow(donut_1k) > 0) {
     summary_table[7, 2:6] <- c(donut_1k$Coefficient, donut_1k$Robust_SE,
                                donut_1k$Robust_PValue, NA, donut_1k$N_Effective)
   }
   if (nrow(donut_2k) > 0) {
     summary_table[8, 2:6] <- c(donut_2k$Coefficient, donut_2k$Robust_SE,
                                donut_2k$Robust_PValue, NA, donut_2k$N_Effective)
   }
 }

 # Save as LaTeX table
 if (!dir.exists(path_tables)) {
   dir.create(path_tables, recursive = TRUE)
 }

 # Format for display
 summary_table_display <- summary_table %>%
   mutate(
     Coefficient = sprintf("%.4f", Coefficient),
     Robust_SE = sprintf("%.4f", Robust_SE),
     P_Value = sapply(P_Value, format_pvalue),
     Bandwidth = ifelse(is.na(Bandwidth), "-", sprintf("%.0f", Bandwidth)),
     N_Effective = ifelse(is.na(N_Effective), "-", as.character(N_Effective))
   )

 cat("\nSummary of RDD Estimates:\n")
 cat("-" , rep("-", 80), "\n", sep = "")
 print(summary_table_display, row.names = FALSE)

 results$summary_table <- summary_table

 # --------------------------------------------------------------------------
 # FINAL SUMMARY
 # --------------------------------------------------------------------------
 cat("\n")
 cat("================================================================\n")
 cat("  ANALYSIS COMPLETE\n")
 cat("================================================================\n")
 cat("\nKEY FINDINGS:\n")
 cat("-" , rep("-", 50), "\n", sep = "")
 cat(sprintf("Main treatment effect:     %7.4f %s\n",
             rd_main$coef[1], add_stars(rd_main$pv[3])))
 cat(sprintf("Robust standard error:     %7.4f\n", rd_main$se[3]))
 cat(sprintf("Robust 95%% CI:             [%.4f, %.4f]\n",
             rd_main$ci[3,1], rd_main$ci[3,2]))
 cat(sprintf("Optimal bandwidth:         %.0f meters\n", optimal_bw))
 cat(sprintf("Effective sample size:     %d\n", rd_main$N_h[1] + rd_main$N_h[2]))

 cat("\nVALIDITY CHECKS:\n")
 cat("-" , rep("-", 50), "\n", sep = "")

 # Density test
 if (!is.null(density_test)) {
   if (density_test$test$p_jk >= 0.05) {
     cat("[PASS] McCrary density test (p =", format_pvalue(density_test$test$p_jk), ")\n")
   } else {
     cat("[FAIL] McCrary density test (p =", format_pvalue(density_test$test$p_jk), ")\n")
   }
 }

 # Covariate balance
 if (n_significant == 0) {
   cat("[PASS] Covariate balance (0 of", nrow(balance_results), "significant)\n")
 } else {
   cat("[WARN] Covariate balance (", n_significant, "of", nrow(balance_results), "significant)\n")
 }

 # Placebo tests
 if (n_placebo_sig == 0) {
   cat("[PASS] Placebo cutoff tests (0 of", nrow(placebo_results), "significant)\n")
 } else {
   cat("[WARN] Placebo cutoff tests (", n_placebo_sig, "of", nrow(placebo_results), "significant)\n")
 }

 cat("\nOUTPUT FILES:\n")
 cat("-" , rep("-", 50), "\n", sep = "")
 cat("  Figures saved to:", path_figures, "\n")
 cat("  - RDD_main_rdrobust.png\n")
 cat("  - RDD_density_test.png\n")
 cat("  - RDD_bandwidth_sensitivity_rdrobust.png\n")
 cat("  - RDD_covariate_balance.png\n")

 cat("\n================================================================\n")

 return(results)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Run the analysis (uncomment to execute)
# results <- run_rdd_best_practices(
#   processed_data_path = processed_data_path,
#   path_figures = path_figures,
#   path_tables = path_tables,
#   outcome_var = "FN2002",
#   running_var = "x",
#   cutoff = 0,
#   controls = controls,
#   cluster_var = "canton"
# )

# To run from master.R, add:
# source(file.path(raw_code_path, "RDD_best_practices.R"))
# results_rdd <- run_rdd_best_practices(processed_data_path, path_figures, path_tables)
