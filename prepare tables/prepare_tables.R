# ================================================================================
# TABLES CREATION PIPELINE
# ================================================================================
# This script loads all the necessary datasets to produce the main tables of the
# article.
# ================================================================================

#' Clean data by removing rows with NA, Inf, or -Inf values
#' @param df Data frame to clean
#' @param variables Character vector of variable names to check
#' @return Cleaned data frame
clean_data_variables <- function(df, variables) {
  cat("Cleaning data for", length(variables), "variables...\n")
  initial_rows <- nrow(df)
  
  for (var in variables) {
    if (var %in% names(df)) {
      # Remove NA values
      df <- df[!is.na(df[[var]]), ]
      
      # Remove Inf and -Inf values (only for numeric variables)
      if (is.numeric(df[[var]])) {
        df <- df[!is.infinite(df[[var]]), ]
      }
    } else {
      warning(paste("Variable", var, "not found in dataset"))
    }
  }
  
  final_rows <- nrow(df)
  cat("Removed", initial_rows - final_rows, "rows with missing/invalid values\n")
  cat("Final dataset:", final_rows, "observations\n")
  
  return(df)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

## county_consistency_check.R
source(paste0(path_code_prepare_tables, "county_consistency_check.R"))

## descriptive_statistics.R
source(paste0(path_code_prepare_tables, "descriptive_statistics.R"))

## DID_results.R
source(paste0(path_code_prepare_tables, "DID_results.R"))

## summary_stats_bandwidth.R
source(paste0(path_code_prepare_tables, "summary_stats_bandwidth.R"))

## main_results_different_bandwidths.R
source(paste0(path_code_prepare_tables, "main_results_different_bandwidths.R"))

## main_results_diff_specifications.R
source(paste0(path_code_prepare_tables, "main_results_diff_specifications.R"))

## main_results_diff_outcomes.R
source(paste0(path_code_prepare_tables, "main_results_diff_outcomes.R"))

## balancing_tests.R
source(paste0(path_code_prepare_tables, "balancing_tests.R"))

## border_muni_results.R
source(paste0(path_code_prepare_tables, "border_muni_results.R"))

## winsorizing_trimming_doughnut.R
source(paste0(path_code_prepare_tables, "winsorizing_trimming_doughnut.R"))

## heterogeneity_causal.R
source(paste0(path_code_prepare_tables, "heterogeneity_causal_fm.R"))

## effect_on_1999_socioeco.R
source(paste0(path_code_prepare_tables, "effect_on_1999_socioeco.R"))

## absolute_vote.R
source(paste0(path_code_prepare_tables, "absolute_vote.R"))


## annex_var_evolution.R
source(paste0(path_code_prepare_tables, "annex_var_evolution.R"))

## annex_no_epicenter.R
source(paste0(path_code_prepare_tables, "annex_no_epicenter.R"))

## annex_no_epicenter_diff_bd.R
source(paste0(path_code_prepare_tables, "annex_no_epicenter_diff_bd.R"))

## annex_border_pair_ttest.R
source(paste0(path_code_prepare_tables, "annex_border_pair_ttest.R"))

## annex_main_dif_bd_1995.R
source(paste0(path_code_prepare_tables, "annex_main_dif_bd_1995.R"))

## annex_matching.R
source(paste0(path_code_prepare_tables, "annex_matching.R"))

## annex_main_results_1995.R
source(paste0(path_code_prepare_tables, "annex_main_results_1995.R"))


cat("\n===============================================\n")
cat("TABLES PRODUCED SUCCESSFULLY\n")
cat("===============================================\n")

