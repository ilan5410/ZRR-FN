# ================================================================================
# FIGURES CREATION PIPELINE
# ================================================================================
# This script loads all the necessary datasets to produce the main figures of the
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

## evolution_FN_voteshare.R
source(paste0(path_code_prepare_figures, "evolution_FN_voteshare.R"))

## FN_typologie.R
source(paste0(path_code_prepare_figures, "FN_typologie.R"))

## map_ZRR.R
source(paste0(path_code_prepare_figures, "map_ZRR.R"))

## map_FN.R
source(paste0(path_code_prepare_figures, "map_FN.R"))

## FN_growth.R
source(paste0(path_code_prepare_figures, "FN_growth.R"))

## FN_versus_pop.R
source(paste0(path_code_prepare_figures, "FN_versus_pop.R"))

## FN_evolution_by_treat_status.R
source(paste0(path_code_prepare_figures, "FN_evolution_by_treat_status.R"))

## map_dist_running_var.R
source(paste0(path_code_prepare_figures, "map_dist_running_var.R"))

## RDD_FN1988_placebo.R
source(paste0(path_code_prepare_figures, "RDD_FN1988_placebo.R"))

## balancing_checks.R
source(paste0(path_code_prepare_figures, "balancing_checks.R"))

## RDD_outcomes.R
source(paste0(path_code_prepare_figures, "RDD_outcomes.R"))

## later_elections.R
source(paste0(path_code_prepare_figures, "later_elections.R"))

## map_dist_running_var_random.R
source(paste0(path_code_prepare_figures, "map_dist_running_var_random.R"))

## local_linear_reg_FN2002.R
source(paste0(path_code_prepare_figures, "local_linear_reg_FN2002.R"))

## heterogeneity_effects.R
source(paste0(path_code_prepare_figures, "heterogeneity_effects.R"))

## RDD_randomization.R
source(paste0(path_code_prepare_figures, "RDD_randomization.R"))

# Annex

## map_dist_running_var_shortest_dist.R
source(paste0(path_code_prepare_figures, "map_dist_running_var_shortest_dist.R"))

## balancing_checks_shortest_dist.R
source(paste0(path_code_prepare_figures, "balancing_checks_shortest_dist.R"))

## RDD_FN1988_placebo_shortest_dist.R
source(paste0(path_code_prepare_figures, "RDD_FN1988_placebo_shortest_dist.R"))

## RDD_outcomes_shortest_dist.R
source(paste0(path_code_prepare_figures, "RDD_outcomes_shortest_dist.R"))

## RDD_FN1995_signal.R
source(paste0(path_code_prepare_figures, "RDD_FN1995_signal.R"))



cat("\n===============================================\n")
cat("FIGURES PRODUCED SUCCESSFULLY\n")
cat("===============================================\n")

