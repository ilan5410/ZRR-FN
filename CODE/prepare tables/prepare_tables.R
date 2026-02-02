# ================================================================================
# TABLES CREATION PIPELINE
# ================================================================================
# This script loads all the necessary datasets to produce the main tables of the
# article.
# ================================================================================

# ==============================================================================
# LATEX TABLE FORMATTING HELPER FUNCTIONS
# ==============================================================================

#' Post-process a LaTeX table file to fix width issues
#'
#' This function reads a .tex table file and applies formatting fixes:
#' - Wraps table in resizebox for width management
#' - Fixes long notes with parbox
#' - Optionally adds landscape mode
#' - Changes font size
#'
#' @param tex_file Path to the .tex file to process
#' @param use_resizebox Wrap tabular in resizebox (default: TRUE)
#' @param font_size Font size: "footnotesize", "scriptsize", "small" (default: "footnotesize")
#' @param use_landscape Wrap in landscape environment (default: FALSE)
#' @param notes_width Width for notes parbox as fraction of textwidth (default: 0.9)
#' @return NULL (modifies file in place)
format_latex_table <- function(tex_file,
                                use_resizebox = TRUE,
                                font_size = "footnotesize",
                                use_landscape = FALSE,
                                notes_width = 0.9) {

  if (!file.exists(tex_file)) {
    warning("File does not exist: ", tex_file)
    return(NULL)
  }

  # Read the file
  lines <- readLines(tex_file, warn = FALSE)
  content <- paste(lines, collapse = "\n")

  # Replace font size in table
  content <- gsub("\\\\small\\s*\n", paste0("\\\\", font_size, "\n"), content)
  content <- gsub("\\\\footnotesize\\s*\n", paste0("\\\\", font_size, "\n"), content)

  # Add resizebox wrapper around tabular
  if (use_resizebox) {
    # Find the tabular environment and wrap it
    content <- gsub(
      "(\\\\begin\\{tabular\\})",
      "\\\\resizebox{\\\\textwidth}{!}{%\n\\1",
      content
    )
    content <- gsub(
      "(\\\\end\\{tabular\\})(\\s*\n\\\\end\\{table\\})",
      "\\1\n}% end resizebox\\2",
      content
    )
  }

  # Fix notes formatting - wrap long notes in parbox
  # Match the Note line with multicolumn
  notes_pattern <- "(\\\\textit\\{Note:\\}\\s*&\\s*\\\\multicolumn\\{\\d+\\}\\{[lr]\\}\\{)([^}]+)(\\}\\s*\\\\\\\\)"
  notes_replacement <- paste0("\\1\\\\parbox{", notes_width, "\\\\textwidth}{\\\\", font_size, " \\2}\\3")
  content <- gsub(notes_pattern, notes_replacement, content, perl = TRUE)

  # Add landscape wrapper if needed
  if (use_landscape) {
    content <- gsub(
      "\\\\begin\\{table\\}\\[!htbp\\]",
      "\\\\begin{landscape}\n\\\\begin{table}[!htbp]",
      content
    )
    content <- gsub(
      "\\\\end\\{table\\}\\s*$",
      "\\\\end{table}\n\\\\end{landscape}\n",
      content
    )
  }

  # Write back to file
  writeLines(content, tex_file)
  cat("✓ Formatted table:", basename(tex_file), "\n")

  return(NULL)
}

#' Create a proper longtable from a two-part kable table
#'
#' This function converts a split table into a proper longtable environment
#'
#' @param tex_file Path to the .tex file to process
#' @param footnote_text Additional footnote text to add
#' @return NULL (modifies file in place)
convert_to_longtable <- function(tex_file, footnote_text = NULL) {

  if (!file.exists(tex_file)) {
    warning("File does not exist: ", tex_file)
    return(NULL)
  }

  # Read the file
  lines <- readLines(tex_file, warn = FALSE)
  content <- paste(lines, collapse = "\n")

  # Remove the split table artifacts
  # Remove \ContinuedFloat, \clearpage, and merge the two tables
  content <- gsub("\\\\ContinuedFloat", "", content)
  content <- gsub("\\\\clearpage", "", content)

  # Remove the extra \end{table} at the end if present
  content <- gsub("\\\\end\\{table\\}\\s*\\\\end\\{table\\}", "\\\\end{table}", content)

  # Add footnote if provided
  if (!is.null(footnote_text)) {
    # Insert before the last \end{tablenotes} or \end{table}
    content <- gsub(
      "(\\\\end\\{tablenotes\\})",
      paste0("\\\\item ", footnote_text, "\n\\1"),
      content
    )
  }

  # Write back to file
  writeLines(content, tex_file)
  cat("✓ Converted to longtable:", basename(tex_file), "\n")

  return(NULL)
}

# ==============================================================================
# DATA CLEANING HELPER FUNCTIONS
# ==============================================================================

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

