# ================================================================================
# DATA LOADING AND PROCESSING PIPELINE
# ================================================================================
# This script loads all the necessary datasets and combines them in different
# ways to serve the analysis.
# ================================================================================

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Standardize commune codes by removing leading zeros
#' @param codes Character vector of commune codes
#' @return Character vector with leading zeros removed
standardize_commune_codes <- function(codes) {
  sub("^0+", "", as.character(codes))
}

#' Interpolate time series variables for communes
#' @param df Data frame containing commune data
#' @param vars Character vector of variable prefixes to interpolate
#' @return Long format data frame with interpolated values
interpolate_variable <- function(df, vars) {
  results <- lapply(vars, function(var) {
    # Reshape to long format
    df_long <- df %>%
      pivot_longer(
        cols = starts_with(var),
        names_to = "year",
        values_to = "value",
        names_prefix = var,
        names_transform = list(year = as.integer)
      ) %>%
      filter(!is.na(value), !is.na(codecommune)) %>%
      mutate(
        year = as.numeric(year),
        value = as.numeric(value)
      )
    
    
    # Define complete year range
    years_range <- seq(min(df_long$year), max(df_long$year))
    
    # Complete missing years and interpolate
    df_long %>%
      complete(codecommune, year = years_range, fill = list(value = NA_real_)) %>%
      group_by(codecommune) %>%
      mutate(
        value = if (sum(!is.na(value)) >= 2) {
          na.approx(value, x = year, rule = 2, na.rm = FALSE)
        } else {
          NA_real_
        }
      ) %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      rename(!!var := value)
  })
  
  # Combine all interpolated variables
  reduce(results, full_join, by = c("codecommune", "year"))
}

#' Load and process Excel/CSV data with standardized commune codes
#' @param file_path Path to the data file
#' @param sheet Sheet name for Excel files (optional)
#' @param select_cols Columns to select (optional)
#' @param col_types Column types specification (optional)
#' @return Processed data frame
load_and_process_data <- function(file_path, sheet = NULL, select_cols = NULL, col_types = NULL) {
  # Determine file type and read accordingly
  if (grepl("\\.xlsx?$", file_path)) {
    df <- read_excel(file_path, sheet = sheet, col_types = col_types)
  } else if (grepl("\\.csv$", file_path)) {
    df <- read_csv(file_path, col_types = col_types, show_col_types = FALSE)
  } else {
    stop("Unsupported file format")
  }
  
  # Select columns if specified
  if (!is.null(select_cols)) {
    df <- df %>% select(all_of(select_cols))
  }
  
  # Standardize commune codes if present
  if ("codecommune" %in% names(df)) {
    df <- df %>% mutate(codecommune = standardize_commune_codes(codecommune))
  }
  
  return(df)
}

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

#' Fill missing values using alternative year data
#' @param df Main dataset
#' @param source_df Source dataset with alternative years
#' @param variable Variable name to fill
#' @param alternative_year Year to use for missing values
#' @return Dataset with filled values
fill_missing_from_alternative_year <- function(df, source_df, variable, alternative_year) {
  cat("Filling missing", variable, "values using", alternative_year, "data...\n")
  
  alternative_data <- source_df %>%
    filter(year == alternative_year) %>%
    select(codecommune, !!sym(variable))
  
  result <- df %>%
    left_join(alternative_data, by = "codecommune", suffix = c("", paste0("_", alternative_year))) %>%
    mutate(
      !!sym(variable) := if_else(
        is.na(!!sym(variable)),
        !!sym(paste0(variable, "_", alternative_year)),
        !!sym(variable)
      )
    ) %>%
    select(-!!sym(paste0(variable, "_", alternative_year)))
  
  return(result)
}


# ==============================================================================
# EXECUTION
# ==============================================================================

## main.R
source(paste0(path_code_prepare_data, "main.R"))

## borders_pair.R
source(paste0(path_code_prepare_data, "borders_pair.R"))

## FN_growth.R
source(paste0(path_code_prepare_data, "FN_growth.R"))

## script_sharp.R
source(paste0(path_code_prepare_data, "script_sharp.R"))

## script_sharp_noEpicenter.R
source(paste0(path_code_prepare_data, "script_sharp_noEpicenter.R"))

## dataDes.R
source(paste0(path_code_prepare_data, "dataDes.R"))

## eco_outcomes.R
source(paste0(path_code_prepare_data, "eco_outcomes.R"))

## Cleaning
source(paste0(main_path, "CODE/configurations.R"))

cat("\n===============================================\n")
cat("DATA LOADING AND PROCESSING COMPLETED SUCCESSFULLY\n")
cat("===============================================\n")

