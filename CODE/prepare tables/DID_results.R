# ==============================================================================
# DIFFERENCE-IN-DIFFERENCES ANALYSIS OF ZRR PROGRAM IMPACT ON FN VOTE SHARE
# ==============================================================================
# This script analyzes the effect of the ZRR (Zone de Revitalisation Rurale) 
# program on National Front (FN) vote share using a difference-in-differences 
# approach comparing elections in 1988 and 2002.

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

prepare_main_dataset <- function(dfZRRControls) {
  #' Prepare the main dataset with basic filters
  #' 
  #' @param dfZRRControls Raw dataset from loaded environment
  #' @return Filtered dataframe
  
  df <- dfZRRControls %>% 
    filter(year_treat > 0) %>%           # Keep only treated municipalities
    filter(!is.na(FN)) %>%               # Remove missing FN vote share
    filter(!is.na(treatment))            # Remove missing treatment status
  
  return(df)
}

create_election_subset <- function(df, df_merged) {
  #' Create subset for election years with DID variables
  #' 
  #' @param df Main filtered dataset
  #' @param df_merged Merged dataset for imputation
  #' @return Dataset subset for analysis
  
  # Filter for election years and create DID variables
  dfSpe <- df %>%
    filter(year %in% c(1988, 2002)) %>%
    mutate(
      post = ifelse(year >= 1995, 1, 0),          # Post-treatment indicator
      did = post * treatment,                      # DID interaction term
      time_since_open = year - year_treat          # Years since treatment
    )
  
  # Impute missing revenue data using 1994 values for 1988
  dfSpe <- impute_revenue_data(dfSpe, df_merged)
  
  # Impute missing demographic data using 1995 values for 1988
  dfSpe <- impute_demographic_data(dfSpe, df_merged)
  
  return(dfSpe)
}

impute_revenue_data <- function(dfSpe, df_merged) {
  #' Impute missing revenue per capita data for 1988
  #' 
  #' @param dfSpe Election subset dataset
  #' @param df_merged Full merged dataset
  #' @return Dataset with imputed revenue data
  
  dfSpe <- dfSpe %>%
    left_join(
      df_merged %>% 
        filter(year == 1994) %>% 
        select(codecommune, revenuPerK), 
      by = "codecommune", 
      suffix = c("", "_1994")
    ) %>%
    mutate(
      revenuPerK = if_else(
        year == 1988 & is.na(revenuPerK),
        revenuPerK_1994,
        revenuPerK
      )
    ) %>%
    select(-revenuPerK_1994)
  
  return(dfSpe)
}

impute_demographic_data <- function(dfSpe, df_merged) {
  #' Impute missing demographic data for 1988
  #' 
  #' @param dfSpe Election subset dataset  
  #' @param df_merged Full merged dataset
  #' @return Dataset with imputed demographic data
  
  dfSpe <- dfSpe %>%
    left_join(
      df_merged %>% 
        filter(year == 1995) %>% 
        select(codecommune, popYoungOld), 
      by = "codecommune", 
      suffix = c("", "_1995")
    ) %>%
    mutate(
      popYoungOld = if_else(
        year == 1988 & is.na(popYoungOld),
        popYoungOld_1995,
        popYoungOld
      )
    ) %>%
    select(-popYoungOld_1995)
  
  return(dfSpe)
}

define_control_variables <- function(dfSpe) {
  #' Define control variables by excluding key analysis variables
  #' 
  #' @param dfSpe Analysis dataset
  #' @return Vector of control variable names
  
  # Variables to exclude from controls
  exclude_vars <- c(
    "codecommune", "nomcommune", "dep", "year", "reg", "nom", 
    "time_since_open", "year_treat", "post", "FN_log", "treated", 
    "FN", "treatment", "FN1995", "RPR", "deltaFN", "turnout_2002", 
    "canton", "treatment_in_1995", "did"
  )
  
  controls <- setdiff(names(dfSpe), exclude_vars)
  return(controls)
}

clean_analysis_data <- function(dfSpe, controls) {
  #' Clean data by removing observations with missing or infinite values
  #' 
  #' @param dfSpe Analysis dataset
  #' @param controls Vector of control variable names
  #' @return Cleaned dataset
  
  # Define variables that must be non-missing and finite
  essential_vars <- c(
    "codecommune", "FN", "year", "post", "treatment", "year_treat", 
    "time_since_open", "canton", "reg", "dep", controls
  )
  
  # Remove rows with missing or infinite values in essential variables
  for (var in essential_vars) {
    if (var %in% names(dfSpe)) {
      dfSpe <- dfSpe[!is.na(dfSpe[[var]]) & !is.infinite(dfSpe[[var]]), ]
    }
  }
  
  # Select final variables and remove duplicates
  final_vars <- c("codecommune", "FN", "year", "post", "treatment", 
                  "dep", "canton", controls)
  
  dfSpe <- dfSpe %>%
    select(all_of(final_vars)) %>%
    distinct()
  
  return(dfSpe)
}

run_regression_models <- function(dfSpe, controls) {
  #' Run the two main regression models with clustered standard errors
  #' 
  #' @param dfSpe Cleaned analysis dataset
  #' @param controls Vector of control variable names
  #' @return List containing models and standard errors
  
  # Convert to panel data format
  panel <- pdata.frame(dfSpe, "codecommune")
  
  # Model 1: First Difference without controls
  formula1 <- as.formula("FN ~ post:treatment")
  did_model1 <- plm(formula1, data = panel, model = "fd")
  
  # Model 2: First Difference with controls
  controls_formula <- paste(controls, collapse = " + ")
  formula2 <- as.formula(paste("FN ~ post:treatment +", controls_formula))
  did_model2 <- plm(formula2, data = panel, model = "fd")
  
  # Calculate clustered standard errors at canton level
  se_clustered1 <- coef_test(did_model1, cluster = panel$canton, vcov = "CR2")
  se_clustered2 <- coef_test(did_model2, cluster = panel$canton, vcov = "CR2")
  
  return(list(
    models = list(did_model1, did_model2),
    standard_errors = list(
      se_clustered1$SE,
      se_clustered2$SE
    )
  ))
}

generate_results_table <- function(models, controls, path_tables) {
  #' Generate and save regression results table
  #' 
  #' @param models List containing regression models and standard errors
  #' @param controls Vector of control variable names
  #' @param path_tables Output directory path
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_tables)) {
    dir.create(path_tables, recursive = TRUE)
  }
  
  # Generate stargazer table for 2 models
  # stargazer(
  #   models$models,
  #   type = "latex",
  #   se = models$standard_errors,
  #   omit = c(controls, "factor\\(dep\\)", "factor\\(post\\)", "Constant"),
  #   add.lines = list(c("Controls", "No", "Yes")),
  #   dep.var.labels = "Vote share for FN (2002)",
  #   model.names = TRUE,
  #   title = "Effect of the ZRR Program on FN Vote Share (2002)",
  #   label = "tab:did_result",
  #   out = paste0(path_tables, "DID_results.tex"),
  #   keep.stat = c("n", "rsq"),
  # )
  
  output_file <- paste0(path_tables, "DID_results.tex")

  stargazer(
    models$models,
    type = "latex",
    title = "Preliminary evidence: estimated effect of the ZRR program on FN Vote Share (2002)",
    se = models$standard_errors,
    omit = c(controls, "factor\\(dep\\)", "factor\\(post\\)", "Constant"),
    add.lines = list(c("Controls", "No", "Yes")),
    dep.var.labels = "Vote share for FN (2002)",
    dep.var.labels.include = TRUE,
    model.numbers = TRUE,
    label = "tab:did_result",
    keep.stat = c("n", "rsq"),
    omit.stat = c("adj.rsq", "ser", "f"),
    single.row = TRUE,
    no.space = TRUE,
    font.size = "footnotesize",
    column.sep.width = "0pt",
    covariate.labels = c("Post $\\times$ Treat"),
    out = output_file
  )

  # Post-process: add notes below the tabular (not inside it) to avoid overflow
  lines <- readLines(output_file, warn = FALSE)
  note_text <- paste0(
    "\\parbox{\\textwidth}{\\footnotesize \\textit{Notes:} ",
    "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01. ",
    "All models are estimated using a first-difference approach between 1988 and 2002, ",
    "comparing localities that entered the ZRR program in 1988 with the ones that entered after 2004. ",
    "Control variables are unemployment rate, FN vote share in 1988, population size, association density, ",
    "educational attainment (share with no diploma, with higher education, with a baccalaureate, ",
    "and with a vocational diploma), number of men and women aged 20--40, agricultural employment, ",
    "independent workers, intermediate occupations, total employment, poverty rate, altitude, area, ",
    "housing vacancy rate (log), land use (fences and vines per km\\textsuperscript{2}), and typology ",
    "of the municipality. Standard errors are clustered at the county (canton) level.}"
  )
  # Insert the note between \end{tabular} and \end{table}
  idx_end_tabular <- grep("\\\\end\\{tabular\\}", lines)
  idx_end_table <- grep("\\\\end\\{table\\}", lines)
  if (length(idx_end_tabular) > 0 && length(idx_end_table) > 0) {
    end_tab <- max(idx_end_tabular)
    end_tbl <- max(idx_end_table)
    lines <- c(lines[1:end_tab], note_text, lines[end_tbl:length(lines)])
  }
  writeLines(lines, output_file)
  
  
  
  
  cat("Results table saved to:", paste0(path_tables, "DID_results.tex"), "\n")
}

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

run_did_analysis <- function(processed_data_path, path_tables) {
  #' Run Difference-in-Differences Analysis of ZRR Program
  #' 
  #' @param processed_data_path String path to processed data directory
  #' @param path_tables String path to output tables directory
  #' @return List containing regression models and summary statistics
  
  # --------------------------------------------------------------------------
  # 1. LOAD AND VALIDATE DATA
  # --------------------------------------------------------------------------
  
  cat("Loading data...\n")
  data_file <- paste0(processed_data_path, "dataDes.RData")
  
  if (!file.exists(data_file)) {
    stop("Data environment does not exist at: ", data_file)
  }
  
  load(data_file)
  cat("Data loaded successfully.\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE MAIN DATASET
  # --------------------------------------------------------------------------
  
  cat("Preparing main dataset...\n")
  df <- prepare_main_dataset(dfZRRControls)
  
  # --------------------------------------------------------------------------
  # 3. CREATE ANALYSIS SUBSET
  # --------------------------------------------------------------------------
  
  cat("Creating analysis subset for election years 1988 and 2002...\n")
  dfSpe <- create_election_subset(df, dfZRRControls)
  
  # --------------------------------------------------------------------------
  # 4. DEFINE CONTROL VARIABLES
  # --------------------------------------------------------------------------
  
  controls_clean <- define_control_variables(dfSpe)
  cat("Control variables defined:", length(controls_clean), "variables\n")
  
  # --------------------------------------------------------------------------
  # 5. CLEAN DATA
  # --------------------------------------------------------------------------
  
  cat("Cleaning data and removing missing values...\n")
  dfSpe <- clean_analysis_data(dfSpe, controls_clean)
  cat("Final dataset:", nrow(dfSpe), "observations\n")
  
  # --------------------------------------------------------------------------
  # 6. RUN REGRESSION MODELS
  # --------------------------------------------------------------------------
  
  cat("Running regression models...\n")
  models <- run_regression_models(dfSpe, controls_clean)
  
  # --------------------------------------------------------------------------
  # 7. GENERATE OUTPUT TABLE
  # --------------------------------------------------------------------------
  
  cat("Generating results table...\n")
  generate_results_table(models, controls_clean, path_tables)
  
  cat("Analysis completed successfully!\n")
  
  return(models)
}

# ==============================================================================
# EXECUTE ANALYSIS
# ==============================================================================

# Run the complete analysis
tryCatch({
  results <- run_did_analysis(processed_data_path, path_tables)
}, error = function(e) {
  cat("Error in analysis:", e$message, "\n")
})