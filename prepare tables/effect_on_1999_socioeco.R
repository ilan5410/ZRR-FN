# ==============================================================================
# ZRR Program Effect on 1999 Socioeconomic Variables
# ==============================================================================
# This script analyzes the effect of the ZRR program on 1999 socioeconomic
# variables using RDD methodology with multiple bandwidths and clustered 
# standard errors
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Analyze ZRR program effects on 1999 socioeconomic variables
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param outcomes Vector of outcome variable names
#' @param labels Named vector of variable labels for output
#' @param bandwidths Vector of bandwidths in meters (default: c(20000, 10000, 5000))
#' @param threshold RDD threshold (default: 0)
#' @param target_year Target year for outcomes (default: "1999")
#' @param control_year Control year for baseline variables (default: "1990")
#' @return Summary table with regression results
generate_zrr_socioeconomic_analysis <- function(processed_data_path, path_tables, 
                                                labels,
                                                target_year = "1999", 
                                                control_year = "1990") {
  
  cat("===============================================\n")
  cat("ZRR SOCIOECONOMIC VARIABLES ANALYSIS\n")
  cat("===============================================\n")
  cat("Target year:", target_year, "\n")
  cat("Control year:", control_year, "\n")
  cat("Bandwidths:", paste(bandwidths/1000, collapse = ", "), "km\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
  data_file <- file.path(processed_data_path, "eco_outcomes.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist")
  }
  
  load(data_file)
  cat("✓ Loaded eco_outcomes.RData\n")
  
  cat("Number of outcomes:", length(outcomes), "\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE BASE DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing base data...\n")
  
  # Set bandwidth bounds
  max_bandwidth <- max(bandwidths)
  lower_bound <-  - max_bandwidth
  upper_bound <-  + max_bandwidth
  
  # Filter and prepare dataset
  df_rdd <- subset(df_eco %>% mutate(x = distance_to_border), 
                   x >= lower_bound & x <= upper_bound)
  
  # Create required variables
  df_rdd$dist <- df_rdd$x
  df_rdd$treatmentZRR <- df_rdd$treatment
  df_rdd$log_pop <- log(df_rdd$pop)
  
  # Clean and rename
  df_rdd <- df_rdd %>%
    select(-treatment_in_1995, -index, -nom, -distance_to_border) %>%
    select(-"...1") %>%
    rename(z = treatment)
  
  cat("✓ Prepared base dataset\n")
  cat("  - Initial observations:", nrow(df_rdd), "\n")
  
  # --------------------------------------------------------------------------
  # 3. RESHAPE DATA TO WIDE FORMAT
  # --------------------------------------------------------------------------
  cat("\n3. Reshaping data to wide format...\n")
  
  # Update outcomes list
  outcomes_clean <- setdiff(c(outcomes, "log_pop"), c("pop"))
  
  # Define columns for reshaping
  id_cols <- setdiff(names(df_rdd), c(outcomes_clean, "year", "pop"))
  
  # Pivot to wide format
  df_rdd_wide <- df_rdd %>%
    mutate(.yr = sprintf("%04d", as.integer(year))) %>%
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = .yr,
      values_from = all_of(outcomes_clean),
      names_glue = "{.value}_{.yr}",
      values_fn = ~ dplyr::first(.x)
    )
  
  cat("✓ Reshaped data to wide format\n")
  cat("  - Wide format observations:", nrow(df_rdd_wide), "\n")
  
  # --------------------------------------------------------------------------
  # 4. DEFINE VARIABLES AND CONTROLS
  # --------------------------------------------------------------------------
  cat("\n4. Defining variables and controls...\n")
  
  # Define outcome and control variables
  outcomes_target <- paste0(outcomes_clean, "_", target_year)
  controls_clean <- setdiff(paste0(outcomes_clean, "_", control_year), 
                      c("popYoungOld_1990", "revenuPerK_1990"))
  
  cat("✓ Defined variables\n")
  cat("  - Target outcomes:", length(outcomes_target), "\n")
  cat("  - Control variables:", length(controls), "\n")
  
  # --------------------------------------------------------------------------
  # 5. RUN REGRESSIONS FOR ALL OUTCOMES AND BANDWIDTHS
  # --------------------------------------------------------------------------
  cat("\n5. Running regressions...\n")
  
  all_models <- list()
  nobs_vector <- c()
  
  # Loop over each outcome
  for (y in outcomes_target) {
    cat("  - Processing outcome:", y, "\n")
    
    df <- unique(df_rdd_wide)
    
    # Create regression formula
    formula <- paste(y, " ~ z + x +", paste(controls_clean, collapse = " + "), "+ factor(dep)")
    
    # Fit models for each bandwidth
    for (bw in bandwidths) {
      
      # Filter data by bandwidth
      filtered_data <- df %>%
        filter(x >= -bw & x <= bw) %>%
        select(any_of(c(c(y, "z", "x", "dep", "codecanton"), controls_clean))) %>%
        filter(
          if_all(everything(), ~ !is.na(.x)),
          if_all(where(is.numeric), ~ is.finite(.x))
        )
      
      nobs_vector <- c(nobs_vector, nrow(filtered_data))
      
      # Run regression with clustered standard errors
      model <- lm(as.formula(formula), data = filtered_data)
      coefs <- coeftest(model, vcov = vcovHC(model, type = "HC1", 
                                             cluster = "group", 
                                             cluster.id = filtered_data$codecanton))
      
      # Store model results
      model_name <- paste(y, "bw", bw, sep = "_")
      all_models[[model_name]] <- coefs
    }
  }
  
  cat("✓ Completed", length(all_models), "regressions\n")
  
  # --------------------------------------------------------------------------
  # 6. EXTRACT AND FORMAT COEFFICIENTS
  # --------------------------------------------------------------------------
  cat("\n6. Extracting and formatting coefficients...\n")
  
  # Function to extract coefficients with significance stars
  extract_coefficients <- function(model, variable) {
    if (variable %in% rownames(model)) {
      coef <- model[variable, "Estimate"]
      se <- model[variable, "Std. Error"]
      p_val <- model[variable, "Pr(>|t|)"]
      
      stars <- ifelse(p_val < 0.001, "***", 
                      ifelse(p_val < 0.01, "**", 
                             ifelse(p_val < 0.05, "*", "")))
      
      return(paste0(round(coef, 3), " (", round(se, 3), ")", stars))
    } else {
      return(NA)
    }
  }
  
  # Create summary table
  summary_table <- data.frame(Outcome = outcomes_target)
  
  # Fill table with treatment coefficients for each bandwidth
  for (i in seq_along(bandwidths)) {
    bw <- bandwidths[i]
    column_name <- paste("bw", bw, sep = "_")
    
    for (outcome in outcomes_target) {
      model_name <- paste(outcome, "bw", bw, sep = "_")
      summary_table[summary_table$Outcome == outcome, column_name] <- 
        extract_coefficients(all_models[[model_name]], "z")
    }
  }
  
  cat("✓ Extracted coefficients for", nrow(summary_table), "outcomes\n")
  
  # --------------------------------------------------------------------------
  # 7. ADD SAMPLE SIZE INFORMATION
  # --------------------------------------------------------------------------
  cat("\n7. Adding sample size information...\n")
  
  # Create observation count row
  nobs_row <- data.frame(Outcome = paste0("nobs_", target_year))
  
  # Add observation counts for each bandwidth
  nobs_by_bandwidth <- split(nobs_vector, rep(1:length(bandwidths), length(outcomes_target)))
  
  for (i in seq_along(bandwidths)) {
    bw <- bandwidths[i]
    column_name <- paste("bw", bw, sep = "_")
    # Take first observation count for each bandwidth (should be same across outcomes)
    nobs_row[1, column_name] <- nobs_by_bandwidth[[i]][1]
  }
  
  # Combine with summary table
  summary_table <- rbind(summary_table, nobs_row)
  
  cat("✓ Added sample size information\n")
  
  # --------------------------------------------------------------------------
  # 8. APPLY LABELS AND FORMAT OUTPUT
  # --------------------------------------------------------------------------
  cat("\n8. Applying labels and formatting output...\n")
  
  # Create labels for target year
  labels_target <- labels
  names(labels_target) <- paste0(names(labels), "_", target_year)
  
  # Apply labels to outcomes
  summary_table$Outcome <- ifelse(
    summary_table$Outcome %in% names(labels_target),
    labels_target[summary_table$Outcome],
    summary_table$Outcome
  )
  
  cat("✓ Applied variable labels\n")
  
  # --------------------------------------------------------------------------
  # 9. GENERATE OUTPUT TABLE
  # --------------------------------------------------------------------------
  cat("\n9. Generating output table...\n")
  
  # Create column names for output
  colnames(summary_table) <- c("Outcome", paste0(bandwidths/1000, " km"))
  
  # Generate LaTeX table
  kable_output <- summary_table %>%
    kable(format = "latex", booktabs = TRUE, 
          caption = paste("Effect of the ZRR program on the", target_year, "socioeconomic variables"),
          label = paste0("tab:effect_", target_year)) %>%
    kable_styling()
  
  output_file <- file.path(path_tables, paste0("effect_on_", target_year, "_socioeco.tex"))
  save_kable(kable_output, file = output_file)
  
  cat("✓ Generated output table\n")
  cat("  - Output file:", output_file, "\n")
  
  # --------------------------------------------------------------------------
  # 10. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n10. Results Summary:\n")
  
  # Count significant results
  significant_count <- 0
  total_count <- 0
  
  for (i in seq_along(bandwidths)) {
    bw <- bandwidths[i]
    cat("  - Bandwidth", bw/1000, "km:\n")
    
    bw_results <- summary_table[1:(nrow(summary_table)-1), i+1]  # Exclude nobs row
    sig_results <- sum(grepl("\\*", bw_results), na.rm = TRUE)
    total_results <- sum(!is.na(bw_results))
    
    cat("    Significant results:", sig_results, "/", total_results, "\n")
    cat("    Sample size:", summary_table[nrow(summary_table), i+1], "\n")
    
    significant_count <- significant_count + sig_results
    total_count <- total_count + total_results
  }
  
  cat("\n  - Overall significant results:", significant_count, "/", total_count, "\n")
  cat("  - Significance rate:", round((significant_count/total_count)*100, 1), "%\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("ZRR SOCIOECONOMIC ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_zrr_socioeconomic_analysis(processed_data_path, path_tables, labels) 
  