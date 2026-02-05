# ==============================================================================
# Causal Forest Heterogeneity Analysis
# ==============================================================================
# This script performs heterogeneity analysis using Causal Forest to identify
# the top 10% and bottom 10% of the treatment effect distribution and compare
# their characteristics through t-tests
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Perform Causal Forest heterogeneity analysis
#' @param processed_data_path Path to processed data directory
#' @param path_tables Path to output tables directory
#' @param controls Vector of control variable names
#' @param labels Named vector of variable labels for output
#' @param seed Random seed for reproducibility (default: 142)
#' @param num_trees Number of trees in causal forest (default: 10)
#' @return List containing model results and predictions
generate_causal_forest_heterogeneity <- function(processed_data_path, path_tables, controls, labels,
                                                 seed = 142, num_trees = 10) {
  
  cat("===============================================\n")
  cat("CAUSAL FOREST HETEROGENEITY ANALYSIS\n")
  cat("===============================================\n")
  cat("Number of trees:", num_trees, "\n")
  cat("Random seed:", seed, "\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD DATA
  # --------------------------------------------------------------------------
  cat("1. Loading data...\n")
  
  data_file <- file.path(processed_data_path, "borders_pair.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist")
  }
  
  load(data_file)
  cat("✓ Loaded borders_pair.RData\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing data...\n")
  
  # Clean controls list
  controls <- setdiff(controls, c("typologie", "x"))
  
  # Select and prepare variables
  df_rct <- dfZRRControls %>%
    select(-x) %>%
    select(c("y", "z", "canton", "dep", "border_pair", all_of(controls))) %>%
    rename(treatmentZRR = z)
  
  # Clean and transform data
  my_data <- df_rct[complete.cases(df_rct), ]
  my_data$pop <- log(my_data$pop)
  my_data$treatmentZRR <- as.numeric(my_data$treatmentZRR)
  
  # Convert to numeric (except border_pair)
  my_data[] <- lapply(names(my_data), function(x) {
    if (x != "border_pair") {
      as.numeric(my_data[[x]])
    } else {
      my_data[[x]]
    }
  })
  
  df <- my_data
  
  cat("✓ Prepared dataset\n")
  cat("  - Observations:", nrow(df), "\n")
  cat("  - Controls:", length(controls), "\n")
  
  # --------------------------------------------------------------------------
  # 3. SPLIT DATA
  # --------------------------------------------------------------------------
  cat("\n3. Splitting data...\n")
  
  set.seed(seed)
  folds <- createFolds(1:nrow(df), k = 2)
  
  # Extract data components
  Y1 <- df[folds[[1]], 1]  # Training outcome
  Y2 <- df[folds[[2]], 1]  # Testing outcome
  X1 <- df[folds[[1]], 2]  # Training treatment
  X2 <- df[folds[[2]], 2]  # Testing treatment
  W1 <- df[folds[[1]], controls]  # Training covariates
  W2 <- df[folds[[2]], controls]  # Testing covariates
  
  cat("✓ Split data into training and testing sets\n")
  cat("  - Training observations:", length(Y1), "\n")
  cat("  - Testing observations:", length(Y2), "\n")
  
  # --------------------------------------------------------------------------
  # 4. TRAIN CAUSAL FOREST
  # --------------------------------------------------------------------------
  cat("\n4. Training Causal Forest...\n")
  
  # Create formula
  tree_fml <- as.formula(paste("Y", paste(names(W1), collapse = " + "), sep = " ~ "))
  
  # Train causal forest
  causalforest <- causalForest(
    tree_fml,
    data = data.frame(Y = Y1, W1),
    treatment = X1,
    split.Rule = "CT",
    split.Honest = TRUE,
    split.Bucket = TRUE,
    bucketNum = 5,
    bucketMax = 100,
    cv.option = "CT",
    cv.Honest = TRUE,
    minsize = 2,
    split.alpha = 0.5,
    cv.alpha = 0.5,
    sample.size.total = floor(length(Y1) / 2),
    sample.size.train.frac = 0.5,
    mtry = ceiling(ncol(W1) / 3),
    nodesize = 5,
    num.trees = num_trees,
    ncov_sample = ncol(W1),
    ncolx = ncol(W1)
  )
  
  cat("✓ Trained Causal Forest model\n")
  
  # --------------------------------------------------------------------------
  # 5. PREDICT TREATMENT EFFECTS
  # --------------------------------------------------------------------------
  cat("\n5. Predicting treatment effects...\n")
  
  cate_causalforest <- predict(causalforest, 
                               newdata = data.frame(Y = Y2, W2), 
                               type = "vector")
  
  cat("✓ Generated", length(cate_causalforest), "treatment effect predictions\n")
  
  # --------------------------------------------------------------------------
  # 6. IDENTIFY EXTREME GROUPS
  # --------------------------------------------------------------------------
  cat("\n6. Identifying extreme treatment effect groups...\n")
  
  # Calculate thresholds
  threshold_bottom <- quantile(cate_causalforest, 0.1)
  threshold_top <- quantile(cate_causalforest, 0.9)
  
  # Create subsamples
  strong_effect_subsample_bottom <- W1[cate_causalforest <= threshold_bottom, ]
  strong_effect_subsample_top <- W1[cate_causalforest >= threshold_top, ]
  
  cat("✓ Identified extreme groups\n")
  cat("  - Bottom 10% group:", nrow(strong_effect_subsample_bottom), "observations\n")
  cat("  - Top 10% group:", nrow(strong_effect_subsample_top), "observations\n")
  
  # --------------------------------------------------------------------------
  # 7. CONDUCT T-TESTS
  # --------------------------------------------------------------------------
  cat("\n7. Conducting t-tests between groups...\n")
  
  # Perform t-tests for each variable
  t_test_results <- lapply(1:ncol(strong_effect_subsample_bottom), function(i) {
    t.test(strong_effect_subsample_bottom[, i], strong_effect_subsample_top[, i])
  })
  
  # Extract results
  t_test_summary <- data.frame(
    Variable = names(labels)[names(labels) %in% colnames(strong_effect_subsample_bottom)],
    p_value = sapply(t_test_results, function(x) x$p.value),
    mean_bottom = sapply(t_test_results, function(x) x$estimate[1]),
    mean_top = sapply(t_test_results, function(x) x$estimate[2])
  )
  
  # Add significance stars
  t_test_summary$Significance <- cut(
    t_test_summary$p_value,
    breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
    labels = c("***", "**", "*", "")
  )
  
  cat("✓ Completed t-tests for", nrow(t_test_summary), "variables\n")
  
  # --------------------------------------------------------------------------
  # 8. PREPARE FINAL RESULTS
  # --------------------------------------------------------------------------
  cat("\n8. Preparing final results...\n")
  
  # Add descriptions
  t_test_summary$Description <- labels[t_test_summary$Variable]
  
  # Reorder columns
  t_test_summary <- t_test_summary[, c("Description", "p_value", "Significance", "mean_bottom", "mean_top")]
  
  cat("✓ Prepared results table\n")
  
  # --------------------------------------------------------------------------
  # 9. GENERATE OUTPUT TABLE
  # --------------------------------------------------------------------------
  cat("\n9. Generating output table...\n")
  
  output_file <- file.path(path_tables, "heterogeneity_causal.tex")

  # Build notes with parbox for proper text wrapping
  notes_text <- paste0(
    "\\parbox{0.95\\textwidth}{\\footnotesize ",
    "The table presents a comparison between the top 10\\% and bottom 10\\% ",
    "of the treatment effect distribution as estimated by the Causal Forest Model. ",
    "The columns display the mean values of key socioeconomic and demographic variables ",
    "for both groups, the corresponding p-value, and significance levels indicated by stars ",
    "(*** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1). This comparison highlights the characteristics ",
    "of the groups most and least affected by the treatment.}"
  )

  stargazer(
    t_test_summary,
    type = "latex",
    summary = FALSE,
    digits = 3,
    out = output_file,
    title = "Comparison of Heterogeneity Effects",
    label = "tab:heterogeneity",
    font.size = "footnotesize",
    notes = notes_text,
    notes.append = FALSE,
    notes.align = "l",
    rownames = FALSE
  )

  cat("✓ Generated output table\n")
  cat("  - Output file:", output_file, "\n")

  # Apply formatting fixes for width management
  format_latex_table(
    tex_file = output_file,
    use_resizebox = TRUE,
    font_size = "footnotesize",
    use_landscape = FALSE,
    notes_width = 0.95
  )
  
  # --------------------------------------------------------------------------
  # 10. DISPLAY RESULTS SUMMARY
  # --------------------------------------------------------------------------
  cat("\n10. Results Summary:\n")
  
  # Show significant differences
  significant_vars <- t_test_summary[t_test_summary$p_value < 0.05, ]
  
  if (nrow(significant_vars) > 0) {
    cat("  - Significant differences found in", nrow(significant_vars), "variables:\n")
    for (i in 1:nrow(significant_vars)) {
      cat("    ", significant_vars$Description[i], "(p =", round(significant_vars$p_value[i], 3), ")\n")
    }
  } else {
    cat("  - No significant differences found between groups\n")
  }
  
  # Treatment effect distribution summary
  cat("\n  - Treatment effect distribution:\n")
  cat("    Min:", round(min(cate_causalforest), 4), "\n")
  cat("    Max:", round(max(cate_causalforest), 4), "\n")
  cat("    Mean:", round(mean(cate_causalforest), 4), "\n")
  cat("    Bottom 10% threshold:", round(threshold_bottom, 4), "\n")
  cat("    Top 10% threshold:", round(threshold_top, 4), "\n")
  
  cat("\n✓ Analysis completed successfully\n")
  cat("\n===============================================\n")
  cat("CAUSAL FOREST ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  
}

# ==============================================================================
# EXECUTION EXAMPLE
# ==============================================================================

generate_causal_forest_heterogeneity(processed_data_path, path_tables, controls, labels)