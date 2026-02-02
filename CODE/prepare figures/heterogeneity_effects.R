# ==============================================================================
# Heterogeneity Effects Comparison Analysis (Machine Learning Methods)
# ==============================================================================
# This script compares different machine learning methods for estimating
# conditional average treatment effects (CATE) including OLS, Post-selection
# Lasso, Honest Causal Trees, and Causal Forests with comprehensive validation
# ==============================================================================

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' Generate heterogeneity effects comparison analysis using multiple ML methods
#' @param processed_data_path Path to processed data directory
#' @param path_figures Path to output figures directory
#' @param figure_width Width of output figure in inches (default: 10)
#' @param figure_height Height of output figure in inches (default: 6)
#' @param figure_dpi DPI resolution for output figure (default: 300)
#' @param controls Vector of control variable names to include in models
#' @param outcome_variable Name of outcome variable (default: "y")
#' @param treatment_variable Name of treatment variable (default: "z")
#' @param n_folds Number of folds for sample splitting (default: 2)
#' @param random_seed Random seed for reproducibility (default: 142)
#' @param hist_bins Number of histogram bins (default: 30)
#' @param tree_params List of parameters for causal tree methods
#' @param forest_params List of parameters for causal forest method
#' @return List containing plots, CATE estimates, and method comparison statistics
generate_heterogeneity_effects_analysis <- function(processed_data_path, path_figures,
                                                    figure_width = 10,
                                                    figure_height = 6,
                                                    figure_dpi = 300,
                                                    outcome_variable = "y",
                                                    treatment_variable = "z",
                                                    n_folds = 2,
                                                    random_seed = 142,
                                                    hist_bins = 30,
                                                    tree_params = NULL,
                                                    forest_params = NULL) {
  
  cat("===============================================\n")
  cat("HETEROGENEITY EFFECTS COMPARISON ANALYSIS\n")
  cat("===============================================\n")
  cat("Outcome variable:", outcome_variable, "\n")
  cat("Treatment variable:", treatment_variable, "\n")
  cat("Number of folds:", n_folds, "\n")
  cat("Random seed:", random_seed, "\n")
  cat("Output dimensions:", figure_width, "x", figure_height, "inches @", figure_dpi, "DPI\n")
  cat("\n")
  
  # --------------------------------------------------------------------------
  # 1. LOAD AND VALIDATE DATA
  # --------------------------------------------------------------------------
  cat("1. Loading and validating data...\n")
  
  data_file <- file.path(processed_data_path, "borders_pair.RData")
  if (!file.exists(data_file)) {
    stop("Data environment does not exist: ", data_file)
  }
  
  load(data_file)
  cat("✓ Loaded borders_pair.RData\n")
  
  # Use controls from environment if not provided
  if (is.null(controls) && exists("controls")) {
    controls <- get("controls")
  } else if (is.null(controls)) {
    stop("Control variables not specified and 'controls' object not found in environment")
  }
  
  cat("  - Dataset 'dfZRRControls' available with", nrow(dfZRRControls), "observations\n")
  cat("  - Control variables before filtering:", length(controls), "\n")
  
  # --------------------------------------------------------------------------
  # 2. PREPARE AND CLEAN DATA
  # --------------------------------------------------------------------------
  cat("\n2. Preparing and cleaning data...\n")
  
  # Remove problematic controls
  controls_filtered <- setdiff(controls, c("typologie", "x"))
  cat("  - Filtered controls (removed typologie, x):", length(controls_filtered), "variables\n")
  
  # Prepare RCT-style dataset
  df_rct <- dfZRRControls %>%
    select(-x, -typologie) %>%
    select(all_of(c(outcome_variable, treatment_variable, "canton", "dep", "border_pair", controls_filtered))) %>%
    rename(treatmentZRR = !!sym(treatment_variable))
  
  cat("  - Initial dataset shape:", nrow(df_rct), "x", ncol(df_rct), "\n")
  
  # Remove incomplete cases
  my_data <- clean_data_variables(df_rct, names(df_rct))
  cat("  - Complete cases:", nrow(my_data), "observations\n")
  cat("  - Dropped", nrow(df_rct) - nrow(my_data), "observations due to missing values\n")
  
  
  # Apply transformations
  my_data$pop <- log(my_data$pop)
  my_data$treatmentZRR <- as.numeric(my_data$treatmentZRR)
  
  # Convert all variables except border_pair to numeric
  my_data[] <- lapply(names(my_data), function(x) {
    if (x != "border_pair") {
      as.numeric(my_data[[x]])
    } else {
      my_data[[x]]
    }
  })
  
  df <- my_data
  
  cat("✓ Prepared analysis dataset\n")
  cat("  - Final dataset shape:", nrow(df), "x", ncol(df), "\n")
  cat("  - Treatment variable mean:", sprintf("%.3f", mean(df$treatmentZRR, na.rm = TRUE)), "\n")
  cat("  - Outcome variable mean:", sprintf("%.3f", mean(df[[outcome_variable]], na.rm = TRUE)), "\n")
  
  # --------------------------------------------------------------------------
  # 3. SET PARAMETERS AND SPLIT DATA
  # --------------------------------------------------------------------------
  cat("\n3. Setting parameters and splitting data...\n")
  
  # Set random seed for reproducibility
  set.seed(random_seed)
  
  # Set default tree parameters if not provided
  if (is.null(tree_params)) {
    tree_params <- list(
      split.alpha = 0.5,
      cv.alpha = 0.5,
      bucketNum = 5,
      bucketMax = 100,
      minsize = 50
    )
  }
  
  # Set default forest parameters if not provided
  if (is.null(forest_params)) {
    forest_params <- list(
      split.alpha = 0.5,
      cv.alpha = 0.5,
      bucketNum = 5,
      bucketMax = 100,
      minsize = 2,
      sample.size.train.frac = 0.5,
      nodesize = 5,
      num.trees = 10
    )
  }
  
  # Create sample splits
  folds <- caret::createFolds(1:nrow(df), k = n_folds)
  
  # Split the data
  Y1 <- df[folds[[1]], outcome_variable]
  Y2 <- df[folds[[2]], outcome_variable]
  X1 <- df[folds[[1]], "treatmentZRR"]
  X2 <- df[folds[[2]], "treatmentZRR"]
  W1 <- df[folds[[1]], controls_filtered]
  W2 <- df[folds[[2]], controls_filtered]
  
  cat("✓ Split data into", n_folds, "folds\n")
  cat("  - Fold 1:", length(Y1), "observations\n")
  cat("  - Fold 2:", length(Y2), "observations\n")
  cat("  - Treatment rate Fold 1:", sprintf("%.3f", mean(X1)), "\n")
  cat("  - Treatment rate Fold 2:", sprintf("%.3f", mean(X2)), "\n")
  
  # --------------------------------------------------------------------------
  # 4. DEFINE HELPER FUNCTIONS
  # --------------------------------------------------------------------------
  cat("\n4. Defining helper functions...\n")
  
  # Helper functions for creating indicator vectors
  zeros <- function(n) {
    return(integer(n))
  }
  
  ones <- function(n) {
    return(integer(n) + 1)
  }
  
  # Helper function to extract Lasso coefficients
  get_lasso_coeffs <- function(sl_lasso) {
    return(coef(sl_lasso$fitLibrary$lasso_1_All$object, s = "lambda.min")[-1, ])
  }
  
  cat("✓ Defined helper functions\n")
  
  # --------------------------------------------------------------------------
  # 5. ESTIMATE CATE WITH OLS
  # --------------------------------------------------------------------------
  cat("\n5. Estimating CATE with OLS...\n")
  
  tryCatch({
    # Fit OLS model with interactions
    sl_lm <- SuperLearner(Y = Y1,
                          X = data.frame(X = X1, W1, W1 * X1),
                          family = gaussian(),
                          SL.library = "SL.lm",
                          cvControl = list(V = 0))
    
    # Predict under control and treatment
    ols_pred_0s <- predict(sl_lm, 
                           data.frame(X = zeros(nrow(W2)), W2, W2 * zeros(nrow(W2))), 
                           onlySL = TRUE)
    ols_pred_1s <- predict(sl_lm, 
                           data.frame(X = ones(nrow(W2)), W2, W2 * ones(nrow(W2))), 
                           onlySL = TRUE)
    
    # Calculate CATE
    cate_ols <- ols_pred_1s$pred - ols_pred_0s$pred
    
    cat("✓ OLS CATE estimation completed\n")
    cat("  - CATE range:", sprintf("%.4f", min(cate_ols)), "to", sprintf("%.4f", max(cate_ols)), "\n")
    cat("  - CATE mean:", sprintf("%.4f", mean(cate_ols)), "\n")
    
  }, error = function(e) {
    cat("  ✗ OLS estimation failed:", e$message, "\n")
    cate_ols <- rep(NA_real_, nrow(W2))
  })
  
  # --------------------------------------------------------------------------
  # 6. ESTIMATE CATE WITH POST-SELECTION LASSO
  # --------------------------------------------------------------------------
  cat("\n6. Estimating CATE with Post-selection Lasso...\n")
  
  tryCatch({
    # Create Lasso learner
    lasso <- SuperLearner::create.Learner("SL.glmnet", 
                                          params = list(alpha = 1), 
                                          name_prefix = "lasso")
    
    # First stage: predict outcome with Lasso
    predict_y_lasso <- SuperLearner(Y = Y1,
                                    X = data.frame(treatmentZRR = X1, W1, X1 * W1),
                                    family = gaussian(),
                                    SL.library = lasso$names,
                                    cvControl = list(V = 0))
    
    kept_variables <- which(get_lasso_coeffs(predict_y_lasso) != 0)
    
    # Second stage: predict treatment with Lasso
    predict_x_lasso <- SuperLearner(Y = X1,
                                    X = W1,
                                    family = gaussian(),
                                    SL.library = lasso$names,
                                    cvControl = list(V = 0))
    
    kept_variables2 <- which(get_lasso_coeffs(predict_x_lasso) != 0) + 1  # +1 to include X
    
    # Post-Lasso estimation with selected variables
    selected_vars <- c(kept_variables, kept_variables2)
    
    if (length(selected_vars) > 0) {
      sl_post_lasso <- SuperLearner(Y = Y1,
                                    X = data.frame(treatmentZRR = X1, W1, X1 * W1)[, selected_vars],
                                    family = gaussian(),
                                    SL.library = "SL.lm",
                                    cvControl = list(V = 0))
      
      # Predict under control and treatment
      postlasso_pred_0s <- predict(sl_post_lasso,
                                   data.frame(treatmentZRR = zeros(nrow(W2)), W2, W2 * zeros(nrow(W2)))[, selected_vars],
                                   onlySL = TRUE)
      postlasso_pred_1s <- predict(sl_post_lasso,
                                   data.frame(treatmentZRR = ones(nrow(W2)), W2, W2 * ones(nrow(W2)))[, selected_vars],
                                   onlySL = TRUE)
      
      cate_postlasso <- postlasso_pred_1s$pred - postlasso_pred_0s$pred
      
      cat("✓ Post-selection Lasso CATE estimation completed\n")
      cat("  - Variables selected:", length(selected_vars), "\n")
      cat("  - CATE range:", sprintf("%.4f", min(cate_postlasso)), "to", sprintf("%.4f", max(cate_postlasso)), "\n")
      cat("  - CATE mean:", sprintf("%.4f", mean(cate_postlasso)), "\n")
    } else {
      cat("  ✗ No variables selected by Lasso\n")
      cate_postlasso <- rep(0, nrow(W2))
    }
    
  }, error = function(e) {
    cat("  ✗ Post-selection Lasso estimation failed:", e$message, "\n")
    cate_postlasso <- rep(NA_real_, nrow(W2))
  })
  
  # --------------------------------------------------------------------------
  # 7. ESTIMATE CATE WITH HONEST CAUSAL TREES
  # --------------------------------------------------------------------------
  cat("\n7. Estimating CATE with Honest Causal Trees...\n")
  
  tryCatch({
    # Create formula
    tree_fml <- as.formula(paste("Y", paste(names(W1), collapse = " + "), sep = " ~ "))
    
    # Fit honest causal tree
    honest_tree <- causalTree::honest.causalTree(
      formula = tree_fml,
      data = data.frame(Y = Y1, W1),
      treatment = X1,
      est_data = data.frame(Y = Y2, W2),
      est_treatment = X2,
      split.alpha = tree_params$split.alpha,
      split.Rule = "CT",
      split.Honest = TRUE,
      cv.alpha = tree_params$cv.alpha,
      cv.option = "CT",
      cv.Honest = TRUE,
      split.Bucket = TRUE,
      bucketNum = tree_params$bucketNum,
      bucketMax = tree_params$bucketMax,
      minsize = tree_params$minsize
    )
    
    # Prune tree
    honest_tree_prune <- prune(honest_tree, 
                               cp = honest_tree$cp[which.min(honest_tree$cp[, 4]), 1])
    
    # Predict CATE
    cate_honesttree <- predict(honest_tree_prune, 
                               newdata = data.frame(Y = Y2, W2), 
                               type = "vector")
    
    cat("✓ Honest Causal Tree CATE estimation completed\n")
    cat("  - Final tree complexity parameter:", sprintf("%.6f", honest_tree$cp[which.min(honest_tree$cp[, 4]), 1]), "\n")
    cat("  - CATE range:", sprintf("%.4f", min(cate_honesttree)), "to", sprintf("%.4f", max(cate_honesttree)), "\n")
    cat("  - CATE mean:", sprintf("%.4f", mean(cate_honesttree)), "\n")
    
  }, error = function(e) {
    cat("  ✗ Honest Causal Tree estimation failed:", e$message, "\n")
    cate_honesttree <- rep(NA_real_, nrow(W2))
  })
  
  # --------------------------------------------------------------------------
  # 8. ESTIMATE CATE WITH CAUSAL FORESTS
  # --------------------------------------------------------------------------
  cat("\n8. Estimating CATE with Causal Forests...\n")
  
  tryCatch({
    # Calculate forest parameters
    sample_size_total <- floor(length(Y1) / 2)
    mtry_param <- ceiling(ncol(W1) / 3)
    
    # Fit causal forest
    causalforest <- causalTree::causalForest(
      tree_fml,
      data = data.frame(Y = Y1, W1),
      treatment = X1,
      split.Rule = "CT",
      split.Honest = TRUE,
      split.Bucket = TRUE,
      bucketNum = forest_params$bucketNum,
      bucketMax = forest_params$bucketMax,
      cv.option = "CT",
      cv.Honest = TRUE,
      minsize = forest_params$minsize,
      split.alpha = forest_params$split.alpha,
      cv.alpha = forest_params$cv.alpha,
      sample.size.total = sample_size_total,
      sample.size.train.frac = forest_params$sample.size.train.frac,
      mtry = mtry_param,
      nodesize = forest_params$nodesize,
      num.trees = forest_params$num.trees,
      ncov_sample = ncol(W1),
      ncolx = ncol(W1)
    )
    
    # Predict CATE
    cate_causalforest <- predict(causalforest, 
                                 newdata = data.frame(Y = Y2, W2), 
                                 type = "vector")
    
    cat("✓ Causal Forest CATE estimation completed\n")
    cat("  - Number of trees:", forest_params$num.trees, "\n")
    cat("  - mtry parameter:", mtry_param, "\n")
    cat("  - CATE range:", sprintf("%.4f", min(cate_causalforest)), "to", sprintf("%.4f", max(cate_causalforest)), "\n")
    cat("  - CATE mean:", sprintf("%.4f", mean(cate_causalforest)), "\n")
    
  }, error = function(e) {
    cat("  ✗ Causal Forest estimation failed:", e$message, "\n")
    cate_causalforest <- rep(NA_real_, nrow(W2))
  })
  
  # --------------------------------------------------------------------------
  # 9. COMBINE AND ANALYZE HETEROGENEITY EFFECTS
  # --------------------------------------------------------------------------
  cat("\n9. Combining and analyzing heterogeneity effects...\n")
  
  # Combine all CATE estimates
  het_effects <- data.frame(
    ols = cate_ols,
    post_selec_lasso = cate_postlasso,
    causal_tree = cate_honesttree,
    causal_forest = cate_causalforest
  )
  
  # Calculate summary statistics
  het_summary <- het_effects %>%
    summarise(across(everything(), list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      min = ~ min(.x, na.rm = TRUE),
      max = ~ max(.x, na.rm = TRUE),
      na_count = ~ sum(is.na(.x))
    )))
  
  cat("✓ Combined heterogeneity effects\n")
  cat("  - Methods estimated:", ncol(het_effects), "\n")
  cat("  - Observations per method:", nrow(het_effects), "\n")
  
  # Display summary statistics
  cat("  - Summary statistics:\n")
  method_names <- names(het_effects)
  for (i in seq_along(method_names)) {
    method <- method_names[i]
    mean_val <- het_summary[[paste0(method, "_mean")]]
    sd_val <- het_summary[[paste0(method, "_sd")]]
    na_count <- het_summary[[paste0(method, "_na_count")]]
    
    cat("    ", method, ": Mean =", sprintf("%.4f", mean_val), 
        ", SD =", sprintf("%.4f", sd_val), 
        ", Missing =", na_count, "\n")
  }
  
  # --------------------------------------------------------------------------
  # 10. CREATE VISUALIZATION
  # --------------------------------------------------------------------------
  cat("\n10. Creating visualization...\n")
  
  # Prepare data for plotting
  het_effects_long <- reshape2::melt(het_effects, 
                                     variable.name = "Method", 
                                     value.name = "Heterogeneity_Effect")
  
  # Map method names to panel titles
  het_effects_long$Method <- factor(het_effects_long$Method,
                                    levels = c("ols", "post_selec_lasso", "causal_tree", "causal_forest"),
                                    labels = c("Panel A: OLS",
                                               "Panel B: Post-selection Lasso",
                                               "Panel C: Causal Tree",
                                               "Panel D: Causal Forest"))
  
  # Remove NA values for plotting
  het_effects_clean <- het_effects_long[!is.na(het_effects_long$Heterogeneity_Effect), ]
  
  # Calculate means for annotation
  means <- aggregate(Heterogeneity_Effect ~ Method, data = het_effects_clean, mean)
  
  # Create the plot
  p <- ggplot(het_effects_clean, aes(x = Heterogeneity_Effect, fill = Method)) +
    geom_histogram(color = "black", bins = hist_bins, alpha = 0.8) +
    facet_wrap(~ Method, scales = "free") +
    theme_minimal() +
    labs(
      # title = "Comparison of Heterogeneous Treatment Effects",
      # subtitle = "Distribution of Conditional Average Treatment Effects (CATE) by Method",
      x = "Effect Size",
      y = "Frequency"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "none",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    ) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(data = means, 
              aes(label = paste0("Mean: ", round(Heterogeneity_Effect, 3)),
                  x = -Inf, y = Inf),
              hjust = -0.1, vjust = 2, size = 4, color = "black", inherit.aes = FALSE)
  
  cat("✓ Created visualization\n")
  cat("  - Plot includes", nrow(het_effects_clean), "data points\n")
  cat("  - Methods displayed:", length(unique(het_effects_clean$Method)), "\n")
  
  # --------------------------------------------------------------------------
  # 11. SAVE RESULTS
  # --------------------------------------------------------------------------
  cat("\n11. Saving results...\n")
  
  # Create output directory if needed
  if (!dir.exists(path_figures)) {
    dir.create(path_figures, recursive = TRUE)
    cat("  - Created output directory:", path_figures, "\n")
  }
  
  # Save plot
  output_file <- file.path(path_figures, "heterogeneity_effects.png")
  ggsave(output_file, p, width = figure_width, height = figure_height, dpi = figure_dpi, bg = "white")
  
  cat("✓ Saved visualization\n")
  cat("  - Output file:", output_file, "\n")
  cat("  - Dimensions:", figure_width, "x", figure_height, "inches\n")
  cat("  - Resolution:", figure_dpi, "DPI\n")
  
  # --------------------------------------------------------------------------
  # 12. COMPILE FINAL RESULTS
  # --------------------------------------------------------------------------
  cat("\n12. Compiling final results...\n")
  
  # Create comprehensive results object
  analysis_results <- list(
    plot = p,
    cate_estimates = het_effects,
    summary_statistics = het_summary,
    method_comparison = list(
      correlations = cor(het_effects, use = "complete.obs"),
      mean_differences = means
    ),
    analysis_parameters = list(
      n_folds = n_folds,
      random_seed = random_seed,
      tree_params = tree_params,
      forest_params = forest_params,
      hist_bins = hist_bins,
      n_observations = nrow(df),
      n_controls = length(controls_filtered)
    ),
    data_info = list(
      total_observations = nrow(df),
      complete_cases = nrow(het_effects),
      treatment_rate = mean(df$treatmentZRR),
      outcome_mean = mean(df[[outcome_variable]])
    )
  )
  
  cat("✓ Compiled final results\n")
  cat("  - Results object contains:", length(analysis_results), "main components\n")
  
  # --------------------------------------------------------------------------
  # 13. DISPLAY FINAL SUMMARY
  # --------------------------------------------------------------------------
  cat("\n13. Final summary of heterogeneity analysis...\n")
  
  cat("✓ Heterogeneity Effects Comparison completed successfully\n")
  cat("\n===============================================\n")
  cat("HETEROGENEITY EFFECTS ANALYSIS COMPLETED\n")
  cat("===============================================\n")
  cat("Key findings:\n")
  
  # Display method comparison
  for (i in seq_along(method_names)) {
    method <- method_names[i]
    mean_val <- het_summary[[paste0(method, "_mean")]]
    sd_val <- het_summary[[paste0(method, "_sd")]]
    na_count <- het_summary[[paste0(method, "_na_count")]]
    
    status <- ifelse(na_count == 0, "✓", "⚠")
    cat("- ", status, " ", method, ": Mean =", sprintf("%.4f", mean_val), 
        ", SD =", sprintf("%.4f", sd_val), "\n")
  }
  
  # Display correlations between methods
  cat("\nMethod correlations:\n")
  corr_matrix <- cor(het_effects, use = "complete.obs")
  method_pairs <- combn(method_names, 2)
  for (i in 1:ncol(method_pairs)) {
    method1 <- method_pairs[1, i]
    method2 <- method_pairs[2, i]
    corr_val <- corr_matrix[method1, method2]
    cat("- ", method1, " vs ", method2, ": r =", sprintf("%.3f", corr_val), "\n")
  }
  
  cat("\nData summary:\n")
  cat("- Total observations:", nrow(df), "\n")
  cat("- Treatment rate:", sprintf("%.3f", mean(df$treatmentZRR)), "\n")
  cat("- Methods successfully estimated:", sum(sapply(het_effects, function(x) !all(is.na(x)))), "/4\n")
  cat("\n")
  
}

# ==============================================================================
# EXECUTION 
# ==============================================================================

generate_heterogeneity_effects_analysis(processed_data_path, path_figures)


