# ==============================================================================
# Comparing RDD Approaches: Why Different Results?
# ==============================================================================

library(dplyr)
library(rdrobust)
library(sandwich)
library(lmtest)

# Load data
main_path <- here::here()
if (!endsWith(main_path, "/")) main_path <- paste0(main_path, "/")
source(file.path(main_path, "CODE/configurations.R"))

load(file.path(processed_data_path, "script_sharp.RData"))

cat("================================================================\n")
cat("COMPARING RDD APPROACHES\n")
cat("================================================================\n\n")

# Prepare data (same as your main_results script)
df <- dfZRRControls %>%
  filter(x >= -20000 & x <= 20000) %>%
  mutate(
    treatmentZRR = z,
    pop = log(pop),
    popDensity = log(popDensity),
    x_scaled = x / 10000  # Scale to 10km units like your code
  ) %>%
  distinct(codecommune, .keep_all = TRUE)

df <- df[complete.cases(df[, c("FN2002", "x", "z", controls, "dep")]), ]

cat("Sample size:", nrow(df), "\n\n")

# --------------------------------------------------------------------------
# 1. CHECK DATA STRUCTURE
# --------------------------------------------------------------------------
cat("=" , rep("=", 60), "\n", sep = "")
cat("1. DATA STRUCTURE CHECK\n")
cat("=" , rep("=", 60), "\n", sep = "")

cat("\nRunning variable (x = distance_to_border):\n")
cat("  - Range:", min(df$x), "to", max(df$x), "\n")
cat("  - Mean:", round(mean(df$x), 1), "\n")

cat("\nTreatment variable (z = treatmentZRR):\n")
cat("  - z = TRUE (inside program):", sum(df$z == TRUE), "\n")
cat("  - z = FALSE (outside program):", sum(df$z == FALSE), "\n")

cat("\nCross-tabulation of x and z:\n")
cat("  - x <= 0 AND z = TRUE:", sum(df$x <= 0 & df$z == TRUE), "\n")
cat("  - x <= 0 AND z = FALSE:", sum(df$x <= 0 & df$z == FALSE), "\n")
cat("  - x > 0 AND z = TRUE:", sum(df$x > 0 & df$z == TRUE), "\n")
cat("  - x > 0 AND z = FALSE:", sum(df$x > 0 & df$z == FALSE), "\n")

cat("\nOutcome (FN2002) by treatment status:\n")
cat("  - Mean FN2002 when z = TRUE (inside):", round(mean(df$FN2002[df$z == TRUE]), 4), "\n")
cat("  - Mean FN2002 when z = FALSE (outside):", round(mean(df$FN2002[df$z == FALSE]), 4), "\n")
cat("  - Difference (inside - outside):", round(mean(df$FN2002[df$z == TRUE]) - mean(df$FN2002[df$z == FALSE]), 4), "\n")

# --------------------------------------------------------------------------
# 2. YOUR SPECIFICATION (with controls + dept FE)
# --------------------------------------------------------------------------
cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("2. YOUR SPECIFICATION (Controls + Dept FE)\n")
cat("=" , rep("=", 60), "\n", sep = "")

# Filter to 10km bandwidth and clean data
df_10km <- df %>%
  filter(x >= -10000 & x <= 10000) %>%
  mutate(x_scaled = x / 10000) %>%
  mutate(across(all_of(controls), ~ifelse(is.infinite(.), NA, .))) %>%
  na.omit()

# Your full specification
formula_full <- as.formula(paste(
  "FN2002 ~ treatmentZRR + x_scaled + treatmentZRR:x_scaled +",
  paste(controls, collapse = " + "),
  "+ factor(dep)"
))

model_full <- lm(formula_full, data = df_10km)
se_clustered <- sqrt(diag(vcovCL(model_full, cluster = df_10km$canton, type = "HC1")))

cat("\nYour specification (10km bandwidth, controls + dept FE):\n")
cat("  - Coefficient on ZRR:", round(coef(model_full)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_clustered["treatmentZRRTRUE"], 4), "\n")
cat("  - t-statistic:", round(coef(model_full)["treatmentZRRTRUE"] / se_clustered["treatmentZRRTRUE"], 2), "\n")
cat("  - R-squared:", round(summary(model_full)$r.squared, 3), "\n")
cat("  - N:", nobs(model_full), "\n")

# --------------------------------------------------------------------------
# 3. STRIPPING DOWN: What happens without controls?
# --------------------------------------------------------------------------
cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("3. STRIPPING DOWN THE SPECIFICATION\n")
cat("=" , rep("=", 60), "\n", sep = "")

# Spec 1: No controls, no FE
model_1 <- lm(FN2002 ~ treatmentZRR + x_scaled + treatmentZRR:x_scaled, data = df_10km)
se_1 <- sqrt(diag(vcovCL(model_1, cluster = df_10km$canton, type = "HC1")))

cat("\nSpec 1: No controls, no FE:\n")
cat("  - Coefficient on ZRR:", round(coef(model_1)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_1["treatmentZRRTRUE"], 4), "\n")
cat("  - R-squared:", round(summary(model_1)$r.squared, 3), "\n")

# Spec 2: Dept FE only
model_2 <- lm(FN2002 ~ treatmentZRR + x_scaled + treatmentZRR:x_scaled + factor(dep), data = df_10km)
se_2 <- sqrt(diag(vcovCL(model_2, cluster = df_10km$canton, type = "HC1")))

cat("\nSpec 2: Dept FE only (no controls):\n")
cat("  - Coefficient on ZRR:", round(coef(model_2)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_2["treatmentZRRTRUE"], 4), "\n")
cat("  - R-squared:", round(summary(model_2)$r.squared, 3), "\n")

# Spec 3: Controls only
formula_3 <- as.formula(paste(
  "FN2002 ~ treatmentZRR + x_scaled + treatmentZRR:x_scaled +",
  paste(controls, collapse = " + ")
))
model_3 <- lm(formula_3, data = df_10km)
se_3 <- sqrt(diag(vcovCL(model_3, cluster = df_10km$canton, type = "HC1")))

cat("\nSpec 3: Controls only (no FE):\n")
cat("  - Coefficient on ZRR:", round(coef(model_3)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_3["treatmentZRRTRUE"], 4), "\n")
cat("  - R-squared:", round(summary(model_3)$r.squared, 3), "\n")

# Spec 4: Full (controls + FE)
cat("\nSpec 4: Full (controls + dept FE):\n")
cat("  - Coefficient on ZRR:", round(coef(model_full)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_clustered["treatmentZRRTRUE"], 4), "\n")
cat("  - R-squared:", round(summary(model_full)$r.squared, 3), "\n")

# --------------------------------------------------------------------------
# 4. RDROBUST COMPARISON
# --------------------------------------------------------------------------
cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("4. RDROBUST ANALYSIS\n")
cat("=" , rep("=", 60), "\n", sep = "")

# Note: rdrobust sign convention
# It estimates the jump from LEFT to RIGHT at cutoff
# LEFT (x < 0) = inside program, RIGHT (x >= 0) = outside program
# So positive coefficient = higher Y outside = lower Y inside (ZRR reduces FN)

cat("\nNOTE ON SIGN CONVENTION:\n")
cat("  - Your OLS: coefficient on ZRR (=1 when inside) -> negative = inside has lower FN\n")
cat("  - rdrobust: estimates jump from left to right -> positive = outside has higher FN\n")
cat("  - These are OPPOSITE signs for the SAME effect!\n\n")

# rdrobust without covariates
rd_no_cov <- rdrobust(y = df$FN2002, x = df$x, c = 0)

cat("rdrobust without covariates:\n")
cat("  - Coefficient (jump at cutoff):", round(rd_no_cov$coef[1], 4), "\n")
cat("  - Robust SE:", round(rd_no_cov$se[3], 4), "\n")
cat("  - Robust p-value:", round(rd_no_cov$pv[3], 4), "\n")
cat("  - Optimal bandwidth:", round(rd_no_cov$bws[1,1]), "meters\n")
cat("  - N effective:", rd_no_cov$N_h[1], "+", rd_no_cov$N_h[2], "\n")

# Equivalent interpretation
cat("\n  >> Equivalent to OLS 'ZRR' coefficient:", round(-rd_no_cov$coef[1], 4), "(flip sign)\n")

# rdrobust WITH covariates
cov_vars <- intersect(controls, names(df))
cov_vars <- cov_vars[!cov_vars %in% c("pop", "popDensity")]  # Already transformed
df_cov <- df %>% select(FN2002, x, all_of(cov_vars)) %>% na.omit()
cov_matrix <- as.matrix(df_cov[, cov_vars])

rd_with_cov <- tryCatch({
  rdrobust(y = df_cov$FN2002, x = df_cov$x, c = 0, covs = cov_matrix)
}, error = function(e) NULL)

if (!is.null(rd_with_cov)) {
  cat("\nrdrobust WITH covariates:\n")
  cat("  - Coefficient (jump at cutoff):", round(rd_with_cov$coef[1], 4), "\n")
  cat("  - Robust SE:", round(rd_with_cov$se[3], 4), "\n")
  cat("  - Robust p-value:", round(rd_with_cov$pv[3], 4), "\n")
  cat("  - Optimal bandwidth:", round(rd_with_cov$bws[1,1]), "meters\n")
  cat("\n  >> Equivalent to OLS 'ZRR' coefficient:", round(-rd_with_cov$coef[1], 4), "(flip sign)\n")
}

# --------------------------------------------------------------------------
# 5. KEY INSIGHT: WHAT'S DRIVING YOUR RESULT?
# --------------------------------------------------------------------------
cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("5. KEY INSIGHT: WHAT DRIVES YOUR SIGNIFICANT RESULT?\n")
cat("=" , rep("=", 60), "\n", sep = "")

cat("\nSUMMARY OF COEFFICIENTS ON ZRR (or equivalent):\n")
cat("-" , rep("-", 60), "\n", sep = "")
cat(sprintf("%-40s %10s %10s\n", "Specification", "Coef", "Signif"))
cat("-" , rep("-", 60), "\n", sep = "")
cat(sprintf("%-40s %10.4f %10s\n", "OLS: No controls, no FE", coef(model_1)["treatmentZRRTRUE"],
    ifelse(abs(coef(model_1)["treatmentZRRTRUE"]/se_1["treatmentZRRTRUE"]) > 1.96, "**", "")))
cat(sprintf("%-40s %10.4f %10s\n", "OLS: Dept FE only", coef(model_2)["treatmentZRRTRUE"],
    ifelse(abs(coef(model_2)["treatmentZRRTRUE"]/se_2["treatmentZRRTRUE"]) > 1.96, "**", "")))
cat(sprintf("%-40s %10.4f %10s\n", "OLS: Controls only", coef(model_3)["treatmentZRRTRUE"],
    ifelse(abs(coef(model_3)["treatmentZRRTRUE"]/se_3["treatmentZRRTRUE"]) > 1.96, "**", "")))
cat(sprintf("%-40s %10.4f %10s\n", "OLS: Full (controls + FE)", coef(model_full)["treatmentZRRTRUE"],
    ifelse(abs(coef(model_full)["treatmentZRRTRUE"]/se_clustered["treatmentZRRTRUE"]) > 1.96, "**", "")))
cat(sprintf("%-40s %10.4f %10s\n", "rdrobust: No covariates", -rd_no_cov$coef[1],
    ifelse(rd_no_cov$pv[3] < 0.05, "**", "")))
if (!is.null(rd_with_cov)) {
  cat(sprintf("%-40s %10.4f %10s\n", "rdrobust: With covariates", -rd_with_cov$coef[1],
      ifelse(rd_with_cov$pv[3] < 0.05, "**", "")))
}
cat("-" , rep("-", 60), "\n", sep = "")

cat("\n")
cat("================================================================\n")
cat("CONCLUSION\n")
cat("================================================================\n")
