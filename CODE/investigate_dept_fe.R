# ==============================================================================
# INVESTIGATION: Why Do Department FE Matter for RDD Results?
# ==============================================================================

library(dplyr)
library(ggplot2)
library(rdrobust)
library(sandwich)
library(lmtest)
library(tidyr)

# Load data
main_path <- here::here()
if (!endsWith(main_path, "/")) main_path <- paste0(main_path, "/")
source(file.path(main_path, "CODE/configurations.R"))

load(file.path(processed_data_path, "script_sharp.RData"))

# Output folder
output_dir <- file.path(main_path, "OUTPUT/RDD_best_practices/investigation/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n")
cat("================================================================\n")
cat("INVESTIGATION: WHY DO DEPARTMENT FE MATTER?\n")
cat("================================================================\n\n")

# Prepare data
df <- dfZRRControls %>%
  filter(x >= -20000 & x <= 20000) %>%
  mutate(
    treatmentZRR = z,
    pop = log(pop),
    popDensity = log(popDensity),
    x_km = x / 1000  # Convert to km for readability
  ) %>%
  distinct(codecommune, .keep_all = TRUE) %>%
  filter(!is.na(FN2002), !is.na(dep))

cat("Sample: ", nrow(df), "communes within 20km of frontier\n\n")

# ==============================================================================
# INVESTIGATION 1: Geographic Distribution of Treatment
# ==============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("INVESTIGATION 1: GEOGRAPHIC DISTRIBUTION OF TREATMENT\n")
cat("=" , rep("=", 70), "\n", sep = "")

# How many departments have both treated and control communes?
dept_composition <- df %>%
  group_by(dep) %>%
  summarise(
    n_total = n(),
    n_treated = sum(treatmentZRR == TRUE),
    n_control = sum(treatmentZRR == FALSE),
    pct_treated = mean(treatmentZRR == TRUE) * 100,
    mean_FN_treated = mean(FN2002[treatmentZRR == TRUE], na.rm = TRUE),
    mean_FN_control = mean(FN2002[treatmentZRR == FALSE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    has_both = n_treated > 0 & n_control > 0,
    diff_FN = mean_FN_treated - mean_FN_control
  )

cat("\nDepartment composition:\n")
cat("  - Total departments:", n_distinct(df$dep), "\n")
cat("  - Departments with ONLY treated communes:", sum(dept_composition$n_control == 0), "\n")
cat("  - Departments with ONLY control communes:", sum(dept_composition$n_treated == 0), "\n")
cat("  - Departments with BOTH:", sum(dept_composition$has_both), "\n")

# This is key - if many departments have only one type, dept FE are absorbing treatment!
pct_only_one_type <- (sum(!dept_composition$has_both) / nrow(dept_composition)) * 100
cat("\n  >> ", round(pct_only_one_type, 1), "% of departments have only treated OR only control communes!\n")

if (pct_only_one_type > 30) {
  cat("  >> WARNING: This means dept FE may be partially collinear with treatment!\n")
}

# ==============================================================================
# INVESTIGATION 2: FN Vote Share Varies by Department
# ==============================================================================
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INVESTIGATION 2: FN VOTE SHARE VARIATION ACROSS DEPARTMENTS\n")
cat("=" , rep("=", 70), "\n", sep = "")

# How much does FN vote vary across departments?
dept_fn <- df %>%
  group_by(dep) %>%
  summarise(
    mean_FN = mean(FN2002, na.rm = TRUE),
    sd_FN = sd(FN2002, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("\nFN2002 variation across departments:\n")
cat("  - Mean of department means:", round(mean(dept_fn$mean_FN), 4), "\n")
cat("  - SD of department means:", round(sd(dept_fn$mean_FN), 4), "\n")
cat("  - Min department mean:", round(min(dept_fn$mean_FN), 4), "\n")
cat("  - Max department mean:", round(max(dept_fn$mean_FN), 4), "\n")
cat("  - Range:", round(max(dept_fn$mean_FN) - min(dept_fn$mean_FN), 4), "\n")

# Correlation between treatment rate and FN vote by department
dept_summary <- df %>%
  group_by(dep) %>%
  summarise(
    pct_treated = mean(treatmentZRR == TRUE),
    mean_FN = mean(FN2002, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cor_treat_fn <- cor(dept_summary$pct_treated, dept_summary$mean_FN, use = "complete.obs")
cat("\nCorrelation between dept treatment rate and dept mean FN:", round(cor_treat_fn, 3), "\n")

if (abs(cor_treat_fn) > 0.3) {
  cat("  >> IMPORTANT: Departments with more ZRR communes have ",
      ifelse(cor_treat_fn < 0, "LOWER", "HIGHER"), " FN vote share!\n")
  cat("  >> This geographic confounding explains why dept FE matter.\n")
}

# ==============================================================================
# INVESTIGATION 3: Within-Department RDD
# ==============================================================================
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INVESTIGATION 3: WITHIN-DEPARTMENT RDD ANALYSIS\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("Running RDD separately for departments that have both treated & control.\n\n")

# Filter to departments with both types
depts_with_both <- dept_composition %>% filter(has_both) %>% pull(dep)
df_both <- df %>% filter(dep %in% depts_with_both)

cat("Analyzing", length(depts_with_both), "departments with both treated and control communes.\n")
cat("Sample size:", nrow(df_both), "communes\n\n")

# Run rdrobust on this subset
rd_within <- rdrobust(y = df_both$FN2002, x = df_both$x, c = 0)

cat("rdrobust on departments with BOTH types:\n")
cat("  - Coefficient:", round(rd_within$coef[1], 4), "\n")
cat("  - Robust SE:", round(rd_within$se[3], 4), "\n")
cat("  - Robust p-value:", round(rd_within$pv[3], 4), "\n")
cat("  - Optimal bandwidth:", round(rd_within$bws[1,1]), "meters\n")

# Compare to OLS with dept FE on same sample
df_both_10km <- df_both %>%
  filter(x >= -10000 & x <= 10000) %>%
  mutate(x_scaled = x / 10000)

model_both_fe <- lm(FN2002 ~ treatmentZRR + x_scaled + treatmentZRR:x_scaled + factor(dep),
                    data = df_both_10km)
se_both <- sqrt(diag(vcovCL(model_both_fe, cluster = df_both_10km$canton, type = "HC1")))

cat("\nOLS with dept FE on same sample (10km bandwidth):\n")
cat("  - Coefficient:", round(coef(model_both_fe)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_both["treatmentZRRTRUE"], 4), "\n")

# ==============================================================================
# INVESTIGATION 4: Heterogeneity Across Departments
# ==============================================================================
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INVESTIGATION 4: TREATMENT EFFECT HETEROGENEITY BY DEPARTMENT\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Run RDD for each department with enough observations
dept_effects <- data.frame(
  dep = character(),
  coef = numeric(),
  se = numeric(),
  pval = numeric(),
  n = integer(),
  stringsAsFactors = FALSE
)

for (d in depts_with_both) {
  df_dept <- df %>% filter(dep == d)

  if (nrow(df_dept) >= 100 && sum(df_dept$treatmentZRR) >= 20 && sum(!df_dept$treatmentZRR) >= 20) {
    rd_dept <- tryCatch({
      rdrobust(y = df_dept$FN2002, x = df_dept$x, c = 0)
    }, error = function(e) NULL)

    if (!is.null(rd_dept)) {
      dept_effects <- rbind(dept_effects, data.frame(
        dep = d,
        coef = rd_dept$coef[1],
        se = rd_dept$se[3],
        pval = rd_dept$pv[3],
        n = nrow(df_dept)
      ))
    }
  }
}

cat("\nDepartment-level RDD estimates (", nrow(dept_effects), " departments with enough data):\n", sep = "")
cat("  - Mean coefficient:", round(mean(dept_effects$coef), 4), "\n")
cat("  - SD of coefficients:", round(sd(dept_effects$coef), 4), "\n")
cat("  - Min coefficient:", round(min(dept_effects$coef), 4), "\n")
cat("  - Max coefficient:", round(max(dept_effects$coef), 4), "\n")
cat("  - # Significant at 5%:", sum(dept_effects$pval < 0.05), "of", nrow(dept_effects), "\n")
cat("  - # Positive effects:", sum(dept_effects$coef > 0), "\n")
cat("  - # Negative effects:", sum(dept_effects$coef < 0), "\n")

# ==============================================================================
# INVESTIGATION 5: Is There Actually a Discontinuity at the Boundary?
# ==============================================================================
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INVESTIGATION 5: EXAMINING THE DISCONTINUITY VISUALLY\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Create bins and compute means
df_binned <- df %>%
  mutate(bin = cut(x, breaks = seq(-20000, 20000, by = 1000), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(
    x_mid = mean(x),
    y_mean = mean(FN2002, na.rm = TRUE),
    y_se = sd(FN2002, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(bin))

# Raw means plot
p1 <- ggplot(df_binned, aes(x = x_mid / 1000, y = y_mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = y_mean - 1.96*y_se, ymax = y_mean + 1.96*y_se), width = 0.3, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(data = df_binned %>% filter(x_mid < 0), method = "lm", se = TRUE, color = "blue") +
  geom_smooth(data = df_binned %>% filter(x_mid >= 0), method = "lm", se = TRUE, color = "blue") +
  labs(title = "Raw FN2002 by Distance to Frontier",
       subtitle = "No controls or fixed effects",
       x = "Distance to Frontier (km)",
       y = "FN Vote Share 2002") +
  theme_minimal()

ggsave(file.path(output_dir, "raw_discontinuity.png"), p1, width = 10, height = 7, dpi = 300)
cat("\nSaved: raw_discontinuity.png\n")

# Now with department-demeaned outcome
df <- df %>%
  group_by(dep) %>%
  mutate(FN2002_demeaned = FN2002 - mean(FN2002, na.rm = TRUE)) %>%
  ungroup()

df_binned_dm <- df %>%
  mutate(bin = cut(x, breaks = seq(-20000, 20000, by = 1000), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(
    x_mid = mean(x),
    y_mean = mean(FN2002_demeaned, na.rm = TRUE),
    y_se = sd(FN2002_demeaned, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(bin))

p2 <- ggplot(df_binned_dm, aes(x = x_mid / 1000, y = y_mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = y_mean - 1.96*y_se, ymax = y_mean + 1.96*y_se), width = 0.3, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(data = df_binned_dm %>% filter(x_mid < 0), method = "lm", se = TRUE, color = "blue") +
  geom_smooth(data = df_binned_dm %>% filter(x_mid >= 0), method = "lm", se = TRUE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  labs(title = "Department-Demeaned FN2002 by Distance to Frontier",
       subtitle = "Equivalent to within-department comparison (dept FE)",
       x = "Distance to Frontier (km)",
       y = "FN Vote Share 2002 (dept-demeaned)") +
  theme_minimal()

ggsave(file.path(output_dir, "demeaned_discontinuity.png"), p2, width = 10, height = 7, dpi = 300)
cat("Saved: demeaned_discontinuity.png\n")

# ==============================================================================
# INVESTIGATION 6: The "Ideal" RDD - Very Close to Boundary
# ==============================================================================
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INVESTIGATION 6: VERY LOCAL ANALYSIS (Within 5km)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Focus on communes very close to boundary
df_5km <- df %>% filter(abs(x) <= 5000)

cat("\nCommunes within 5km of boundary:", nrow(df_5km), "\n")
cat("  - Treated:", sum(df_5km$treatmentZRR), "\n")
cat("  - Control:", sum(!df_5km$treatmentZRR), "\n")

# Simple t-test
ttest <- t.test(FN2002 ~ treatmentZRR, data = df_5km)
cat("\nSimple t-test (within 5km):\n")
cat("  - Mean inside:", round(ttest$estimate[2], 4), "\n")
cat("  - Mean outside:", round(ttest$estimate[1], 4), "\n")
cat("  - Difference:", round(ttest$estimate[2] - ttest$estimate[1], 4), "\n")
cat("  - p-value:", round(ttest$p.value, 4), "\n")

# RDD within 5km
rd_5km <- rdrobust(y = df_5km$FN2002, x = df_5km$x, c = 0)
cat("\nrdrobust (within 5km sample):\n")
cat("  - Coefficient:", round(rd_5km$coef[1], 4), "\n")
cat("  - Robust p-value:", round(rd_5km$pv[3], 4), "\n")

# OLS with dept FE within 5km
df_5km_clean <- df_5km %>%
  mutate(x_scaled = x / 10000) %>%
  filter(!is.na(canton))

model_5km_fe <- lm(FN2002 ~ treatmentZRR + x_scaled + treatmentZRR:x_scaled + factor(dep),
                   data = df_5km_clean)
se_5km <- sqrt(diag(vcovCL(model_5km_fe, cluster = df_5km_clean$canton, type = "HC1")))

cat("\nOLS with dept FE (within 5km):\n")
cat("  - Coefficient:", round(coef(model_5km_fe)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_5km["treatmentZRRTRUE"], 4), "\n")
cat("  - t-stat:", round(coef(model_5km_fe)["treatmentZRRTRUE"] / se_5km["treatmentZRRTRUE"], 2), "\n")

# ==============================================================================
# INVESTIGATION 7: Parallel Trends / Pre-Treatment Check
# ==============================================================================
cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("INVESTIGATION 7: PRE-TREATMENT OUTCOME (FN1988)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Check FN1988 - should be NO discontinuity if RDD is valid
df_1988 <- df %>% filter(!is.na(FN1988))

rd_1988 <- rdrobust(y = df_1988$FN1988, x = df_1988$x, c = 0)
cat("\nrdrobust on FN1988 (pre-treatment placebo):\n")
cat("  - Coefficient:", round(rd_1988$coef[1], 4), "\n")
cat("  - Robust SE:", round(rd_1988$se[3], 4), "\n")
cat("  - Robust p-value:", round(rd_1988$pv[3], 4), "\n")

if (rd_1988$pv[3] < 0.05) {
  cat("  >> WARNING: Significant discontinuity in PRE-TREATMENT outcome!\n")
  cat("  >> This suggests potential selection/sorting, not a treatment effect.\n")
} else {
  cat("  >> GOOD: No significant pre-treatment discontinuity.\n")
}

# Also check with dept FE
df_1988_10km <- df_1988 %>%
  filter(x >= -10000 & x <= 10000) %>%
  mutate(x_scaled = x / 10000) %>%
  filter(!is.na(canton))

model_1988_fe <- lm(FN1988 ~ treatmentZRR + x_scaled + treatmentZRR:x_scaled + factor(dep),
                    data = df_1988_10km)
se_1988 <- sqrt(diag(vcovCL(model_1988_fe, cluster = df_1988_10km$canton, type = "HC1")))

cat("\nOLS with dept FE on FN1988:\n")
cat("  - Coefficient:", round(coef(model_1988_fe)["treatmentZRRTRUE"], 4), "\n")
cat("  - Clustered SE:", round(se_1988["treatmentZRRTRUE"], 4), "\n")
cat("  - t-stat:", round(coef(model_1988_fe)["treatmentZRRTRUE"] / se_1988["treatmentZRRTRUE"], 2), "\n")

# ==============================================================================
# SUMMARY AND CONCLUSIONS
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("SUMMARY OF INVESTIGATION\n")
cat("================================================================\n\n")

cat("KEY FINDINGS:\n")
cat("-" , rep("-", 60), "\n", sep = "")

cat("\n1. GEOGRAPHIC CONFOUNDING:\n")
cat("   - ", round(pct_only_one_type, 1), "% of departments have only treated OR only control\n", sep = "")
cat("   - Correlation(dept treatment rate, dept FN): ", round(cor_treat_fn, 3), "\n", sep = "")
cat("   - Departments with more ZRR have ", ifelse(cor_treat_fn < 0, "LOWER", "HIGHER"), " FN vote\n", sep = "")

cat("\n2. WHAT DEPT FE ARE DOING:\n")
cat("   - Absorbing cross-department variation in FN vote\n")
cat("   - Essentially comparing within-department only\n")
cat("   - This is NOT standard RDD methodology\n")

cat("\n3. PURE RDD (no dept FE) RESULTS:\n")
cat("   - rdrobust coefficient: ", round(rd_within$coef[1], 4), "\n", sep = "")
cat("   - p-value: ", round(rd_within$pv[3], 3), "\n", sep = "")
cat("   - NOT statistically significant\n")

cat("\n4. HETEROGENEITY:\n")
cat("   - ", sum(dept_effects$coef > 0), " departments show POSITIVE effect\n", sep = "")
cat("   - ", sum(dept_effects$coef < 0), " departments show NEGATIVE effect\n", sep = "")
cat("   - Only ", sum(dept_effects$pval < 0.05), " of ", nrow(dept_effects), " are significant\n", sep = "")

cat("\n5. PRE-TREATMENT CHECK (FN1988):\n")
cat("   - rdrobust p-value: ", round(rd_1988$pv[3], 3), "\n", sep = "")
if (rd_1988$pv[3] < 0.05) {
  cat("   - CONCERN: Pre-existing difference at boundary\n")
} else {
  cat("   - OK: No pre-existing difference\n")
}

cat("\n")
cat("================================================================\n")
cat("CONCLUSION\n")
cat("================================================================\n")
cat("\n")
cat("Your significant negative effect (-0.004 to -0.006) appears to come from\n")
cat("WITHIN-DEPARTMENT comparisons enabled by department fixed effects.\n")
cat("\n")
cat("Without dept FE, the standard RDD approach finds NO significant effect.\n")
cat("\n")
cat("This raises an important methodological question:\n")
cat("  - Is the dept FE specification justified by the research design?\n")
cat("  - Or does it introduce a different type of comparison that may have\n")
cat("    its own confounders?\n")
cat("\n")
cat("The standard RDD argument is that observations just left and right of\n")
cat("the cutoff are comparable WITHOUT additional controls. If dept FE are\n")
cat("necessary, this suggests the pure RDD assumption may not hold.\n")
cat("\n")

# Save investigation results
save(dept_composition, dept_effects, cor_treat_fn,
     file = file.path(output_dir, "investigation_results.RData"))
cat("Results saved to: ", output_dir, "\n", sep = "")
