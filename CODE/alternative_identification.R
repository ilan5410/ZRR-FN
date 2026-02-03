# ==============================================================================
# ALTERNATIVE IDENTIFICATION STRATEGIES
# ==============================================================================
# Given the issues with the standard RDD approach, we explore:
#   1. Difference-in-Differences (DiD)
#   2. Border Pair Analysis (geographic matching)
#   3. Propensity Score Matching + DiD
#   4. Local RDD with narrower bandwidth
#   5. Spatial RDD (border communes only)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(rdrobust)
library(sandwich)
library(lmtest)
library(tidyr)
library(MatchIt)  # For propensity score matching

# Load data
main_path <- here::here()
if (!endsWith(main_path, "/")) main_path <- paste0(main_path, "/")
source(file.path(main_path, "CODE/configurations.R"))

load(file.path(processed_data_path, "script_sharp.RData"))

# Output folder
output_dir <- file.path(main_path, "OUTPUT/RDD_best_practices/alternative_strategies/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n")
cat("================================================================\n")
cat("ALTERNATIVE IDENTIFICATION STRATEGIES\n")
cat("================================================================\n\n")

# Prepare data
df <- dfZRRControls %>%
  filter(x >= -20000 & x <= 20000) %>%
  mutate(
    treated = as.numeric(z),
    pop_log = log(pop),
    popDensity_log = log(popDensity),
    x_km = x / 1000,
    # Change in FN vote (for DiD)
    delta_FN = FN2002 - FN1988
  ) %>%
  distinct(codecommune, .keep_all = TRUE) %>%
  filter(!is.na(FN2002), !is.na(FN1988), !is.na(dep))

cat("Sample: ", nrow(df), "communes within 20km of frontier\n")
cat("  - Treated:", sum(df$treated), "\n")
cat("  - Control:", sum(!df$treated), "\n\n")

# ==============================================================================
# STRATEGY 1: DIFFERENCE-IN-DIFFERENCES
# ==============================================================================
cat("================================================================\n")
cat("STRATEGY 1: DIFFERENCE-IN-DIFFERENCES (DiD)\n")
cat("================================================================\n")
cat("\nLogic: Compare CHANGE in FN vote (2002-1988) between treated and control.\n")
cat("This differences out time-invariant confounders.\n\n")

# Simple DiD
cat("1a. Simple DiD (no controls):\n")
did_simple <- lm(delta_FN ~ treated, data = df)
cat("    Coefficient:", round(coef(did_simple)["treated"], 4), "\n")
cat("    SE:", round(summary(did_simple)$coefficients["treated", "Std. Error"], 4), "\n")
cat("    p-value:", round(summary(did_simple)$coefficients["treated", "Pr(>|t|)"], 4), "\n")

# DiD with controls
cat("\n1b. DiD with controls (no dept FE):\n")
formula_did_controls <- as.formula(paste(
  "delta_FN ~ treated +",
  paste(setdiff(controls, c("FN1988")), collapse = " + ")
))
df_did <- df %>%
  mutate(across(all_of(setdiff(controls, "FN1988")), ~ifelse(is.infinite(.), NA, .))) %>%
  select(delta_FN, treated, all_of(setdiff(controls, "FN1988")), dep, canton) %>%
  na.omit()

did_controls <- lm(formula_did_controls, data = df_did)
se_did <- sqrt(diag(vcovCL(did_controls, cluster = df_did$canton, type = "HC1")))
cat("    Coefficient:", round(coef(did_controls)["treated"], 4), "\n")
cat("    Clustered SE:", round(se_did["treated"], 4), "\n")
cat("    t-stat:", round(coef(did_controls)["treated"] / se_did["treated"], 2), "\n")

# DiD with dept FE
cat("\n1c. DiD with controls + dept FE:\n")
formula_did_fe <- as.formula(paste(
  "delta_FN ~ treated +",
  paste(setdiff(controls, "FN1988"), collapse = " + "),
  "+ factor(dep)"
))
did_fe <- lm(formula_did_fe, data = df_did)
se_did_fe <- sqrt(diag(vcovCL(did_fe, cluster = df_did$canton, type = "HC1")))
cat("    Coefficient:", round(coef(did_fe)["treated"], 4), "\n")
cat("    Clustered SE:", round(se_did_fe["treated"], 4), "\n")
cat("    t-stat:", round(coef(did_fe)["treated"] / se_did_fe["treated"], 2), "\n")

# DiD within narrow bandwidth
cat("\n1d. DiD within 5km bandwidth (no dept FE):\n")
df_did_5km <- df_did %>% filter(abs(df$x[match(rownames(df_did), rownames(df))]) <= 5000)
# Rebuild for 5km
df_5km <- df %>%
  filter(abs(x) <= 5000) %>%
  mutate(across(all_of(setdiff(controls, "FN1988")), ~ifelse(is.infinite(.), NA, .))) %>%
  select(delta_FN, treated, all_of(setdiff(controls, "FN1988")), dep, canton) %>%
  na.omit()

did_5km <- lm(formula_did_controls, data = df_5km)
se_did_5km <- sqrt(diag(vcovCL(did_5km, cluster = df_5km$canton, type = "HC1")))
cat("    Coefficient:", round(coef(did_5km)["treated"], 4), "\n")
cat("    Clustered SE:", round(se_did_5km["treated"], 4), "\n")
cat("    t-stat:", round(coef(did_5km)["treated"] / se_did_5km["treated"], 2), "\n")
cat("    N:", nrow(df_5km), "\n")

# ==============================================================================
# STRATEGY 2: PROPENSITY SCORE MATCHING + DiD
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("STRATEGY 2: PROPENSITY SCORE MATCHING + DiD\n")
cat("================================================================\n")
cat("\nLogic: Match treated communes to similar control communes based on\n")
cat("pre-treatment characteristics, then compare change in outcomes.\n\n")

# Prepare matching data
match_vars <- c("FN1988", "pchom", "pop_log", "ratEmp", "ratForeigners",
                "educNoDiplomaPerK", "altitude", "superficie")
match_vars <- intersect(match_vars, names(df))

df_match <- df %>%
  select(codecommune, treated, delta_FN, FN2002, FN1988, all_of(match_vars), dep, canton, x) %>%
  mutate(across(all_of(match_vars), ~ifelse(is.infinite(.), NA, .))) %>%
  na.omit()

cat("Matching on:", paste(match_vars, collapse = ", "), "\n")
cat("Sample for matching:", nrow(df_match), "communes\n\n")

# Propensity score matching
tryCatch({
  m_out <- matchit(
    as.formula(paste("treated ~", paste(match_vars, collapse = " + "))),
    data = df_match,
    method = "nearest",
    distance = "glm",
    ratio = 1,
    caliper = 0.1
  )

  cat("Matching results:\n")
  cat("  - Treated matched:", sum(m_out$weights[df_match$treated == 1] > 0), "\n")
  cat("  - Control matched:", sum(m_out$weights[df_match$treated == 0] > 0), "\n")

  # Get matched data
  df_matched <- match.data(m_out)

  # DiD on matched sample
  cat("\n2a. DiD on PSM-matched sample:\n")
  did_matched <- lm(delta_FN ~ treated, data = df_matched, weights = weights)
  cat("    Coefficient:", round(coef(did_matched)["treated"], 4), "\n")
  cat("    SE:", round(summary(did_matched)$coefficients["treated", "Std. Error"], 4), "\n")
  cat("    p-value:", round(summary(did_matched)$coefficients["treated", "Pr(>|t|)"], 4), "\n")

  # Check balance
  cat("\n  Balance check (standardized mean differences):\n")
  for (v in match_vars[1:min(5, length(match_vars))]) {
    smd_before <- (mean(df_match[[v]][df_match$treated == 1], na.rm = TRUE) -
                   mean(df_match[[v]][df_match$treated == 0], na.rm = TRUE)) /
                   sd(df_match[[v]], na.rm = TRUE)
    smd_after <- (mean(df_matched[[v]][df_matched$treated == 1], na.rm = TRUE) -
                  mean(df_matched[[v]][df_matched$treated == 0], na.rm = TRUE)) /
                  sd(df_matched[[v]], na.rm = TRUE)
    cat(sprintf("    %-20s: Before=%.3f, After=%.3f\n", v, smd_before, smd_after))
  }

}, error = function(e) {
  cat("  Matching failed:", e$message, "\n")
})

# ==============================================================================
# STRATEGY 3: GEOGRAPHIC/BORDER PAIR MATCHING
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("STRATEGY 3: BORDER PAIR ANALYSIS\n")
cat("================================================================\n")
cat("\nLogic: Compare communes that are very close to each other but on\n")
cat("opposite sides of the ZRR frontier. This is the 'cleanest' RDD.\n\n")

# Create distance bins and match within bins
df_border <- df %>%
  filter(abs(x) <= 3000) %>%  # Within 3km of border
  mutate(
    dist_bin = cut(abs(x), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000),
                   labels = c("0-500m", "500-1000m", "1000-1500m", "1500-2000m", "2000-2500m", "2500-3000m"))
  )

cat("Communes within 3km of border:", nrow(df_border), "\n")
cat("  - Treated:", sum(df_border$treated), "\n")
cat("  - Control:", sum(!df_border$treated), "\n\n")

# Simple comparison at border
cat("3a. Simple comparison (within 3km):\n")
ttest_border <- t.test(FN2002 ~ treated, data = df_border)
cat("    Mean treated:", round(ttest_border$estimate[2], 4), "\n")
cat("    Mean control:", round(ttest_border$estimate[1], 4), "\n")
cat("    Difference:", round(ttest_border$estimate[2] - ttest_border$estimate[1], 4), "\n")
cat("    p-value:", round(ttest_border$p.value, 4), "\n")

# DiD at border
cat("\n3b. DiD at border (within 3km):\n")
did_border <- lm(delta_FN ~ treated, data = df_border)
cat("    Coefficient:", round(coef(did_border)["treated"], 4), "\n")
cat("    SE:", round(summary(did_border)$coefficients["treated", "Std. Error"], 4), "\n")
cat("    p-value:", round(summary(did_border)$coefficients["treated", "Pr(>|t|)"], 4), "\n")

# Very narrow: within 1km
df_1km <- df %>% filter(abs(x) <= 1000)
cat("\n3c. DiD at border (within 1km only):\n")
cat("    N:", nrow(df_1km), "(treated:", sum(df_1km$treated), ", control:", sum(!df_1km$treated), ")\n")
if (nrow(df_1km) >= 100) {
  did_1km <- lm(delta_FN ~ treated, data = df_1km)
  cat("    Coefficient:", round(coef(did_1km)["treated"], 4), "\n")
  cat("    SE:", round(summary(did_1km)$coefficients["treated", "Std. Error"], 4), "\n")
  cat("    p-value:", round(summary(did_1km)$coefficients["treated", "Pr(>|t|)"], 4), "\n")
}

# ==============================================================================
# STRATEGY 4: BORDER DISCONTINUITY WITH PAIR FE
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("STRATEGY 4: BORDER SEGMENT ANALYSIS\n")
cat("================================================================\n")
cat("\nLogic: The ZRR frontier crosses multiple department boundaries.\n")
cat("Compare within each border segment (controlling for local conditions).\n\n")

# Identify border segments by department pairs
# A commune's 'border segment' is defined by which departments are on each side
df_segments <- df %>%
  filter(abs(x) <= 10000) %>%
  group_by(dep) %>%
  mutate(
    dept_has_both = (sum(treated) > 0) & (sum(!treated) > 0)
  ) %>%
  ungroup() %>%
  filter(dept_has_both)

cat("Departments with both treated and control within 10km:", n_distinct(df_segments$dep), "\n")
cat("Communes in these departments:", nrow(df_segments), "\n\n")

# RDD within each department (pooled with dept FE)
cat("4a. RDD with department FE (depts with both types only):\n")
rd_segments <- rdrobust(y = df_segments$FN2002, x = df_segments$x, c = 0)
cat("    Coefficient:", round(rd_segments$coef[1], 4), "\n")
cat("    Robust SE:", round(rd_segments$se[3], 4), "\n")
cat("    Robust p-value:", round(rd_segments$pv[3], 4), "\n")

# DiD version
cat("\n4b. DiD with department FE (depts with both types):\n")
df_seg_clean <- df_segments %>%
  select(delta_FN, treated, dep, canton) %>%
  na.omit()
did_segments <- lm(delta_FN ~ treated + factor(dep), data = df_seg_clean)
se_seg <- sqrt(diag(vcovCL(did_segments, cluster = df_seg_clean$canton, type = "HC1")))
cat("    Coefficient:", round(coef(did_segments)["treated"], 4), "\n")
cat("    Clustered SE:", round(se_seg["treated"], 4), "\n")
cat("    t-stat:", round(coef(did_segments)["treated"] / se_seg["treated"], 2), "\n")

# ==============================================================================
# STRATEGY 5: REGRESSION DISCONTINUITY IN TIME (Event Study)
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("STRATEGY 5: EVENT STUDY / DYNAMIC DiD\n")
cat("================================================================\n")
cat("\nLogic: Look at the trajectory of FN vote over multiple elections.\n")
cat("Check if treated/control diverge after 1995 (when ZRR started).\n\n")

# Get all FN variables
fn_vars <- grep("^FN[0-9]{4}$", names(df), value = TRUE)
fn_vars <- fn_vars[fn_vars %in% names(df)]
cat("Available FN vote years:", paste(fn_vars, collapse = ", "), "\n\n")

# Create panel structure
df_panel <- df %>%
  select(codecommune, treated, dep, canton, all_of(fn_vars)) %>%
  pivot_longer(
    cols = all_of(fn_vars),
    names_to = "year",
    names_prefix = "FN",
    values_to = "FN_vote"
  ) %>%
  mutate(
    year = as.numeric(year),
    post = as.numeric(year >= 1995),
    treat_post = treated * post
  ) %>%
  filter(!is.na(FN_vote))

cat("Panel observations:", nrow(df_panel), "\n")
cat("Years:", paste(sort(unique(df_panel$year)), collapse = ", "), "\n\n")

# Event study regression
cat("5a. Basic DiD (pre/post 1995):\n")
did_panel <- lm(FN_vote ~ treated + post + treat_post, data = df_panel)
cat("    Treatment x Post coefficient:", round(coef(did_panel)["treat_post"], 4), "\n")
cat("    SE:", round(summary(did_panel)$coefficients["treat_post", "Std. Error"], 4), "\n")
cat("    p-value:", round(summary(did_panel)$coefficients["treat_post", "Pr(>|t|)"], 4), "\n")

# Event study with year effects
cat("\n5b. Event study with commune and year FE:\n")
df_panel_clean <- df_panel %>%
  filter(!is.na(canton)) %>%
  mutate(year_factor = factor(year))

# Interact treatment with each year (relative to 1988 as base)
years_for_es <- sort(unique(df_panel_clean$year))
years_for_es <- years_for_es[years_for_es != 1988]  # Base year

for (y in years_for_es) {
  df_panel_clean[[paste0("treat_", y)]] <- df_panel_clean$treated * (df_panel_clean$year == y)
}

formula_es <- as.formula(paste(
  "FN_vote ~ factor(year) +",
  paste(paste0("treat_", years_for_es), collapse = " + ")
))

es_model <- lm(formula_es, data = df_panel_clean)

cat("    Year-specific treatment effects (relative to 1988):\n")
es_coefs <- data.frame(
  year = years_for_es,
  coef = NA,
  se = NA
)

for (i in seq_along(years_for_es)) {
  y <- years_for_es[i]
  var_name <- paste0("treat_", y)
  if (var_name %in% names(coef(es_model))) {
    es_coefs$coef[i] <- coef(es_model)[var_name]
    es_coefs$se[i] <- summary(es_model)$coefficients[var_name, "Std. Error"]
  }
}

for (i in 1:nrow(es_coefs)) {
  sig <- ifelse(!is.na(es_coefs$coef[i]) && abs(es_coefs$coef[i]/es_coefs$se[i]) > 1.96, "**", "")
  cat(sprintf("      %d: %7.4f (SE: %.4f) %s\n",
              es_coefs$year[i], es_coefs$coef[i], es_coefs$se[i], sig))
}

# Plot event study
es_coefs_plot <- rbind(
  data.frame(year = 1988, coef = 0, se = 0),  # Base year
  es_coefs
) %>%
  mutate(
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  )

p_es <- ggplot(es_coefs_plot, aes(x = year, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 1995, linetype = "dotted", color = "red", size = 1) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.5, color = "gray40") +
  geom_point(size = 3) +
  geom_line() +
  annotate("text", x = 1995, y = max(es_coefs_plot$ci_upper, na.rm = TRUE),
           label = "ZRR starts", color = "red", hjust = -0.1, size = 3) +
  labs(title = "Event Study: Treatment Effect by Year",
       subtitle = "Base year = 1988 (pre-treatment)",
       x = "Election Year",
       y = "Treatment Effect on FN Vote Share") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "event_study.png"), p_es, width = 10, height = 6, dpi = 300)
cat("\nSaved: event_study.png\n")

# ==============================================================================
# SUMMARY COMPARISON
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("SUMMARY: COMPARISON OF ALL STRATEGIES\n")
cat("================================================================\n\n")

summary_table <- data.frame(
  Strategy = c(
    "1. DiD (simple)",
    "1. DiD (controls)",
    "1. DiD (controls + dept FE)",
    "1. DiD (5km bandwidth)",
    "2. PSM + DiD",
    "3. Border pair (3km)",
    "3. Border pair (1km)",
    "4. RDD (depts with both)",
    "4. DiD (depts with both + FE)",
    "5. Panel DiD (treat x post)",
    "Original RDD (10km + FE)",
    "rdrobust (optimal BW)"
  ),
  Coefficient = NA,
  SE = NA,
  Significant = NA,
  stringsAsFactors = FALSE
)

# Fill in values
summary_table[1, 2:3] <- c(coef(did_simple)["treated"],
                           summary(did_simple)$coefficients["treated", "Std. Error"])
summary_table[2, 2:3] <- c(coef(did_controls)["treated"], se_did["treated"])
summary_table[3, 2:3] <- c(coef(did_fe)["treated"], se_did_fe["treated"])
summary_table[4, 2:3] <- c(coef(did_5km)["treated"], se_did_5km["treated"])

if (exists("did_matched")) {
  summary_table[5, 2:3] <- c(coef(did_matched)["treated"],
                             summary(did_matched)$coefficients["treated", "Std. Error"])
}

summary_table[6, 2:3] <- c(coef(did_border)["treated"],
                           summary(did_border)$coefficients["treated", "Std. Error"])

if (exists("did_1km") && nrow(df_1km) >= 100) {
  summary_table[7, 2:3] <- c(coef(did_1km)["treated"],
                             summary(did_1km)$coefficients["treated", "Std. Error"])
}

summary_table[8, 2:3] <- c(-rd_segments$coef[1], rd_segments$se[3])  # Flip sign for comparability
summary_table[9, 2:3] <- c(coef(did_segments)["treated"], se_seg["treated"])
summary_table[10, 2:3] <- c(coef(did_panel)["treat_post"],
                            summary(did_panel)$coefficients["treat_post", "Std. Error"])

# Original estimates (from your tables)
summary_table[11, 2:3] <- c(-0.0044, 0.0020)  # From your main results
summary_table[12, 2:3] <- c(-0.0029, 0.0060)  # From rdrobust

# Calculate significance
summary_table$Significant <- ifelse(
  abs(summary_table$Coefficient / summary_table$SE) > 1.96,
  "Yes", "No"
)

cat("Strategy Comparison:\n")
cat("-" , rep("-", 70), "\n", sep = "")
cat(sprintf("%-35s %10s %10s %12s\n", "Strategy", "Coef", "SE", "Significant"))
cat("-" , rep("-", 70), "\n", sep = "")
for (i in 1:nrow(summary_table)) {
  if (!is.na(summary_table$Coefficient[i])) {
    cat(sprintf("%-35s %10.4f %10.4f %12s\n",
                summary_table$Strategy[i],
                summary_table$Coefficient[i],
                summary_table$SE[i],
                summary_table$Significant[i]))
  }
}
cat("-" , rep("-", 70), "\n", sep = "")

# ==============================================================================
# CONCLUSIONS
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("CONCLUSIONS\n")
cat("================================================================\n")

# Count significant results
n_sig <- sum(summary_table$Significant == "Yes", na.rm = TRUE)
n_total <- sum(!is.na(summary_table$Significant))

cat("\n")
cat("Out of", n_total, "specifications,", n_sig, "show significant effects.\n\n")

cat("KEY OBSERVATIONS:\n")
cat("-" , rep("-", 60), "\n", sep = "")

cat("\n1. DiD APPROACH:\n")
cat("   - Simple DiD shows a ", ifelse(coef(did_simple)["treated"] < 0, "NEGATIVE", "POSITIVE"), " effect\n")
cat("   - The effect is ", ifelse(abs(coef(did_simple)["treated"]/summary(did_simple)$coefficients["treated", "Std. Error"]) > 1.96, "SIGNIFICANT", "NOT significant"), "\n")

cat("\n2. GEOGRAPHIC PROXIMITY:\n")
cat("   - Narrowing bandwidth generally ",
    ifelse(abs(coef(did_border)["treated"]) < abs(coef(did_simple)["treated"]), "REDUCES", "INCREASES"),
    " the effect size\n")
cat("   - Closest to border (1km): effect is ",
    ifelse(exists("did_1km") && abs(coef(did_1km)["treated"]/summary(did_1km)$coefficients["treated", "Std. Error"]) < 1.96,
           "NOT significant", "significant"), "\n")

cat("\n3. EVENT STUDY:\n")
cat("   - Pre-trends (1988): by construction = 0 (base year)\n")
cat("   - Post-treatment effects vary by year\n")
cat("   - Check event_study.png for visual pattern\n")

cat("\n4. OVERALL PATTERN:\n")
if (n_sig <= n_total / 3) {
  cat("   - MOST specifications show NO significant effect\n")
  cat("   - This suggests the effect may not be robust\n")
} else if (n_sig >= 2 * n_total / 3) {
  cat("   - MOST specifications show significant effect\n")
  cat("   - This suggests a robust finding\n")
} else {
  cat("   - Results are MIXED across specifications\n")
  cat("   - Interpretation depends on which assumptions you find most credible\n")
}

cat("\n")

# Save results
save(summary_table, es_coefs, file = file.path(output_dir, "alternative_results.RData"))
cat("Results saved to:", output_dir, "\n")
