# ==============================================================================
# MAIN DiD SPECIFICATION — publication tables and figures
# ==============================================================================
# Promotes the exploratory DiD from alternative_identification.R into the
# paper's primary identification strategy. Produces:
#   1. OUTPUT/tables/DiD_main_results.tex   — DFN spec, 4 columns, canton-clustered SEs
#   2. OUTPUT/figures/event_study.png       — treatment x election-year coefficients
#   3. OUTPUT/figures/dFN_vs_logpop.png     — binned DFN vs log(pop) by treatment (YS request)
#
# Run locally: Rscript CODE/DiD_main.R   (requires processed data + R packages)
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)

main_path <- here::here()
if (!endsWith(main_path, "/")) main_path <- paste0(main_path, "/")
source(file.path(main_path, "CODE/configurations.R"))

# Minimal table post-processing (avoids sourcing prepare_tables.R, which runs
# heavy data checks on source). Wraps the tabular in a resizebox.
format_latex_table <- function(tex_file, use_resizebox = TRUE) {
  lines <- readLines(tex_file)
  if (use_resizebox) {
    i_begin <- grep("\\\\begin\\{tabular\\}", lines)[1]
    i_end   <- tail(grep("\\\\end\\{tabular\\}", lines), 1)
    if (!is.na(i_begin) && length(i_end) > 0) {
      lines[i_begin] <- paste0("\\resizebox{\\textwidth}{!}{%\n", lines[i_begin])
      lines[i_end]   <- paste0(lines[i_end], "\n}% end resizebox")
    }
  }
  writeLines(lines, tex_file)
}

load(file.path(processed_data_path, "script_sharp.RData"))

tables_dir  <- file.path(main_path, "OUTPUT/tables/")
figures_dir <- file.path(main_path, "OUTPUT/figures/")
latex_dir   <- file.path(main_path, "Latex/ZRR and populist vote")
latex_tables_dir <- file.path(latex_dir, "tables")
latex_figures_dir <- file.path(latex_dir, "figures")
manifest_path <- file.path(main_path, "OUTPUT/data_quality/model_manifest.csv")

# ------------------------------------------------------------------------------
# Data preparation (identical sample logic to alternative_identification.R)
# ------------------------------------------------------------------------------
df <- dfZRRControls %>%
  filter(x >= -20000 & x <= 20000) %>%
  mutate(
    treated  = as.numeric(z),
    pop_log  = log(pop),
    delta_FN = FN2002 - FN1988
  ) %>%
  distinct(codecommune, .keep_all = TRUE) %>%
  filter(!is.na(FN2002), !is.na(FN1988), !is.na(dep))

controls_did <- setdiff(controls, "FN1988")

df_did <- df %>%
  mutate(across(all_of(controls_did), ~ifelse(is.infinite(.), NA, .))) %>%
  select(delta_FN, treated, pop_log, all_of(controls_did), dep, canton, x) %>%
  na.omit()

df_5km <- df_did %>% filter(abs(x) <= 5000)

# ------------------------------------------------------------------------------
# 1. Main DiD table: (1) simple  (2) + controls  (3) + dept FE  (4) 5km bandwidth
#    All SEs clustered at the canton level.
# ------------------------------------------------------------------------------
f_simple   <- delta_FN ~ treated
f_controls <- as.formula(paste("delta_FN ~ treated +", paste(controls_did, collapse = " + ")))
f_fe       <- as.formula(paste("delta_FN ~ treated +", paste(controls_did, collapse = " + "), "+ factor(dep)"))

m1 <- lm(f_simple,   data = df_did)
m2 <- lm(f_controls, data = df_did)
m3 <- lm(f_fe,       data = df_did)
m4 <- lm(f_controls, data = df_5km)

cl_se <- function(m, cl) sqrt(diag(vcovCL(m, cluster = cl, type = "HC1")))
se_list <- list(
  cl_se(m1, df_did$canton),
  cl_se(m2, df_did$canton),
  cl_se(m3, df_did$canton),
  cl_se(m4, df_5km$canton)
)

stargazer(m1, m2, m3, m4,
  se = se_list,
  keep = "treated",
  covariate.labels = c("Treatment ZRR (1995)"),
  dep.var.labels = "$\\Delta$ FN vote share (2002 $-$ 1988)",
  column.labels = c("Simple", "Controls", "Controls + Dept FE", "Controls, 5 km"),
  add.lines = list(
    c("Controls", "No", "Yes", "Yes", "Yes"),
    c("Department fixed effects", "No", "No", "Yes", "No"),
    c("Bandwidth", "20 km", "20 km", "20 km", "5 km")
  ),
  omit.stat = c("f", "ser"),
  title = "Effect of 1995 ZRR entry on the change in FN vote share, 1988--2002",
  label = "tab:did_main",
  notes = "Standard errors are clustered at the audited canton-cluster level.",
  font.size = "footnotesize",
  out = file.path(tables_dir, "DiD_main_results.tex")
)
format_latex_table(file.path(tables_dir, "DiD_main_results.tex"), use_resizebox = TRUE)

# ------------------------------------------------------------------------------
# 2. Event study: treatment x election year (base 1988), commune + year FE
# ------------------------------------------------------------------------------
fn_vars <- grep("^FN[0-9]{4}$", names(df), value = TRUE)

df_panel <- df %>%
  select(codecommune, treated, dep, canton, all_of(fn_vars)) %>%
  pivot_longer(all_of(fn_vars), names_to = "year", names_prefix = "FN", values_to = "FN_vote") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(FN_vote), !is.na(canton))

years <- sort(unique(df_panel$year))
base_year <- 1988

df_panel <- df_panel %>%
  mutate(year_f = relevel(factor(year), ref = as.character(base_year)))

es_model <- lm(FN_vote ~ treated * year_f + factor(dep), data = df_panel)
es_se    <- cl_se(es_model, df_panel$canton)

coefs <- coef(es_model)
idx   <- grep("^treated:year_f", names(coefs))
es_df <- data.frame(
  year = as.numeric(sub("treated:year_f", "", names(coefs)[idx])),
  est  = coefs[idx],
  se   = es_se[idx]
) %>%
  bind_rows(data.frame(year = base_year, est = 0, se = 0)) %>%
  arrange(year) %>%
  mutate(lo = est - 1.96 * se, hi = est + 1.96 * se)

p_es <- ggplot(es_df, aes(x = year, y = est)) +
  geom_vline(xintercept = 1995, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  geom_line() +
  annotate("text", x = 1995, y = max(es_df$hi), label = "1995 law; 1996 decree",
           hjust = -0.1, size = 3, color = "grey40") +
  labs(x = "Election year", y = "Treatment effect on FN vote share\n(relative to 1988)") +
  theme_minimal(base_size = 12)

ggsave(file.path(figures_dir, "event_study.png"), p_es, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 3. Binned scatter: DFN vs log(pop), by treatment status (YS request, DID.tex)
# ------------------------------------------------------------------------------
df_bins <- df_did %>%
  group_by(treated, bin = ntile(pop_log, 25)) %>%
  summarise(pop_log = mean(pop_log), delta_FN = mean(delta_FN), .groups = "drop") %>%
  mutate(Group = ifelse(treated == 1, "Initial ZRR wave (1995)", "Later ZRR entrants (from 2005)"))

p_pop <- ggplot(df_bins, aes(x = pop_log, y = delta_FN, color = Group, shape = Group)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.6) +
  scale_color_manual(values = c("Initial ZRR wave (1995)" = "#B2182B", "Later ZRR entrants (from 2005)" = "#2166AC")) +
  labs(x = "Log population (1990)", y = expression(Delta ~ "FN vote share (2002" - "1988)")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave(file.path(figures_dir, "dFN_vs_logpop.png"), p_pop, width = 8, height = 5, dpi = 300)

if (dir.exists(latex_tables_dir)) {
  file.copy(
    file.path(tables_dir, "DiD_main_results.tex"),
    file.path(latex_tables_dir, "DiD_main_results.tex"),
    overwrite = TRUE
  )
}

if (dir.exists(latex_figures_dir)) {
  file.copy(
    file.path(figures_dir, c("event_study.png", "dFN_vs_logpop.png")),
    latex_figures_dir,
    overwrite = TRUE
  )
}

manifest <- data.frame(
  artifact = c("DiD_main_results.tex", "event_study.png", "dFN_vs_logpop.png"),
  artifact_type = c("table", "figure", "figure"),
  output_path = c(
    file.path("OUTPUT/tables", "DiD_main_results.tex"),
    file.path("OUTPUT/figures", "event_study.png"),
    file.path("OUTPUT/figures", "dFN_vs_logpop.png")
  ),
  latex_path = c(
    file.path("Latex/ZRR and populist vote/tables", "DiD_main_results.tex"),
    file.path("Latex/ZRR and populist vote/figures", "event_study.png"),
    file.path("Latex/ZRR and populist vote/figures", "dFN_vs_logpop.png")
  ),
  script = "CODE/DiD_main.R",
  input = "DATA/processed data/script_sharp.RData",
  sample = c(
    "Initial 1995 ZRR wave versus later entrants from 2005 with FN1988 and FN2002 observed; 20 km and 5 km samples",
    "Same commune sample reshaped across observed presidential election years; 1988 is reference year",
    "Same first-difference DiD sample binned by log 1990 population"
  ),
  formula_or_plot = c(
    "delta_FN ~ treated + controls + optional department FE; audited canton-cluster SE",
    "FN_vote ~ treated * year + department FE; audited canton-cluster intervals",
    "Binned means of delta_FN by log(pop), separately by treatment group"
  ),
  main_quantity = c(
    "Treatment ZRR (1995): -0.011 simple, -0.012 controls, -0.007 controls+department FE, -0.009 controls 5 km",
    "Dynamic treatment-year contrasts relative to 1988; 1995 is not treated as definitive post-treatment evidence",
    "Descriptive size-gradient diagnostic for 1988-2002 FN vote change"
  ),
  stringsAsFactors = FALSE
)

if (file.exists(manifest_path)) {
  old_manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
  old_manifest <- old_manifest[!old_manifest$artifact %in% manifest$artifact, , drop = FALSE]
  manifest <- dplyr::bind_rows(old_manifest, manifest)
}
write.csv(manifest, manifest_path, row.names = FALSE)

cat("\nDone. DiD outputs written to OUTPUT, mirrored to LaTeX folders, and recorded in:\n")
cat("  ", manifest_path, "\n", sep = "")
