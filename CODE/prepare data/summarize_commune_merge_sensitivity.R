# ==============================================================================
# Commune Merge Sensitivity Summary
# ==============================================================================
# Summarizes how commune/canton bridge decisions affect analysis samples.
# Outputs are intentionally compact and auditable; they are not replacements for
# the paper's publication tables.
# ==============================================================================

required_packages <- c("dplyr", "readr", "tibble", "fixest")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(fixest)
})

normalise_flags <- function(df) {
  if (!"is_multi_canton" %in% names(df)) {
    df$is_multi_canton <- FALSE
  }
  if (!"has_missing_canton" %in% names(df)) {
    df$has_missing_canton <- FALSE
  }
  df %>%
    mutate(
      is_multi_canton = coalesce(as.logical(.data$is_multi_canton), FALSE),
      has_missing_canton = coalesce(as.logical(.data$has_missing_canton), FALSE),
      nonstandard_canton_bridge = .data$is_multi_canton | .data$has_missing_canton
    )
}

sample_summary <- function(df, dataset_name, commune_col = "codecommune") {
  df <- normalise_flags(df)
  tibble(
    dataset = dataset_name,
    rows = nrow(df),
    communes = n_distinct(df[[commune_col]]),
    split_canton_rows = sum(df$is_multi_canton),
    missing_canton_rows = sum(df$has_missing_canton),
    nonstandard_canton_rows = sum(df$nonstandard_canton_bridge),
    split_canton_communes = n_distinct(df[[commune_col]][df$is_multi_canton]),
    missing_canton_communes = n_distinct(df[[commune_col]][df$has_missing_canton]),
    nonstandard_canton_communes = n_distinct(df[[commune_col]][df$nonstandard_canton_bridge])
  )
}

rdd_sensitivity <- function(df, dataset_name) {
  outcome_var <- if ("y" %in% names(df)) "y" else if ("FN2002" %in% names(df)) "FN2002" else NA_character_
  if (is.na(outcome_var)) {
    return(tibble(
      dataset = dataset_name,
      sample = c("all_communes", "exclude_split_or_missing_canton"),
      observations = NA_integer_,
      estimate_z = NA_real_,
      se_z = NA_real_,
      p_value_z = NA_real_
    ))
  }

  df <- normalise_flags(df) %>%
    mutate(.audit_outcome = .data[[outcome_var]]) %>%
    filter(!is.na(.data$.audit_outcome), !is.na(.data$z), !is.na(.data$x), !is.na(.data$canton))

  run_model <- function(model_df, sample_name) {
    if (nrow(model_df) == 0 || n_distinct(model_df$z) < 2) {
      return(tibble(
        dataset = dataset_name,
        sample = sample_name,
        observations = nrow(model_df),
        estimate_z = NA_real_,
        se_z = NA_real_,
        p_value_z = NA_real_
      ))
    }

    model <- feols(.audit_outcome ~ z + x + I(z * x), data = model_df, cluster = ~canton)
    ct <- coeftable(model)
    z_row <- if ("z" %in% rownames(ct)) "z" else grep("^z", rownames(ct), value = TRUE)[1]
    if (is.na(z_row)) {
      return(tibble(
        dataset = dataset_name,
        sample = sample_name,
        observations = stats::nobs(model),
        estimate_z = NA_real_,
        se_z = NA_real_,
        p_value_z = NA_real_
      ))
    }
    tibble(
      dataset = dataset_name,
      sample = sample_name,
      observations = stats::nobs(model),
      estimate_z = unname(ct[z_row, "Estimate"]),
      se_z = unname(ct[z_row, "Std. Error"]),
      p_value_z = unname(ct[z_row, "Pr(>|t|)"])
    )
  }

  bind_rows(
    run_model(df, "all_communes"),
    run_model(filter(df, !.data$nonstandard_canton_bridge), "exclude_split_or_missing_canton")
  )
}

write_markdown_report <- function(output_dir, sample_rows, model_rows, bridge_rows, education_rows) {
  lines <- c(
    "# Commune Merge Sensitivity Summary",
    "",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Canton Bridge",
    "",
    paste0("- Canton bridge rows: ", nrow(bridge_rows)),
    paste0("- Split-canton communes: ", sum(bridge_rows$is_multi_canton)),
    paste0("- Missing-canton communes: ", sum(bridge_rows$has_missing_canton)),
    paste0("- Nonstandard canton bridge communes: ", sum(bridge_rows$is_multi_canton | bridge_rows$has_missing_canton)),
    "",
    "## Education Duplicate Resolution",
    "",
    paste0("- Duplicate education rows audited: ", nrow(education_rows)),
    paste0("- Rows selected: ", sum(education_rows$.audit_selected)),
    "- Rule: prefer the row with greatest non-zero education coverage; if tied, prefer the commune name matching `ZRR.csv`; fail on remaining conflicting ties.",
    "",
    "## Analysis Samples",
    "",
    paste(capture.output(print(sample_rows)), collapse = "\n"),
    "",
    "## RDD Sensitivity Check",
    "",
    paste(capture.output(print(model_rows)), collapse = "\n"),
    "",
    "## CSV Outputs",
    "",
    "- `OUTPUT/data_quality/commune_merge_sensitivity_samples.csv`",
    "- `OUTPUT/data_quality/commune_merge_sensitivity_models.csv`",
    "- `OUTPUT/data_quality/canton_bridge_issues.csv`",
    "- `OUTPUT/data_quality/education_duplicate_resolution.csv`"
  )
  writeLines(lines, file.path(output_dir, "commune_merge_sensitivity_summary.md"))
}

run_commune_merge_sensitivity_summary <- function(project_root = getwd()) {
  processed_dir <- file.path(project_root, "DATA", "processed data")
  output_dir <- file.path(project_root, "OUTPUT", "data_quality")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  bridge <- read_csv(file.path(output_dir, "multi_canton_bridge.csv"), show_col_types = FALSE)
  education <- read_csv(file.path(output_dir, "education_duplicate_resolution.csv"), show_col_types = FALSE)

  env_sharp <- new.env()
  load(file.path(processed_dir, "script_sharp.RData"), envir = env_sharp)
  sharp <- env_sharp$dfZRRControls

  env_no_epicenter <- new.env()
  load(file.path(processed_dir, "script_sharp_noEpicenter.RData"), envir = env_no_epicenter)
  sharp_no_epicenter <- env_no_epicenter$dfZRRControls

  env_eco <- new.env()
  load(file.path(processed_dir, "eco_outcomes.RData"), envir = env_eco)
  eco <- env_eco$df_eco

  sample_rows <- bind_rows(
    sample_summary(sharp, "script_sharp"),
    sample_summary(sharp_no_epicenter, "script_sharp_noEpicenter"),
    sample_summary(eco, "eco_outcomes")
  )

  model_rows <- bind_rows(
    rdd_sensitivity(sharp, "script_sharp"),
    rdd_sensitivity(sharp_no_epicenter, "script_sharp_noEpicenter")
  )

  write_csv(sample_rows, file.path(output_dir, "commune_merge_sensitivity_samples.csv"))
  write_csv(model_rows, file.path(output_dir, "commune_merge_sensitivity_models.csv"))
  write_markdown_report(output_dir, sample_rows, model_rows, bridge, education)

  cat("Wrote commune merge sensitivity summary to", output_dir, "\n")
  invisible(list(samples = sample_rows, models = model_rows))
}

if (sys.nframe() == 0) {
  run_commune_merge_sensitivity_summary()
}
