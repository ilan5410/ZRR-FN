# Sensitivity of the primary first-difference estimate to commune codes flagged
# by the official INSEE movement history. This is an exclusion sensitivity, not
# a substitute for a one-to-many historical harmonization of merged values.

flagged_commune_codes <- function(flags) {
  required <- c("code_insee", "needs_crosswalk", "unknown_to_official_history", "inactive_at_observed_year")
  missing <- setdiff(required, names(flags))
  if (length(missing) > 0L) stop("Commune-history flags are missing: ", paste(missing, collapse = ", "))

  unique(as.character(flags$code_insee[
    flags$needs_crosswalk | flags$unknown_to_official_history | flags$inactive_at_observed_year
  ]))
}

exclude_flagged_communes <- function(data, codes) {
  if (!"codecommune" %in% names(data)) stop("Analysis data are missing codecommune.")
  data[!format_insee_codes(data$codecommune) %in% codes, , drop = FALSE]
}

run_commune_history_sensitivity <- function(project_root = getwd()) {
  required_packages <- c("dplyr", "readr", "sandwich", "lmtest")
  unavailable <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(unavailable) > 0L) stop("Missing R packages: ", paste(unavailable, collapse = ", "))
  invisible(lapply(required_packages, function(package) suppressPackageStartupMessages(library(package, character.only = TRUE))))

  source(file.path(project_root, "CODE", "prepare data", "data_quality_helpers.R"))
  config <- new.env(parent = globalenv())
  config$main_path <- paste0(normalizePath(project_root), "/")
  sys.source(file.path(project_root, "CODE", "configurations.R"), envir = config)
  processed_data_path <- config$processed_data_path
  controls <- config$controls
  flags_path <- file.path(project_root, "OUTPUT", "data_quality", "commune_history_code_flags.csv")
  sharp_path <- file.path(processed_data_path, "script_sharp.RData")
  if (!file.exists(flags_path) || !file.exists(sharp_path)) {
    stop("Run the commune-history audit and sharp data preparation before this sensitivity.")
  }

  flagged <- flagged_commune_codes(readr::read_csv(flags_path, show_col_types = FALSE))
  env <- new.env(parent = emptyenv())
  load(sharp_path, envir = env)
  data <- env$dfZRRControls

  prepare_sample <- function(sample) {
    controls_did <- setdiff(controls, "FN1988")
    sample %>%
      dplyr::filter(.data$x >= -20000, .data$x <= 20000, !is.na(.data$FN1988), !is.na(.data$FN2002), !is.na(.data$dep)) %>%
      dplyr::mutate(
        treated = as.numeric(.data$z),
        delta_FN = .data$FN2002 - .data$FN1988,
        pop_log = log(.data$pop)
      ) %>%
      dplyr::distinct(.data$codecommune, .keep_all = TRUE) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(controls_did), ~ ifelse(is.infinite(.x), NA, .x))) %>%
      dplyr::select(delta_FN, treated, canton, dep, dplyr::all_of(controls_did)) %>%
      stats::na.omit()
  }

  estimate <- function(sample, label) {
    prepared <- prepare_sample(sample)
    formula <- stats::as.formula(paste("delta_FN ~ treated +", paste(setdiff(controls, "FN1988"), collapse = " + "), "+ factor(dep)"))
    model <- stats::lm(formula, data = prepared)
    robust <- lmtest::coeftest(model, vcov. = sandwich::vcovCL(model, cluster = prepared$canton, type = "HC1"))
    data.frame(
      sample = label,
      observations = stats::nobs(model),
      communes_excluded_by_history_flag = nrow(data) - nrow(sample),
      estimate = unname(stats::coef(model)["treated"]),
      standard_error = robust["treated", "Std. Error"],
      p_value = robust["treated", "Pr(>|t|)"],
      stringsAsFactors = FALSE
    )
  }

  restricted <- exclude_flagged_communes(data, flagged)
  results <- rbind(estimate(data, "Full audited sample"), estimate(restricted, "Exclude official-history flags"))
  output_dir <- file.path(project_root, "OUTPUT", "data_quality")
  readr::write_csv(results, file.path(output_dir, "commune_history_estimate_sensitivity.csv"))
  writeLines(c(
    "# Commune-History Exclusion Sensitivity",
    "",
    "This exercise excludes communes flagged by the official INSEE movement-history audit from the preferred adjusted first-difference specification. It is deliberately conservative: it does not impute a one-to-many historical correspondence or reallocate commune outcomes.",
    "",
    "```text",
    paste(capture.output(print(results, row.names = FALSE)), collapse = "\n"),
    "```",
    "",
    "Interpretation: stability under this exclusion is evidence against a result driven solely by the flagged codes, but is not proof that every historical merge is harmonized."
  ), file.path(output_dir, "commune_history_estimate_sensitivity.md"))
  invisible(results)
}

if (sys.nframe() == 0 && !identical(Sys.getenv("RUN_COMMUNE_HISTORY_SENSITIVITY", "true"), "false")) {
  run_commune_history_sensitivity()
}
