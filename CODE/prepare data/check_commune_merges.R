# ==============================================================================
# Commune Merge Integrity Checks
# ==============================================================================
# Run after CODE/prepare data/prepare_data.R to verify that commune-level and
# commune-year-level datasets have the expected key cardinality.
# ==============================================================================

check_unique_key <- function(df, keys, dataset_name) {
  missing_keys <- setdiff(keys, names(df))
  if (length(missing_keys) > 0) {
    stop(
      dataset_name, " is missing key column(s): ",
      paste(missing_keys, collapse = ", "),
      call. = FALSE
    )
  }

  duplicate_groups <- df %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n") %>%
    dplyr::filter(n > 1)

  if (nrow(duplicate_groups) > 0) {
    examples <- utils::capture.output(utils::head(duplicate_groups, 10))
    stop(
      dataset_name, " has ", nrow(duplicate_groups),
      " duplicate key group(s) for [", paste(keys, collapse = ", "), "].\n",
      paste(examples, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

run_commune_merge_checks <- function(processed_data_path = file.path("DATA", "processed data")) {
  required_packages <- c("dplyr")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
  }
  library(dplyr)

  load(file.path(processed_data_path, "main.RData"))
  check_unique_key(dfZRR_raw, c("codecommune", "year"), "main.RData::dfZRR_raw")
  check_unique_key(df_merged, c("codecommune", "year"), "main.RData::df_merged")

  load(file.path(processed_data_path, "dataDes.RData"))
  check_unique_key(dfZRR, "codecommune", "dataDes.RData::dfZRR")
  check_unique_key(dfZRRlong, c("codecommune", "year"), "dataDes.RData::dfZRRlong")
  check_unique_key(dfZRRControls, c("codecommune", "year"), "dataDes.RData::dfZRRControls")

  load(file.path(processed_data_path, "script_sharp.RData"))
  check_unique_key(dfDistance, "codecommune", "script_sharp.RData::dfDistance")
  check_unique_key(dfZRRControls, "codecommune", "script_sharp.RData::dfZRRControls")

  load(file.path(processed_data_path, "script_sharp_noEpicenter.RData"))
  check_unique_key(dfDistance, "codecommune", "script_sharp_noEpicenter.RData::dfDistance")
  check_unique_key(dfZRRControls, "codecommune", "script_sharp_noEpicenter.RData::dfZRRControls")

  load(file.path(processed_data_path, "FN_growth.RData"))
  check_unique_key(dfZRRControls, c("codecommune", "year"), "FN_growth.RData::dfZRRControls")

  load(file.path(processed_data_path, "eco_outcomes.RData"))
  check_unique_key(df_eco, c("codecommune", "year"), "eco_outcomes.RData::df_eco")

  cat("All commune merge integrity checks passed.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0) {
  run_commune_merge_checks()
}
