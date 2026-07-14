# ==============================================================================
# Raw Data Provenance Review
# ==============================================================================
# Documents what the repository can prove about raw input provenance, and flags
# historical commune-code / canton-bridge assumptions that need source documents.
# ==============================================================================

required_packages <- c("dplyr", "readr", "readxl", "tibble", "tools")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(tibble)
})

read_raw_file_inventory <- function(path) {
  inventory <- read_excel(path, col_names = FALSE)
  header_row <- which(inventory[[1]] == "File")[1]
  if (is.na(header_row)) {
    stop("Could not find File header in ", path, call. = FALSE)
  }
  names(inventory) <- as.character(unlist(inventory[header_row, ]))
  inventory %>%
    slice((header_row + 1):n()) %>%
    filter(!is.na(.data$File)) %>%
    transmute(
      file = .data$File,
      purpose = .data$Purpose,
      used_in = .data$`Used In`
    )
}

file_metadata <- function(raw_dir, file) {
  path <- file.path(raw_dir, file)
  if (!file.exists(path)) {
    return(tibble(
      file = file,
      exists = FALSE,
      size_bytes = NA_real_,
      modified_time = as.POSIXct(NA),
      extension = tools::file_ext(file)
    ))
  }
  info <- file.info(path)
  tibble(
    file = file,
    exists = TRUE,
    size_bytes = as.numeric(info$size),
    modified_time = info$mtime,
    extension = tools::file_ext(file)
  )
}

find_repo_mentions <- function(project_root, file) {
  command <- paste(
    "rg", "-n", shQuote(file),
    shQuote(file.path(project_root, "CODE")),
    shQuote(file.path(project_root, "Latex")),
    shQuote(file.path(project_root, "DATA", "raw_data_files_used.xlsx")),
    "2>/dev/null"
  )
  mentions <- tryCatch(system(command, intern = TRUE), warning = function(w) character())
  paste(utils::head(mentions, 20), collapse = " || ")
}

infer_source_status <- function(file, mentions) {
  if (file == "france1999.dbf") {
    return("missing_source_doc_for_canton_bridge")
  }
  if (grepl("IGN|INSEE|JOAFE|Minist|data.gouv|GEOFLA|Admin Express|OpenData|open data", mentions, ignore.case = TRUE)) {
    return("repo_mentions_possible_source")
  }
  "missing_download_source"
}

write_code_stability_review <- function(output_dir, provenance) {
  canton_status <- provenance %>%
    filter(file == "france1999.dbf") %>%
    pull(source_status)

  bridge_path <- file.path(output_dir, "multi_canton_bridge.csv")
  education_path <- file.path(output_dir, "education_duplicate_resolution.csv")
  bridge <- if (file.exists(bridge_path)) read_csv(bridge_path, show_col_types = FALSE) else tibble()
  education <- if (file.exists(education_path)) read_csv(education_path, show_col_types = FALSE) else tibble()

  split_canton_count <- if ("is_multi_canton" %in% names(bridge)) sum(bridge$is_multi_canton) else NA_integer_
  missing_canton_count <- if ("has_missing_canton" %in% names(bridge)) sum(bridge$has_missing_canton) else NA_integer_
  education_duplicate_rows <- if (nrow(education) > 0) nrow(education) else NA_integer_
  education_selected_rows <- if (".audit_selected" %in% names(education)) sum(education$.audit_selected) else NA_integer_

  lines <- c(
    "# Commune-Code And Canton-Bridge Stability Review",
    "",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## What Is Verified In This Repository",
    "",
    "- `DATA/raw_data_files_used.xlsx` lists raw file names, purposes, and consuming scripts, but it does not provide download URLs or source vintages for most files.",
    "- The merge audit normalizes commune codes consistently before joins and records missing/unmatched keys.",
    paste0("- `france1999.dbf` contains multiple canton rows for ", split_canton_count, " commune codes in this checkout. The pipeline therefore no longer assigns those communes to the first sorted canton; it uses commune-specific split cluster IDs and saves the canton list for audit."),
    paste0("- `france1999.dbf` has missing canton codes for ", missing_canton_count, " commune codes. The pipeline no longer collapses these into artificial department-level `NA` clusters; it uses commune-specific `missing_canton_<codecommune>` cluster IDs."),
    paste0("- `educProcessed.xlsx` has duplicate rows for ", education_selected_rows, " commune codes (", education_duplicate_rows, " raw duplicate rows audited). The pipeline now chooses the row with the greatest non-zero education coverage, then breaks ties by matching the `ZRR.csv` commune name."),
    "",
    "## What Still Needs External Documentation",
    "",
    "- Locate the original download page or archival source for `france1999.dbf` and identify the official geography/year it represents.",
    "- Confirm whether multiple rows per commune in `france1999.dbf` are official canton fractions, pseudo-cantons such as `Canton non precise`, or GIS artifacts.",
    "- Check whether commune codes changed between the election/control years and the source geography vintages. This is especially important for historical controls, 1999 canton geography, and 2022 shapefile comparisons.",
    "- Identify whether raw electoral and socio-economic files already harmonize communes to a common vintage or require an explicit commune-history crosswalk.",
    "",
    "## Current Decision",
    "",
    paste0("- Canton bridge source status: `", canton_status, "`. Until external documentation is found, split communes are handled conservatively through split-cluster IDs and sensitivity checks rather than arbitrary canton assignment."),
    "- Missing-canton communes are treated as commune-specific clusters until the `france1999.dbf` documentation explains whether missing `CT` represents a non-precise canton, an official category, or a source artifact.",
    "- Education duplicates are resolved by coverage and ZRR-name matching, but the duplicated names/codes still indicate that a commune-history crosswalk should be added if the raw source vintage can be recovered.",
    "- Manuscript wording should describe the canton cluster as an analysis clustering convention, not as proof of a unique historical canton for split communes.",
    "",
    "## Outputs To Check",
    "",
    "- `OUTPUT/data_quality/raw_data_provenance.csv`",
    "- `OUTPUT/data_quality/multi_canton_bridge.csv`",
    "- `OUTPUT/data_quality/canton_bridge_issues.csv`",
    "- `OUTPUT/data_quality/education_duplicate_resolution.csv`",
    "- `OUTPUT/data_quality/commune_merge_sensitivity_summary.md`",
    "- `OUTPUT/data_quality/raw_source_profile.csv`"
  )

  writeLines(lines, file.path(output_dir, "commune_code_stability_review.md"))
}

run_raw_data_provenance_review <- function(project_root = getwd()) {
  raw_dir <- file.path(project_root, "DATA", "raw data")
  output_dir <- file.path(project_root, "OUTPUT", "data_quality")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  inventory <- read_raw_file_inventory(file.path(project_root, "DATA", "raw_data_files_used.xlsx"))
  metadata <- bind_rows(lapply(inventory$file, function(file) file_metadata(raw_dir, file)))

  provenance <- inventory %>%
    left_join(metadata, by = "file") %>%
    rowwise() %>%
    mutate(
      repo_mentions = find_repo_mentions(project_root, file),
      source_status = infer_source_status(file, repo_mentions)
    ) %>%
    ungroup()

  write_csv(provenance, file.path(output_dir, "raw_data_provenance.csv"))
  write_code_stability_review(output_dir, provenance)

  cat("Wrote raw-data provenance review to", output_dir, "\n")
  invisible(provenance)
}

if (sys.nframe() == 0) {
  run_raw_data_provenance_review()
}
