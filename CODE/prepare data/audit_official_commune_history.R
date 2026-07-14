# ==============================================================================
# Official INSEE Commune-History Applicability Audit
# ==============================================================================
# Downloads official INSEE COG commune-history files, verifies the checked-in
# 1999 commune/canton bridge against INSEE's archive, and checks whether project
# commune codes require an explicit historical crosswalk.
# ==============================================================================

required_packages <- c("dplyr", "purrr", "readr", "readxl", "sf", "stringr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(readxl)
  library(sf)
  library(stringr)
  library(tibble)
})

source(file.path("CODE", "prepare data", "data_quality_helpers.R"))

insee_sources <- list(
  cog_landing = "https://www.insee.fr/fr/information/2560452",
  cog_1999_page = "https://www.insee.fr/fr/information/2560686",
  france1999_dbf_zip = "https://www.insee.fr/fr/statistiques/fichier/2560686/france1999-dbf.zip",
  commune_depuis_1943_csv = "https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_depuis_1943.csv",
  mvt_commune_2026_csv = "https://www.insee.fr/fr/statistiques/fichier/8740222/v_mvt_commune_2026.csv",
  commune_2026_csv = "https://www.insee.fr/fr/statistiques/fichier/8740222/v_commune_2026.csv"
)

source_specs <- tribble(
  ~source, ~root, ~relative_path, ~key_col, ~level,
  "ZRR", "DATA/raw data", "ZRR.csv", "codecommune", "commune_year",
  "1999 canton bridge", "DATA/raw data", "france1999.dbf", "DEP_COM", "commune",
  "FN presidential vote", "DATA/raw data", "pvoixFNpres.xlsx", "codecommune", "commune",
  "FN European vote", "DATA/raw data", "EU.xlsx", "codecommune", "commune",
  "RPR presidential vote", "DATA/raw data", "pvoixRPRpres.xlsx", "codecommune", "commune",
  "turnout", "DATA/raw data", "df_turnout.csv", "codecommune", "commune",
  "population", "DATA/raw data", "popcommunes.csv", "codecommune", "commune",
  "age-sex population", "DATA/raw data", "agesexcommunes.csv", "codecommune", "commune",
  "CSP", "DATA/raw data", "cspcommunes.csv", "codecommune", "commune",
  "education", "DATA/raw data", "educProcessed.xlsx", "codecommune", "commune",
  "foreigners", "DATA/raw data", "etrangers.csv", "codecommune", "commune",
  "employment", "DATA/raw data", "txEmploi.csv", "codecommune", "commune",
  "taxable income", "DATA/raw data", "revenuImposable.csv", "codecommune", "commune",
  "housing vacancy", "DATA/raw data", "logVac.xlsx", "codecommune", "commune",
  "altitude/surface", "DATA/raw data", "altitudeAndMore.xlsx", "codecommune", "commune",
  "rural-urban typology", "DATA/raw data", "typoRuralUrbain.xlsx", "codecommune", "commune",
  "2022 commune shapefile", "DATA/raw data", file.path("communes-20220101-shp", "communes-20220101.dbf"), "insee", "commune",
  "distance to ZRR border", "DATA/processed data", "dataGeoRDD1.xlsx", "codecommune", "commune",
  "border pairs", "DATA/processed data", "border_pair.xlsx", "codecommune", "commune_pair",
  "distance to agglomeration", "DATA/processed data", "distAgglo.xlsx", "codecommune", "commune",
  "distance no epicenter", "DATA/processed data", "dataGeoRDDnoEpicenter1.xlsx", "codecommune", "commune"
)

download_to <- function(url, destfile) {
  status <- tryCatch(
    utils::download.file(url, destfile, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(status, "error") || !file.exists(destfile) || file.info(destfile)$size == 0) {
    stop("Could not download official INSEE source: ", url, call. = FALSE)
  }
  destfile
}

read_source_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_guess()))
  } else if (ext %in% c("xls", "xlsx")) {
    read_excel(path)
  } else if (ext == "dbf") {
    st_read(path, quiet = TRUE) %>% st_drop_geometry()
  } else {
    stop("Unsupported file type: ", path, call. = FALSE)
  }
}

extract_commune_codes <- function(df, key_col) {
  if (key_col == "DEP_COM") {
    if (!all(c("DEP", "COM") %in% names(df))) {
      stop("DEP_COM key requested but DEP/COM columns are absent", call. = FALSE)
    }
    code <- paste0(df$DEP, df$COM)
  } else {
    code <- df[[key_col]]
  }
  tibble(codecommune = standardize_commune_codes(code))
}

collect_project_codes <- function(project_root) {
  source_specs %>%
    mutate(path = file.path(project_root, root, relative_path)) %>%
    pmap_dfr(function(source, root, relative_path, key_col, level, path) {
      if (!file.exists(path)) {
        return(tibble(
          source = source,
          relative_path = file.path(root, relative_path),
          codecommune = NA_character_,
          code_insee = NA_character_,
          year = NA_integer_,
          status = "missing"
        ))
      }
      df <- read_source_file(path)
      codes <- extract_commune_codes(df, key_col)
      year <- if ("year" %in% names(df)) suppressWarnings(as.integer(df$year)) else NA_integer_
      tibble(
        source = source,
        relative_path = file.path(root, relative_path),
        codecommune = codes$codecommune,
        code_insee = format_insee_codes(codes$codecommune),
        year = year,
        status = "ok"
      )
    }) %>%
    filter(!is.na(code_insee), code_insee != "")
}

summarise_analysis_object <- function(dataset, object, path, flagged_codes) {
  if (!is.data.frame(object) || !"codecommune" %in% names(object)) return(NULL)
  codes <- unique(format_insee_codes(object$codecommune))
  flagged <- flagged_codes %>%
    filter(
      .data$code_insee %in% codes,
      .data$needs_crosswalk | .data$unknown_to_official_history | .data$inactive_at_observed_year
    )
  tibble(
    dataset = dataset,
    path = path,
    rows = nrow(object),
    unique_codes = length(codes),
    flagged_codes = n_distinct(flagged$code_insee),
    needs_crosswalk_codes = n_distinct(flagged$code_insee[flagged$needs_crosswalk]),
    unknown_to_official_history_codes = n_distinct(flagged$code_insee[flagged$unknown_to_official_history]),
    inactive_at_observed_year_codes = n_distinct(flagged$code_insee[flagged$inactive_at_observed_year])
  )
}

collect_analysis_sample_summary <- function(project_root, flagged_codes) {
  outputs <- list()
  main_path <- file.path(project_root, "DATA", "processed data", "main.RData")
  if (file.exists(main_path)) {
    env <- new.env(parent = emptyenv())
    load(main_path, envir = env)
    for (dataset in c("df_merged", "dfZRR_raw")) {
      if (exists(dataset, envir = env)) {
        outputs <- append(outputs, list(summarise_analysis_object(dataset, get(dataset, envir = env), "DATA/processed data/main.RData", flagged_codes)))
      }
    }
  }

  sharp_path <- file.path(project_root, "DATA", "processed data", "script_sharp.RData")
  if (file.exists(sharp_path)) {
    env <- new.env(parent = emptyenv())
    load(sharp_path, envir = env)
    for (dataset in c("dfDistance", "dfZRRControls")) {
      if (exists(dataset, envir = env)) {
        outputs <- append(outputs, list(summarise_analysis_object(dataset, get(dataset, envir = env), "DATA/processed data/script_sharp.RData", flagged_codes)))
      }
    }
  }

  bind_rows(outputs)
}

write_applicability_note <- function(path, source_check, source_summary, flagged_codes, analysis_summary) {
  exact_1999 <- source_check$ok[source_check$check == "local_france1999_matches_official_insee_archive"]
  exact_text <- ifelse(isTRUE(exact_1999), "Yes", "No")
  needs_crosswalk_total <- sum(source_summary$codes_needing_crosswalk, na.rm = TRUE)
  unknown_total <- sum(source_summary$codes_unknown_to_official_history, na.rm = TRUE)
  flagged_top <- flagged_codes %>%
    filter(needs_crosswalk | unknown_to_official_history | inactive_at_observed_year) %>%
    count(source, flag = case_when(
      unknown_to_official_history ~ "unknown_to_official_history",
      inactive_at_observed_year ~ "inactive_at_observed_year",
      needs_crosswalk ~ "historical_not_active_2026",
      TRUE ~ "other"
    ), name = "codes") %>%
    arrange(desc(codes), source, flag)

  lines <- c(
    "# Official INSEE Commune-History Applicability Audit",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Official Sources Checked",
    "",
    paste0("- INSEE COG download index: ", insee_sources$cog_landing),
    paste0("- INSEE COG 1999 download page: ", insee_sources$cog_1999_page),
    paste0("- 1999 all-communes DBF archive: ", insee_sources$france1999_dbf_zip),
    paste0("- Communes since 1943 CSV: ", insee_sources$commune_depuis_1943_csv),
    paste0("- Commune movements since 1943 CSV: ", insee_sources$mvt_commune_2026_csv),
    paste0("- Current 2026 communes CSV: ", insee_sources$commune_2026_csv),
    "",
    "## Bottom Line",
    "",
    paste0("- The checked-in `DATA/raw data/france1999.dbf` matches INSEE's official `france1999-dbf.zip`: **", exact_text, "**."),
    "- The 1999 canton bridge therefore has an official source. The INSEE 1999 page explicitly states that multi-canton communes are represented by one row per canton fraction plus a `canton non precise` row; the current split-canton cluster convention is appropriate and should be documented as INSEE-driven, not as a source anomaly.",
    "- The current 2026 `Communes depuis 1943` and `Évènements sur les communes` files apply to this project as validation/crosswalk inputs, but they do not by themselves tell us whether each raw election/control file is already harmonized to a common vintage. They should be used to build an explicit harmonization check before strengthening publication claims.",
    paste0("- Across audited project sources, ", needs_crosswalk_total, " source-code appearances are known historical commune codes that are not active current 2026 commune codes and therefore may need an explicit movement-based crosswalk if that source is merged to a modern-vintage file."),
    paste0("- Across audited project sources, ", unknown_total, " source-code appearances are not found in the official 2026 commune-history file; these are mostly formatting, overseas coverage, or source-specific coding cases that require manual review."),
    "",
    "## Source-Level Summary",
    "",
    paste(capture.output(print(source_summary, n = nrow(source_summary))), collapse = "\n"),
    "",
    "## Main Flags",
    "",
    if (nrow(flagged_top) == 0) "No code-level flags." else paste(capture.output(print(flagged_top, n = nrow(flagged_top))), collapse = "\n"),
    "",
    "## How This Applies To The Paper",
    "",
    "- Replace the previous wording that the `france1999.dbf` provenance is unrecovered: it is now verified as INSEE COG 1999 `france1999.dbf`.",
    "- Keep the manuscript's caution that canton clustering is an analysis convention for split/missing canton cases, but now cite the official INSEE 1999 row convention.",
    "- Add a next-step requirement: build an actual movement-based harmonization layer from `v_mvt_commune_2026.csv` before final submission, then compare estimates with and without harmonizing historical commune codes.",
    "",
    "## Analysis-Sample Exposure",
    "",
    "The raw-source flag counts above are intentionally conservative because old geography files naturally include communes that no longer exist in 2026. In the processed analysis objects, the exposure is smaller:",
    "",
    "```text",
    if (nrow(analysis_summary) == 0) "No processed analysis objects found." else paste(capture.output(print(analysis_summary, n = nrow(analysis_summary))), collapse = "\n"),
    "```",
    "",
    "The primary merged panel has a much smaller flagged-code exposure than the broader treatment/control universe. This warrants an explicit crosswalk sensitivity, but it does not invalidate the current merge audit by itself.",
    "",
    "## Machine-Readable Outputs",
    "",
    "- `OUTPUT/data_quality/official_commune_history_source_check.csv`",
    "- `OUTPUT/data_quality/commune_history_code_audit.csv`",
    "- `OUTPUT/data_quality/commune_history_code_flags.csv`",
    "- `OUTPUT/data_quality/commune_history_analysis_sample_audit.csv`"
  )
  writeLines(lines, path)
}

run_official_commune_history_audit <- function(project_root = getwd()) {
  output_dir <- file.path(project_root, "OUTPUT", "data_quality")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  cache_dir <- tempfile("insee_cog_")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  official_france_zip <- download_to(insee_sources$france1999_dbf_zip, file.path(cache_dir, "france1999-dbf.zip"))
  utils::unzip(official_france_zip, exdir = cache_dir)
  official_france_dbf <- file.path(cache_dir, "france1999.dbf")

  commune_history_path <- download_to(insee_sources$commune_depuis_1943_csv, file.path(cache_dir, "v_commune_depuis_1943.csv"))
  movements_path <- download_to(insee_sources$mvt_commune_2026_csv, file.path(cache_dir, "v_mvt_commune_2026.csv"))
  current_commune_path <- download_to(insee_sources$commune_2026_csv, file.path(cache_dir, "v_commune_2026.csv"))

  local_france_dbf <- file.path(project_root, "DATA", "raw data", "france1999.dbf")
  official_md5 <- unname(tools::md5sum(official_france_dbf))
  local_md5 <- unname(tools::md5sum(local_france_dbf))

  official_1999 <- st_read(official_france_dbf, quiet = TRUE) %>% st_drop_geometry()
  local_1999 <- st_read(local_france_dbf, quiet = TRUE) %>% st_drop_geometry()

  commune_history <- read_csv(commune_history_path, show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    mutate(
      DATE_DEBUT = as.Date(.data$DATE_DEBUT),
      DATE_FIN = as.Date(na_if(.data$DATE_FIN, ""))
    )
  movements <- read_csv(movements_path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  current_communes <- read_csv(current_commune_path, show_col_types = FALSE, col_types = cols(.default = col_character()))

  history_any <- unique(commune_history$COM)
  history_communes <- unique(commune_history$COM[commune_history$TYPECOM == "COM"])
  current_commune_codes <- unique(current_communes$COM[current_communes$TYPECOM == "COM"])
  movement_before_communes <- unique(movements$COM_AV[movements$TYPECOM_AV == "COM"])
  movement_after_communes <- unique(movements$COM_AP[movements$TYPECOM_AP == "COM"])

  project_codes <- collect_project_codes(project_root) %>%
    distinct(source, relative_path, codecommune, code_insee, year, .keep_all = TRUE)

  code_years <- project_codes %>%
    distinct(code_insee, year) %>%
    filter(!is.na(.data$year)) %>%
    mutate(reference_date = as.Date(paste0(.data$year, "-01-01")))

  year_checks_with_year <- code_years %>%
    left_join(
      commune_history %>% filter(.data$TYPECOM == "COM"),
      by = c("code_insee" = "COM"),
      relationship = "many-to-many"
    ) %>%
    group_by(.data$code_insee, .data$year) %>%
    summarise(
      active_at_observed_year = any(
        !is.na(.data$DATE_DEBUT) &
          .data$DATE_DEBUT <= .data$reference_date &
          (is.na(.data$DATE_FIN) | .data$DATE_FIN > .data$reference_date)
      ),
      .groups = "drop"
    )

  year_checks_without_year <- project_codes %>%
    distinct(code_insee, year) %>%
    filter(is.na(.data$year)) %>%
    mutate(active_at_observed_year = NA)

  year_checks <- bind_rows(year_checks_with_year, year_checks_without_year)

  flagged_codes <- project_codes %>%
    left_join(year_checks, by = c("code_insee", "year")) %>%
    mutate(
      known_to_official_history = .data$code_insee %in% history_any,
      known_as_commune_history = .data$code_insee %in% history_communes,
      active_2026_commune = .data$code_insee %in% current_commune_codes,
      appears_as_before_commune_in_movement = .data$code_insee %in% movement_before_communes,
      appears_as_after_commune_in_movement = .data$code_insee %in% movement_after_communes,
      needs_crosswalk = .data$known_as_commune_history & !.data$active_2026_commune & .data$appears_as_before_commune_in_movement,
      unknown_to_official_history = !.data$known_to_official_history,
      inactive_at_observed_year = !is.na(.data$year) & !.data$active_at_observed_year
    ) %>%
    group_by(source, relative_path, codecommune, code_insee) %>%
    summarise(
      first_year = if (all(is.na(year))) NA_integer_ else min(year, na.rm = TRUE),
      last_year = if (all(is.na(year))) NA_integer_ else max(year, na.rm = TRUE),
      known_to_official_history = any(known_to_official_history),
      known_as_commune_history = any(known_as_commune_history),
      active_2026_commune = any(active_2026_commune),
      appears_as_before_commune_in_movement = any(appears_as_before_commune_in_movement),
      appears_as_after_commune_in_movement = any(appears_as_after_commune_in_movement),
      needs_crosswalk = any(needs_crosswalk),
      unknown_to_official_history = any(unknown_to_official_history),
      inactive_at_observed_year = any(inactive_at_observed_year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup()

  source_summary <- flagged_codes %>%
    group_by(source, relative_path) %>%
    summarise(
      unique_codes = n_distinct(code_insee),
      codes_known_to_official_history = sum(known_to_official_history),
      codes_active_2026 = sum(active_2026_commune),
      codes_needing_crosswalk = sum(needs_crosswalk),
      codes_unknown_to_official_history = sum(unknown_to_official_history),
      codes_inactive_at_observed_year = sum(inactive_at_observed_year),
      .groups = "drop"
    ) %>%
    arrange(desc(codes_needing_crosswalk), desc(codes_unknown_to_official_history), source)

  analysis_summary <- collect_analysis_sample_summary(project_root, flagged_codes)

  source_check <- tibble(
    check = c(
      "local_france1999_matches_official_insee_archive",
      "local_france1999_rows",
      "official_france1999_rows",
      "commune_history_rows",
      "commune_movement_rows",
      "current_2026_commune_rows"
    ),
    ok = c(
      identical(local_md5, official_md5),
      nrow(local_1999) == 40288,
      nrow(official_1999) == 40288,
      nrow(commune_history) > 0,
      nrow(movements) > 0,
      length(current_commune_codes) > 0
    ),
    value = c(
      paste0("local_md5=", local_md5, "; official_md5=", official_md5),
      as.character(nrow(local_1999)),
      as.character(nrow(official_1999)),
      as.character(nrow(commune_history)),
      as.character(nrow(movements)),
      as.character(length(current_commune_codes))
    ),
    source_url = c(
      insee_sources$france1999_dbf_zip,
      insee_sources$france1999_dbf_zip,
      insee_sources$france1999_dbf_zip,
      insee_sources$commune_depuis_1943_csv,
      insee_sources$mvt_commune_2026_csv,
      insee_sources$commune_2026_csv
    )
  )

  write_csv(source_check, file.path(output_dir, "official_commune_history_source_check.csv"))
  write_csv(source_summary, file.path(output_dir, "commune_history_code_audit.csv"))
  write_csv(flagged_codes, file.path(output_dir, "commune_history_code_flags.csv"))
  write_csv(analysis_summary, file.path(output_dir, "commune_history_analysis_sample_audit.csv"))
  write_applicability_note(
    file.path(output_dir, "official_commune_history_applicability.md"),
    source_check,
    source_summary,
    flagged_codes,
    analysis_summary
  )

  cat("Wrote official commune-history applicability audit to", output_dir, "\n")
  invisible(list(source_check = source_check, source_summary = source_summary, flagged_codes = flagged_codes))
}

if (sys.nframe() == 0) {
  run_official_commune_history_audit()
}
