# ==============================================================================
# Raw Commune-Key Audit
# ==============================================================================
# Produces machine-readable diagnostics for raw and processed inputs used in the
# commune-level merge pipeline.
# ==============================================================================

required_packages <- c("dplyr", "purrr", "readr", "readxl", "stringr", "sf", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(readxl)
  library(stringr)
  library(sf)
  library(tibble)
})

standardize_commune_codes_audit <- function(codes) {
  codes <- trimws(as.character(codes))
  codes <- sub("\\.0$", "", codes)
  codes[codes %in% c("", "NA", "NaN")] <- NA_character_
  sub("^0+", "", codes)
}

format_insee_codes_audit <- function(codes) {
  codes <- trimws(as.character(codes))
  codes <- sub("\\.0$", "", codes)
  codes[codes %in% c("", "NA", "NaN")] <- NA_character_
  ifelse(
    grepl("^[0-9]+$", codes) & nchar(codes) < 5,
    str_pad(codes, width = 5, side = "left", pad = "0"),
    codes
  )
}

read_audit_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_guess()))
  } else if (ext %in% c("xls", "xlsx")) {
    read_excel(path)
  } else if (ext == "dbf") {
    st_read(path, quiet = TRUE) %>% st_drop_geometry()
  } else {
    stop("Unsupported audit file type: ", path, call. = FALSE)
  }
}

extract_key_data <- function(df, key_col) {
  if (key_col == "DEP_COM") {
    if (!all(c("DEP", "COM") %in% names(df))) {
      stop("DEP_COM key requested but DEP/COM columns are absent", call. = FALSE)
    }
    df %>% transmute(codecommune = standardize_commune_codes_audit(paste0(DEP, COM)))
  } else if (key_col == "insee") {
    df %>% transmute(codecommune = standardize_commune_codes_audit(.data[[key_col]]))
  } else {
    df %>% transmute(codecommune = standardize_commune_codes_audit(.data[[key_col]]))
  }
}

audit_one_source <- function(root, source_name, relative_path, key_col, level) {
  path <- file.path(root, relative_path)
  if (!file.exists(path)) {
    return(list(
      summary = tibble(
        source = source_name,
        path = relative_path,
        level = level,
        status = "missing",
        rows = NA_integer_,
        unique_communes = NA_integer_,
        missing_commune_codes = NA_integer_,
        duplicate_commune_rows = NA_integer_,
        unique_commune_years = NA_integer_,
        duplicate_commune_year_rows = NA_integer_
      ),
      duplicates = tibble()
    ))
  }

  df <- read_audit_file(path)
  key_data <- extract_key_data(df, key_col)
  if ("year" %in% names(df)) {
    key_data <- key_data %>% mutate(year = suppressWarnings(as.integer(df$year)))
  }
  key_data <- key_data %>% mutate(code_insee = format_insee_codes_audit(codecommune))

  duplicate_communes <- key_data %>%
    filter(!is.na(codecommune)) %>%
    count(codecommune, name = "n") %>%
    filter(n > 1)

  if (all(c("codecommune", "year") %in% names(key_data))) {
    duplicate_commune_years <- key_data %>%
      filter(!is.na(codecommune), !is.na(year)) %>%
      count(codecommune, year, name = "n") %>%
      filter(n > 1)
    unique_commune_years <- nrow(distinct(filter(key_data, !is.na(codecommune), !is.na(year)), codecommune, year))
    duplicate_commune_year_rows <- sum(duplicate_commune_years$n - 1)
  } else {
    duplicate_commune_years <- tibble(codecommune = character(), year = integer(), n = integer())
    unique_commune_years <- NA_integer_
    duplicate_commune_year_rows <- NA_integer_
  }

  duplicate_tables <- list()
  if (level == "commune" && nrow(duplicate_communes) > 0) {
    duplicate_tables <- append(
      duplicate_tables,
      list(
        duplicate_communes %>%
          mutate(source = source_name, duplicate_type = "commune") %>%
          select(source, duplicate_type, codecommune, n)
      )
    )
  }
  if (level == "commune_year" && nrow(duplicate_commune_years) > 0) {
    duplicate_tables <- append(
      duplicate_tables,
      list(
        duplicate_commune_years %>%
          mutate(source = source_name, duplicate_type = "commune_year") %>%
          select(source, duplicate_type, codecommune, year, n)
      )
    )
  }
  duplicates <- bind_rows(duplicate_tables)

  list(
    summary = tibble(
      source = source_name,
      path = relative_path,
      level = level,
      status = "ok",
      rows = nrow(df),
      unique_communes = n_distinct(key_data$codecommune, na.rm = TRUE),
      missing_commune_codes = sum(is.na(key_data$codecommune)),
      duplicate_commune_rows = ifelse(nrow(duplicate_communes) == 0, 0L, sum(duplicate_communes$n - 1)),
      unique_commune_years = unique_commune_years,
      duplicate_commune_year_rows = duplicate_commune_year_rows
    ),
    duplicates = duplicates
  )
}

run_commune_key_audit <- function(project_root = getwd()) {
  raw_dir <- file.path(project_root, "DATA", "raw data")
  processed_dir <- file.path(project_root, "DATA", "processed data")
  output_dir <- file.path(project_root, "OUTPUT", "data_quality")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  sources <- tribble(
    ~source, ~relative_path, ~key_col, ~level,
    "ZRR", "ZRR.csv", "codecommune", "commune_year",
    "1999 canton bridge", "france1999.dbf", "DEP_COM", "commune",
    "FN presidential vote", "pvoixFNpres.xlsx", "codecommune", "commune",
    "FN European vote", "EU.xlsx", "codecommune", "commune",
    "RPR presidential vote", "pvoixRPRpres.xlsx", "codecommune", "commune",
    "turnout", "df_turnout.csv", "codecommune", "commune",
    "population", "popcommunes.csv", "codecommune", "commune",
    "age-sex population", "agesexcommunes.csv", "codecommune", "commune",
    "CSP", "cspcommunes.csv", "codecommune", "commune",
    "education", "educProcessed.xlsx", "codecommune", "commune",
    "foreigners", "etrangers.csv", "codecommune", "commune",
    "employment", "txEmploi.csv", "codecommune", "commune",
    "taxable income", "revenuImposable.csv", "codecommune", "commune",
    "housing vacancy", "logVac.xlsx", "codecommune", "commune",
    "altitude/surface", "altitudeAndMore.xlsx", "codecommune", "commune",
    "rural-urban typology", "typoRuralUrbain.xlsx", "codecommune", "commune",
    "2022 commune shapefile", file.path("communes-20220101-shp", "communes-20220101.dbf"), "insee", "commune"
  )

  processed_sources <- tribble(
    ~source, ~relative_path, ~key_col, ~level,
    "distance to ZRR border", file.path("..", "processed data", "dataGeoRDD1.xlsx"), "codecommune", "commune",
    "border pairs", file.path("..", "processed data", "border_pair.xlsx"), "codecommune", "commune_pair",
    "distance to agglomeration", file.path("..", "processed data", "distAgglo.xlsx"), "codecommune", "commune",
    "distance no epicenter", file.path("..", "processed data", "dataGeoRDDnoEpicenter1.xlsx"), "codecommune", "commune"
  )

  raw_results <- pmap(
    sources,
    ~ audit_one_source(raw_dir, ..1, ..2, ..3, ..4)
  )
  processed_results <- pmap(
    processed_sources,
    ~ audit_one_source(raw_dir, ..1, ..2, ..3, ..4)
  )

  summaries <- bind_rows(map(c(raw_results, processed_results), "summary"))
  duplicates <- bind_rows(map(c(raw_results, processed_results), "duplicates"))

  write_csv(summaries, file.path(output_dir, "raw_commune_key_audit.csv"))
  write_csv(duplicates, file.path(output_dir, "raw_commune_key_duplicates.csv"))

  cat("Wrote commune-key audit to", output_dir, "\n")
  invisible(list(summary = summaries, duplicates = duplicates))
}

if (sys.nframe() == 0) {
  run_commune_key_audit()
}
