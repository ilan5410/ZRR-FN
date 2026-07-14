#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source(file.path("CODE", "prepare data", "data_quality_helpers.R"))

fail <- function(message) {
  stop(message, call. = FALSE)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

expect_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    fail(paste0(
      message,
      "\nExpected: ", paste(capture.output(str(expected)), collapse = " "),
      "\nActual: ", paste(capture.output(str(actual)), collapse = " ")
    ))
  }
}

expect_error <- function(expr, pattern, message) {
  err <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
  if (is.null(err)) fail(paste0(message, " (no error was raised)"))
  if (!grepl(pattern, conditionMessage(err))) {
    fail(paste0(
      message,
      "\nExpected error pattern: ", pattern,
      "\nActual error: ", conditionMessage(err)
    ))
  }
}

test_standardize_commune_codes <- function() {
  input <- c("00123", " 2A004 ", "75056.0", "", "NA", NA)
  actual <- standardize_commune_codes(input)
  expected <- c("123", "2A004", "75056", NA_character_, NA_character_, NA_character_)
  expect_equal(actual, expected, "Commune-code normalization must keep alphanumeric Corsican codes and strip numeric padding.")
}

test_non_missing_sorted_helpers <- function() {
  values <- c("7802", NA, "7801", "7801")
  expect_equal(collapse_non_missing_sorted(values), "7801|7802", "Canton audit lists should ignore missing values and sort unique codes.")
  expect_equal(first_non_missing_sorted(values), "7801", "Primary audit canton should be the first sorted non-missing code.")
  expect_equal(collapse_non_missing_sorted(c(NA, NA)), NA_character_, "All-missing canton lists should stay missing.")
}

test_duplicate_classification <- function() {
  df <- tibble(
    codecommune = c("1", "1", "2", "2", "3", "3"),
    year = c(1995, 1995, 1995, 1995, 1995, 1995),
    value = c(10, 10, NA, 20, 30, 31),
    label = c("a", "a", "b", "b", "c", "c")
  )

  duplicates <- classify_duplicate_key_groups(df, c("codecommune", "year"))
  classes <- duplicates %>%
    arrange(codecommune) %>%
    pull(duplicate_class)

  expect_equal(classes, c("exact", "compatible", "conflicting"), "Duplicate classifier must distinguish exact, compatible, and conflicting groups.")
}

test_duplicate_resolution_rejects_conflicts <- function() {
  conflicting <- tibble(
    codecommune = c("1", "1"),
    year = c(1995, 1995),
    value = c(10, 11)
  )

  compatible <- tibble(
    codecommune = c("1", "1"),
    year = c(1995, 1995),
    value = c(NA_real_, 11)
  )

  expect_error(
    collapse_duplicate_key_groups(conflicting, c("codecommune", "year"), "conflicting_source"),
    "conflicting duplicate",
    "Conflicting duplicates must not be silently collapsed."
  )

  resolved <- collapse_duplicate_key_groups(compatible, c("codecommune", "year"), "compatible_source")
  expect_equal(nrow(resolved), 1L, "Compatible duplicates should collapse to one row.")
  expect_equal(resolved$value, 11, "Compatible duplicates should keep the observed non-missing value.")
}

test_duplicate_free_resolution_is_noop <- function() {
  unique_rows <- tibble(
    codecommune = c("1", "2"),
    year = c(1995, 1995),
    value = c(10, 20)
  )

  resolved <- collapse_duplicate_key_groups(unique_rows, c("codecommune", "year"), "unique_source")
  expect_equal(nrow(resolved), 2L, "Duplicate-free inputs should pass through unchanged.")
  expect_equal(resolved$value, c(10, 20), "Duplicate-free inputs should retain their values.")
}

test_duplicate_resolution_prefers_fuller_rows <- function() {
  duplicated <- tibble(
    codecommune = c("1", "1", "2"),
    label = c("sparse", "full", "unique"),
    value_1990 = c(0, 10, 30),
    value_1999 = c(0, 20, 40)
  )

  resolved <- resolve_duplicate_rows_by_nonzero_coverage(
    duplicated,
    keys = "codecommune",
    value_cols = c("value_1990", "value_1999"),
    dataset_name = "coverage_source"
  ) %>%
    arrange(codecommune)

  expect_equal(nrow(resolved), 2L, "Coverage resolution should retain one row per duplicated key.")
  expect_equal(resolved$label, c("full", "unique"), "Coverage resolution should prefer the row with more non-zero values.")
}

test_duplicate_resolution_rejects_tied_conflicts <- function() {
  tied <- tibble(
    codecommune = c("1", "1"),
    label = c("a", "b"),
    value_1990 = c(10, 11),
    value_1999 = c(20, 21)
  )

  expect_error(
    resolve_duplicate_rows_by_nonzero_coverage(
      tied,
      keys = "codecommune",
      value_cols = c("value_1990", "value_1999"),
      dataset_name = "tied_source"
    ),
    "tied on coverage",
    "Coverage resolution must reject tied duplicate rows with conflicting values."
  )
}

test_duplicate_resolution_uses_preference_tiebreaker <- function() {
  tied <- tibble(
    codecommune = c("1", "1"),
    label = c("old_name", "base_name"),
    preference = c(0, 1),
    value_1990 = c(10, 11),
    value_1999 = c(20, 21)
  )

  resolved <- resolve_duplicate_rows_by_nonzero_coverage(
    tied,
    keys = "codecommune",
    value_cols = c("value_1990", "value_1999"),
    dataset_name = "preferred_source",
    preference_col = "preference"
  )

  expect_equal(resolved$label, "base_name", "Coverage ties should use the optional preference column.")
}

test_audited_join_detects_multiplication_and_unmatched <- function() {
  left <- tibble(codecommune = c("1", "2"), year = c(1995, 1995), x = c(1, 2))
  right_duplicate <- tibble(codecommune = c("1", "1"), y = c(10, 11))

  expect_error(
    audited_left_join(left, right_duplicate, by = "codecommune", relationship = "many-to-one", join_name = "bad_join"),
    "duplicate key",
    "Audited joins must reject right-side duplicates under many-to-one relationships."
  )

  right <- tibble(codecommune = c("1", "3"), y = c(10, 30))
  ledger <- new_merge_ledger()
  joined <- audited_left_join(
    left,
    right,
    by = "codecommune",
    relationship = "many-to-one",
    join_name = "coverage_join",
    ledger = ledger
  )
  expect_equal(nrow(joined), 2L, "Audited left joins should preserve left rows for many-to-one joins.")
  expect_equal(ledger$entries[[1]]$unmatched_left_keys, 1L, "Merge ledger must count unmatched left keys.")
  expect_equal(ledger$unmatched[[1]]$codecommune, "2", "Merge ledger must store unmatched key examples.")
}

tests <- list(
  test_standardize_commune_codes,
  test_non_missing_sorted_helpers,
  test_duplicate_classification,
  test_duplicate_resolution_rejects_conflicts,
  test_duplicate_free_resolution_is_noop,
  test_duplicate_resolution_prefers_fuller_rows,
  test_duplicate_resolution_rejects_tied_conflicts,
  test_duplicate_resolution_uses_preference_tiebreaker,
  test_audited_join_detects_multiplication_and_unmatched
)

for (test in tests) {
  test()
}

cat("All data-quality helper tests passed.\n")
