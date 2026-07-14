#!/usr/bin/env Rscript

fail <- function(message) stop(message, call. = FALSE)

source(file.path("CODE", "prepare data", "data_quality_helpers.R"))
Sys.setenv(RUN_COMMUNE_HISTORY_SENSITIVITY = "false")
source(file.path("CODE", "prepare data", "commune_history_sensitivity.R"))

flags <- data.frame(
  code_insee = c("01001", "01002", "01003"),
  needs_crosswalk = c(TRUE, FALSE, FALSE),
  unknown_to_official_history = c(FALSE, TRUE, FALSE),
  inactive_at_observed_year = c(FALSE, FALSE, FALSE)
)
codes <- flagged_commune_codes(flags)
if (!identical(codes, c("01001", "01002"))) fail("The sensitivity must use every official-history flag type.")

data <- data.frame(codecommune = c("01001", "01002", "01003"), value = 1:3)
kept <- exclude_flagged_communes(data, codes)
if (!identical(kept$codecommune, "01003")) fail("Flagged communes must be removed using normalized INSEE codes.")

cat("Commune-history sensitivity tests passed.\n")
