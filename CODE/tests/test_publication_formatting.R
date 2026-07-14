#!/usr/bin/env Rscript

fail <- function(message) stop(message, call. = FALSE)

expect_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) fail(paste(message, "Expected:", expected, "Actual:", actual))
}

source(file.path("CODE", "publication_formatting.R"))

expect_equal(
  latex_escape_cell("Unemployed (%) & income_1"),
  "Unemployed (\\%) \\& income\\_1",
  "LaTeX table cells must escape special characters in variable labels."
)

expect_equal(
  latex_escape_cell(c("No diploma (%)", "Area in km2 (log)")),
  c("No diploma (\\%)", "Area in km2 (log)"),
  "LaTeX escaping must preserve vector order."
)

cat("Publication formatting tests passed.\n")
