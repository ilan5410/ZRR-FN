#!/usr/bin/env Rscript

fail <- function(message) stop(message, call. = FALSE)

expect_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) fail(paste(message, "Expected:", expected, "Actual:", actual))
}

expect_error <- function(expr, pattern, message) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  if (is.null(error) || !grepl(pattern, conditionMessage(error))) fail(message)
}

source(file.path("CODE", "prepare tables", "matching_helpers.R"))

toy <- data.frame(
  codecommune = c("A", "B", "A", "C"),
  border_pair = c("p1", "p1", "p2", "p2"),
  treatmentZRR = c(TRUE, FALSE, TRUE, FALSE)
)

ledger <- matching_sample_ledger(toy)
expect_equal(ledger$observations, 4L, "The matching ledger must count rows as commune-pair observations.")
expect_equal(ledger$communes, 3L, "The matching ledger must count unique communes separately.")
expect_equal(ledger$border_pairs, 2L, "The matching ledger must count unique border-pair identifiers separately.")
expect_equal(ledger$treated, 2L, "The matching ledger must report treated observations.")
expect_equal(ledger$controls, 2L, "The matching ledger must report control observations.")

expect_error(
  assert_same_matching_sample(c("A", "B"), c("A", "C")),
  "different rows",
  "Balance and regression outputs must not silently use different matched samples."
)

cat("Matching helper tests passed.\n")
