#!/usr/bin/env Rscript

fail <- function(message) stop(message, call. = FALSE)

expect_true <- function(value, message) {
  if (!isTRUE(value)) fail(message)
}

project_root <- normalizePath(".", mustWork = TRUE)
source(file.path(project_root, "CODE", "publish_artifacts.R"))

manifest <- publication_artifact_manifest(project_root)

expect_true(nrow(manifest) > 0L, "The publication manifest must include generated artifacts.")
expect_true(all(manifest$kind %in% c("table", "figure")), "Every publication artifact must be classified as a table or figure.")
expect_true(all(file.exists(manifest$source)), "Every manifest source must exist before synchronization.")
expect_true(all(file.exists(manifest$destination)), "Every manuscript artifact must have a destination.")
expect_true(!anyDuplicated(manifest$destination), "Each manuscript artifact must have exactly one generated source.")

status <- verify_publication_artifacts(project_root)
expect_true(all(status$identical), "All generated artifacts must match the copies included by LaTeX.")

cat("Publication artifact synchronization tests passed.\n")
