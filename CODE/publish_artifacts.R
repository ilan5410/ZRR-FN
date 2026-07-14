# Synchronize generated tables and figures with the copies included by LaTeX.

publication_artifact_manifest <- function(project_root = ".") {
  project_root <- normalizePath(project_root, mustWork = TRUE)
  specs <- data.frame(
    kind = c("table", "figure"),
    source_dir = c(file.path("OUTPUT", "tables"), file.path("OUTPUT", "figures")),
    destination_dir = c(file.path("Latex", "ZRR and populist vote", "tables"), file.path("Latex", "ZRR and populist vote", "figures")),
    pattern = c("\\.tex$", "\\.png$"),
    stringsAsFactors = FALSE
  )

  manifests <- lapply(seq_len(nrow(specs)), function(i) {
    source_dir <- file.path(project_root, specs$source_dir[[i]])
    names <- list.files(source_dir, pattern = specs$pattern[[i]], full.names = FALSE)
    data.frame(
      kind = specs$kind[[i]],
      source = file.path(source_dir, names),
      destination = file.path(project_root, specs$destination_dir[[i]], names),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, manifests)
}

normalize_generated_table <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines[!grepl("^% Date and time:", lines)]
}

artifact_files_identical <- function(source, destination, kind) {
  if (!file.exists(source) || !file.exists(destination)) return(FALSE)
  if (identical(kind, "table")) {
    return(identical(normalize_generated_table(source), normalize_generated_table(destination)))
  }
  unname(tools::md5sum(source)) == unname(tools::md5sum(destination))
}

verify_publication_artifacts <- function(project_root = ".") {
  manifest <- publication_artifact_manifest(project_root)
  manifest$source_exists <- file.exists(manifest$source)
  manifest$destination_exists <- file.exists(manifest$destination)
  manifest$identical <- mapply(
    artifact_files_identical,
    manifest$source,
    manifest$destination,
    manifest$kind,
    USE.NAMES = FALSE
  )
  manifest
}

sync_publication_artifacts <- function(project_root = ".", quiet = FALSE) {
  manifest <- publication_artifact_manifest(project_root)
  if (!all(file.exists(manifest$source))) {
    missing <- manifest$source[!file.exists(manifest$source)]
    stop("Cannot synchronize missing generated artifacts: ", paste(missing, collapse = ", "))
  }

  for (destination_dir in unique(dirname(manifest$destination))) {
    dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  }

  stale <- !mapply(
    artifact_files_identical,
    manifest$source,
    manifest$destination,
    manifest$kind,
    USE.NAMES = FALSE
  )
  if (any(stale) && !all(file.copy(manifest$source[stale], manifest$destination[stale], overwrite = TRUE))) {
    stop("Failed to synchronize one or more publication artifacts.")
  }

  status <- verify_publication_artifacts(project_root)
  if (!all(status$identical)) {
    stop("Artifact synchronization completed with mismatches.")
  }
  if (!quiet) cat("Synchronized", nrow(status), "publication artifacts.\n")
  invisible(status)
}
