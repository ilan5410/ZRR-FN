# ==============================================================================
# Commune Data Quality Helpers
# ==============================================================================
# Shared by the data-preparation pipeline, raw-source audits, and synthetic tests.
# These helpers keep commune-code normalization and merge-cardinality checks in one
# place so production joins cannot silently multiply rows or discard conflicts.
# ==============================================================================

#' Standardize commune codes by removing leading zeros
#' @param codes Character vector of commune codes
#' @return Character vector with leading zeros removed
standardize_commune_codes <- function(codes) {
  codes <- trimws(as.character(codes))
  codes <- sub("\\.0$", "", codes)
  codes[codes %in% c("", "NA", "NaN")] <- NA_character_
  sub("^0+", "", codes)
}

#' Format commune codes as 5-character INSEE strings for audit output
#' @param codes Character vector of commune codes
#' @return Character vector with numeric codes left-padded to 5 characters
format_insee_codes <- function(codes) {
  codes <- trimws(as.character(codes))
  codes <- sub("\\.0$", "", codes)
  codes[codes %in% c("", "NA", "NaN")] <- NA_character_
  ifelse(
    grepl("^[0-9]+$", codes) & nchar(codes) < 5,
    stringr::str_pad(codes, width = 5, side = "left", pad = "0"),
    codes
  )
}

#' Assert that a data frame has at most one row per key
#' @param df Data frame to check
#' @param keys Character vector of key columns
#' @param dataset_name Human-readable name used in error messages
assert_unique_key <- function(df, keys, dataset_name) {
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

  invisible(df)
}

#' Drop fully duplicated rows before key checks
#' @param df Data frame to deduplicate
#' @param dataset_name Human-readable name used in log messages
drop_identical_rows <- function(df, dataset_name) {
  n_before <- nrow(df)
  df <- dplyr::distinct(df)
  n_removed <- n_before - nrow(df)
  if (n_removed > 0) {
    cat("Removed", n_removed, "fully duplicated row(s) from", dataset_name, "\n")
  }
  df
}

non_missing_sorted_values <- function(values) {
  sort(unique(values[!is.na(values)]))
}

collapse_non_missing_sorted <- function(values) {
  observed <- non_missing_sorted_values(values)
  if (length(observed) == 0) {
    return(NA_character_)
  }
  paste(observed, collapse = "|")
}

first_non_missing_sorted <- function(values) {
  observed <- non_missing_sorted_values(values)
  if (length(observed) == 0) {
    return(NA_character_)
  }
  observed[[1]]
}

collapse_value <- function(values) {
  observed <- unique(values[!is.na(values)])
  if (length(observed) == 0) {
    return(NA)
  }
  if (length(observed) == 1) {
    return(observed[[1]])
  }
  stop("Internal error: conflicting values reached collapse_value()", call. = FALSE)
}

duplicate_group_class <- function(group_df, keys) {
  audit_cols <- grep("^\\.audit_", names(group_df), value = TRUE)
  non_key_cols <- setdiff(names(group_df), c(keys, audit_cols))
  if (nrow(dplyr::distinct(group_df)) == 1) {
    return(list(class = "exact", conflict_columns = character()))
  }
  conflict_columns <- non_key_cols[
    vapply(
      group_df[non_key_cols],
      function(x) length(unique(x[!is.na(x)])) > 1,
      logical(1)
    )
  ]
  if (length(conflict_columns) > 0) {
    return(list(class = "conflicting", conflict_columns = conflict_columns))
  }
  list(class = "compatible", conflict_columns = character())
}

row_signature <- function(df) {
  if (ncol(df) == 0) return(rep("", nrow(df)))
  pieces <- lapply(df, function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr)] <- "<NA>"
    x_chr
  })
  do.call(paste, c(pieces, sep = "\r"))
}

#' Classify duplicate key groups by their non-key content
#' @param df Data frame to inspect
#' @param keys Key columns defining the declared grain
classify_duplicate_key_groups <- function(df, keys) {
  missing_keys <- setdiff(keys, names(df))
  if (length(missing_keys) > 0) {
    stop("Missing key column(s): ", paste(missing_keys, collapse = ", "), call. = FALSE)
  }

  audit_cols <- grep("^\\.audit_", names(df), value = TRUE)
  non_key_cols <- setdiff(names(df), c(keys, audit_cols))
  signature_cols <- c(keys, non_key_cols)

  duplicate_keys <- df %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "duplicate_rows") %>%
    dplyr::filter(duplicate_rows > 1)

  if (nrow(duplicate_keys) == 0) {
    return(tibble::tibble())
  }

  duplicate_df <- df %>%
    dplyr::inner_join(duplicate_keys %>% dplyr::select(dplyr::all_of(keys)), by = keys) %>%
    dplyr::mutate(.row_signature = row_signature(dplyr::pick(dplyr::all_of(signature_cols))))

  base_summary <- duplicate_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      duplicate_rows = dplyr::n(),
      distinct_rows = dplyr::n_distinct(.data$.row_signature),
      example_values = paste(utils::head(.data$.row_signature, 3), collapse = " || "),
      .groups = "drop"
    )

  if (length(non_key_cols) == 0) {
    return(base_summary %>%
      dplyr::mutate(
        duplicate_class = "exact",
        conflict_columns = "",
        distinct_rows = NULL
      ))
  }

  conflict_flags <- duplicate_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(non_key_cols),
        ~ dplyr::n_distinct(.x[!is.na(.x)]) > 1
      ),
      .groups = "drop"
    )

  conflict_columns <- apply(conflict_flags[non_key_cols], 1, function(flags) {
    paste(non_key_cols[which(as.logical(flags))], collapse = "|")
  })
  conflict_summary <- conflict_flags[keys]
  conflict_summary$conflict_columns <- conflict_columns

  base_summary %>%
    dplyr::left_join(conflict_summary, by = keys) %>%
    dplyr::mutate(
      conflict_columns = dplyr::coalesce(.data$conflict_columns, ""),
      duplicate_class = dplyr::case_when(
        .data$distinct_rows == 1 ~ "exact",
        .data$conflict_columns != "" ~ "conflicting",
        TRUE ~ "compatible"
      )
    ) %>%
    dplyr::select(dplyr::all_of(keys), duplicate_rows, duplicate_class, conflict_columns, example_values)
}

#' Collapse exact or compatible duplicate key groups; reject conflicts
#' @param df Data frame to collapse
#' @param keys Key columns defining the declared grain
#' @param dataset_name Human-readable name used in errors
collapse_duplicate_key_groups <- function(df, keys, dataset_name) {
  duplicates <- classify_duplicate_key_groups(df, keys)
  if (nrow(duplicates) == 0) {
    return(dplyr::distinct(df))
  }

  conflicts <- duplicates %>% dplyr::filter(.data$duplicate_class == "conflicting")
  if (nrow(conflicts) > 0) {
    examples <- utils::capture.output(utils::head(conflicts, 10))
    stop(
      dataset_name, " has ", nrow(conflicts),
      " conflicting duplicate key group(s) for [", paste(keys, collapse = ", "), "].\n",
      paste(examples, collapse = "\n"),
      call. = FALSE
    )
  }

  non_key_cols <- setdiff(names(df), keys)
  if (length(non_key_cols) == 0) {
    return(dplyr::distinct(df))
  }

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(non_key_cols), collapse_value),
      .groups = "drop"
    )
}

row_id_column <- ".audit_row_id"

#' Resolve duplicate key rows by choosing the row with the fullest observed data
#' @param df Data frame to resolve
#' @param keys Key columns defining the duplicate groups
#' @param value_cols Columns used to measure row coverage
#' @param dataset_name Human-readable name used in errors and logs
#' @param audit_path Optional CSV path where duplicate resolution decisions are written
#' @param preference_col Optional numeric/logical column used as a final tie-breaker
resolve_duplicate_rows_by_nonzero_coverage <- function(df, keys, value_cols, dataset_name, audit_path = NULL, preference_col = NULL) {
  missing_keys <- setdiff(keys, names(df))
  missing_values <- setdiff(value_cols, names(df))
  missing_preference <- if (!is.null(preference_col)) setdiff(preference_col, names(df)) else character()
  if (length(missing_keys) > 0 || length(missing_values) > 0 || length(missing_preference) > 0) {
    stop(
      dataset_name, " is missing column(s): ",
      paste(c(missing_keys, missing_values, missing_preference), collapse = ", "),
      call. = FALSE
    )
  }

  duplicate_keys <- df %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "duplicate_rows") %>%
    dplyr::filter(.data$duplicate_rows > 1)

  if (nrow(duplicate_keys) == 0) {
    return(df)
  }

  with_coverage <- df %>%
    dplyr::mutate(
      !!row_id_column := dplyr::row_number(),
      .audit_nonzero_coverage = rowSums(dplyr::across(
        dplyr::all_of(value_cols),
        ~ !is.na(.x) & .x != 0
      )),
      .audit_observed_coverage = rowSums(dplyr::across(
        dplyr::all_of(value_cols),
        ~ !is.na(.x)
      ))
    )

  best_rows <- with_coverage %>%
    dplyr::semi_join(duplicate_keys %>% dplyr::select(dplyr::all_of(keys)), by = keys) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$.audit_nonzero_coverage == max(.data$.audit_nonzero_coverage)) %>%
    dplyr::filter(.data$.audit_observed_coverage == max(.data$.audit_observed_coverage)) %>%
    dplyr::ungroup()

  if (!is.null(preference_col)) {
    best_rows <- best_rows %>%
      dplyr::mutate(.audit_preference = as.numeric(.data[[preference_col]])) %>%
      dplyr::mutate(.audit_preference = dplyr::coalesce(.data$.audit_preference, 0)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
      dplyr::filter(.data$.audit_preference == max(.data$.audit_preference)) %>%
      dplyr::ungroup()
  }

  unresolved <- best_rows %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "candidate_rows") %>%
    dplyr::filter(.data$candidate_rows > 1)

  if (nrow(unresolved) > 0) {
    unresolved_rows <- best_rows %>%
      dplyr::inner_join(unresolved %>% dplyr::select(dplyr::all_of(keys)), by = keys)
    unresolved_classes <- classify_duplicate_key_groups(
      unresolved_rows %>% dplyr::select(-dplyr::all_of(row_id_column)),
      keys
    )
    conflicting_ties <- unresolved_classes %>%
      dplyr::filter(.data$duplicate_class == "conflicting")
    if (nrow(conflicting_ties) > 0) {
      examples <- utils::capture.output(utils::head(conflicting_ties, 10))
      stop(
        dataset_name, " has ", nrow(conflicting_ties),
        " duplicate key group(s) tied on coverage with conflicting values.\n",
        paste(examples, collapse = "\n"),
        call. = FALSE
      )
    }
  }

  selected_ids <- best_rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(row_id_column))

  if (!is.null(audit_path)) {
    duplicate_audit <- with_coverage %>%
      dplyr::semi_join(duplicate_keys %>% dplyr::select(dplyr::all_of(keys)), by = keys) %>%
      dplyr::mutate(
        .audit_selected = .data[[row_id_column]] %in% selected_ids[[row_id_column]]
      )
    utils::write.csv(duplicate_audit, audit_path, row.names = FALSE)
  }

  with_coverage %>%
    dplyr::anti_join(duplicate_keys %>% dplyr::select(dplyr::all_of(keys)), by = keys) %>%
    dplyr::bind_rows(
      with_coverage %>%
        dplyr::semi_join(selected_ids, by = row_id_column)
    ) %>%
    dplyr::arrange(.data[[row_id_column]]) %>%
    dplyr::select(-dplyr::all_of(c(
      row_id_column,
      ".audit_nonzero_coverage",
      ".audit_observed_coverage"
    )))
}

new_merge_ledger <- function() {
  ledger <- new.env(parent = emptyenv())
  ledger$entries <- list()
  ledger$unmatched <- list()
  ledger
}

join_key_names <- function(by) {
  if (is.null(names(by)) || all(names(by) == "")) {
    return(list(left = unname(by), right = unname(by)))
  }
  named <- names(by) != ""
  list(
    left = ifelse(named, names(by), by),
    right = unname(by)
  )
}

unique_key_count <- function(df, keys) {
  if (length(keys) == 0) return(NA_integer_)
  nrow(dplyr::distinct(df, dplyr::across(dplyr::all_of(keys))))
}

key_signature <- function(df, keys) {
  if (length(keys) == 0) return(character(nrow(df)))
  do.call(paste, c(df[keys], sep = "\r"))
}

record_merge_ledger <- function(ledger, entry, unmatched_examples) {
  if (is.null(ledger)) return(invisible(NULL))
  ledger$entries[[length(ledger$entries) + 1L]] <- entry
  if (nrow(unmatched_examples) > 0) {
    ledger$unmatched[[length(ledger$unmatched) + 1L]] <- unmatched_examples
  }
  invisible(NULL)
}

#' Audited left join with cardinality and coverage diagnostics
#' @param left Left data frame
#' @param right Right data frame
#' @param by Join keys, as accepted by dplyr
#' @param relationship Expected dplyr relationship
#' @param join_name Human-readable join label
#' @param ledger Optional merge ledger environment from new_merge_ledger()
audited_left_join <- function(left, right, by, relationship, join_name, ledger = NULL) {
  keys <- join_key_names(by)
  if (relationship %in% c("many-to-one", "one-to-one")) {
    assert_unique_key(right, keys$right, paste0(join_name, "::right"))
  }
  if (relationship %in% c("one-to-many", "one-to-one")) {
    assert_unique_key(left, keys$left, paste0(join_name, "::left"))
  }

  left_keys <- dplyr::distinct(left, dplyr::across(dplyr::all_of(keys$left)))
  right_keys <- dplyr::distinct(right, dplyr::across(dplyr::all_of(keys$right)))
  left_sig <- key_signature(left_keys, keys$left)
  right_sig <- key_signature(right_keys, keys$right)
  matched_left <- sum(left_sig %in% right_sig)
  unmatched_examples <- left_keys[!(left_sig %in% right_sig), , drop = FALSE] %>%
    utils::head(25) %>%
    dplyr::mutate(join_name = join_name, .before = 1)

  output <- dplyr::left_join(left, right, by = by, relationship = relationship)
  expected_preserve_left <- relationship %in% c("many-to-one", "one-to-one")
  cardinality_ok <- !expected_preserve_left || nrow(output) == nrow(left)

  entry <- list(
    join_name = join_name,
    relationship = relationship,
    left_rows = nrow(left),
    right_rows = nrow(right),
    output_rows = nrow(output),
    left_unique_keys = unique_key_count(left, keys$left),
    right_unique_keys = unique_key_count(right, keys$right),
    matched_left_keys = matched_left,
    unmatched_left_keys = length(left_sig) - matched_left,
    match_rate = ifelse(length(left_sig) == 0, NA_real_, matched_left / length(left_sig)),
    cardinality_ok = cardinality_ok
  )
  record_merge_ledger(ledger, entry, unmatched_examples)

  if (!cardinality_ok) {
    stop(
      join_name, " changed row count from ", nrow(left), " to ", nrow(output),
      " under declared relationship ", relationship,
      call. = FALSE
    )
  }

  output
}

write_merge_ledger <- function(ledger, output_dir) {
  if (is.null(ledger)) return(invisible(NULL))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  entries <- dplyr::bind_rows(lapply(ledger$entries, tibble::as_tibble))
  unmatched <- dplyr::bind_rows(ledger$unmatched)
  readr::write_csv(entries, file.path(output_dir, "merge_ledger.csv"))
  readr::write_csv(unmatched, file.path(output_dir, "unmatched_commune_codes.csv"))
  invisible(list(entries = entries, unmatched = unmatched))
}
