# Shared accounting checks for border-municipality matching outputs.

matching_sample_ledger <- function(data, treatment_var = "treatmentZRR") {
  required <- c("codecommune", "border_pair", treatment_var)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Matching sample is missing required columns: ", paste(missing, collapse = ", "))
  }

  treatment <- data[[treatment_var]]
  list(
    observations = as.integer(nrow(data)),
    communes = as.integer(length(unique(data$codecommune))),
    border_pairs = as.integer(length(unique(data$border_pair))),
    treated = as.integer(sum(treatment %in% TRUE, na.rm = TRUE)),
    controls = as.integer(sum(treatment %in% FALSE, na.rm = TRUE))
  )
}

assert_same_matching_sample <- function(balance_ids, regression_ids) {
  if (!setequal(balance_ids, regression_ids)) {
    stop("Balance and regression outputs use different rows in the matching sample.")
  }
  invisible(TRUE)
}

format_matching_ledger <- function(ledger) {
  paste0(
    ledger$observations, " commune-pair observations (",
    ledger$communes, " unique communes across ",
    ledger$border_pairs, " border pairs; ",
    ledger$treated, " treated and ", ledger$controls, " control observations)"
  )
}
