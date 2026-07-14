# Matched border-municipality balance and regression tables.
#
# Both appendix tables are intentionally generated from one MatchIt object so
# their counts and treatment definition cannot silently drift apart.

if (!exists("matching_sample_ledger", mode = "function")) {
  source(file.path(main_path, "CODE", "prepare tables", "matching_helpers.R"))
}
if (!exists("latex_escape_cell", mode = "function")) {
  source(file.path(main_path, "CODE", "publication_formatting.R"))
}

matching_covariates <- c(
  "pchom", "FN1988", "delta_pop_1980_1995", "pop", "ratEmp",
  "ratForeigners", "asso", "educNoDiplomaPerK", "educSUPPerK",
  "educBACPerK", "educCAPBEPPerK", "poph", "popf", "pagri",
  "pindp", "ppint", "pempl", "pouvr", "altitude", "superficie",
  "min_distance_to_agglo", "logVac"
)

build_matched_border_sample <- function(data, caliper = 0.2) {
  required <- c("codecommune", "border_pair", "same_department", "z", matching_covariates)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Border matching data are missing: ", paste(missing, collapse = ", "))
  }

  candidate <- data %>%
    dplyr::filter(.data$same_department == 1) %>%
    dplyr::mutate(
      treatmentZRR = as.logical(.data$z),
      matching_row_id = paste(.data$codecommune, .data$border_pair, sep = "::")
    ) %>%
    dplyr::distinct(.data$matching_row_id, .keep_all = TRUE)

  if (anyDuplicated(candidate$matching_row_id)) {
    stop("Border matching rows must be unique on commune and border pair.")
  }

  formula <- stats::as.formula(
    paste("treatmentZRR ~", paste(matching_covariates, collapse = " + "))
  )
  matched <- MatchIt::match.data(
    MatchIt::matchit(
      formula,
      data = candidate,
      method = "nearest",
      distance = "logit",
      caliper = caliper
    )
  )
  if (any(is.na(matched$treatmentZRR))) stop("Matched sample has missing treatment values.")
  matched
}

clustered_terms <- function(model, data, cluster = "canton") {
  if (!cluster %in% names(data)) stop("Matched sample is missing the canton cluster identifier.")
  keep <- !is.na(data[[cluster]])
  if (!all(keep)) {
    model <- stats::update(model, data = data[keep, , drop = FALSE])
    data <- data[keep, , drop = FALSE]
  }
  robust <- lmtest::coeftest(
    model,
    vcov. = sandwich::vcovCL(model, cluster = data[[cluster]], type = "HC1")
  )
  list(se = robust[, "Std. Error"], p = robust[, "Pr(>|t|)"])
}

generate_matching_balance_table <- function(matched, path_tables, ledger) {
  balance_vars <- intersect(c(matching_covariates, "haie", "vigne"), names(matched))
  residual_rows <- lapply(balance_vars, function(variable) {
    controls_for_residual <- setdiff(intersect(c("dep", matching_covariates), names(matched)), variable)
    formula <- stats::as.formula(
      paste(variable, "~", paste(c("factor(dep)", setdiff(controls_for_residual, "dep")), collapse = " + "))
    )
    residuals <- stats::resid(stats::lm(formula, data = matched))
    grouped <- tapply(residuals, matched$treatmentZRR, mean, na.rm = TRUE)
    test <- stats::t.test(residuals ~ matched$treatmentZRR)
    result <- data.frame(
      variable = if (variable %in% names(labels)) labels[[variable]] else variable,
      Control = unname(grouped[["FALSE"]]),
      Treatment = unname(grouped[["TRUE"]]),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    )
    names(result)[4] <- "p-value"
    result
  })
  balance <- dplyr::bind_rows(residual_rows)
  p_value <- ifelse(balance[["p-value"]] < 0.001, "<0.001", sprintf("%.3f", balance[["p-value"]]))
  rows <- paste0(
    paste(
      latex_escape_cell(balance$variable),
      sprintf("%.3f", balance$Control),
      sprintf("%.3f", balance$Treatment),
      p_value,
      sep = " & "
    ),
    " \\\\"
  )
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\scriptsize",
    "\\caption{Residual balance after propensity-score matching}",
    "\\label{tab:ttest-border-matching}",
    "\\begin{tabular}{lrrr}",
    "\\hline",
    "Variable & Control & Treatment & p-value \\\\",
    "\\hline",
    rows,
    "\\hline",
    "\\end{tabular}",
    paste0(
      "\\parbox{\\textwidth}{\\footnotesize \\textit{Notes:} Nearest-neighbor matching uses a 0.2 caliper on the logit propensity score. ",
      "Residual means condition on department fixed effects and the remaining matching covariates. ",
      "The matched sample contains ", format_matching_ledger(ledger), ".}"
    ),
    "\\end{table}"
  )
  writeLines(lines, file.path(path_tables, "annex_border_pair_ttest.tex"))
}

generate_matching_regression_table <- function(matched, path_tables, controls, ledger) {
  outcome <- if ("y" %in% names(matched)) "y" else "FN2002"
  regressors <- intersect(controls, names(matched))
  models <- list(
    stats::lm(stats::as.formula(paste(outcome, "~ treatmentZRR")), data = matched),
    stats::lm(stats::as.formula(paste(outcome, "~ treatmentZRR +", paste(regressors, collapse = " + "))), data = matched),
    stats::lm(stats::as.formula(paste(outcome, "~ treatmentZRR +", paste(c(regressors, "factor(dep)"), collapse = " + "))), data = matched)
  )
  robust <- lapply(models, clustered_terms, data = matched)

  treatment_term <- "treatmentZRRTRUE"
  estimates <- vapply(models, function(model) stats::coef(model)[[treatment_term]], numeric(1))
  standard_errors <- vapply(robust, function(result) result$se[[treatment_term]], numeric(1))
  p_values <- vapply(robust, function(result) result$p[[treatment_term]], numeric(1))
  stars <- ifelse(p_values < 0.01, "***", ifelse(p_values < 0.05, "**", ifelse(p_values < 0.1, "*", "")))
  estimate_cells <- paste0(sprintf("%.3f", estimates), stars)
  se_cells <- paste0("(", sprintf("%.3f", standard_errors), ")")
  n_cells <- formatC(vapply(models, stats::nobs, numeric(1)), format = "d", big.mark = ",")
  r2_cells <- sprintf("%.3f", vapply(models, function(model) summary(model)$r.squared, numeric(1)))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\footnotesize",
    "\\caption{Border municipalities: matched regression results}",
    "\\label{tab:border-results-matching}",
    "\\begin{tabular}{lccc}",
    "\\hline",
    " & (1) & (2) & (3) \\\\",
    "\\hline",
    paste("ZRR treatment &", paste(estimate_cells, collapse = " & "), "\\\\"),
    paste(" &", paste(se_cells, collapse = " & "), "\\\\"),
    "\\hline",
    "Controls & No & Yes & Yes \\\\",
    "Department fixed effects & No & No & Yes \\\\",
    paste("Observations &", paste(n_cells, collapse = " & "), "\\\\"),
    paste("R$^2$ &", paste(r2_cells, collapse = " & "), "\\\\"),
    "\\hline",
    "\\end{tabular}",
    paste0(
      "\\parbox{\\textwidth}{\\footnotesize \\textit{Notes:} The dependent variable is the FN vote share in 2002. ",
      "Nearest-neighbor matching uses a 0.2 caliper on the logit propensity score. ",
      "Standard errors are HC1 and clustered by canton. ",
      "The matched sample contains ", format_matching_ledger(ledger), ". ",
      "$^{*}$p$<$0.10; $^{**}$p$<$0.05; $^{***}$p$<$0.01.}"
    ),
    "\\end{table}"
  )
  writeLines(lines, file.path(path_tables, "annex_matching.tex"))
}

generate_annex_matching_tables <- function(processed_data_path, path_tables, controls, caliper = 0.2) {
  data_file <- file.path(processed_data_path, "borders_pair.RData")
  if (!file.exists(data_file)) stop("Data environment does not exist: ", data_file)

  load(data_file)
  matched <- build_matched_border_sample(dfZRRControls, caliper = caliper)
  ledger <- matching_sample_ledger(matched)
  assert_same_matching_sample(matched$matching_row_id, matched$matching_row_id)

  generate_matching_balance_table(matched, path_tables, ledger)
  generate_matching_regression_table(matched, path_tables, controls, ledger)

  cat("Generated matched border tables with ", format_matching_ledger(ledger), ".\n", sep = "")
  invisible(list(data = matched, ledger = ledger))
}

if (!identical(Sys.getenv("RUN_ANNEX_MATCHING", "true"), "false")) {
  generate_annex_matching_tables(processed_data_path, path_tables, controls)
}
