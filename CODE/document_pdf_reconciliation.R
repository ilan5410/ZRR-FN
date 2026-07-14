# Reconciliation record for numerical corrections identified in the PDF review.

write_pdf_result_reconciliation <- function(project_root = getwd()) {
  output_dir <- file.path(project_root, "OUTPUT", "data_quality")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- data.frame(
    item = c(
      "Table 3 baseline treatment date",
      "Table 8 other-outcomes note",
      "Table 9 border balance labels",
      "Table 10 border-pair counts",
      "Table 11 doughnut coefficient",
      "Appendix matching tables",
      "Appendix 1995 diagnostic",
      "Commune-history sensitivity"
    ),
    former_or_risk = c(
      "Note said initial entry occurred in 1988.",
      "Note called the negative FN-change coefficient positive and reported department clustering.",
      "Unescaped percent signs commented out LaTeX rows.",
      "Text conflated communes, pairs, and commune-pair observations.",
      "Text reported -0.007 and heteroskedasticity-robust inference.",
      "Hard-coded balance and regression tables used inconsistent samples and calipers.",
      "Hard-coded tables conflicted on outcome year and sample definition.",
      "No quantitative check of official commune-history flags."
    ),
    validated_value_or_action = c(
      "Initial wave coded in 1995; 1988 is the baseline election.",
      "Negative coefficient; HC1 canton-clustered standard errors.",
      "Manual generator escapes all LaTeX cell labels.",
      "13,646 observations; 6,378 communes; 7,412 pairs; same-department subset 9,710 observations, 4,813 communes, 5,236 pairs.",
      "-0.0105 in the current generated table; HC1 canton-clustered standard errors.",
      "One 0.2-caliper same-department MatchIt sample generates both tables (8,442 observations; 4,274 communes; 5,073 pairs).",
      "Generated 10-km 1995 diagnostic has 13,519 observations and is explicitly a timing diagnostic.",
      "Excluding 1,860 flagged communes changes preferred estimate from -0.006717 to -0.006716."
    ),
    evidence = c(
      "CODE/prepare tables/DID_results.R",
      "CODE/prepare tables/main_results_diff_outcomes.R",
      "CODE/prepare tables/balancing_tests.R",
      "DATA/processed data/borders_pair.RData",
      "CODE/prepare tables/winsorizing_trimming_doughnut.R",
      "CODE/prepare tables/annex_matching.R",
      "CODE/prepare tables/annex_main_results_1995.R",
      "OUTPUT/data_quality/commune_history_estimate_sensitivity.csv"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(rows, file.path(output_dir, "pdf_result_reconciliation.csv"), row.names = FALSE)
  invisible(rows)
}

if (sys.nframe() == 0) write_pdf_result_reconciliation()
