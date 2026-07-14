#!/usr/bin/env Rscript

fail <- function(message) stop(message, call. = FALSE)

old_setting <- Sys.getenv("RUN_ANNEX_MAIN_1995", unset = "")
Sys.setenv(RUN_ANNEX_MAIN_1995 = "false")
on.exit(Sys.setenv(RUN_ANNEX_MAIN_1995 = old_setting), add = TRUE)

source(file.path("CODE", "prepare tables", "annex_main_results_1995.R"))

if (!exists("run_annex_main_results_1995", mode = "function")) {
  fail("The 1995 appendix table must expose an explicit standalone entry point.")
}

arguments <- names(formals(generate_rdd_main_results))
if (!identical(arguments, c("processed_data_path", "path_tables", "bandwidths", "controls"))) {
  fail("The 1995 table generator must receive all analysis settings explicitly.")
}

cat("1995 appendix table interface tests passed.\n")
