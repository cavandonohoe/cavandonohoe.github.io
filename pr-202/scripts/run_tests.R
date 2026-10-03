#!/usr/bin/env Rscript
#
# Shared test runner used by both the pre-push hook (.githooks/pre-push)
# and CI (.github/workflows/test.yml) so the two never diverge.
#
# Runs, and fails (exit 1) on any failure from:
#   1. testthat suite in tests/testthat/ (if present)
#   2. flat tests/test*.R files
#
# Flat files are sourced from the repo root (so their relative paths such
# as source("scripts/..") and readLines("tests/fixtures/..") resolve) while
# an active testthat reporter counts failures. This catches BOTH bare
# stop()/stopifnot() errors AND test_that() expectation failures, unlike a
# plain sys.source() which only surfaces thrown errors.

if (!requireNamespace("testthat", quietly = TRUE)) {
  cat("    testthat not installed; skipping.\n")
  quit(status = 0)
}

failed <- FALSE

if (dir.exists("tests/testthat")) {
  results <- testthat::test_dir("tests/testthat", reporter = "summary")
  df <- as.data.frame(results)
  failed <- failed || any(df$failed > 0) || any(df$error)
} else {
  cat("    No tests/testthat dir.\n")
}

flat_tests <- list.files("tests", pattern = "^test.*\\.R$", full.names = TRUE)
if (length(flat_tests)) {
  cat("    Running flat test file(s):\n")
  for (f in flat_tests) {
    cat("      - ", f, "\n", sep = "")
    reporter <- testthat::ListReporter$new()
    file_failed <- tryCatch({
      testthat::with_reporter(reporter, {
        sys.source(f, envir = new.env(parent = globalenv()))
      })
      FALSE
    }, error = function(e) {
      message("    flat test errored in ", f, ": ", conditionMessage(e))
      TRUE
    })
    res <- as.data.frame(reporter$get_results())
    if (nrow(res) && (any(res$failed > 0) || any(res$error))) {
      file_failed <- TRUE
    }
    if (file_failed) {
      failed <- TRUE
    }
  }
} else {
  cat("    No flat tests/test*.R files.\n")
}

if (failed) {
  quit(status = 1)
}
cat("    All tests passed.\n")
