#!/usr/bin/env Rscript
# data_sanity.R
#
# Guard for the scheduled data-refresh workflows. Each refresh scrapes an
# external source and commits the result straight to `main`, so a silent
# upstream change (layout tweak, rate limit, empty response) would otherwise
# commit truncated or empty data unnoticed.
#
# Compares each refreshed CSV in the working tree against the version
# committed in a git ref and fails if the new copy looks collapsed.
#
# The verdict logic is pure and separated from IO so it can be unit tested
# (see tests/testthat/test-data_sanity.R).
#
# Usage:
#   Rscript scripts/data_sanity.R [--min-rows N] [--max-drop-pct P] <csv>...
#
# Exits 1 (with a per-file explanation) if any file fails its checks.

#' Decide whether a refreshed dataset looks sane next to its committed version.
#'
#' @param new_rows Row count of the refreshed data.
#' @param new_cols Character vector of column names in the refreshed data.
#' @param old_rows Row count of the committed data, or `NA` when the file is new.
#' @param old_cols Character vector of committed column names, or `NULL` when new.
#' @param min_rows Minimum acceptable row count regardless of history.
#' @param max_drop_pct Largest tolerated percentage drop in rows vs. committed.
#' @return A list with `ok` (logical) and `problems` (character vector).
data_sanity_verdict <- function(new_rows,
                                new_cols,
                                old_rows = NA_integer_,
                                old_cols = NULL,
                                min_rows = 1L,
                                max_drop_pct = 20) {
  problems <- character()

  if (is.na(new_rows)) {
    return(list(ok = FALSE, problems = "refreshed file could not be read as CSV"))
  }

  if (new_rows < min_rows) {
    problems <- c(problems, sprintf("only %d row(s); expected at least %d", new_rows, min_rows))
  }

  if (!is.na(old_rows) && old_rows > 0) {
    floor_rows <- old_rows * (1 - max_drop_pct / 100)
    if (new_rows < floor_rows) {
      problems <- c(problems, sprintf(
        "row count fell from %d to %d (%.1f%% drop; %.1f%% is the most tolerated)",
        old_rows, new_rows, 100 * (old_rows - new_rows) / old_rows, max_drop_pct
      ))
    }
  }

  dropped_cols <- setdiff(old_cols, new_cols)
  if (length(dropped_cols)) {
    problems <- c(problems, sprintf(
      "column(s) disappeared: %s", paste(dropped_cols, collapse = ", ")
    ))
  }

  list(ok = length(problems) == 0, problems = problems)
}

#' Read a CSV as committed in a git ref.
#'
#' @param path Repo-relative path.
#' @param ref Git ref to read from (default `HEAD`).
#' @return A data frame, or `NULL` when the path does not exist in `ref`.
read_csv_at_ref <- function(path, ref = "HEAD") {
  lines <- suppressWarnings(system2(
    "git",
    c("show", shQuote(sprintf("%s:%s", ref, path))),
    stdout = TRUE,
    stderr = FALSE
  ))
  if (!is.null(attr(lines, "status")) || !length(lines)) {
    return(NULL)
  }
  tryCatch(
    utils::read.csv(text = paste(lines, collapse = "\n"), check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}

#' Check one refreshed CSV against its committed version.
#'
#' @inheritParams data_sanity_verdict
#' @param path Repo-relative path to the refreshed CSV in the working tree.
#' @param ref Git ref holding the previous version.
#' @return The `data_sanity_verdict()` list, plus a `path` element.
check_csv_sanity <- function(path, min_rows = 1L, max_drop_pct = 20, ref = "HEAD") {
  if (!file.exists(path)) {
    return(list(ok = FALSE, problems = "file is missing from the working tree", path = path))
  }

  new_df <- tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  old_df <- read_csv_at_ref(path, ref = ref)

  verdict <- data_sanity_verdict(
    new_rows = if (is.null(new_df)) NA_integer_ else nrow(new_df),
    new_cols = if (is.null(new_df)) character() else names(new_df),
    old_rows = if (is.null(old_df)) NA_integer_ else nrow(old_df),
    old_cols = if (is.null(old_df)) NULL else names(old_df),
    min_rows = min_rows,
    max_drop_pct = max_drop_pct
  )
  c(verdict, list(path = path))
}

main <- function(args) {
  min_rows <- 1L
  max_drop_pct <- 20
  paths <- character()

  i <- 1
  while (i <= length(args)) {
    switch(args[[i]],
      "--min-rows" = {
        min_rows <- as.integer(args[[i + 1]])
        i <- i + 1
      },
      "--max-drop-pct" = {
        max_drop_pct <- as.numeric(args[[i + 1]])
        i <- i + 1
      },
      paths <- c(paths, args[[i]])
    )
    i <- i + 1
  }

  if (!length(paths)) {
    stop("usage: Rscript scripts/data_sanity.R [--min-rows N] [--max-drop-pct P] <csv>...")
  }

  failed <- character()
  for (path in paths) {
    if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
      cat(sprintf("  skip  %s (not a CSV)\n", path))
      next
    }
    result <- check_csv_sanity(path, min_rows = min_rows, max_drop_pct = max_drop_pct)
    if (result$ok) {
      cat(sprintf("  ok    %s\n", path))
    } else {
      failed <- c(failed, path)
      cat(sprintf("  FAIL  %s\n", path))
      for (problem in result$problems) {
        cat(sprintf("          - %s\n", problem))
      }
    }
  }

  if (length(failed)) {
    cat(sprintf(
      "\nERROR: %d file(s) failed the data sanity check; refusing to commit.\n",
      length(failed)
    ))
    quit(status = 1)
  }
  cat("Data sanity check passed.\n")
}

if (sys.nframe() == 0) {
  main(commandArgs(trailingOnly = TRUE))
}
