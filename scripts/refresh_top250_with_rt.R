#!/usr/bin/env Rscript
# refresh_top250_with_rt.R
#
# Wrapper for web_scraping/top250_movies_with_rt.R that disables the Gmail
# notification step (which requires user OAuth, not available in CI) and
# preserves the CSV write.
#
# IMDb intermittently returns empty / unparseable bodies to GitHub Actions
# IPs, so the scrape is retried with backoff before we give up.
#
# Usage:
#   Rscript scripts/refresh_top250_with_rt.R

send_gmail_notification <- function(...) {
  message("[CI] Skipping Gmail notification.")
  invisible(NULL)
}

orig_source <- base::source
source <- function(file, ...) {
  if (grepl("gmail_notify\\.R$", file)) {
    message("[CI] Skipping source(", file, ")")
    return(invisible(NULL))
  }
  orig_source(file, ...)
}

orig_top250 <- file.path("web_scraping", "top250_movies_with_rt.R")
if (!file.exists(orig_top250)) {
  stop("Cannot find ", orig_top250, " from working directory ", getwd())
}

run_with_retry <- function(label, expr, max_attempts = 3) {
  expr <- substitute(expr)
  envir <- parent.frame()
  for (attempt in seq_len(max_attempts)) {
    res <- tryCatch(eval(expr, envir = envir), error = function(e) e)
    if (!inherits(res, "error")) {
      return(invisible(res))
    }
    msg <- conditionMessage(res)
    message(sprintf(
      "[CI] %s attempt %d/%d failed: %s",
      label, attempt, max_attempts, msg
    ))
    cache_dir <- here::here("web_scraping", "cache")
    if (dir.exists(cache_dir)) {
      message("[CI] Clearing scraper cache at ", cache_dir)
      unlink(cache_dir, recursive = TRUE, force = TRUE)
    }
    if (attempt < max_attempts) {
      Sys.sleep(2 ^ attempt * 5)
    } else {
      stop(sprintf(
        "%s failed after %d attempts: %s",
        label, max_attempts, msg
      ))
    }
  }
}

run_with_retry(
  "Top 250 with Rotten Tomatoes scrape",
  orig_source(orig_top250, local = FALSE)
)
message("[CI] Refresh complete: data/top250_movies_with_rt.csv")
