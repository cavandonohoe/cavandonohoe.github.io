#!/usr/bin/env Rscript
# refresh_top250_with_rt.R
#
# Wrapper for web_scraping/top250_movies_with_rt.R that disables the Gmail
# notification step (which requires user OAuth, not available in CI) and
# preserves the CSV write.
#
# IMDb intermittently returns empty / unparsable bodies to GitHub Actions
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

source(here::here("scripts", "_scrape_retry.R"))

run_with_retry(
  "Top 250 with Rotten Tomatoes scrape",
  orig_source(orig_top250, local = FALSE)
)
message("[CI] Refresh complete: data/top250_movies_with_rt.csv")
