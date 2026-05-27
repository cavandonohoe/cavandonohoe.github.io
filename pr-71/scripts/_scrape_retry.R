#!/usr/bin/env Rscript
# scripts/_scrape_retry.R
#
# Shared helper for CI-driven scrapers that hit IMDb / Rotten Tomatoes /
# similar sites and intermittently get empty or malformed bodies from
# anti-bot infrastructure on GitHub Actions IPs.
#
# Source this file from refresh_*.R wrappers (or any CI script) and call
# `run_with_retry(label, expr)`.

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
