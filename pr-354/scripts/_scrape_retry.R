#!/usr/bin/env Rscript
# scripts/_scrape_retry.R
#
# Shared helpers for CI-driven scrapers that hit IMDb / Rotten Tomatoes /
# similar sites and intermittently get empty or malformed bodies from
# anti-bot infrastructure on GitHub Actions IPs.
#
# Source this file from refresh_*.R / check_*.R wrappers (or any CI
# script) and call:
#   - run_with_retry(label, expr): retry expr up to 3 times, clearing
#     the scraper cache between attempts. Re-raises on terminal failure.
#   - signal_imdb_block(url): throw a typed condition the wrappers can
#     catch and turn into a soft failure when IMDb is actively blocking
#     GH Actions IPs (HTTP 200 with 0-byte body).
#   - run_or_softfail_on_imdb_block(label, expr): wrap a scrape so that
#     an `imdb_blocked` condition becomes an annotated success (exit 0
#     with a GITHUB_STEP_SUMMARY note) instead of a red CI failure. Any
#     other error still propagates.

signal_imdb_block <- function(url) {
  cond <- structure(
    class = c("imdb_blocked", "error", "condition"),
    list(
      message = sprintf(
        "IMDb returned 0-byte body for %s after all retries (anti-bot block)",
        url
      ),
      call = sys.call(-1),
      url = url
    )
  )
  stop(cond)
}

run_with_retry <- function(label, expr, max_attempts = 3) {
  expr <- substitute(expr)
  envir <- parent.frame()
  for (attempt in seq_len(max_attempts)) {
    res <- tryCatch(eval(expr, envir = envir), error = function(e) e)
    if (!inherits(res, "error") && !inherits(res, "condition")) {
      return(invisible(res))
    }
    if (inherits(res, "imdb_blocked")) {
      stop(res)
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

write_step_summary <- function(lines) {
  path <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (!nzchar(path)) {
    message(paste(lines, collapse = "\n"))
    return(invisible(NULL))
  }
  cat(paste(lines, collapse = "\n"), "\n",
    file = path, append = TRUE, sep = "")
}

run_or_softfail_on_imdb_block <- function(label, expr) {
  expr <- substitute(expr)
  envir <- parent.frame()
  res <- tryCatch(
    eval(expr, envir = envir),
    imdb_blocked = function(c) c,
    error = function(e) e
  )
  if (inherits(res, "imdb_blocked")) {
    message(sprintf(
      "[CI] %s soft-failed: %s",
      label, conditionMessage(res)
    ))
    write_step_summary(c(
      sprintf("## %s skipped: IMDb anti-bot block", label),
      "",
      sprintf("- URL: `%s`", res$url %||% "(unknown)"),
      "- IMDb returned HTTP 200 with a 0-byte body for every retry.",
      "- Existing data file is preserved; will refresh on the next run",
      "  once IMDb releases the block on GitHub Actions IPs.",
      ""
    ))
    return(structure(list(blocked = TRUE), class = "imdb_block_softfail"))
  }
  if (inherits(res, "error")) {
    stop(res)
  }
  invisible(structure(list(blocked = FALSE), class = "imdb_block_success"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
