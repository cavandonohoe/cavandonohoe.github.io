#!/usr/bin/env Rscript
# refresh_top1000_box_office.R
#
# CI-safe wrapper that runs the Best Picture winners + Top 1000 box office
# scrapers and writes the resulting CSVs, skipping the Gmail notify step
# that requires user-level OAuth.
#
# IMDb intermittently returns empty / unparsable bodies to GitHub Actions
# IPs, so each underlying scrape is retried up to a few times with backoff
# before we give up.
#
# Usage:
#   Rscript scripts/refresh_top1000_box_office.R

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

sleep_after <- FALSE
refresh_box_office <- TRUE
refresh_ratings <- FALSE

source(here::here("scripts", "_scrape_retry.R"))

message("[CI] Starting Best Picture scrape...")
bp_result <- run_or_softfail_on_imdb_block(
  "Best Picture scrape",
  run_with_retry(
    "Best Picture scrape",
    orig_source(
      here::here("web_scraping", "best_picture_winners.R"),
      local = FALSE
    )
  )
)
if (!inherits(bp_result, "imdb_block_softfail")) {
  readr::write_csv(
    oscar_winners_clean,
    here::here("data", "best_picture_winners.csv")
  )
  message("[CI] Best Picture scrape complete.")
} else {
  message("[CI] Best Picture scrape skipped (IMDb block); CSV unchanged.")
}

message("[CI] Starting Top 1000 Box Office scrape...")
bo_result <- run_or_softfail_on_imdb_block(
  "Top 1000 Box Office scrape",
  run_with_retry(
    "Top 1000 Box Office scrape",
    orig_source(
      here::here("web_scraping", "top1000_box_office.R"),
      local = FALSE
    )
  )
)
if (!inherits(bo_result, "imdb_block_softfail")) {
  # If MAX_NEW_RATINGS capped the run, some IDs may still be NA in the
  # produced data frame. Don't publish a degraded CSV in that case;
  # the cache still gets committed so the next run picks up where we
  # left off.
  na_count <- sum(is.na(imdb_rank_ratings_clean$imdb_rating))
  if (na_count > 0) {
    message(sprintf(
      paste0(
        "[CI] %d / %d ratings still missing after this run; ",
        "preserving existing CSV. Cache will be committed so the ",
        "next run continues from here."
      ),
      na_count,
      nrow(imdb_rank_ratings_clean)
    ))
  } else {
    readr::write_csv(
      imdb_rank_ratings_clean,
      here::here("data", "top1000_box_office.csv")
    )
    message("[CI] Top 1000 Box Office scrape complete.")
  }
} else {
  message("[CI] Top 1000 Box Office scrape skipped (IMDb block); CSV unchanged.")
}
