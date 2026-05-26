#!/usr/bin/env Rscript
# refresh_top1000_box_office.R
#
# CI-safe wrapper that runs the Best Picture winners + Top 1000 box office
# scrapers and writes the resulting CSVs, skipping the Gmail notify step
# that requires user-level OAuth.
#
# Usage:
#   Rscript scripts/refresh_top1000_box_office.R

# Defang the email step before any sourcing happens.
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

# Match the variables expected by the scrapers.
sleep_after <- FALSE
refresh_box_office <- TRUE
refresh_ratings <- FALSE

message("[CI] Starting Best Picture scrape...")
orig_source(here::here("web_scraping", "best_picture_winners.R"), local = FALSE)
readr::write_csv(
  oscar_winners_clean,
  here::here("data", "best_picture_winners.csv")
)
message("[CI] Best Picture scrape complete.")

message("[CI] Starting Top 1000 Box Office scrape...")
orig_source(here::here("web_scraping", "top1000_box_office.R"), local = FALSE)
readr::write_csv(
  imdb_rank_ratings_clean,
  here::here("data", "top1000_box_office.csv")
)
message("[CI] Top 1000 Box Office scrape complete.")
