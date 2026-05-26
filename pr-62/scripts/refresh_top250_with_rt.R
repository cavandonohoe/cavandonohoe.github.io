#!/usr/bin/env Rscript
# refresh_top250_with_rt.R
#
# Wrapper for web_scraping/top250_movies_with_rt.R that disables the Gmail
# notification step (which requires user OAuth, not available in CI) and
# preserves the CSV write.
#
# Usage:
#   Rscript scripts/refresh_top250_with_rt.R

# Stub out the gmail helper before sourcing the scraper so it doesn't try
# to authenticate with a missing client secret.
send_gmail_notification <- function(...) {
  message("[CI] Skipping Gmail notification.")
  invisible(NULL)
}

# Override the source() of gmail_notify.R to a no-op when called from inside
# the scraper. We do this by pre-creating the function in the global env;
# the scraper's `source(... gmail_notify.R)` will overwrite it, so we also
# defang the resulting function by wrapping the call in a tryCatch.
#
# Simpler: monkey-patch source() locally to skip that one file.
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

orig_source(orig_top250, local = FALSE)
message("[CI] Refresh complete: data/top250_movies_with_rt.csv")
