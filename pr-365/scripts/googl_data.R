#!/usr/bin/env Rscript
# Downloads Alphabet (GOOGL) and S&P 500 (SPY) daily prices from Yahoo Finance
# back to the August 2004 IPO, for the "Why I Own Google" page.
# Run locally: Rscript scripts/googl_data.R
# Output: data/googl_vs_spy_prices.csv

symbols <- c("GOOGL", "SPY")
from <- as.Date("2004-08-19")

cat("Downloading", paste(symbols, collapse = ", "), "from Yahoo Finance...\n")
prices <- lapply(symbols, function(sym) {
  xts_data <- quantmod::getSymbols(
    sym, src = "yahoo", from = from, to = Sys.Date() + 1,
    auto.assign = FALSE
  )
  if (is.null(xts_data) || nrow(xts_data) == 0) {
    stop("Failed to download ", sym, ". Check your network connection.")
  }
  data.frame(
    symbol = sym,
    date = zoo::index(xts_data),
    close = as.numeric(quantmod::Cl(xts_data)),
    adjusted = as.numeric(quantmod::Ad(xts_data))
  )
})
prices <- do.call(rbind, prices)
prices <- prices[!is.na(prices$adjusted), ]

readr::write_csv(prices, here::here("data", "googl_vs_spy_prices.csv"))
cat("Saved", nrow(prices), "rows to data/googl_vs_spy_prices.csv\n")
cat(
  "Date range:", as.character(min(prices$date)), "to",
  as.character(max(prices$date)), "\n"
)
