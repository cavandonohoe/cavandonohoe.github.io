#!/usr/bin/env Rscript
# Downloads S&P 500 (VOO) historical stock price data from Yahoo Finance.
# Run locally: Rscript scripts/sp500_data.R
# Output: data/voo_stock_prices.csv

`%>%` <- magrittr::`%>%`

cat("Downloading VOO stock prices from Yahoo Finance...\n")
voo <- tidyquant::tq_get(
  x = "VOO", from = "2010-01-01",
  to = as.character(Sys.Date()),
  get = "stock.prices"
)

if (is.null(voo) || nrow(voo) == 0) {
  stop("Failed to download VOO data. Check your network connection.")
}

readr::write_csv(voo, here::here("data", "voo_stock_prices.csv"))
cat("Saved", nrow(voo), "rows to data/voo_stock_prices.csv\n")
cat("Date range:", as.character(min(voo$date)), "to", as.character(max(voo$date)), "\n")
