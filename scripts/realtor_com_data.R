#!/usr/bin/env Rscript
# realtor_com_data.R
# Pulls Realtor.com RDC Inventory Core Metrics at county and ZIP level.
# "Current month only" CSVs (no history needed) — small, fast, monthly update.
# Adds active/new/pending listing counts, price-reduced share, pending ratio,
# median DOM, median list price. Writes two CSVs (county, zip).
#
# Source: https://www.realtor.com/research/data/
# No key required.
#
# Usage:
#   Rscript scripts/realtor_com_data.R

`%>%` <- magrittr::`%>%`

cat("=== Realtor.com RDC Inventory Core Metrics ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

county_url <- "https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_County.csv"
zip_url    <- "https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_Zip.csv"
hot_county_url <- "https://econdata.s3-us-west-2.amazonaws.com/Reports/Hotness/RDC_Inventory_Hotness_Metrics_County_History.csv"

cat("Fetching Realtor.com county inventory...\n")
rdc_county_raw <- readr::read_csv(county_url, show_col_types = FALSE)
cat("  Rows:", nrow(rdc_county_raw), "\n")

cat("Fetching Realtor.com ZIP inventory...\n")
rdc_zip_raw <- readr::read_csv(zip_url, show_col_types = FALSE)
cat("  Rows:", nrow(rdc_zip_raw), "\n")

cat("Fetching Realtor.com county hotness (history; keeping latest month)...\n")
rdc_hot_raw <- readr::read_csv(hot_county_url, show_col_types = FALSE)
max_hot_period <- max(rdc_hot_raw$month_date_yyyymm, na.rm = TRUE)
rdc_hot_latest <- rdc_hot_raw %>%
  dplyr::filter(month_date_yyyymm == max_hot_period)
cat("  Hotness rows (latest month):", nrow(rdc_hot_latest), "  period:", max_hot_period, "\n")

# --- County ---
rdc_county <- rdc_county_raw %>%
  dplyr::transmute(
    fips = stringr::str_pad(as.character(county_fips), 5, pad = "0"),
    county_name,
    rdc_period = month_date_yyyymm,
    rdc_median_list = median_listing_price,
    rdc_median_list_yoy = median_listing_price_yy,
    rdc_dom = median_days_on_market,
    rdc_active_listings = active_listing_count,
    rdc_new_listings = new_listing_count,
    rdc_pending_listings = pending_listing_count,
    rdc_price_reduced_share = price_reduced_share,
    rdc_pending_ratio = pending_ratio
  ) %>%
  dplyr::filter(!is.na(fips), nchar(fips) == 5) %>%
  dplyr::distinct(fips, .keep_all = TRUE)

# Attach hotness
rdc_hot_clean <- rdc_hot_latest %>%
  dplyr::transmute(
    fips = stringr::str_pad(as.character(county_fips), 5, pad = "0"),
    rdc_hotness_score = hotness_score,
    rdc_supply_score = supply_score,
    rdc_demand_score = demand_score,
    rdc_hotness_rank = hotness_rank
  ) %>%
  dplyr::distinct(fips, .keep_all = TRUE)

rdc_county <- dplyr::left_join(rdc_county, rdc_hot_clean, by = "fips")

readr::write_csv(rdc_county, file.path(data_dir, "realtor_com_county.csv"))
cat(sprintf("  Written: data/realtor_com_county.csv (%d counties)\n", nrow(rdc_county)))

# --- ZIP ---
rdc_zip <- rdc_zip_raw %>%
  dplyr::transmute(
    zip = stringr::str_pad(as.character(postal_code), 5, pad = "0"),
    zip_name,
    rdc_period = month_date_yyyymm,
    rdc_median_list = median_listing_price,
    rdc_median_list_yoy = median_listing_price_yy,
    rdc_dom = median_days_on_market,
    rdc_active_listings = active_listing_count,
    rdc_new_listings = new_listing_count,
    rdc_pending_listings = pending_listing_count,
    rdc_price_reduced_share = price_reduced_share,
    rdc_pending_ratio = pending_ratio
  ) %>%
  dplyr::filter(!is.na(zip), nchar(zip) == 5) %>%
  dplyr::distinct(zip, .keep_all = TRUE)

readr::write_csv(rdc_zip, file.path(data_dir, "realtor_com_zip.csv"))
cat(sprintf("  Written: data/realtor_com_zip.csv (%d ZIPs)\n", nrow(rdc_zip)))

cat("\n=== 10 hottest counties (RDC hotness rank) ===\n")
rdc_county %>%
  dplyr::arrange(rdc_hotness_rank) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::transmute(
    County = stringr::str_trunc(county_name, 35),
    `Hotness rank` = rdc_hotness_rank,
    `Hotness score` = round(rdc_hotness_score, 1),
    `Median list` = scales::dollar(rdc_median_list),
    DOM = rdc_dom,
    `Price reduced %` = scales::percent(rdc_price_reduced_share, accuracy = 0.1)
  ) %>%
  print(n = 10, width = 250)
