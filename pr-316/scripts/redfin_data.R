#!/usr/bin/env Rscript
# redfin_data.R
# Pulls Redfin's county-level Market Tracker (gzipped TSV, ~230MB compressed),
# filters to the latest monthly snapshot of "All Residential" properties, and
# writes a tidy per-county CSV keyed on 5-digit county FIPS.
#
# Source: https://www.redfin.com/news/data-center/
# Re-run weekly/monthly. Update frequency: monthly, 3rd Friday.
#
# Usage:
#   Rscript scripts/redfin_data.R

`%>%` <- magrittr::`%>%`

cat("=== Redfin County Market Tracker ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

url <- "https://redfin-public-data.s3.us-west-2.amazonaws.com/redfin_market_tracker/county_market_tracker.tsv000.gz"
gz_cache <- file.path(data_dir, "redfin_county_market_tracker.tsv.gz")

# Check cache age (refresh if older than 7 days)
if (!file.exists(gz_cache) || difftime(Sys.time(), file.info(gz_cache)$mtime, units = "days") > 7) {
  cat("Downloading Redfin county market tracker (~230 MB)...\n")
  utils::download.file(url, gz_cache, mode = "wb", quiet = FALSE)
} else {
  cat("Using cached file:", gz_cache, "\n")
}

# Stream-read; we only care about latest monthly snapshot
cat("Parsing TSV...\n")
redfin_all <- readr::read_tsv(
  gz_cache,
  show_col_types = FALSE,
  na = c("", "NA"),
  guess_max = 50000
)
names(redfin_all) <- tolower(names(redfin_all))
cat("  Rows loaded:", nrow(redfin_all), "\n")

# Filter: all-residential, monthly (not rolling), latest period
latest_period <- redfin_all %>%
  dplyr::filter(
    property_type == "All Residential",
    period_duration %in% c(30, 31)
  ) %>%
  dplyr::pull(period_end) %>%
  max(na.rm = TRUE)

cat("  Latest monthly snapshot:", as.character(latest_period), "\n")

redfin_latest <- redfin_all %>%
  dplyr::filter(
    property_type == "All Residential",
    period_duration %in% c(30, 31),
    period_end == latest_period
  )

# Parse the geo fields. `region` looks like "Los Angeles County, CA" for counties.
# `state_code` is the 2-letter abbrev. Need the 5-digit county FIPS.
# Redfin includes `region` but not FIPS directly; derive from table_id when present,
# else via a county-name -> FIPS lookup downloaded once.

# Use the Census FIPS crosswalk
fips_cache <- file.path(data_dir, "national_county2024.txt")
if (!file.exists(fips_cache)) {
  utils::download.file(
    "https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt",
    fips_cache, quiet = TRUE, mode = "wb"
  )
}
fips_raw <- readr::read_delim(fips_cache, delim = "|", show_col_types = FALSE,
                               col_types = readr::cols(.default = readr::col_character()))
fips_lookup <- fips_raw %>%
  dplyr::mutate(
    fips = paste0(STATEFP, COUNTYFP),
    county_clean = stringr::str_remove(COUNTYNAME, "\\s+(County|Parish|Borough|Census Area|Municipality|City and Borough)$") %>%
      stringr::str_trim() %>%
      tolower()
  ) %>%
  dplyr::select(state = STATE, county_clean, fips)

redfin_clean <- redfin_latest %>%
  dplyr::transmute(
    period_end = as.Date(period_end),
    region = region,
    state = state_code,
    county_clean = stringr::str_remove(region, ",.*$") %>%
      stringr::str_remove("\\s+(County|Parish|Borough|Census Area|Municipality|City and Borough)$") %>%
      stringr::str_trim() %>%
      tolower(),
    median_sale_price, median_list_price,
    median_sale_price_yoy = median_sale_price_yoy,
    median_list_price_yoy = median_list_price_yoy,
    median_dom = median_dom,
    median_dom_yoy,
    inventory,
    inventory_yoy,
    new_listings,
    homes_sold,
    months_of_supply,
    avg_sale_to_list,
    sold_above_list,
    price_drops,
    off_market_in_two_weeks
  ) %>%
  dplyr::left_join(fips_lookup, by = c("state", "county_clean"))

n_matched <- sum(!is.na(redfin_clean$fips))
cat(sprintf("  FIPS matched: %d of %d counties\n", n_matched, nrow(redfin_clean)))

# Collapse duplicates (shouldn't happen, but guard)
redfin_out <- redfin_clean %>%
  dplyr::filter(!is.na(fips)) %>%
  dplyr::group_by(fips) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    fips, county = region, state, period_end,
    redfin_median_sale = median_sale_price,
    redfin_median_list = median_list_price,
    redfin_sale_yoy_pct = median_sale_price_yoy,
    redfin_list_yoy_pct = median_list_price_yoy,
    redfin_dom = median_dom,
    redfin_dom_yoy = median_dom_yoy,
    redfin_inventory = inventory,
    redfin_inventory_yoy = inventory_yoy,
    redfin_new_listings = new_listings,
    redfin_homes_sold = homes_sold,
    redfin_months_supply = months_of_supply,
    redfin_sale_to_list = avg_sale_to_list,
    redfin_sold_above_list = sold_above_list,
    redfin_price_drops = price_drops,
    redfin_off_market_2wk = off_market_in_two_weeks
  )

readr::write_csv(redfin_out, file.path(data_dir, "redfin_county.csv"))

cat(sprintf(
  "\nOutput: data/redfin_county.csv (%d counties, period %s)\n",
  nrow(redfin_out), as.character(latest_period)
))
cat("\n=== Top 10 by sale-to-list ratio ===\n")
redfin_out %>%
  dplyr::arrange(dplyr::desc(redfin_sale_to_list)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::transmute(
    County = stringr::str_trunc(county, 35),
    State = state,
    `Median Sale` = scales::dollar(redfin_median_sale),
    `DOM` = redfin_dom,
    `Sale/List` = scales::percent(redfin_sale_to_list, accuracy = 0.1),
    `% above list` = scales::percent(redfin_sold_above_list, accuracy = 0.1),
    `Price drops` = scales::percent(redfin_price_drops, accuracy = 0.1)
  ) %>%
  print(n = 10, width = 250)
