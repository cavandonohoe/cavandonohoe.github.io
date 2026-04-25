#!/usr/bin/env Rscript
# zillow_zip_data.R
# Pulls ZIP-level ZHVI + ZORI from Zillow Research and computes the same
# leverage / yield metrics the city-level pipeline uses, one row per ZIP.
# Also joins Census ZCTA lat/lon (via the 2024 Gazetteer ZCTA file) so the map
# can plot ZIPs, and computes nearest Trader Joe's distance per ZIP.
#
# Output: data/us_rental_markets_zip.csv  -  ready to power a
# ZIP-level drill-down section on the site.
#
# Usage:
#   Rscript scripts/zillow_zip_data.R

`%>%` <- magrittr::`%>%`

cat("=== Zillow ZIP-level markets ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

zhvi_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
zori_url <- "https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_uc_sfrcondomfr_sm_month.csv"

cat("Fetching ZIP ZHVI (~121 MB)...\n")
zhvi_raw <- readr::read_csv(zhvi_url, show_col_types = FALSE)
cat("  Rows:", nrow(zhvi_raw), "\n")

cat("Fetching ZIP ZORI (~9 MB)...\n")
zori_raw <- readr::read_csv(zori_url, show_col_types = FALSE)
cat("  Rows:", nrow(zori_raw), "\n")

date_cols <- function(df) {
  names(df)[stringr::str_detect(names(df), "^\\d{4}-\\d{2}-\\d{2}$")]
}

melt_zillow <- function(df, value_col) {
  dc <- date_cols(df)
  df %>%
    dplyr::mutate(zip = stringr::str_pad(as.character(RegionName), 5, pad = "0")) %>%
    dplyr::select(zip, City, State, Metro, CountyName, dplyr::all_of(dc)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(dc), names_to = "date",
                        values_to = value_col, values_drop_na = TRUE) %>%
    dplyr::mutate(date = as.Date(date))
}

zhvi_long <- melt_zillow(zhvi_raw, "zhvi")
zori_long <- melt_zillow(zori_raw, "zori")

# Latest month per ZIP
zhvi_latest <- zhvi_long %>%
  dplyr::group_by(zip) %>%
  dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(zhvi_latest = zhvi, zhvi_date = date)

# 1-year-ago ZHVI for YoY appreciation
zhvi_yoy <- zhvi_long %>%
  dplyr::inner_join(zhvi_latest %>% dplyr::select(zip, zhvi_date),
                    by = "zip") %>%
  dplyr::mutate(diff = abs(as.numeric(date - (zhvi_date - 365)))) %>%
  dplyr::group_by(zip) %>%
  dplyr::slice_min(diff, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(zip, zhvi_1y_ago = zhvi)

zori_latest <- zori_long %>%
  dplyr::group_by(zip) %>%
  dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(zori_latest = zori, zori_date = date)

zip_base <- zhvi_latest %>%
  dplyr::select(zip, city = City, state = State, metro = Metro,
                county = CountyName, zhvi_latest, zhvi_date) %>%
  dplyr::left_join(zhvi_yoy, by = "zip") %>%
  dplyr::left_join(zori_latest %>% dplyr::select(zip, zori_latest, zori_date),
                   by = "zip") %>%
  dplyr::mutate(
    zhvi_yoy_pct = round(100 * (zhvi_latest - zhvi_1y_ago) / zhvi_1y_ago, 2),
    gross_yield_pct = round(100 * 12 * zori_latest / zhvi_latest, 2)
  )

cat("  ZIPs with both ZHVI+ZORI:", sum(!is.na(zip_base$zori_latest)), "\n")

# Leverage math: use the same 25% down / 6.5% rate / 30y / 1% tax / etc.
# assumptions as the city pipeline. Keep this self-contained so ZIP-level
# numbers are comparable.
DOWN_PCT <- 0.25
MORTGAGE_RATE <- 0.065
TERM_YRS <- 30
PROP_TAX_PCT <- 0.010
INSURANCE_PCT <- 0.005
MAINT_PCT <- 0.010
MGMT_PCT_OF_RENT <- 0.08
VACANCY_LOSS_PCT <- 0.05
CLOSING_COSTS_PCT <- 0.03

pmt_monthly <- function(P, r_annual, n_years) {
  r <- r_annual / 12
  n <- n_years * 12
  ifelse(r == 0, P / n, P * r * (1 + r)^n / ((1 + r)^n - 1))
}

zip_leverage <- zip_base %>%
  dplyr::filter(!is.na(zori_latest), !is.na(zhvi_latest),
                gross_yield_pct > 0, gross_yield_pct <= 20) %>%
  dplyr::mutate(
    down_payment = DOWN_PCT * zhvi_latest,
    loan_amount = (1 - DOWN_PCT) * zhvi_latest,
    mortgage_monthly = pmt_monthly(loan_amount, MORTGAGE_RATE, TERM_YRS),
    tax_monthly = (PROP_TAX_PCT * zhvi_latest) / 12,
    insurance_monthly = (INSURANCE_PCT * zhvi_latest) / 12,
    maint_monthly = (MAINT_PCT * zhvi_latest) / 12,
    mgmt_monthly = MGMT_PCT_OF_RENT * zori_latest,
    vacancy_monthly = VACANCY_LOSS_PCT * zori_latest,
    total_expense_monthly = mortgage_monthly + tax_monthly + insurance_monthly +
      maint_monthly + mgmt_monthly + vacancy_monthly,
    cash_flow_monthly = zori_latest - total_expense_monthly,
    cash_invested = down_payment + CLOSING_COSTS_PCT * zhvi_latest,
    cash_on_cash_pct = round(100 * 12 * cash_flow_monthly / cash_invested, 2),
    noi_monthly = zori_latest * (1 - VACANCY_LOSS_PCT - MGMT_PCT_OF_RENT) -
      tax_monthly - insurance_monthly - maint_monthly,
    cap_rate_pct = round(100 * 12 * noi_monthly / zhvi_latest, 2)
  )

cat("  ZIPs passing yield gate:", nrow(zip_leverage), "\n")

# --- ZCTA centroid coords from 2024 Gazetteer ---
gaz_url <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_zcta_national.zip"
gaz_cache <- file.path(data_dir, "2024_Gaz_zcta_national.txt")
if (!file.exists(gaz_cache)) {
  cat("Downloading Gazetteer ZCTA file...\n")
  tmp_zip <- tempfile(fileext = ".zip")
  utils::download.file(gaz_url, tmp_zip, mode = "wb", quiet = TRUE)
  utils::unzip(tmp_zip, exdir = data_dir)
}
gaz <- readr::read_tsv(gaz_cache, show_col_types = FALSE, trim_ws = TRUE)
# Gazetteer ZCTA columns: GEOID (5-digit ZCTA), INTPTLAT, INTPTLONG
names(gaz) <- tolower(stringr::str_trim(names(gaz)))
gaz_lookup <- gaz %>%
  dplyr::transmute(
    zip = stringr::str_pad(as.character(geoid), 5, pad = "0"),
    zip_lat = as.numeric(intptlat),
    zip_lon = as.numeric(intptlong)
  )

zip_leverage <- zip_leverage %>%
  dplyr::left_join(gaz_lookup, by = "zip")

# --- Nearest Trader Joe's per ZIP ---
tj <- readr::read_csv(file.path(data_dir, "trader_joes_locations.csv"),
                       show_col_types = FALSE) %>%
  dplyr::filter(!is.na(lat), !is.na(lon))

haversine_mi <- function(lat1, lon1, lat2, lon2) {
  R <- 3958.8
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

nearest_tj_for <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return(list(miles = NA_real_, idx = NA_integer_))
  d <- haversine_mi(lat, lon, tj$lat, tj$lon)
  i <- which.min(d)
  list(miles = d[i], idx = i)
}

cat("Computing nearest Trader Joe's per ZIP...\n")
tj_result <- purrr::map2(zip_leverage$zip_lat, zip_leverage$zip_lon, nearest_tj_for)
zip_leverage$tj_miles <- purrr::map_dbl(tj_result, "miles")
tj_idx <- purrr::map_int(tj_result, "idx")
zip_leverage$nearest_tj_city  <- ifelse(is.na(tj_idx), NA_character_, tj$city[tj_idx])
zip_leverage$nearest_tj_state <- ifelse(is.na(tj_idx), NA_character_, tj$state[tj_idx])

zip_leverage <- zip_leverage %>%
  dplyr::mutate(
    tj_within_2mi = ifelse(is.na(tj_miles), NA, tj_miles <= 2),
    tj_within_5mi = ifelse(is.na(tj_miles), NA, tj_miles <= 5),
    tj_within_10mi = ifelse(is.na(tj_miles), NA, tj_miles <= 10)
  )

# Final output columns
zip_out <- zip_leverage %>%
  dplyr::transmute(
    zip, city, state, metro, county,
    zip_lat, zip_lon,
    zhvi_latest = round(zhvi_latest, 0),
    zori_latest = round(zori_latest, 0),
    zhvi_yoy_pct, gross_yield_pct,
    cap_rate_pct, cash_on_cash_pct,
    cash_flow_monthly = round(cash_flow_monthly, 0),
    total_expense_monthly = round(total_expense_monthly, 0),
    down_payment = round(down_payment, 0),
    tj_miles = round(tj_miles, 2),
    nearest_tj_city, nearest_tj_state,
    tj_within_2mi, tj_within_5mi, tj_within_10mi,
    zhvi_date, zori_date
  )

readr::write_csv(zip_out, file.path(data_dir, "us_rental_markets_zip.csv"))
cat(sprintf("\nOutput: data/us_rental_markets_zip.csv (%d ZIPs)\n", nrow(zip_out)))

cat("\n=== Top 10 ZIPs by cash-on-cash return ===\n")
zip_out %>%
  dplyr::arrange(dplyr::desc(cash_on_cash_pct)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::transmute(
    ZIP = zip, City = city, ST = state,
    `Price` = scales::dollar(zhvi_latest),
    `Rent` = scales::dollar(zori_latest),
    `CoC %` = cash_on_cash_pct,
    `Cap %` = cap_rate_pct,
    `TJ mi` = tj_miles
  ) %>%
  print(n = 10, width = 200)
