#!/usr/bin/env Rscript
# bls_laus_data.R
# Pulls BLS Local Area Unemployment Statistics (LAUS) at the metro and county
# level. Unemployment rate + 12-month change is a strong demand-side signal:
# rental demand tracks job market health. Writes two CSVs (metro, county).
#
# Source: https://download.bls.gov/pub/time.series/la/
# No key, but BLS requires a real User-Agent (email) or returns 403.
#
# Usage:
#   Rscript scripts/bls_laus_data.R

`%>%` <- magrittr::`%>%`

cat("=== BLS LAUS (Local Area Unemployment Statistics) ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

UA <- "cavandonohoe-website (cdonohoe@grailbio.com) R/httr2"
base <- "https://download.bls.gov/pub/time.series/la/"

fetch_bls <- function(file, cache) {
  if (file.exists(cache) && difftime(Sys.time(), file.info(cache)$mtime, units = "days") < 7) {
    cat("  Using cache:", basename(cache), "\n")
    return(readr::read_tsv(cache, show_col_types = FALSE, trim_ws = TRUE,
                            na = c("", "NA", "-")))
  }
  cat("  Fetching:", file, "\n")
  resp <- httr2::request(paste0(base, file)) |>
    httr2::req_user_agent(UA) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  txt <- httr2::resp_body_string(resp)
  writeLines(txt, cache)
  readr::read_tsv(cache, show_col_types = FALSE, trim_ws = TRUE, na = c("", "NA", "-"))
}

series <- fetch_bls("la.series", file.path(data_dir, "bls_la_series.tsv"))
area   <- fetch_bls("la.area",   file.path(data_dir, "bls_la_area.tsv"))
metro  <- fetch_bls("la.data.60.Metro",  file.path(data_dir, "bls_la_metro.tsv"))
county <- fetch_bls("la.data.64.County", file.path(data_dir, "bls_la_county.tsv"))

cat("\nSeries rows:", nrow(series), "  Area rows:", nrow(area),
    "  Metro data rows:", nrow(metro), "  County data rows:", nrow(county), "\n")

# Measure code 03 = unemployment rate. Series ID positions 19-20 hold the measure.
# Filter out annual average (period M13), keep only monthly (M01-M12).
# Take the latest month per series.

latest_by_series <- function(df) {
  df %>%
    dplyr::filter(stringr::str_sub(series_id, 19, 20) == "03",
                  period != "M13") %>%
    dplyr::mutate(
      year = as.integer(year),
      month = as.integer(stringr::str_sub(period, 2, 3)),
      date = as.Date(sprintf("%d-%02d-01", year, month))
    ) %>%
    dplyr::group_by(series_id) %>%
    dplyr::filter(date == max(date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(series_id, date, unemp_rate = value)
}

# We also want the value from ~12 months prior for YoY change.
yoy_by_series <- function(df) {
  latest <- df %>%
    dplyr::filter(stringr::str_sub(series_id, 19, 20) == "03",
                  period != "M13") %>%
    dplyr::mutate(
      year = as.integer(year),
      month = as.integer(stringr::str_sub(period, 2, 3)),
      date = as.Date(sprintf("%d-%02d-01", year, month))
    )

  # Pick the latest month per series (series publish at slightly different lags)
  current <- latest %>%
    dplyr::group_by(series_id) %>%
    dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(series_id, unemp_rate = value, date)

  # Join a 12-month-prior value per series (same calendar month, prior year)
  prior <- latest %>%
    dplyr::inner_join(
      current %>% dplyr::transmute(series_id, target_date = date - 365),
      by = "series_id"
    ) %>%
    dplyr::mutate(diff = abs(as.numeric(date - target_date))) %>%
    dplyr::filter(diff <= 45) %>%
    dplyr::group_by(series_id) %>%
    dplyr::slice_min(diff, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(series_id, prior_rate = value)

  dplyr::left_join(current, prior, by = "series_id") %>%
    dplyr::mutate(unemp_yoy_pp = round(unemp_rate - prior_rate, 2))
}

cat("\nProcessing metro series...\n")
metro_latest <- yoy_by_series(metro) %>%
  dplyr::left_join(
    dplyr::select(series, series_id, area_code, seasonal, series_title),
    by = "series_id"
  ) %>%
  # Prefer not-seasonally-adjusted (U) for consistency across small metros
  dplyr::filter(seasonal == "U") %>%
  dplyr::left_join(dplyr::select(area, area_code, area_text), by = "area_code") %>%
  dplyr::transmute(
    bls_area_code = area_code,
    metro_name = area_text,
    bls_date = date,
    bls_unemp_rate = unemp_rate,
    bls_unemp_yoy_pp = unemp_yoy_pp
  ) %>%
  dplyr::distinct(metro_name, .keep_all = TRUE)

readr::write_csv(metro_latest, file.path(data_dir, "bls_laus_metro.csv"))
cat(sprintf("  Written: data/bls_laus_metro.csv (%d metros, date %s)\n",
            nrow(metro_latest), as.character(max(metro_latest$bls_date, na.rm = TRUE))))

cat("\nProcessing county series...\n")
# County area codes in la.area look like "CN0603700000000" — the "CN" + 5 digits
# is the state+county FIPS.
county_latest <- yoy_by_series(county) %>%
  dplyr::left_join(
    dplyr::select(series, series_id, area_code, seasonal),
    by = "series_id"
  ) %>%
  dplyr::filter(seasonal == "U") %>%
  dplyr::left_join(dplyr::select(area, area_code, area_text), by = "area_code") %>%
  dplyr::transmute(
    fips = stringr::str_sub(area_code, 3, 7),
    county_name = area_text,
    bls_date = date,
    bls_unemp_rate = unemp_rate,
    bls_unemp_yoy_pp = unemp_yoy_pp
  ) %>%
  dplyr::filter(nchar(fips) == 5) %>%
  dplyr::distinct(fips, .keep_all = TRUE)

readr::write_csv(county_latest, file.path(data_dir, "bls_laus_county.csv"))
cat(sprintf("  Written: data/bls_laus_county.csv (%d counties, date %s)\n",
            nrow(county_latest), as.character(max(county_latest$bls_date, na.rm = TRUE))))

cat("\n=== 10 highest unemployment counties (latest month) ===\n")
county_latest %>%
  dplyr::arrange(dplyr::desc(bls_unemp_rate)) %>%
  dplyr::slice_head(n = 10) %>%
  print(n = 10, width = 200)

cat("\n=== 10 lowest unemployment metros (latest month) ===\n")
metro_latest %>%
  dplyr::arrange(bls_unemp_rate) %>%
  dplyr::slice_head(n = 10) %>%
  print(n = 10, width = 200)
