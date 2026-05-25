#!/usr/bin/env Rscript
#
# us_rental_markets_data.R
# Downloads Zillow ZHVI + ZORI at city level and computes rental investment metrics.
#
# Data sources (all public Zillow Research CSVs, updated monthly):
#   - ZHVI: City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv
#   - ZORI: City_zori_uc_sfrcondomfr_sm_month.csv
#
# Output:
#   data/us_rental_markets.csv -- every US city with both home value + rent data
#
# Usage:
#   Rscript scripts/us_rental_markets_data.R

`%>%` <- magrittr::`%>%`

cat("=== US Rental Markets Data Update ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. Download Zillow ZHVI (city-level home values)
# ---------------------------------------------------------------------------
cat("Downloading Zillow ZHVI (city-level)...\n")
zhvi_url <- paste0(
  "https://files.zillowstatic.com/research/public_csvs/zhvi/",
  "City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
)
zhvi <- readr::read_csv(zhvi_url, show_col_types = FALSE)

date_cols <- names(zhvi) %>% stringr::str_subset("^\\d{4}-\\d{2}-\\d{2}$")
latest_date <- max(date_cols)
one_year_ago <- as.character(as.Date(latest_date) - 365)
one_year_col <- date_cols[which.min(abs(as.Date(date_cols) - as.Date(one_year_ago)))]

cat("  Rows:", nrow(zhvi), "| Latest:", latest_date, "\n")

zhvi_slim <- zhvi %>%
  dplyr::select(
    region_id = RegionID, city = RegionName, state = StateName,
    metro = Metro, county = CountyName,
    zhvi_current = dplyr::all_of(latest_date),
    zhvi_1yr_ago = dplyr::all_of(one_year_col)
  ) %>%
  dplyr::mutate(
    zhvi_yoy_pct = round(100 * (zhvi_current - zhvi_1yr_ago) / zhvi_1yr_ago, 2)
  )

# ---------------------------------------------------------------------------
# 2. Download Zillow ZORI (city-level rents)
# ---------------------------------------------------------------------------
cat("Downloading Zillow ZORI (city-level)...\n")
zori_url <- paste0(
  "https://files.zillowstatic.com/research/public_csvs/zori/",
  "City_zori_uc_sfrcondomfr_sm_month.csv"
)
zori <- readr::read_csv(zori_url, show_col_types = FALSE)

zori_date_cols <- names(zori) %>% stringr::str_subset("^\\d{4}-\\d{2}-\\d{2}$")
zori_latest <- max(zori_date_cols)
zori_1yr_col <- zori_date_cols[which.min(
  abs(as.Date(zori_date_cols) - (as.Date(zori_latest) - 365))
)]

cat("  Rows:", nrow(zori), "| Latest:", zori_latest, "\n")

zori_slim <- zori %>%
  dplyr::select(
    region_id = RegionID, city = RegionName,
    zori_current = dplyr::all_of(zori_latest),
    zori_1yr_ago = dplyr::all_of(zori_1yr_col)
  ) %>%
  dplyr::mutate(
    zori_yoy_pct = round(100 * (zori_current - zori_1yr_ago) / zori_1yr_ago, 2)
  )

# ---------------------------------------------------------------------------
# 3. Merge and compute investment metrics
# ---------------------------------------------------------------------------
cat("\nMerging ZHVI + ZORI...\n")

markets <- zhvi_slim %>%
  dplyr::inner_join(
    zori_slim %>% dplyr::select(region_id, zori_current, zori_1yr_ago, zori_yoy_pct),
    by = "region_id"
  ) %>%
  dplyr::filter(!is.na(zhvi_current), !is.na(zori_current),
                zhvi_current > 0, zori_current > 0) %>%
  dplyr::mutate(
    annual_rent = zori_current * 12,
    rent_to_price_pct = round(100 * zori_current / zhvi_current, 3),
    price_to_rent = round(zhvi_current / annual_rent, 1),
    gross_yield_pct = round(100 * annual_rent / zhvi_current, 2),
    # Estimated cap rate: gross yield minus ~35% for expenses
    # (taxes ~1%, insurance ~0.5%, vacancy ~5%, maintenance ~8%, mgmt ~8%)
    est_expense_ratio = 0.35,
    est_noi = annual_rent * (1 - est_expense_ratio),
    est_cap_rate_pct = round(100 * est_noi / zhvi_current, 2),
    sqft_per_million = round(1e6 / zhvi_current * (zhvi_current / zori_current / 12)),
    tier = dplyr::case_when(
      price_to_rent < 15 ~ "Strong Buy",
      price_to_rent < 20 ~ "Moderate",
      price_to_rent < 25 ~ "Lean Rent",
      TRUE ~ "Rent Favored"
    ),
    zhvi_date = latest_date,
    zori_date = zori_latest
  ) %>%
  dplyr::select(
    city, state, metro, county,
    zhvi = zhvi_current, zhvi_yoy_pct,
    zori = zori_current, zori_yoy_pct,
    annual_rent,
    rent_to_price_pct, price_to_rent, gross_yield_pct, est_cap_rate_pct,
    tier,
    zhvi_date, zori_date
  ) %>%
  dplyr::arrange(dplyr::desc(est_cap_rate_pct))

cat("  Cities with both ZHVI + ZORI:", nrow(markets), "\n")

readr::write_csv(markets, file.path(data_dir, "us_rental_markets.csv"))

# ---------------------------------------------------------------------------
# 4. Summary output
# ---------------------------------------------------------------------------
cat("\n=== Top 30 Cities by Estimated Cap Rate ===\n")
markets %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::transmute(
    `#` = dplyr::row_number(),
    City = city, State = state,
    `Home Value` = scales::dollar(zhvi),
    `Rent/Mo` = scales::dollar(zori, accuracy = 1),
    `Gross Yield` = paste0(gross_yield_pct, "%"),
    `Est Cap Rate` = paste0(est_cap_rate_pct, "%"),
    `Rent/Price` = paste0(rent_to_price_pct, "%"),
    `P/R Ratio` = price_to_rent,
    `ZHVI YoY` = paste0(zhvi_yoy_pct, "%"),
    `ZORI YoY` = paste0(zori_yoy_pct, "%"),
    Tier = tier
  ) %>%
  print(n = 30, width = 250)

cat("\n=== Bottom 15 (Rent Favored / Appreciation Plays) ===\n")
markets %>%
  dplyr::slice_tail(n = 15) %>%
  dplyr::transmute(
    City = city, State = state,
    `Home Value` = scales::dollar(zhvi),
    `Rent/Mo` = scales::dollar(zori, accuracy = 1),
    `Gross Yield` = paste0(gross_yield_pct, "%"),
    `P/R Ratio` = price_to_rent,
    Tier = tier
  ) %>%
  print(n = 15, width = 200)

cat("\n=== Tier Distribution ===\n")
markets %>%
  dplyr::count(tier) %>%
  print()

cat("\nOutput: data/us_rental_markets.csv (", nrow(markets), " cities)\n")
cat("Sources: Zillow ZHVI (", latest_date, ") + ZORI (", zori_latest, ")\n")
cat("To refresh: Rscript scripts/us_rental_markets_data.R\n")
