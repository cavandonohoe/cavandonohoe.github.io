#!/usr/bin/env Rscript
#
# la_real_estate_data.R
# Downloads Zillow public research data and assembles LA neighborhood dataset.
#
# Data sources:
#   1. Zillow ZHVI (neighborhood-level): typical home values
#      https://www.zillow.com/research/data/
#      CSV: Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv
#
#   2. Zillow ZORI (zip-level): typical observed rents
#      CSV: Zip_zori_uc_sfrcondomfr_sm_month.csv
#
#   3. Manual supplement: price_per_sqft and Redfin median sale prices
#      Source URLs documented in data/la_neighborhoods_redfin.csv
#      These are not available as bulk downloads and must be updated manually.
#
# Usage:
#   Rscript scripts/la_real_estate_data.R
#
# Output:
#   data/la_neighborhoods.csv          -- final merged dataset
#   data/la_zhvi_raw.csv               -- Zillow ZHVI extract for LA neighborhoods
#   data/la_zori_raw.csv               -- Zillow ZORI extract for LA zip codes

`%>%` <- magrittr::`%>%`

cat("=== LA Real Estate Data Update ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. Download and filter Zillow ZHVI (neighborhood-level home values)
# ---------------------------------------------------------------------------
cat("Downloading Zillow ZHVI neighborhood data...\n")
zhvi_url <- paste0(
  "https://files.zillowstatic.com/research/public_csvs/zhvi/",
  "Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
)

zhvi_raw <- readr::read_csv(zhvi_url, show_col_types = FALSE)

la_neighborhoods_zillow <- c(
  "Bel Air", "Beverly Crest", "Holmby Hills",
  "Hollywood Hills West", "Pacific Palisades",
  "Venice", "Cheviot Hills", "Silver Lake", "West Hollywood",
  "Hancock Park", "Studio City", "Sherman Oaks",
  "Echo Park", "Atwater Village", "Highland Park", "Eagle Rock",
  "Encino", "Westchester",
  "North Hollywood", "Boyle Heights", "El Sereno",
  "Koreatown", "South Los Angeles", "Pico-Union",
  "Historic South Central", "South Park"
)

zhvi_la <- zhvi_raw %>%
  dplyr::filter(
    City == "Los Angeles",
    State == "CA",
    RegionName %in% la_neighborhoods_zillow
  )

date_cols <- names(zhvi_la) %>%
  stringr::str_subset("^\\d{4}-\\d{2}-\\d{2}$")
latest_date <- max(date_cols)

zhvi_la_latest <- zhvi_la %>%
  dplyr::select(
    region_id = RegionID,
    neighborhood = RegionName,
    dplyr::all_of(latest_date)
  ) %>%
  dplyr::rename(zhvi = !!latest_date) %>%
  dplyr::mutate(zhvi_date = latest_date)

cat("  Found", nrow(zhvi_la_latest), "LA neighborhoods in Zillow ZHVI\n")
cat("  Latest ZHVI date:", latest_date, "\n")

readr::write_csv(
  zhvi_la %>% dplyr::select(RegionID, RegionName, dplyr::all_of(tail(date_cols, 12))),
  file.path(data_dir, "la_zhvi_raw.csv")
)

# Also pull Beverly Hills and Manhattan Beach (separate cities, not LA neighborhoods)
zhvi_cities <- zhvi_raw %>%
  dplyr::filter(
    State == "CA",
    (City == "Beverly Hills" | City == "Manhattan Beach" |
       (City == "Malibu" & RegionName == "Point Dume") |
       (City == "Los Angeles" & RegionName == "Brentwood") |
       (City == "Santa Monica") |
       (City == "Los Angeles" & RegionName == "Culver City") |
       (City == "Pasadena") |
       (City == "Los Angeles" & RegionName == "Los Feliz"))
  )

# For Beverly Hills, Santa Monica, Culver City, Pasadena, Manhattan Beach:
# these are separate cities, so we need city-level ZHVI
zhvi_city_url <- paste0(
  "https://files.zillowstatic.com/research/public_csvs/zhvi/",
  "City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
)
cat("Downloading Zillow ZHVI city-level data...\n")
zhvi_city_raw <- readr::read_csv(zhvi_city_url, show_col_types = FALSE)

city_names <- c("Beverly Hills", "Manhattan Beach", "Culver City",
                "Pasadena", "Santa Monica", "Malibu")

zhvi_city_la <- zhvi_city_raw %>%
  dplyr::filter(State == "CA", RegionName %in% city_names) %>%
  dplyr::select(
    region_id = RegionID,
    neighborhood = RegionName,
    dplyr::all_of(latest_date)
  ) %>%
  dplyr::rename(zhvi = !!latest_date) %>%
  dplyr::mutate(zhvi_date = latest_date)

cat("  Found", nrow(zhvi_city_la), "LA-area cities in Zillow city ZHVI\n")

# Combine neighborhood + city ZHVI
zhvi_combined <- dplyr::bind_rows(zhvi_la_latest, zhvi_city_la)

# Rename to match our neighborhood labels
zhvi_combined <- zhvi_combined %>%
  dplyr::mutate(
    neighborhood = dplyr::case_when(
      neighborhood == "South Park" ~ "South Park (DTLA)",
      neighborhood == "Malibu" ~ "Malibu (Point Dume)",
      TRUE ~ neighborhood
    )
  )

# ---------------------------------------------------------------------------
# 2. Download and filter Zillow ZORI (zip-level rents)
# ---------------------------------------------------------------------------
cat("\nDownloading Zillow ZORI zip-level rent data...\n")
zori_url <- paste0(
  "https://files.zillowstatic.com/research/public_csvs/zori/",
  "Zip_zori_uc_sfrcondomfr_sm_month.csv"
)

zori_raw <- readr::read_csv(zori_url, show_col_types = FALSE)

# Map LA neighborhoods to their primary zip codes
neighborhood_zips <- tibble::tribble(
  ~neighborhood,            ~zip,
  "Bel Air",                "90077",
  "Beverly Crest",          "90210",
  "Beverly Hills",          "90210",
  "Holmby Hills",           "90024",
  "Malibu (Point Dume)",    "90265",
  "Hollywood Hills West",   "90046",
  "Pacific Palisades",      "90272",
  "Manhattan Beach",        "90266",
  "Brentwood",              "90049",
  "Santa Monica",           "90401",
  "Los Feliz",              "90027",
  "Venice",                 "90291",
  "Cheviot Hills",          "90064",
  "Silver Lake",            "90039",
  "West Hollywood",         "90069",
  "Hancock Park",           "90004",
  "Studio City",            "91604",
  "Sherman Oaks",           "91403",
  "Culver City",            "90232",
  "Pasadena",               "91101",
  "Echo Park",              "90026",
  "Atwater Village",        "90039",
  "Highland Park",          "90042",
  "Eagle Rock",             "90041",
  "Encino",                 "91316",
  "Westchester",            "90045",
  "North Hollywood",        "91601",
  "Boyle Heights",          "90033",
  "El Sereno",              "90032",
  "Koreatown",              "90020",
  "Eastside LA",            "90063",
  "South Los Angeles",      "90037",
  "Pico-Union",             "90006",
  "Historic South Central", "90011",
  "South Park (DTLA)",      "90015"
)

zori_date_cols <- names(zori_raw) %>%
  stringr::str_subset("^\\d{4}-\\d{2}-\\d{2}$")
zori_latest_date <- max(zori_date_cols)

zori_la <- zori_raw %>%
  dplyr::filter(as.character(RegionName) %in% neighborhood_zips$zip) %>%
  dplyr::select(
    zip = RegionName,
    dplyr::all_of(zori_latest_date)
  ) %>%
  dplyr::rename(zori = !!zori_latest_date) %>%
  dplyr::mutate(
    zip = as.character(zip),
    zori_date = zori_latest_date
  )

zori_merged <- neighborhood_zips %>%
  dplyr::left_join(zori_la, by = "zip")

cat("  Latest ZORI date:", zori_latest_date, "\n")
cat("  Matched", sum(!is.na(zori_merged$zori)), "of", nrow(zori_merged),
    "neighborhoods to zip-level rent data\n")

readr::write_csv(
  zori_merged,
  file.path(data_dir, "la_zori_raw.csv")
)

# ---------------------------------------------------------------------------
# 3. Read manual supplement (Redfin $/sqft + median sale prices)
# ---------------------------------------------------------------------------
redfin_file <- file.path(data_dir, "la_neighborhoods_redfin.csv")

if (!file.exists(redfin_file)) {
  cat("\nWARNING: Manual Redfin data not found at", redfin_file, "\n")
  cat("  Creating template...\n")

  redfin_template <- neighborhood_zips %>%
    dplyr::select(neighborhood) %>%
    dplyr::mutate(
      redfin_median_price = NA_real_,
      redfin_price_per_sqft = NA_real_,
      redfin_url = NA_character_,
      redfin_date = NA_character_,
      notes = NA_character_
    )
  readr::write_csv(redfin_template, redfin_file)
  cat("  Template written. Fill in Redfin data and re-run.\n")
}

redfin_data <- readr::read_csv(redfin_file, show_col_types = FALSE)
cat("\nLoaded Redfin manual data:",
    sum(!is.na(redfin_data$redfin_price_per_sqft)), "neighborhoods with $/sqft\n")

# ---------------------------------------------------------------------------
# 4. Merge everything into final dataset
# ---------------------------------------------------------------------------
cat("\nAssembling final dataset...\n")

la_final <- neighborhood_zips %>%
  dplyr::left_join(
    zhvi_combined %>% dplyr::select(neighborhood, zhvi, zhvi_date),
    by = "neighborhood"
  ) %>%
  dplyr::left_join(
    zori_merged %>% dplyr::select(neighborhood, zori, zori_date),
    by = "neighborhood"
  ) %>%
  dplyr::left_join(
    redfin_data %>%
      dplyr::select(neighborhood, redfin_median_price, redfin_price_per_sqft,
                    redfin_url, redfin_date),
    by = "neighborhood"
  ) %>%
  dplyr::mutate(
    median_price = dplyr::coalesce(redfin_median_price, zhvi),
    price_source = dplyr::case_when(
      !is.na(redfin_median_price) ~ "redfin",
      !is.na(zhvi) ~ "zillow_zhvi",
      TRUE ~ NA_character_
    ),
    price_per_sqft = redfin_price_per_sqft,
    typical_sqft = dplyr::if_else(
      !is.na(median_price) & !is.na(price_per_sqft) & price_per_sqft > 0,
      round(median_price / price_per_sqft),
      NA_integer_
    ),
    median_rent = zori,
    tier = dplyr::case_when(
      median_price > 3e6  ~ "Ultra-Luxury",
      median_price > 2e6  ~ "Luxury",
      median_price > 1.3e6 ~ "Upper-Mid",
      median_price > 9e5  ~ "Mid-Range",
      TRUE                ~ "Affordable"
    )
  ) %>%
  dplyr::select(
    neighborhood, zip, tier,
    median_price, price_source,
    zhvi, zhvi_date,
    price_per_sqft, redfin_url, redfin_date,
    typical_sqft,
    median_rent, zori_date
  ) %>%
  dplyr::arrange(dplyr::desc(price_per_sqft))

readr::write_csv(la_final, file.path(data_dir, "la_neighborhoods.csv"))

cat("\n=== Output ===\n")
cat("  data/la_neighborhoods.csv      -", nrow(la_final), "neighborhoods\n")
cat("  data/la_zhvi_raw.csv           - Zillow ZHVI extract (last 12 months)\n")
cat("  data/la_zori_raw.csv           - Zillow ZORI by zip\n")
cat("  data/la_neighborhoods_redfin.csv - Manual Redfin supplement\n")

cat("\n=== Summary ===\n")
la_final %>%
  dplyr::transmute(
    Neighborhood = neighborhood,
    Tier = tier,
    `Median Price` = scales::dollar(median_price),
    `$/sqft` = dplyr::if_else(
      is.na(price_per_sqft), "---",
      scales::dollar(price_per_sqft, accuracy = 1)
    ),
    `Typical sqft` = dplyr::if_else(
      is.na(typical_sqft), "---",
      scales::comma(typical_sqft)
    ),
    `Rent (ZORI)` = dplyr::if_else(
      is.na(median_rent), "---",
      scales::dollar(median_rent, accuracy = 1)
    ),
    Source = price_source
  ) %>%
  print(n = 40, width = 200)

cat("\nDone. To update:\n")
cat("  1. Re-run this script to refresh Zillow data\n")
cat("  2. Update data/la_neighborhoods_redfin.csv with latest Redfin numbers\n")
cat("  3. Re-render la_real_estate.Rmd\n")
