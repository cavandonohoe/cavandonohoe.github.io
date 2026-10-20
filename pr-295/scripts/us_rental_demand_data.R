#!/usr/bin/env Rscript
#
# us_rental_demand_data.R
# Pulls demand-side signals for rental markets: population growth, vacancy rates,
# net migration. Merges with us_rental_markets.csv to create a comprehensive dataset.
#
# Data sources (all public, no API key required):
#   - Census Population Estimates (2020-2024): city/town level from sub-est2024.csv
#   - Census ACS 5-Year (2023): vacancy rates at county level via Census API
#   - Census Population Estimates (2020-2024): county-level components of change
#
# Output:
#   data/us_rental_markets.csv -- updated with demand columns
#
# Usage:
#   Rscript scripts/us_rental_demand_data.R

`%>%` <- magrittr::`%>%`

cat("=== US Rental Markets: Demand Data ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
markets <- readr::read_csv(file.path(data_dir, "us_rental_markets.csv"), show_col_types = FALSE)
cat("Loaded", nrow(markets), "cities from us_rental_markets.csv\n\n")

# Drop any demand / leverage columns from a prior run so joins don't create .x/.y suffixes.
demand_cols <- c(
  "pop_2024", "pop_growth_pct", "county_pop_2024", "county_pop_growth_pct",
  "net_mig_rate_per1k", "vacancy_rate_pct", "rental_vacancy_pct", "renter_pct",
  "down_payment", "loan_amount", "monthly_mortgage",
  "monthly_cash_flow", "annual_cash_flow", "cash_on_cash_pct",
  "break_even_rent", "yield_spread_pct",
  "one_pct_rule", "two_pct_rule",
  "city_lat", "city_lon",
  "tj_miles", "nearest_tj_city", "nearest_tj_state",
  "tj_within_2mi", "tj_within_5mi", "tj_within_10mi", "tj_within_25mi",
  "investment_score", "hidden_gem", "tj_unicorn",
  "fips",
  # Redfin
  "redfin_median_sale", "redfin_median_list", "redfin_sale_yoy_pct",
  "redfin_list_yoy_pct", "redfin_dom", "redfin_dom_yoy", "redfin_inventory",
  "redfin_inventory_yoy", "redfin_new_listings", "redfin_homes_sold",
  "redfin_months_supply", "redfin_sale_to_list", "redfin_sold_above_list",
  "redfin_price_drops", "redfin_off_market_2wk",
  # HUD FMR
  "fmr_0br", "fmr_1br", "fmr_2br", "fmr_3br", "fmr_4br",
  "zori_vs_fmr_pct", "fmr_flag_outlier",
  # BLS
  "bls_unemp_rate", "bls_unemp_yoy_pp", "bls_date",
  # Realtor.com
  "rdc_median_list", "rdc_median_list_yoy", "rdc_dom",
  "rdc_active_listings", "rdc_new_listings", "rdc_pending_listings",
  "rdc_price_reduced_share", "rdc_pending_ratio",
  "rdc_hotness_score", "rdc_supply_score", "rdc_demand_score",
  "rdc_hotness_rank"
)
markets <- markets %>% dplyr::select(-dplyr::any_of(demand_cols))

# ---------------------------------------------------------------------------
# 1. Census city/town population estimates (2020-2024)
# ---------------------------------------------------------------------------
cat("Downloading Census city/town population estimates (2020-2024)...\n")
pop_url <- paste0(
  "https://www2.census.gov/programs-surveys/popest/datasets/",
  "2020-2024/cities/totals/sub-est2024.csv"
)
pop_raw <- readr::read_csv(pop_url, show_col_types = FALSE)
cat("  Rows:", nrow(pop_raw), "\n")

# SUMLEV 162 = incorporated places; 170 = consolidated cities
pop_cities <- pop_raw %>%
  dplyr::filter(SUMLEV %in% c(162, 170)) %>%
  dplyr::transmute(
    city = NAME,
    state_fips = sprintf("%02d", STNAME %>% match(state.name) %>% (\(x) ifelse(is.na(x), NA_integer_, x))()),
    state_name = STNAME,
    pop_2020 = POPESTIMATE2020,
    pop_2024 = POPESTIMATE2024,
    pop_change = pop_2024 - pop_2020,
    pop_growth_pct = round(100 * (pop_2024 - pop_2020) / pop_2020, 2)
  )

# Clean city names to match Zillow format (remove suffixes like "city", "town", "village")
pop_cities <- pop_cities %>%
  dplyr::mutate(
    city_clean = city %>%
      stringr::str_remove("\\s+(city|town|village|City|Town|Village|CDP|borough|Borough)$") %>%
      stringr::str_remove("\\s+(city and borough|municipality|Municipality)$") %>%
      stringr::str_trim()
  )

cat("  Incorporated places:", nrow(pop_cities), "\n")

# ---------------------------------------------------------------------------
# 2. Census ACS 5-Year: vacancy rates at county level
# ---------------------------------------------------------------------------
cat("\nDownloading ACS 5-Year vacancy data (county level)...\n")

# B25002_001E = total housing units
# B25002_003E = vacant housing units
# B25004_002E = vacant for rent
# B25003_002E = owner-occupied
# B25003_003E = renter-occupied
acs_vars <- "B25002_001E,B25002_003E,B25004_002E,B25003_002E,B25003_003E"
acs_url <- paste0(
  "https://api.census.gov/data/2023/acs/acs5?get=NAME,", acs_vars,
  "&for=county:*&in=state:*"
)

acs_json <- jsonlite::fromJSON(acs_url)
acs_df <- tibble::as_tibble(acs_json[-1, ], .name_repair = "minimal")
names(acs_df) <- acs_json[1, ]

acs_counties <- acs_df %>%
  dplyr::transmute(
    county_name = NAME,
    state_fips = state,
    county_fips = county,
    fips = paste0(state, county),
    total_housing = as.numeric(B25002_001E),
    vacant_housing = as.numeric(B25002_003E),
    vacant_for_rent = as.numeric(B25004_002E),
    owner_occupied = as.numeric(B25003_002E),
    renter_occupied = as.numeric(B25003_003E),
    vacancy_rate_pct = round(100 * vacant_housing / total_housing, 2),
    rental_vacancy_pct = round(
      100 * vacant_for_rent / (renter_occupied + vacant_for_rent), 2
    ),
    renter_pct = round(100 * renter_occupied / (owner_occupied + renter_occupied), 2)
  )

cat("  Counties:", nrow(acs_counties), "\n")

# ---------------------------------------------------------------------------
# 3. County-level population components of change
# ---------------------------------------------------------------------------
cat("\nDownloading county population components of change (2020-2024)...\n")
county_pop_url <- paste0(
  "https://www2.census.gov/programs-surveys/popest/datasets/",
  "2020-2024/counties/totals/co-est2024-alldata.csv"
)
county_pop <- readr::read_csv(county_pop_url, show_col_types = FALSE)

county_components <- county_pop %>%
  dplyr::filter(SUMLEV == "050") %>%
  dplyr::transmute(
    fips = paste0(
      stringr::str_pad(as.character(STATE), 2, pad = "0"),
      stringr::str_pad(as.character(COUNTY), 3, pad = "0")
    ),
    county_pop_2024 = POPESTIMATE2024,
    county_pop_2020 = POPESTIMATE2020,
    county_pop_growth_pct = round(100 * (POPESTIMATE2024 - POPESTIMATE2020) / POPESTIMATE2020, 2),
    net_domestic_mig_2024 = DOMESTICMIG2024,
    net_intl_mig_2024 = INTERNATIONALMIG2024,
    net_migration_2024 = DOMESTICMIG2024 + INTERNATIONALMIG2024,
    net_mig_rate_per1k = round(1000 * (DOMESTICMIG2024 + INTERNATIONALMIG2024) / POPESTIMATE2024, 2)
  )

cat("  Counties with components:", nrow(county_components), "\n")

# ---------------------------------------------------------------------------
# 4. Match Zillow cities to county FIPS via county name
# ---------------------------------------------------------------------------
cat("\nMatching Zillow cities to counties...\n")

# ACS county_name is like "Autauga County, Alabama"
# Zillow has county = "Autauga County" and state = "AL"
# Build lookup: extract state name from ACS, abbreviate it
state_abbr_lookup <- c(setNames(state.abb, state.name), "District of Columbia" = "DC")

county_fips_lookup <- acs_counties %>%
  dplyr::transmute(
    fips,
    county_from_acs = county_name %>%
      stringr::str_remove(",.*$") %>%
      stringr::str_trim(),
    state_abbr = county_name %>%
      stringr::str_extract("[^,]+$") %>%
      stringr::str_trim() %>%
      (\(x) state_abbr_lookup[x])()
  ) %>%
  dplyr::filter(!is.na(state_abbr))

markets_with_fips <- markets %>%
  dplyr::left_join(
    county_fips_lookup,
    by = c("county" = "county_from_acs", "state" = "state_abbr")
  )

matched <- sum(!is.na(markets_with_fips$fips))
cat("  Matched", matched, "of", nrow(markets), "cities to county FIPS\n")

# ---------------------------------------------------------------------------
# 5. Join county-level demand data
# ---------------------------------------------------------------------------
cat("\nMerging demand data...\n")

markets_demand <- markets_with_fips %>%
  dplyr::left_join(
    acs_counties %>% dplyr::select(fips, vacancy_rate_pct, rental_vacancy_pct, renter_pct),
    by = "fips"
  ) %>%
  dplyr::left_join(
    county_components %>% dplyr::select(
      fips, county_pop_2024, county_pop_growth_pct,
      net_migration_2024, net_mig_rate_per1k
    ),
    by = "fips"
  )

# Match city population -- add state_name for matching
state_name_lookup <- c(setNames(state.name, state.abb), "DC" = "District of Columbia")

markets_demand <- markets_demand %>%
  dplyr::mutate(state_name = state_name_lookup[state]) %>%
  dplyr::left_join(
    pop_cities %>%
      dplyr::group_by(city_clean, state_name) %>%
      dplyr::slice_max(pop_2024, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(city_clean, state_name, pop_2024, pop_growth_pct),
    by = c("city" = "city_clean", "state_name" = "state_name")
  ) %>%
  dplyr::select(-state_name)

pop_matched <- sum(!is.na(markets_demand$pop_2024))
cat("  City population matched:", pop_matched, "of", nrow(markets), "\n")

# ---------------------------------------------------------------------------
# 5b. Listing activity (Redfin) + rent ground-truth (HUD FMR) +
#     job market (BLS) + realtor.com inventory
# ---------------------------------------------------------------------------
# Each of these is a county-FIPS-keyed table produced by a sibling script.
# Treat any missing file as optional so the pipeline still runs headless.
cat("\nMerging Redfin + HUD + BLS + Realtor.com signals...\n")

maybe_read <- function(path) {
  if (!file.exists(path)) {
    cat("  SKIP (missing):", path, "\n")
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(
    fips = readr::col_character(), .default = readr::col_guess()
  ))
}

redfin_county <- maybe_read(file.path(data_dir, "redfin_county.csv"))
hud_county    <- maybe_read(file.path(data_dir, "hud_fmr_county.csv"))
bls_county    <- maybe_read(file.path(data_dir, "bls_laus_county.csv"))
rdc_county    <- maybe_read(file.path(data_dir, "realtor_com_county.csv"))

if (!is.null(redfin_county)) {
  markets_demand <- markets_demand %>%
    dplyr::left_join(
      redfin_county %>% dplyr::select(
        fips, redfin_median_sale, redfin_median_list, redfin_sale_yoy_pct,
        redfin_list_yoy_pct, redfin_dom, redfin_dom_yoy, redfin_inventory,
        redfin_inventory_yoy, redfin_new_listings, redfin_homes_sold,
        redfin_months_supply, redfin_sale_to_list, redfin_sold_above_list,
        redfin_price_drops, redfin_off_market_2wk
      ),
      by = "fips"
    )
  cat(sprintf("  Redfin matched: %d of %d cities\n",
              sum(!is.na(markets_demand$redfin_median_sale)), nrow(markets_demand)))
}

if (!is.null(hud_county)) {
  markets_demand <- markets_demand %>%
    dplyr::left_join(
      hud_county %>% dplyr::select(fips, fmr_0br, fmr_1br, fmr_2br, fmr_3br, fmr_4br),
      by = "fips"
    ) %>%
    dplyr::mutate(
      # ZORI is a blended index; FMR 2BR is HUD's official "moderately priced"
      # rent at 40th percentile. If ZORI exceeds FMR by > 100%, that's either
      # a glitch or a genuine luxury market. Flag as outlier.
      zori_vs_fmr_pct = dplyr::if_else(
        is.na(fmr_2br) | is.na(zori), NA_real_,
        round(100 * (zori - fmr_2br) / fmr_2br, 1)
      ),
      fmr_flag_outlier = !is.na(zori_vs_fmr_pct) & zori_vs_fmr_pct >= 100
    )
  cat(sprintf("  HUD FMR matched: %d of %d cities\n",
              sum(!is.na(markets_demand$fmr_2br)), nrow(markets_demand)))
}

if (!is.null(bls_county)) {
  markets_demand <- markets_demand %>%
    dplyr::left_join(
      bls_county %>% dplyr::select(fips, bls_unemp_rate, bls_unemp_yoy_pp, bls_date),
      by = "fips"
    )
  cat(sprintf("  BLS LAUS matched: %d of %d cities\n",
              sum(!is.na(markets_demand$bls_unemp_rate)), nrow(markets_demand)))
}

if (!is.null(rdc_county)) {
  markets_demand <- markets_demand %>%
    dplyr::left_join(
      rdc_county %>% dplyr::select(
        fips, rdc_median_list, rdc_median_list_yoy, rdc_dom,
        rdc_active_listings, rdc_new_listings, rdc_pending_listings,
        rdc_price_reduced_share, rdc_pending_ratio,
        rdc_hotness_score, rdc_supply_score, rdc_demand_score, rdc_hotness_rank
      ),
      by = "fips"
    )
  cat(sprintf("  Realtor.com matched: %d of %d cities\n",
              sum(!is.na(markets_demand$rdc_median_list)), nrow(markets_demand)))
}

# ---------------------------------------------------------------------------
# 6. Trader Joe's proximity
# ---------------------------------------------------------------------------
# Load TJ store locations and the Census 2024 Places Gazetteer (so every
# incorporated place has a lat/lon). For each city, compute distance to the
# nearest TJ in miles.
cat("\nLoading Trader Joe's locations + Census Gazetteer...\n")

tj_path <- file.path(data_dir, "trader_joes_locations.csv")
if (!file.exists(tj_path)) {
  stop("Missing data/trader_joes_locations.csv. Run scripts/trader_joes_data.R first.")
}
tj <- readr::read_csv(tj_path, show_col_types = FALSE)
cat("  Trader Joe's stores:", nrow(tj), "\n")

gaz_url <- paste0(
  "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/",
  "2024_Gazetteer/2024_Gaz_place_national.zip"
)
gaz_cache <- file.path(data_dir, "2024_Gaz_place_national.txt")
if (!file.exists(gaz_cache)) {
  tmp_zip <- tempfile(fileext = ".zip")
  utils::download.file(gaz_url, tmp_zip, quiet = TRUE, mode = "wb")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  utils::unzip(tmp_zip, exdir = tmp_dir)
  extracted <- list.files(tmp_dir, pattern = "\\.txt$", full.names = TRUE)[1]
  file.copy(extracted, gaz_cache, overwrite = TRUE)
}
gaz <- readr::read_tsv(
  gaz_cache, show_col_types = FALSE, trim_ws = TRUE,
  name_repair = ~ stringr::str_trim(.x)
)

gaz_clean <- gaz %>%
  dplyr::transmute(
    gaz_state = USPS,
    gaz_name = NAME,
    gaz_lat = as.numeric(INTPTLAT),
    gaz_lon = as.numeric(INTPTLONG),
    # Strip "city", "town", "village", "CDP", "borough" suffixes
    gaz_name_clean = gaz_name %>%
      stringr::str_remove(
        "\\s+(city|town|village|CDP|borough|municipality|township)$"
      ) %>%
      stringr::str_trim()
  ) %>%
  dplyr::filter(!is.na(gaz_lat), !is.na(gaz_lon))

# Collapse duplicates by state + clean name (keep the largest / first match)
gaz_lookup <- gaz_clean %>%
  dplyr::group_by(gaz_state, gaz_name_clean) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()

markets_geo <- markets_demand %>%
  dplyr::left_join(
    gaz_lookup %>% dplyr::select(gaz_state, gaz_name_clean, city_lat = gaz_lat, city_lon = gaz_lon),
    by = c("state" = "gaz_state", "city" = "gaz_name_clean")
  )

n_geo <- sum(!is.na(markets_geo$city_lat))
cat(sprintf("  Geocoded %d of %d cities via Gazetteer\n", n_geo, nrow(markets_geo)))

# Haversine distance, vectorized against TJ stores
haversine_miles <- function(lat1, lon1, lat2, lon2) {
  R <- 3958.7613
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# For each city with a lat/lon, find the minimum distance to any TJ store.
tj_lat <- tj$lat
tj_lon <- tj$lon
nearest_tj <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) return(c(NA_real_, NA_character_, NA_character_))
  d <- haversine_miles(lat, lon, tj_lat, tj_lon)
  i <- which.min(d)
  c(round(d[i], 2), tj$city[i], tj$state[i])
}

cat("  Computing nearest TJ distance per city...\n")
tj_out <- mapply(
  nearest_tj,
  markets_geo$city_lat,
  markets_geo$city_lon,
  SIMPLIFY = TRUE
)
# tj_out is a 3 x n character matrix when any NAs; coerce carefully
markets_geo$tj_miles <- suppressWarnings(as.numeric(tj_out[1, ]))
markets_geo$nearest_tj_city <- tj_out[2, ]
markets_geo$nearest_tj_state <- tj_out[3, ]

# Proximity flags
markets_geo <- markets_geo %>%
  dplyr::mutate(
    tj_within_2mi = !is.na(tj_miles) & tj_miles <= 2,
    tj_within_5mi = !is.na(tj_miles) & tj_miles <= 5,
    tj_within_10mi = !is.na(tj_miles) & tj_miles <= 10,
    tj_within_25mi = !is.na(tj_miles) & tj_miles <= 25
  )

cat(sprintf(
  "  TJ-adjacent cities: within 5mi=%d, within 10mi=%d, within 25mi=%d\n",
  sum(markets_geo$tj_within_5mi, na.rm = TRUE),
  sum(markets_geo$tj_within_10mi, na.rm = TRUE),
  sum(markets_geo$tj_within_25mi, na.rm = TRUE)
))

# ---------------------------------------------------------------------------
# 7. Leverage / cash-flow metrics (default investor scenario)
# ---------------------------------------------------------------------------
# Default assumptions used for the baseline columns. The HTML page exposes
# an interactive calculator that lets users override these; this just gives
# a ranked "out of the box" view for the static tables.
DOWN_PCT <- 0.25
MORTGAGE_RATE <- 0.0725
TERM_YEARS <- 30
EXPENSE_RATIO <- 0.35      # taxes, insurance, vacancy, maintenance, mgmt
CLOSING_PCT <- 0.03        # closing costs as % of price

mortgage_payment <- function(loan_amount, annual_rate, years) {
  r <- annual_rate / 12
  n <- years * 12
  if (r == 0) return(loan_amount / n)
  loan_amount * (r * (1 + r)^n) / ((1 + r)^n - 1)
}

markets_leverage <- markets_geo %>%
  dplyr::mutate(
    down_payment = zhvi * DOWN_PCT,
    closing_costs = zhvi * CLOSING_PCT,
    cash_invested = down_payment + closing_costs,
    loan_amount = zhvi * (1 - DOWN_PCT),
    monthly_mortgage = mortgage_payment(loan_amount, MORTGAGE_RATE, TERM_YEARS),
    monthly_opex = zori * EXPENSE_RATIO,
    monthly_cash_flow = round(zori - monthly_mortgage - monthly_opex, 0),
    annual_cash_flow = monthly_cash_flow * 12,
    cash_on_cash_pct = round(100 * annual_cash_flow / cash_invested, 2),
    # Rent a unit would need to hit $0/mo cash flow at default assumptions
    break_even_rent = round((monthly_mortgage) / (1 - EXPENSE_RATIO), 0),
    # Classic investor rules of thumb
    one_pct_rule = zori >= 0.01 * zhvi,
    two_pct_rule = zori >= 0.02 * zhvi,
    # Spread between gross yield and current mortgage rate
    yield_spread_pct = round(gross_yield_pct - MORTGAGE_RATE * 100, 2)
  )

# ---------------------------------------------------------------------------
# 7. Composite investment score
# ---------------------------------------------------------------------------
# Rank-based composite so outliers don't dominate. Each factor is converted
# to a 0-1 percentile and weighted; higher = better rental investment.
score_rank <- function(x, higher_is_better = TRUE) {
  r <- dplyr::percent_rank(x)
  if (!higher_is_better) r <- 1 - r
  tidyr::replace_na(r, 0.5)
}

markets_scored <- markets_leverage %>%
  dplyr::mutate(
    s_cap_rate = score_rank(est_cap_rate_pct, TRUE),
    s_pop_growth = score_rank(dplyr::coalesce(pop_growth_pct, county_pop_growth_pct), TRUE),
    s_net_mig = score_rank(net_mig_rate_per1k, TRUE),
    s_vacancy = score_rank(vacancy_rate_pct, FALSE),
    s_appreciation = score_rank(zhvi_yoy_pct, TRUE),
    # Trader Joe's proximity: a classic realtor proxy for walkable, amenity-rich,
    # demographically-strong neighborhoods. Score is rank-based on inverse distance,
    # capped at 25 miles (beyond that it's noise).
    tj_score_distance = dplyr::case_when(
      is.na(tj_miles) ~ NA_real_,
      tj_miles > 25 ~ 25,
      TRUE ~ tj_miles
    ),
    s_tj = score_rank(tj_score_distance, FALSE),
    # New signals (fall back to 0.5 neutral if the source didn't match).
    # Redfin days on market: lower = faster market = stronger demand
    s_dom = score_rank(redfin_dom, FALSE),
    # Redfin sale-to-list ratio: >1.0 means bidding wars; higher = stronger demand
    s_sale_to_list = score_rank(redfin_sale_to_list, TRUE),
    # BLS unemployment: lower = better
    s_unemployment = score_rank(bls_unemp_rate, FALSE),
    # Realtor.com hotness score (already 0-100 composite)
    s_hotness = score_rank(rdc_hotness_score, TRUE),
    investment_score = round(
      100 * (
        0.25 * s_cap_rate +
        0.15 * s_pop_growth +
        0.10 * s_net_mig +
        0.10 * s_vacancy +
        0.10 * s_appreciation +
        0.07 * s_tj +
        0.08 * s_dom +
        0.05 * s_sale_to_list +
        0.05 * s_unemployment +
        0.05 * s_hotness
      ),
      1
    ),
    # Flag "hidden gems": small population, high cap rate, growing, low vacancy
    hidden_gem = !is.na(pop_2024) & pop_2024 >= 5000 & pop_2024 <= 50000 &
      est_cap_rate_pct >= 5 &
      dplyr::coalesce(pop_growth_pct, county_pop_growth_pct, 0) >= 0 &
      dplyr::coalesce(vacancy_rate_pct, 0) <= 12,
    # "Unicorn": cash flow positive AND within 10 miles of a Trader Joe's
    tj_unicorn = !is.na(monthly_cash_flow) & monthly_cash_flow > 0 &
      !is.na(tj_within_10mi) & tj_within_10mi,
    # "Fast market": cash-flow-positive, listings move fast, bids over asking
    fast_market = !is.na(monthly_cash_flow) & monthly_cash_flow > 0 &
      !is.na(redfin_dom) & redfin_dom <= 25 &
      !is.na(redfin_sale_to_list) & redfin_sale_to_list >= 1.00,
    # "Cheap and stable": cash-flow-positive, low unemployment, low price drops
    cheap_and_stable = !is.na(monthly_cash_flow) & monthly_cash_flow > 0 &
      !is.na(bls_unemp_rate) & bls_unemp_rate <= 4.5 &
      !is.na(redfin_price_drops) & redfin_price_drops <= 0.10
  )

# ---------------------------------------------------------------------------
# 8. Output
# ---------------------------------------------------------------------------
output <- markets_scored %>%
  dplyr::select(
    city, state, metro, county, fips,
    zhvi, zhvi_yoy_pct, zori, zori_yoy_pct,
    annual_rent, rent_to_price_pct, price_to_rent, gross_yield_pct, est_cap_rate_pct,
    tier,
    pop_2024, pop_growth_pct,
    county_pop_2024, county_pop_growth_pct,
    net_mig_rate_per1k,
    vacancy_rate_pct, rental_vacancy_pct, renter_pct,
    down_payment, loan_amount, monthly_mortgage,
    monthly_cash_flow, annual_cash_flow, cash_on_cash_pct,
    break_even_rent, yield_spread_pct,
    one_pct_rule, two_pct_rule,
    city_lat, city_lon,
    tj_miles, nearest_tj_city, nearest_tj_state,
    tj_within_2mi, tj_within_5mi, tj_within_10mi, tj_within_25mi,
    # Redfin listing activity
    dplyr::any_of(c(
      "redfin_median_sale", "redfin_median_list", "redfin_sale_yoy_pct",
      "redfin_list_yoy_pct", "redfin_dom", "redfin_dom_yoy",
      "redfin_inventory", "redfin_inventory_yoy",
      "redfin_new_listings", "redfin_homes_sold", "redfin_months_supply",
      "redfin_sale_to_list", "redfin_sold_above_list",
      "redfin_price_drops", "redfin_off_market_2wk"
    )),
    # HUD FMR ground truth
    dplyr::any_of(c(
      "fmr_0br", "fmr_1br", "fmr_2br", "fmr_3br", "fmr_4br",
      "zori_vs_fmr_pct", "fmr_flag_outlier"
    )),
    # BLS unemployment
    dplyr::any_of(c("bls_unemp_rate", "bls_unemp_yoy_pp", "bls_date")),
    # Realtor.com inventory + hotness
    dplyr::any_of(c(
      "rdc_median_list", "rdc_median_list_yoy", "rdc_dom",
      "rdc_active_listings", "rdc_new_listings", "rdc_pending_listings",
      "rdc_price_reduced_share", "rdc_pending_ratio",
      "rdc_hotness_score", "rdc_supply_score", "rdc_demand_score",
      "rdc_hotness_rank"
    )),
    investment_score, hidden_gem, tj_unicorn, fast_market, cheap_and_stable,
    zhvi_date, zori_date
  )

readr::write_csv(output, file.path(data_dir, "us_rental_markets.csv"))

cat("\n=== Demand + Leverage + TJ Summary for Top 30 by Score ===\n")
output %>%
  dplyr::filter(zhvi >= 50000, zori <= 5000, zori >= 500, gross_yield_pct <= 20,
                !is.na(investment_score)) %>%
  dplyr::arrange(dplyr::desc(investment_score)) %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::transmute(
    City = city, State = state,
    Score = investment_score,
    `Cap Rate` = paste0(est_cap_rate_pct, "%"),
    `Cash/Mo` = scales::dollar(monthly_cash_flow),
    `CoC %` = paste0(cash_on_cash_pct, "%"),
    `TJ mi` = dplyr::if_else(is.na(tj_miles), "---", as.character(tj_miles)),
    Unicorn = tj_unicorn,
    `Population` = scales::comma(pop_2024)
  ) %>%
  print(n = 30, width = 250)

cat("\nOutput: data/us_rental_markets.csv (", nrow(output), " cities)\n")
cat("Columns added in this update:\n")
cat("  Demand: pop_2024, pop_growth_pct, county_pop_growth_pct,\n")
cat("          net_mig_rate_per1k, vacancy_rate_pct, rental_vacancy_pct, renter_pct\n")
cat("  Leverage (25% down, 7.25% 30yr, 35% opex):\n")
cat("          down_payment, loan_amount, monthly_mortgage,\n")
cat("          monthly_cash_flow, annual_cash_flow, cash_on_cash_pct,\n")
cat("          break_even_rent, yield_spread_pct, one_pct_rule, two_pct_rule\n")
cat("  Trader Joe's: tj_miles, nearest_tj_city/state,\n")
cat("          tj_within_2mi/5mi/10mi/25mi, tj_unicorn\n")
cat("  Score: investment_score (0-100, now includes 8% TJ proximity), hidden_gem\n")
cat("Sources: Census Pop Estimates V2024 + ACS 5-Year 2023 + TJ store locator\n")
