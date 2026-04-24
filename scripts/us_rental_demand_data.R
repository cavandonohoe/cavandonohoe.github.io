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
  "investment_score", "hidden_gem"
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
# 6. Leverage / cash-flow metrics (default investor scenario)
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

markets_leverage <- markets_demand %>%
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
    investment_score = round(
      100 * (
        0.35 * s_cap_rate +
        0.20 * s_pop_growth +
        0.15 * s_net_mig +
        0.15 * s_vacancy +
        0.15 * s_appreciation
      ),
      1
    ),
    # Flag "hidden gems": small population, high cap rate, growing, low vacancy
    hidden_gem = !is.na(pop_2024) & pop_2024 >= 5000 & pop_2024 <= 50000 &
      est_cap_rate_pct >= 5 &
      dplyr::coalesce(pop_growth_pct, county_pop_growth_pct, 0) >= 0 &
      dplyr::coalesce(vacancy_rate_pct, 0) <= 12
  )

# ---------------------------------------------------------------------------
# 8. Output
# ---------------------------------------------------------------------------
output <- markets_scored %>%
  dplyr::select(
    city, state, metro, county,
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
    investment_score, hidden_gem,
    zhvi_date, zori_date
  )

readr::write_csv(output, file.path(data_dir, "us_rental_markets.csv"))

cat("\n=== Demand + Leverage Summary for Top 30 Cap Rate Cities ===\n")
output %>%
  dplyr::filter(zhvi >= 50000, zori <= 5000, zori >= 500, gross_yield_pct <= 20) %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::transmute(
    City = city, State = state,
    `Cap Rate` = paste0(est_cap_rate_pct, "%"),
    `Cash/Mo` = scales::dollar(monthly_cash_flow),
    `CoC %` = paste0(cash_on_cash_pct, "%"),
    Score = investment_score,
    `Population` = scales::comma(pop_2024),
    `Pop Gr` = dplyr::if_else(
      is.na(pop_growth_pct), "---", paste0(pop_growth_pct, "%")
    ),
    `Vacancy` = paste0(vacancy_rate_pct, "%"),
    Gem = hidden_gem
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
cat("  Score: investment_score (0-100, rank-weighted), hidden_gem\n")
cat("Sources: Census Pop Estimates V2024 + ACS 5-Year 2023\n")
