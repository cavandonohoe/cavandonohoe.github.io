#!/usr/bin/env Rscript
# Downloads COVID-19 data from Johns Hopkins, NYT, Census, and OWID.
# Run locally: Rscript scripts/covid_data.R
# Output: data/covid_global_confirmed.csv, data/covid_us_states.csv,
#         data/census_state_pop_2019.csv, data/covid_us_vaccinations.csv

cat("Downloading COVID global confirmed cases (Johns Hopkins)...\n")
covid_global <- readr::read_csv(paste0(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
  "csse_covid_19_data/csse_covid_19_time_series/",
  "time_series_covid19_confirmed_global.csv"
), show_col_types = FALSE)
readr::write_csv(covid_global, here::here("data", "covid_global_confirmed.csv"))
cat("  ->", nrow(covid_global), "rows\n")

cat("Downloading COVID US state data (NYT)...\n")
covid_states <- readr::read_csv(paste0(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/",
  "us-states.csv"
), show_col_types = FALSE)
readr::write_csv(covid_states, here::here("data", "covid_us_states.csv"))
cat("  ->", nrow(covid_states), "rows\n")

cat("Downloading Census 2019 state population estimates...\n")
state_pop <- readr::read_csv(paste0(
  "http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/",
  "national/totals/nst-est2019-alldata.csv"
), show_col_types = FALSE)
readr::write_csv(state_pop, here::here("data", "census_state_pop_2019.csv"))
cat("  ->", nrow(state_pop), "rows\n")

cat("Downloading COVID vaccination data (OWID)...\n")
vaccines <- readr::read_csv(paste0(
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/",
  "vaccinations/us_state_vaccinations.csv"
), show_col_types = FALSE)
readr::write_csv(vaccines, here::here("data", "covid_us_vaccinations.csv"))
cat("  ->", nrow(vaccines), "rows\n")

cat("Done! All COVID data cached in data/\n")
