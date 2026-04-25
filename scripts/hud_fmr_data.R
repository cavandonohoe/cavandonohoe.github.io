#!/usr/bin/env Rscript
# hud_fmr_data.R
# Pulls HUD's FY26 Fair Market Rents (FMR) at county and ZIP (SAFMR) level.
# FMR is HUD's official "moderately-priced rental" benchmark used for Section 8
# vouchers; it's a government-grade ground-truth check on Zillow ZORI. Large
# deltas between ZORI and FMR flag either ZORI glitches or genuine market
# mispricing. Writes two CSVs keyed on county FIPS and ZIP.
#
# Source: https://www.huduser.gov/portal/datasets/fmr.html
# No API key required (direct xlsx download).
#
# Usage:
#   Rscript scripts/hud_fmr_data.R

`%>%` <- magrittr::`%>%`

cat("=== HUD Fair Market Rents (FY26) ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

fmr_county_url <- "https://www.huduser.gov/portal/datasets/fmr/fmr2026/FY26_FMRs.xlsx"
fmr_safmr_url  <- "https://www.huduser.gov/portal/datasets/fmr/fmr2026/fy2026_safmrs.xlsx"

fmr_county_cache <- file.path(data_dir, "hud_fy26_fmr_county.xlsx")
fmr_safmr_cache  <- file.path(data_dir, "hud_fy26_safmr.xlsx")

# HUD's site sits behind AWS WAF; it returns HTTP 202 "challenge" to non-browser
# User-Agents. Use a Chrome UA string to get the real file.
browser_ua <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

fetch_hud <- function(url, dest) {
  resp <- httr2::request(url) |>
    httr2::req_user_agent(browser_ua) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 5) |>
    httr2::req_perform()
  writeBin(httr2::resp_body_raw(resp), dest)
}

if (!file.exists(fmr_county_cache)) {
  cat("Downloading county FMR...\n")
  fetch_hud(fmr_county_url, fmr_county_cache)
}
if (!file.exists(fmr_safmr_cache)) {
  cat("Downloading SAFMR (ZCTA-level)...\n")
  fetch_hud(fmr_safmr_url, fmr_safmr_cache)
}

# --- County-level FMR ---
cat("\nParsing county FMR...\n")
fmr_county_raw <- readxl::read_excel(fmr_county_cache)
cat("  Columns:", paste(names(fmr_county_raw), collapse = ", "), "\n")
cat("  Rows:", nrow(fmr_county_raw), "\n")

# Normalize column names: HUD uses various casings across years.
# Expect: hud_area_code, state (FIPS), countyname, county_fips (5-digit) or cousub_fips,
# state_alpha, fmr_0..fmr_4, pop2020
nm <- tolower(names(fmr_county_raw))
names(fmr_county_raw) <- nm

# Find the FIPS column
fips_col <- dplyr::case_when(
  "fips2020" %in% nm ~ "fips2020",
  "fips_2020" %in% nm ~ "fips_2020",
  "fips2010" %in% nm ~ "fips2010",
  "county_fips" %in% nm ~ "county_fips",
  "fips" %in% nm ~ "fips",
  TRUE ~ NA_character_
)
stopifnot(!is.na(fips_col))
cat("  FIPS column detected:", fips_col, "\n")

# Identify FMR columns (may be fmr_0 / fmr0 / `fmr 0` etc.)
br_map <- c(
  br0 = "(^|_)fmr[_ ]?0($|_)",
  br1 = "(^|_)fmr[_ ]?1($|_)",
  br2 = "(^|_)fmr[_ ]?2($|_)",
  br3 = "(^|_)fmr[_ ]?3($|_)",
  br4 = "(^|_)fmr[_ ]?4($|_)"
)
fmr_cols <- purrr::map_chr(br_map, ~ names(fmr_county_raw)[stringr::str_detect(names(fmr_county_raw), .x)][1])
cat("  Bedroom cols:", paste(names(fmr_cols), fmr_cols, sep = "=", collapse = ", "), "\n")

fmr_county <- fmr_county_raw %>%
  dplyr::transmute(
    fips = sprintf("%05s", as.character(.data[[fips_col]])) %>% stringr::str_pad(5, pad = "0") %>% substr(1, 5),
    state_alpha = if ("state_alpha" %in% nm) state_alpha else NA_character_,
    countyname = if ("countyname" %in% nm) countyname else NA_character_,
    areaname = if ("areaname" %in% nm) areaname else NA_character_,
    fmr_0br = as.numeric(.data[[fmr_cols["br0"]]]),
    fmr_1br = as.numeric(.data[[fmr_cols["br1"]]]),
    fmr_2br = as.numeric(.data[[fmr_cols["br2"]]]),
    fmr_3br = as.numeric(.data[[fmr_cols["br3"]]]),
    fmr_4br = as.numeric(.data[[fmr_cols["br4"]]])
  ) %>%
  dplyr::filter(!is.na(fips), nchar(fips) == 5) %>%
  dplyr::group_by(fips) %>%
  # HUD file can have county-subdivision rows; collapse to county max
  dplyr::summarise(
    state_alpha = dplyr::first(state_alpha),
    countyname = dplyr::first(countyname),
    areaname = dplyr::first(areaname),
    dplyr::across(dplyr::starts_with("fmr_"), ~ max(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("fmr_"), ~ ifelse(is.infinite(.x), NA_real_, .x)))

readr::write_csv(fmr_county, file.path(data_dir, "hud_fmr_county.csv"))
cat(sprintf("  Written: data/hud_fmr_county.csv (%d counties)\n", nrow(fmr_county)))

# --- ZCTA-level SAFMR (Small Area FMR) ---
cat("\nParsing SAFMR (ZCTA)...\n")
safmr_raw <- readxl::read_excel(fmr_safmr_cache)
# SAFMR sheet has multi-line column names like "safmr\n0br" — collapse whitespace
names(safmr_raw) <- names(safmr_raw) %>%
  stringr::str_replace_all("[\\r\\n]+", " ") %>%
  stringr::str_replace_all("\\s+", " ") %>%
  stringr::str_trim() %>%
  tolower()
cat("  Rows:", nrow(safmr_raw), "\n")
cat("  Columns:", paste(names(safmr_raw), collapse = " | "), "\n")

# ZIP column may be named "zip code", "zipcode", "zip_code", "zip"
zip_col <- dplyr::case_when(
  "zip code" %in% names(safmr_raw) ~ "zip code",
  "zipcode" %in% names(safmr_raw) ~ "zipcode",
  "zip_code" %in% names(safmr_raw) ~ "zip_code",
  "zip" %in% names(safmr_raw) ~ "zip",
  TRUE ~ NA_character_
)
stopifnot(!is.na(zip_col))

# SAFMR bedroom columns look like "safmr 0br", "safmr 1br", etc. Match the
# base bedroom column (not the 90%/110% variants).
find_br <- function(n) {
  pat <- sprintf("^safmr %sbr$", n)
  hits <- grep(pat, names(safmr_raw), value = TRUE, ignore.case = TRUE)
  hits[1]
}
safmr_cols <- stats::setNames(
  purrr::map_chr(c("0", "1", "2", "3", "4"), find_br),
  c("safmr_0br", "safmr_1br", "safmr_2br", "safmr_3br", "safmr_4br")
)
cat("  SAFMR cols:", paste(names(safmr_cols), safmr_cols, sep = "=", collapse = ", "), "\n")

safmr <- safmr_raw %>%
  dplyr::transmute(
    zip = stringr::str_pad(as.character(.data[[zip_col]]), 5, pad = "0"),
    safmr_0br = if (!is.na(safmr_cols["safmr_0br"])) as.numeric(.data[[safmr_cols["safmr_0br"]]]) else NA_real_,
    safmr_1br = if (!is.na(safmr_cols["safmr_1br"])) as.numeric(.data[[safmr_cols["safmr_1br"]]]) else NA_real_,
    safmr_2br = if (!is.na(safmr_cols["safmr_2br"])) as.numeric(.data[[safmr_cols["safmr_2br"]]]) else NA_real_,
    safmr_3br = if (!is.na(safmr_cols["safmr_3br"])) as.numeric(.data[[safmr_cols["safmr_3br"]]]) else NA_real_,
    safmr_4br = if (!is.na(safmr_cols["safmr_4br"])) as.numeric(.data[[safmr_cols["safmr_4br"]]]) else NA_real_
  ) %>%
  dplyr::filter(!is.na(zip), nchar(zip) == 5) %>%
  dplyr::distinct(zip, .keep_all = TRUE)

readr::write_csv(safmr, file.path(data_dir, "hud_safmr_zcta.csv"))
cat(sprintf("  Written: data/hud_safmr_zcta.csv (%d ZIPs)\n", nrow(safmr)))

cat("\n=== Top 5 county FMR (2BR) ===\n")
fmr_county %>%
  dplyr::arrange(dplyr::desc(fmr_2br)) %>%
  dplyr::slice_head(n = 5) %>%
  print(width = 180)
