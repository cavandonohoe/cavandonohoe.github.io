#!/usr/bin/env Rscript
#
# trader_joes_data.R
# Scrapes the Trader Joe's store locator (https://locations.traderjoes.com/)
# for every US store and writes a clean CSV with lat/lon + address.
#
# Strategy:
#   1. Fetch the sitemap.xml for every store URL (one URL per store,
#      pattern: /<state>/<city>/<store_id>/).
#   2. For each store page, pull `place:location:latitude` / `longitude`
#      meta tags and the JSON-LD address block.
#   3. Write data/trader_joes_locations.csv.
#
# Usage:
#   Rscript scripts/trader_joes_data.R
#
# The TJ page has ~600 stores, so this takes ~2-3 minutes on a warm cache.

`%>%` <- magrittr::`%>%`

cat("=== Trader Joe's Store Locator Scrape ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

# ---------------------------------------------------------------------------
# 1. Pull the sitemap and extract store URLs
# ---------------------------------------------------------------------------
cat("Fetching sitemap.xml...\n")
sitemap_req <- httr2::request("https://locations.traderjoes.com/sitemap.xml") %>%
  httr2::req_headers(`User-Agent` = UA) %>%
  httr2::req_retry(max_tries = 3) %>%
  httr2::req_perform()
sitemap_xml <- httr2::resp_body_string(sitemap_req)

# Store URLs look like /<state>/<city>/<store_id>/. City indexes don't end in a number.
store_urls <- stringr::str_extract_all(
  sitemap_xml,
  "https://locations\\.traderjoes\\.com/[a-z]{2}/[^<]+/[0-9]+/"
)[[1]] %>% unique()

cat("  Found", length(store_urls), "store URLs.\n\n")

# ---------------------------------------------------------------------------
# 2. Fetch each store page in parallel and parse lat/lon/address
# ---------------------------------------------------------------------------
cat("Fetching store pages (parallel)...\n")

reqs <- lapply(store_urls, function(u) {
  httr2::request(u) %>%
    httr2::req_headers(`User-Agent` = UA) %>%
    httr2::req_retry(max_tries = 2) %>%
    httr2::req_timeout(30)
})

resps <- httr2::req_perform_parallel(
  reqs,
  pool = curl::new_pool(total_con = 8, host_con = 6),
  on_error = "continue"
)

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

parse_store <- function(url, resp) {
  if (is.null(resp) || inherits(resp, "error")) {
    return(NULL)
  }
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NA_character_)
  if (is.na(body)) return(NULL)

  m <- stringr::str_match(url, "([a-z]{2})/([^/]+)/([0-9]+)/?$")
  state <- toupper(m[2])
  url_city <- m[3]
  store_id <- m[4]

  lat <- stringr::str_match(
    body, 'place:location:latitude"\\s*content="([-0-9.]+)"'
  )[2] %>% as.numeric()
  lon <- stringr::str_match(
    body, 'place:location:longitude"\\s*content="([-0-9.]+)"'
  )[2] %>% as.numeric()

  street <- stringr::str_match(body, '"streetAddress"\\s*:\\s*"([^"]+)"')[2]
  json_city <- stringr::str_match(body, '"addressLocality"\\s*:\\s*"([^"]+)"')[2]
  postal <- stringr::str_match(body, '"postalCode"\\s*:\\s*"([^"]+)"')[2]
  phone <- stringr::str_match(body, '"telephone"\\s*:\\s*"([^"]+)"')[2]

  city <- if (is.na(json_city)) {
    tools::toTitleCase(stringr::str_replace_all(url_city, "-", " "))
  } else {
    json_city
  }

  tibble::tibble(
    store_id = store_id,
    state = state,
    city = city,
    street = street,
    postal_code = postal,
    phone = phone,
    lat = lat,
    lon = lon,
    url = url
  )
}

stores <- purrr::map2(store_urls, resps, parse_store) %>%
  purrr::compact() %>%
  dplyr::bind_rows()

n_with_coords <- sum(!is.na(stores$lat) & !is.na(stores$lon))
cat(sprintf(
  "  Parsed %d stores (%d with valid lat/lon)\n\n",
  nrow(stores), n_with_coords
))

# ---------------------------------------------------------------------------
# 3. Write CSV
# ---------------------------------------------------------------------------
stores <- stores %>%
  dplyr::filter(!is.na(lat), !is.na(lon)) %>%
  dplyr::mutate(
    lat = round(lat, 5),
    lon = round(lon, 5),
    scraped_at = format(Sys.time(), "%Y-%m-%d")
  ) %>%
  dplyr::arrange(state, city, store_id)

readr::write_csv(stores, file.path(data_dir, "trader_joes_locations.csv"))

cat("=== State Counts ===\n")
stores %>%
  dplyr::count(state, sort = TRUE) %>%
  print(n = 50)

cat(sprintf(
  "\nOutput: data/trader_joes_locations.csv (%d stores)\n",
  nrow(stores)
))
