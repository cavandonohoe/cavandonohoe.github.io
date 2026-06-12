#!/usr/bin/env Rscript
#
# la_heatmap_data.R
# Builds the data assets for the animated LA housing-price heat map plus the
# long-run companion charts.
#
# Outputs:
#   data/la_zhvi_zip_history.csv    -- annual ZHVI by ZIP for LA County (2000+)
#   data/la_county_zips.geojson     -- LA County ZCTA polygons (simplified)
#   data/shiller_us_home_prices.csv -- annual US real & nominal HPI, 1890+
#   data/case_shiller_la.csv        -- monthly Case-Shiller LA HPI, 1987+
#
# Sources:
#   1. Zillow ZHVI ZIP-level monthly history
#      https://files.zillowstatic.com/research/public_csvs/zhvi/
#      Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv
#
#   2. OpenDataDE CA ZIP code GeoJSON (Census TIGER ZCTA 2010, ogr2ogr-converted)
#      https://github.com/OpenDataDE/State-zip-code-GeoJSON
#
#   3. Robert Shiller's "Online Data" Fig 3-1 workbook (US home prices since 1890)
#      http://www.econ.yale.edu/~shiller/data/Fig3-1.xls
#
#   4. FRED -- S&P Cotality Case-Shiller CA-Los Angeles HPI (LXXRSA), 1987+
#      https://fred.stlouisfed.org/graph/fredgraph.csv?id=LXXRSA

`%>%` <- magrittr::`%>%`

cat("=== LA Heat Map Data Build ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- "data"
if (!dir.exists(data_dir)) {
  stop("Run this script from the repo root (so 'data/' resolves correctly).")
}

# ---------------------------------------------------------------------------
# 1. Zillow ZHVI ZIP-level history, annualized to December values
# ---------------------------------------------------------------------------
cat("Downloading Zillow ZHVI ZIP-level history...\n")
zhvi_url <- paste0(
  "https://files.zillowstatic.com/research/public_csvs/zhvi/",
  "Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
)
zhvi_raw <- readr::read_csv(zhvi_url, show_col_types = FALSE)

la_zhvi_wide <- zhvi_raw %>%
  dplyr::filter(State == "CA", CountyName == "Los Angeles County")

cat("  LA County ZIPs in ZHVI:", nrow(la_zhvi_wide), "\n")

date_cols <- names(la_zhvi_wide) %>%
  stringr::str_subset("^[0-9]{4}-[0-9]{2}-[0-9]{2}$")

la_zhvi_long <- la_zhvi_wide %>%
  dplyr::select(
    zip = RegionName,
    city = City,
    metro = Metro,
    dplyr::all_of(date_cols)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(date_cols),
    names_to = "date",
    values_to = "zhvi"
  ) %>%
  dplyr::mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    zip = as.character(zip)
  )

# Pick December values for an annual snapshot. Use the latest available month
# for the current year (which won't have December yet).
latest_year <- max(la_zhvi_long$year, na.rm = TRUE)
latest_partial_month <- la_zhvi_long %>%
  dplyr::filter(year == latest_year, !is.na(zhvi)) %>%
  dplyr::pull(month) %>%
  max(na.rm = TRUE)

la_zhvi_annual <- la_zhvi_long %>%
  dplyr::filter(
    (year < latest_year & month == 12) |
      (year == latest_year & month == latest_partial_month)
  ) %>%
  dplyr::filter(!is.na(zhvi)) %>%
  dplyr::select(zip, city, metro, year, month, date, zhvi) %>%
  dplyr::arrange(zip, year)

n_years <- length(unique(la_zhvi_annual$year))
n_zips <- length(unique(la_zhvi_annual$zip))
cat("  Years covered:", min(la_zhvi_annual$year), "to",
    max(la_zhvi_annual$year), "(", n_years, "snapshots )\n")
cat("  Distinct ZIPs:", n_zips, "\n")

readr::write_csv(la_zhvi_annual, file.path(data_dir, "la_zhvi_zip_history.csv"))
cat("  Wrote data/la_zhvi_zip_history.csv (",
    nrow(la_zhvi_annual), "rows )\n")

la_zip_set <- unique(la_zhvi_annual$zip)

# ---------------------------------------------------------------------------
# 2. LA County ZCTA polygons (filtered + simplified GeoJSON)
# ---------------------------------------------------------------------------
ca_geojson_url <- paste0(
  "https://raw.githubusercontent.com/OpenDataDE/State-zip-code-GeoJSON/",
  "master/ca_california_zip_codes_geo.min.json"
)

tmp_geo <- tempfile(fileext = ".geojson")
cat("\nDownloading CA ZIP GeoJSON (~68 MB, one time)...\n")
utils::download.file(ca_geojson_url, tmp_geo, quiet = TRUE, mode = "wb")

cat("Parsing GeoJSON...\n")
ca_geo <- jsonlite::fromJSON(tmp_geo, simplifyVector = FALSE)

# Filter to LA County ZIPs that we have ZHVI data for.
keep_idx <- vapply(ca_geo$features, function(f) {
  f$properties$ZCTA5CE10 %in% la_zip_set
}, logical(1))
la_features <- ca_geo$features[keep_idx]
cat("  Matched", length(la_features), "of", length(la_zip_set),
    "LA ZIPs to GeoJSON polygons\n")

# Polygon simplification: keep coordinate stride to shrink filesize, and
# round coordinates to 4 decimals (~11 m precision -- plenty for choropleth).
simplify_ring <- function(ring, stride = 2L, digits = 4L) {
  n <- length(ring)
  if (n <= 6) {
    keep <- seq_len(n)
  } else {
    keep <- unique(c(seq.int(1L, n, by = stride), n))
  }
  lapply(ring[keep], function(pt) {
    list(round(pt[[1]], digits), round(pt[[2]], digits))
  })
}

simplify_geom <- function(geom) {
  if (geom$type == "Polygon") {
    geom$coordinates <- lapply(geom$coordinates, simplify_ring)
  } else if (geom$type == "MultiPolygon") {
    geom$coordinates <- lapply(geom$coordinates, function(poly) {
      lapply(poly, simplify_ring)
    })
  }
  geom
}

la_features_simple <- lapply(la_features, function(f) {
  list(
    type = "Feature",
    properties = list(zip = f$properties$ZCTA5CE10),
    geometry = simplify_geom(f$geometry)
  )
})

la_geojson <- list(type = "FeatureCollection", features = la_features_simple)

out_geojson <- file.path(data_dir, "la_county_zips.geojson")
jsonlite::write_json(la_geojson, out_geojson, auto_unbox = TRUE,
                     digits = NA)
cat("  Wrote", out_geojson, "(",
    round(file.info(out_geojson)$size / 1024 / 1024, 2), "MB )\n")

# ---------------------------------------------------------------------------
# 3. Shiller Fig 3-1 (US home prices, 1890+)
# ---------------------------------------------------------------------------
cat("\nDownloading Shiller Fig3-1.xls (1890+, annual + monthly post-1953)...\n")
shiller_url <- "http://www.econ.yale.edu/~shiller/data/Fig3-1.xls"
tmp_xls <- tempfile(fileext = ".xls")
utils::download.file(shiller_url, tmp_xls, quiet = TRUE, mode = "wb")

# Sheet "Data": header rows 4-7, data starts row 8.
# Column 1 = decimal date, column 2 = real HPI, column 9 = nominal HPI.
shiller_raw <- readxl::read_excel(
  tmp_xls, sheet = "Data", col_names = FALSE, skip = 7,
  .name_repair = "minimal"
)

shiller_us <- tibble::tibble(
  date_dec = suppressWarnings(as.numeric(shiller_raw[[1]])),
  real_hpi = suppressWarnings(as.numeric(shiller_raw[[2]])),
  nominal_hpi = suppressWarnings(as.numeric(shiller_raw[[9]]))
) %>%
  dplyr::filter(!is.na(date_dec), !is.na(real_hpi)) %>%
  dplyr::mutate(
    year = as.integer(floor(date_dec)),
    month = as.integer(round((date_dec - floor(date_dec)) * 12 + 1))
  )

# Reduce to one observation per year (annual prior to 1953, December otherwise).
shiller_annual <- shiller_us %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    real_hpi = dplyr::last(real_hpi),
    nominal_hpi = dplyr::last(nominal_hpi),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year)

readr::write_csv(
  shiller_annual,
  file.path(data_dir, "shiller_us_home_prices.csv")
)
cat("  Wrote data/shiller_us_home_prices.csv (",
    nrow(shiller_annual), "rows,",
    min(shiller_annual$year), "to", max(shiller_annual$year), ")\n")

# ---------------------------------------------------------------------------
# 4. Case-Shiller LA (LXXRSA) from FRED
# ---------------------------------------------------------------------------
cat("\nDownloading Case-Shiller LA from FRED (LXXRSA, 1987+ monthly)...\n")
fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=LXXRSA"
tmp_fred <- tempfile(fileext = ".csv")
utils::download.file(fred_url, tmp_fred, method = "curl", quiet = TRUE)
cs_la <- readr::read_csv(tmp_fred, show_col_types = FALSE) %>%
  dplyr::rename(date = observation_date, index = LXXRSA) %>%
  dplyr::filter(!is.na(index)) %>%
  dplyr::arrange(date)

readr::write_csv(cs_la, file.path(data_dir, "case_shiller_la.csv"))
cat("  Wrote data/case_shiller_la.csv (",
    nrow(cs_la), "rows,",
    format(min(cs_la$date), "%Y-%m"), "to",
    format(max(cs_la$date), "%Y-%m"), ")\n")

cat("\nDone.\n")
