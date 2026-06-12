#!/usr/bin/env Rscript
#
# la_heatmap_data.R
# Builds the data assets for the animated LA housing-price heat map.
#
# Outputs:
#   data/la_zhvi_zip_history.csv    -- annual ZHVI by ZIP for LA County (2000+)
#   data/la_county_zips.geojson     -- LA County ZCTA polygons (simplified)
#
# Sources:
#   1. Zillow ZHVI ZIP-level monthly history
#      https://files.zillowstatic.com/research/public_csvs/zhvi/
#      Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv
#
#   2. OpenDataDE CA ZIP code GeoJSON (Census TIGER ZCTA 2010, ogr2ogr-converted)
#      https://github.com/OpenDataDE/State-zip-code-GeoJSON

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

cat("\nDone.\n")
