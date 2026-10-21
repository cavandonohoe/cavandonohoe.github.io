# Refresh data/page_views.json from the GA4 Data API.
#
# Pulls the last 30 days of page views per page path and outbound clicks per
# link URL (GA4 enhanced measurement "click" events, which is how visits to
# externally hosted apps like the shinyapps.io calculators show up), ranks
# them with rank_card_keys(), and writes only the rank order. Raw counts are
# never committed. The file is rewritten only when the ranking changes, so
# unchanged days produce no commit and no redeploy.
#
# Env:
#   GA4_PROPERTY_ID                 numeric GA4 property id (required)
#   GOOGLE_APPLICATION_CREDENTIALS  path to the service-account JSON key; the
#                                   account needs Viewer access on the property
#   PAGE_VIEWS_WINDOW_DAYS          lookback window, default 30
#   PAGE_VIEWS_OUT                  output path, default data/page_views.json

source("scripts/project_cards.R")

property_id <- Sys.getenv("GA4_PROPERTY_ID")
if (!nzchar(property_id) || !grepl("^[0-9]+$", property_id)) {
  stop("GA4_PROPERTY_ID must be set to the numeric GA4 property id")
}
key_path <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
if (!nzchar(key_path) || !file.exists(key_path)) {
  stop("GOOGLE_APPLICATION_CREDENTIALS must point to a service-account JSON key")
}
window_days <- as.integer(Sys.getenv("PAGE_VIEWS_WINDOW_DAYS", "30"))
out_path <- Sys.getenv("PAGE_VIEWS_OUT", "data/page_views.json")

token <- gargle::credentials_service_account(
  scopes = "https://www.googleapis.com/auth/analytics.readonly",
  path = key_path
)
if (is.null(token)) stop("Could not authenticate the service account")
access_token <- token$credentials$access_token

run_report <- function(body) {
  url <- sprintf(
    "https://analyticsdata.googleapis.com/v1beta/properties/%s:runReport",
    property_id
  )
  resp <- httr2::request(url) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  rows <- httr2::resp_body_json(resp)$rows
  if (is.null(rows)) {
    return(data.frame(dim = character(), count = numeric()))
  }
  data.frame(
    dim = vapply(rows, function(r) r$dimensionValues[[1]]$value, character(1)),
    count = vapply(rows, function(r) as.numeric(r$metricValues[[1]]$value), numeric(1))
  )
}

date_range <- list(list(startDate = sprintf("%ddaysAgo", window_days), endDate = "today"))

pages <- run_report(list(
  dateRanges = date_range,
  dimensions = list(list(name = "pagePath")),
  metrics = list(list(name = "screenPageViews")),
  limit = 1000
))
clicks <- run_report(list(
  dateRanges = date_range,
  dimensions = list(list(name = "linkUrl")),
  metrics = list(list(name = "eventCount")),
  dimensionFilter = list(filter = list(
    fieldName = "eventName",
    stringFilter = list(value = "click")
  )),
  limit = 1000
))
cat(sprintf("GA4 rows: %d page paths, %d outbound link URLs\n", nrow(pages), nrow(clicks)))

ranking <- rank_card_keys(
  data.frame(path = pages$dim, count = pages$count),
  data.frame(url = clicks$dim, count = clicks$count)
)
if (length(ranking) == 0) {
  stop("GA4 returned no ranked pages; refusing to overwrite the ranking")
}
ranking <- utils::head(ranking, 200)

if (identical(read_card_ranking(out_path), ranking)) {
  cat("Ranking unchanged; leaving", out_path, "as is.\n")
} else {
  payload <- list(
    source = "GA4 page views + outbound clicks",
    window_days = window_days,
    updated = format(Sys.Date()),
    ranking = ranking
  )
  jsonlite::write_json(payload, out_path, auto_unbox = TRUE, pretty = TRUE)
  cat("Wrote", length(ranking), "ranked keys to", out_path, "\n")
}
