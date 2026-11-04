# Counts for the exceptional-episodes page, recomputed from the official snapshot.
imdb_exceptional_counts <- function(series, episodes, min_votes = 5000L) {
  stopifnot(!anyDuplicated(series$series_id), !anyDuplicated(episodes$episode_id))
  eligible <- series |>
    dplyr::filter(series_votes >= 25000, title_type %in% c("tvSeries", "tvMiniSeries"))
  hits <- episodes |>
    dplyr::filter(rating >= 9.7, votes >= min_votes, series_id %in% eligible$series_id) |>
    dplyr::count(series_id, name = "exceptional_episodes")
  eligible |>
    dplyr::left_join(hits, by = "series_id") |>
    dplyr::mutate(
      exceptional_episodes = dplyr::coalesce(exceptional_episodes, 0L),
      percent_exceptional = dplyr::if_else(
        linked_episodes > 0,
        100 * exceptional_episodes / linked_episodes,
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(exceptional_episodes), dplyr::desc(series_votes), series_id)
}

# Source dates for the three official downloads, read from the HTTP headers
# saved in results/manifest.json. `run_date` is IMDb's export run date
# (x-amz-meta-run-date) when present, else the Last-Modified calendar date.
imdb_source_dates <- function(manifest) {
  header <- function(headers, name) {
    hit <- names(headers)[tolower(names(headers)) == tolower(name)]
    if (length(hit)) as.character(headers[[hit[[1]]]]) else NA_character_
  }
  # Parse RFC 7231 dates without relying on the session's locale for "%b".
  parse_http_date <- function(x) {
    pattern <- "(\\d{1,2}) (\\w{3}) (\\d{4}) (\\d{2}:\\d{2}:\\d{2})"
    m <- regmatches(x, regexec(pattern, x))[[1]]
    month <- if (length(m) == 5) match(m[[3]], month.abb) else NA_integer_
    if (is.na(month)) return(as.POSIXct(NA, tz = "UTC"))
    stamp <- sprintf("%s-%02d-%02d %s", m[[4]], month, as.integer(m[[2]]), m[[5]])
    as.POSIXct(stamp, tz = "UTC")
  }
  datasets <- manifest$datasets
  rows <- lapply(names(datasets), function(name) {
    headers <- datasets[[name]]$headers
    modified <- parse_http_date(header(headers, "Last-Modified"))
    run_date <- header(headers, "x-amz-meta-run-date")
    if (is.na(run_date)) run_date <- format(as.Date(modified, tz = "UTC"))
    tibble::tibble(dataset = name, last_modified = modified, run_date = as.Date(run_date))
  })
  dplyr::bind_rows(rows)
}
