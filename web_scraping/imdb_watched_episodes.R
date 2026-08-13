`%>%` <- magrittr::`%>%`

# -----------------------------------------------------------------------------
# Build "every TV episode I've seen" from my IMDb ratings export.
#
# Assumption: any TV Series or TV Mini Series I rated in data/imdb_ratings.csv
# was watched in its entirety, so every episode of it counts as seen. We pull
# the full episode list for each rated series from IMDb's public GraphQL
# endpoint (reusing the fetcher in imdb_season_episode_ratings_plot.R) and
# write a single combined CSV.
#
# Output: data/watched_tv_episodes.csv with columns
#   imdb_id, series_title, my_series_rating, season, episode, title, rating,
#   votes, release_year
#
# my_imdb.Rmd reads this CSV to render the "All Episodes I've Seen" section.
# Refreshed on a schedule by .github/workflows/update_watched_episodes.yml so
# newly rated shows get their episodes filled in automatically.
# -----------------------------------------------------------------------------

# Reuse the GraphQL episode fetcher (get_imdb_all_episodes, extract_imdb_id).
source(here::here("web_scraping", "imdb_season_episode_ratings_plot.R"))

# Title types in the IMDb export that we treat as "watched in entirety".
SERIES_TITLE_TYPES <- c("TV Series", "TV Mini Series")

# Fetch every episode for one rated series. Returns a tibble (possibly 0 rows
# on failure) tagged with the series' id, title, and my rating.
fetch_series_episodes <- function(imdb_id, series_title, my_series_rating,
                                  release_year) {
  eps <- tryCatch(
    get_imdb_all_episodes(imdb_id),
    error = function(e) {
      message(sprintf("  fetch error for %s (%s): %s",
        series_title, imdb_id, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(eps) || nrow(eps) == 0) {
    return(tibble::tibble(
      imdb_id = character(), series_title = character(),
      my_series_rating = integer(), season = integer(), episode = integer(),
      title = character(), rating = numeric(), votes = integer(),
      release_year = integer()
    ))
  }
  eps %>%
    dplyr::transmute(
      imdb_id = imdb_id,
      series_title = series_title,
      my_series_rating = as.integer(my_series_rating),
      season, episode, title, rating, votes,
      release_year = as.integer(release_year)
    )
}

build_watched_episodes <- function(ratings_csv = here::here("data",
                                      "imdb_ratings.csv")) {
  ratings <- suppressMessages(readr::read_csv(ratings_csv,
    show_col_types = FALSE))

  series <- ratings %>%
    dplyr::filter(`Title Type` %in% SERIES_TITLE_TYPES) %>%
    dplyr::transmute(
      imdb_id = Const,
      series_title = Title,
      my_series_rating = `Your Rating`,
      release_year = Year
    ) %>%
    dplyr::distinct(imdb_id, .keep_all = TRUE) %>%
    dplyr::arrange(series_title)

  cat(sprintf("Rated series/mini-series to fetch: %d\n", nrow(series)))

  results <- vector("list", nrow(series))
  for (i in seq_len(nrow(series))) {
    row <- series[i, ]
    cat(sprintf("[%3d/%3d] %s (%s) ... ", i, nrow(series),
      row$series_title, row$imdb_id))
    eps <- fetch_series_episodes(row$imdb_id, row$series_title,
      row$my_series_rating, row$release_year)
    cat(sprintf("%d episodes\n", nrow(eps)))
    results[[i]] <- eps
    # Be polite to IMDb's endpoint between shows.
    Sys.sleep(0.3)
  }

  dplyr::bind_rows(results) %>%
    dplyr::arrange(series_title, season, episode)
}

# -----------------------------------------------------------------------------
# Run (only when executed, not when sourced for the functions above).
# -----------------------------------------------------------------------------
if (!interactive() && identical(sys.nframe(), 0L)) {
  out_path <- here::here("data", "watched_tv_episodes.csv")
  allow_shrink <- identical(Sys.getenv("IMDB_ALLOW_SHRINK"), "1")

  fetched <- build_watched_episodes()

  if (nrow(fetched) == 0) {
    stop("fetch returned zero episodes across all series; refusing to write.")
  }

  existing_rows <- if (file.exists(out_path)) {
    nrow(suppressMessages(readr::read_csv(out_path, show_col_types = FALSE)))
  } else {
    0L
  }

  # No-shrink guard: a scrape that produces fewer rows than what's committed is
  # almost always a transient IMDb outage, not a real change. Block it unless
  # IMDB_ALLOW_SHRINK=1 is set for an intentional one-off refresh.
  if (existing_rows > 0 && nrow(fetched) < existing_rows && !allow_shrink) {
    stop(sprintf(
      "fetched %d episodes < existing %d (no-shrink guard); set IMDB_ALLOW_SHRINK=1 to override.",
      nrow(fetched), existing_rows
    ))
  }

  readr::write_csv(fetched, out_path)
  cat(sprintf("\nWrote %s (%d episodes across %d series)\n",
    out_path, nrow(fetched), dplyr::n_distinct(fetched$imdb_id)))
}
