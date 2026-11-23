`%>%` <- magrittr::`%>%`

# -----------------------------------------------------------------------------
# Episode data from IMDb's official non-commercial datasets.
#
# IMDb's WAF blocks server-side requests to www.imdb.com and its GraphQL
# endpoint from GitHub Actions (HTTP 403), so instead of scraping we read the
# daily TSV dumps IMDb publishes for personal and non-commercial use:
#   https://developer.imdb.com/non-commercial-datasets/
#
#   title.episode  episode id -> series id, season, episode number
#   title.ratings  episode id -> average rating, vote count
#   title.basics   episode id -> episode title, air year
#
# The dumps are refreshed once a day, which is plenty for the weekly crons.
# Downloads are cached in IMDB_DATASETS_DIR (default: a per-user cache dir)
# and reused for IMDB_DATASETS_MAX_AGE_HOURS (default 20) so repeated local
# runs don't re-download ~250 MB each time.
# -----------------------------------------------------------------------------

IMDB_DATASETS_URL <- "https://datasets.imdbws.com/"

imdb_datasets_dir <- function() {
  dir <- Sys.getenv("IMDB_DATASETS_DIR")
  if (!nzchar(dir)) {
    dir <- tools::R_user_dir("cavandonohoe.github.io", which = "cache")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

# Download one dataset (e.g. "title.episode") if the cached copy is missing or
# stale. Writes to a .part file and renames, so an interrupted download never
# leaves a truncated file behind.
imdb_dataset_path <- function(name, max_tries = 3L) {
  max_age_hours <- as.numeric(Sys.getenv("IMDB_DATASETS_MAX_AGE_HOURS", "20"))
  path <- file.path(imdb_datasets_dir(), paste0(name, ".tsv.gz"))
  if (file.exists(path)) {
    age <- difftime(Sys.time(), file.mtime(path), units = "hours")
    if (as.numeric(age) < max_age_hours) return(path)
  }

  url <- paste0(IMDB_DATASETS_URL, name, ".tsv.gz")
  partial <- paste0(path, ".part")
  for (attempt in seq_len(max_tries)) {
    ok <- tryCatch({
      httr2::request(url) %>%
        httr2::req_user_agent("cavandonohoe.github.io/imdb-datasets") %>%
        httr2::req_timeout(600) %>%
        httr2::req_perform(path = partial)
      TRUE
    }, error = function(e) {
      message(sprintf("  download attempt %d for %s failed: %s",
        attempt, name, conditionMessage(e)))
      FALSE
    })
    if (ok && file.exists(partial) && file.size(partial) > 0) {
      file.rename(partial, path)
      return(path)
    }
    unlink(partial)
    if (attempt < max_tries) Sys.sleep(5 * attempt)
  }
  stop("Could not download IMDb dataset ", name, " from ", url)
}

# readr (vroom) reads .gz natively; data.table::fread would need R.utils,
# which is not in renv.lock.
read_imdb_tsv <- function(name, select) {
  readr::read_tsv(
    imdb_dataset_path(name),
    quote = "", na = "\\N",
    col_select = dplyr::all_of(select),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

# Build (once per R session) a table of every rated episode on IMDb:
#   series_id, season, episode, title, rating, votes, episode_year
# Episodes without a season number or without a rating (unaired/upcoming
# placeholders) are dropped, matching what the old GraphQL fetcher returned.
.imdb_cache <- new.env(parent = emptyenv())

imdb_episode_table <- function() {
  if (!is.null(.imdb_cache$episodes)) return(.imdb_cache$episodes)

  message("Loading IMDb datasets from ", imdb_datasets_dir(), " ...")
  episodes <- read_imdb_tsv(
    "title.episode",
    c("tconst", "parentTconst", "seasonNumber", "episodeNumber")
  )
  ratings <- read_imdb_tsv("title.ratings", c("tconst", "averageRating", "numVotes"))
  basics <- read_imdb_tsv("title.basics", c("tconst", "primaryTitle", "startYear"))
  basics <- basics[basics$tconst %in% episodes$tconst, ]

  out <- episodes %>%
    dplyr::inner_join(ratings, by = "tconst") %>%
    dplyr::left_join(basics, by = "tconst") %>%
    dplyr::transmute(
      series_id = parentTconst,
      season = suppressWarnings(as.integer(seasonNumber)),
      episode = suppressWarnings(as.integer(episodeNumber)),
      title = primaryTitle,
      rating = as.numeric(averageRating),
      votes = as.integer(numVotes),
      episode_year = suppressWarnings(as.integer(startYear))
    ) %>%
    dplyr::filter(!is.na(season), !is.na(rating)) %>%
    tibble::as_tibble()

  .imdb_cache$episodes <- out
  out
}

# Every rated episode of one series, ordered by season and episode.
get_imdb_dataset_episodes <- function(imdb_id) {
  imdb_episode_table() %>%
    dplyr::filter(series_id == imdb_id) %>%
    dplyr::select(season, episode, title, rating, votes, episode_year) %>%
    dplyr::arrange(season, episode)
}
