#!/usr/bin/env Rscript
# update_movie_ranker_personal.R
#
# Pull the singleton row from the movie-ranker Supabase project and replay
# the Elo ladder so my_imdb.html can sort each rating bucket by personal
# preference instead of falling back to IMDb's rating.
#
# What we read from Supabase:
#   public.ranker_state.{comparisons, overrides, graduates, version}
#
# What we read from the repo:
#   data/imdb_ratings.csv  - the IMDb export. Title IDs in Supabase are tconsts
#                            (e.g. tt2560140), which is the `Const` column.
#
# What we write:
#   data/movie_ranker_personal.csv  - one row per rated title with:
#     tconst, your_rating, elo, wins, losses, manual, graduated, personal_rank
#   personal_rank is 1..N within each `your_rating` bucket, sorted by elo desc.
#
# Required env vars (set as repo secrets in the workflow):
#   MOVIE_RANKER_SUPABASE_URL          - https://<ref>.supabase.co
#   MOVIE_RANKER_SUPABASE_SERVICE_KEY  - service role key (bypasses RLS)
#
# Elo math mirrors movie-ranker/src/lib/elo.ts and personal_stats.ts:
#   start  = 800 + your_rating * 50          (per-bucket anchor)
#   K      = 32
#   replay comparisons in created_at order
#   apply manual overrides last (clobber rating, keep W-L)

`%>%` <- magrittr::`%>%`

`%||%` <- function(a, b) if (is.null(a)) b else a

supabase_url <- Sys.getenv("MOVIE_RANKER_SUPABASE_URL", unset = "")
service_key  <- Sys.getenv("MOVIE_RANKER_SUPABASE_SERVICE_KEY", unset = "")

if (!nzchar(supabase_url) || !nzchar(service_key)) {
  stop(
    "MOVIE_RANKER_SUPABASE_URL and MOVIE_RANKER_SUPABASE_SERVICE_KEY must be ",
    "set. Add them as repo secrets and pass them into the workflow env."
  )
}

# --- Fetch singleton row -----------------------------------------------------
fetch_state <- function() {
  req <- httr2::request(paste0(supabase_url, "/rest/v1/ranker_state")) %>%
    httr2::req_url_query(id = "eq.singleton", select = "*") %>%
    httr2::req_headers(
      apikey = service_key,
      Authorization = paste("Bearer", service_key),
      Accept = "application/json"
    ) %>%
    httr2::req_retry(max_tries = 4, backoff = ~ min(2 ^ .x, 30)) %>%
    httr2::req_user_agent("cavandonohoe.github.io/update-movie-ranker-personal")
  resp <- httr2::req_perform(req)
  rows <- jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = FALSE
  )
  if (length(rows) == 0L) {
    stop("Supabase returned no rows for ranker_state singleton.")
  }
  rows[[1]]
}

state <- fetch_state()
comparisons <- state$comparisons %||% list()
overrides   <- state$overrides   %||% list()
graduates   <- state$graduates   %||% list()
version     <- state$version     %||% NA_integer_

cat(sprintf(
  "Supabase: %d comparisons, %d overrides, version %s\n",
  length(comparisons), length(overrides),
  ifelse(is.na(version), "NA", as.character(version))
))

# --- Load titles from imdb_ratings.csv --------------------------------------
ratings_csv <- here::here("data", "imdb_ratings.csv")
if (!file.exists(ratings_csv)) {
  stop("Missing ", ratings_csv,
       " - the IMDb export must be in the repo before this script runs.")
}
ratings <- readr::read_csv(ratings_csv, show_col_types = FALSE) %>%
  dplyr::rename(tconst = Const, your_rating = `Your Rating`) %>%
  dplyr::filter(!is.na(your_rating))

cat(sprintf("IMDb export: %d rated titles\n", nrow(ratings)))

# --- Elo replay --------------------------------------------------------------
bucket_anchor <- function(your_rating) 800 + your_rating * 50
K_FACTOR <- 32

stats <- ratings %>%
  dplyr::transmute(
    tconst,
    rating = bucket_anchor(your_rating),
    wins = 0L,
    losses = 0L,
    manual = FALSE
  )
stats_env <- new.env(parent = emptyenv())
for (i in seq_len(nrow(stats))) {
  stats_env[[stats$tconst[i]]] <- list(
    rating = stats$rating[i],
    wins = 0L,
    losses = 0L,
    manual = FALSE
  )
}

# Sort comparisons by created_at (ISO8601 strings sort lexicographically).
created_at <- vapply(
  comparisons,
  function(c) as.character(c$created_at %||% ""),
  character(1)
)
comparisons <- comparisons[order(created_at)]

skipped <- 0L
applied <- 0L
for (cmp in comparisons) {
  w_id <- cmp$winner_title_id
  l_id <- cmp$loser_title_id
  w <- stats_env[[w_id]]
  l <- stats_env[[l_id]]
  if (is.null(w) || is.null(l)) {
    skipped <- skipped + 1L
    next
  }
  expected_w <- 1 / (1 + 10 ^ ((l$rating - w$rating) / 400))
  expected_l <- 1 / (1 + 10 ^ ((w$rating - l$rating) / 400))
  new_w <- round(w$rating + K_FACTOR * (1 - expected_w))
  new_l <- round(l$rating + K_FACTOR * (0 - expected_l))
  stats_env[[w_id]] <- list(
    rating = new_w, wins = w$wins + 1L, losses = w$losses, manual = FALSE
  )
  stats_env[[l_id]] <- list(
    rating = new_l, wins = l$wins, losses = l$losses + 1L, manual = FALSE
  )
  applied <- applied + 1L
}
cat(sprintf("Replayed %d comparisons (%d skipped: title not in repo)\n",
            applied, skipped))

# Apply manual overrides last - clobber rating, keep W-L.
for (o in overrides) {
  cur <- stats_env[[o$title_id]]
  if (is.null(cur)) next
  stats_env[[o$title_id]] <- list(
    rating = as.numeric(o$rating),
    wins = cur$wins,
    losses = cur$losses,
    manual = TRUE
  )
}

# Flatten graduates: { "movie:10": [tconst, ...], "series:9": [...], ... } ->
# a set of tconsts that are "graduated" in any bucket.
graduated_tconsts <- unique(unlist(graduates, use.names = FALSE))
graduated_tconsts <- graduated_tconsts[!is.na(graduated_tconsts) &
                                        nzchar(graduated_tconsts)]

# --- Build the personal CSV --------------------------------------------------
personal <- ratings %>%
  dplyr::transmute(
    tconst,
    your_rating
  ) %>%
  dplyr::mutate(
    elo = vapply(tconst, function(t) stats_env[[t]]$rating, numeric(1)),
    wins = vapply(tconst, function(t) stats_env[[t]]$wins, integer(1)),
    losses = vapply(tconst, function(t) stats_env[[t]]$losses, integer(1)),
    manual = vapply(tconst, function(t) stats_env[[t]]$manual, logical(1)),
    graduated = tconst %in% graduated_tconsts
  ) %>%
  dplyr::group_by(your_rating) %>%
  dplyr::arrange(dplyr::desc(elo), .by_group = TRUE) %>%
  dplyr::mutate(personal_rank = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(your_rating), personal_rank) %>%
  dplyr::select(
    tconst, your_rating, personal_rank, elo, wins, losses, manual, graduated
  )

out_path <- here::here("data", "movie_ranker_personal.csv")
readr::write_csv(personal, out_path)

cat(sprintf("Wrote %s (%d rows)\n", out_path, nrow(personal)))

# Quick sanity log: top 5 in the 10/10 bucket. Useful to eyeball in CI logs.
top10 <- personal %>%
  dplyr::filter(your_rating == 10) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::left_join(
    ratings %>% dplyr::select(tconst, Title, Year),
    by = "tconst"
  )
cat("Top 5 in 10/10 bucket:\n")
print(top10, n = 5)
