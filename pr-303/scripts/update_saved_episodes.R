#!/usr/bin/env Rscript
# update_saved_episodes.R
#
# Refreshes podcast-dashboard/data/saved_episodes.json from the Spotify Web
# API. Run unattended by the update_saved_episodes workflow on a daily cron.
#
# Auth: exchanges a stored refresh token (SPOTIFY_REFRESH_TOKEN) for a fresh
# hourly access token, so no interactive login is needed. Mint the refresh
# token once with scripts/get_spotify_refresh_token.R.
#
# Endpoint: GET /v1/me/episodes (current_user_saved_episodes). NOTE: this
# returns only episodes you explicitly SAVED (hearted). It does NOT include
# auto-downloaded / followed-show / auto-added episodes that show up under
# "Your Episodes" in the app, and it is capped at offset 200, so it only ever
# appends to the Account Data export membership and never removes episodes.
#
# Played state: with the `user-read-playback-position` scope, each episode
# carries resume_point.fully_played. It is stored as `fully_played`, and
# `finished_at` records the first refresh that saw it completed. The dashboard
# hides fully played episodes; the rows stay in the JSON. Without the scope
# the field is absent, `fully_played` stays null, and nothing is hidden.
#
# Output shape (must match what app.R reads):
#   { "meta": { generated_at, export_n_episodes, source, n_episodes,
#               api_refreshed_at, api_saved_count, ... },
#     "episodes": [ { id, added_at, show, publisher, name, dur_min, release,
#                     url, desc, fully_played, finished_at }, ... ] }

suppressWarnings(suppressMessages({
  ok <- requireNamespace("httr2", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)
}))
if (!ok) stop("This script needs the httr2 and jsonlite packages.")

`%||%` <- function(a, b) if (is.null(a)) b else a

out_path <- "podcast-dashboard/data/saved_episodes.json"

client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
refresh_token <- Sys.getenv("SPOTIFY_REFRESH_TOKEN")

missing <- c(
  if (!nzchar(client_id)) "SPOTIFY_CLIENT_ID",
  if (!nzchar(client_secret)) "SPOTIFY_CLIENT_SECRET",
  if (!nzchar(refresh_token)) "SPOTIFY_REFRESH_TOKEN"
)
if (length(missing)) {
  stop("Missing required env var(s): ", paste(missing, collapse = ", "))
}

# --- fresh access token from the stored refresh token -----------------------
get_access_token <- function() {
  resp <- httr2::request("https://accounts.spotify.com/api/token") |>
    httr2::req_auth_basic(client_id, client_secret) |>
    httr2::req_body_form(
      grant_type = "refresh_token",
      refresh_token = refresh_token
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  if (is.null(resp$access_token)) {
    stop("Spotify token refresh returned no access_token.")
  }
  resp$access_token
}

access_token <- get_access_token()

# --- page through saved episodes --------------------------------------------
fetch_page <- function(offset, limit = 50) {
  httr2::request("https://api.spotify.com/v1/me/episodes") |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_url_query(limit = limit, offset = offset, market = "US") |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

items <- list()
offset <- 0L
limit <- 50L
last_page_size <- 0L
last_page_had_next <- FALSE
repeat {
  page <- fetch_page(offset, limit)
  page_items <- page$items %||% list()
  last_page_size <- length(page_items)
  last_page_had_next <- !is.null(page$`next`)
  if (!length(page_items)) break
  items <- c(items, page_items)
  offset <- offset + limit
  # Spotify caps saved-episode paging at offset 200.
  if (is.null(page$`next`) || offset >= 200L) break
}

if (!length(items)) {
  stop("Spotify returned 0 saved episodes; refusing to overwrite the JSON.")
}

# The export covers older saves, so hitting the cap does not lose library
# membership, but saves older than the 200th stop getting API saved dates.
# Surface it as a GitHub Actions warning annotation instead of failing the run.
if (offset >= 200L && last_page_had_next && last_page_size > 0) {
  cat(paste0(
    "::warning title=Spotify saved-episodes cap reached::",
    "/me/episodes stopped at the 200-episode offset cap with more pages left. ",
    "Saves older than the 200th are not refreshed by the API until the next ",
    "Account Data export is imported.\n"
  ))
}

# --- normalize and merge into the full library export -----------------------
# The dashboard now uses the Account Data export as its membership source of
# truth. The Web API is therefore an incremental enrichment source: refresh
# known metadata and append newly saved episodes, but never replace the full
# exported library with the API's smaller/capped result.
normalize_episode <- function(ep, added_at = NA_character_) {
  ep <- ep %||% list()
  show <- ep$show %||% list()
  dur_ms <- ep$duration_ms %||% NA_real_
  # resume_point is only returned when the token has the
  # user-read-playback-position scope; NA means "unknown", not "unplayed".
  fully_played <- (ep$resume_point %||% list())$fully_played %||% NA
  list(
    id = ep$id %||% NA_character_,
    added_at = added_at,
    show = show$name %||% NA_character_,
    publisher = show$publisher %||% NA_character_,
    name = ep$name %||% NA_character_,
    dur_min = if (is.na(dur_ms)) NA_real_ else round(dur_ms / 60000),
    release = ep$release_date %||% NA_character_,
    url = (ep$external_urls %||% list())$spotify %||% NA_character_,
    desc = ep$description %||% NA_character_,
    fully_played = as.logical(fully_played)
  )
}

api_episodes <- lapply(items, function(it) {
  normalize_episode(it$episode, substr(it$added_at %||% NA_character_, 1, 10))
})

api_played <- vapply(api_episodes, function(e) e$fully_played, logical(1))
if (all(is.na(api_played))) {
  cat(paste0(
    "::warning title=Spotify played state unavailable::",
    "No saved episode returned resume_point, so finished episodes cannot be ",
    "hidden. Re-mint SPOTIFY_REFRESH_TOKEN with scripts/get_spotify_refresh_token.R ",
    "(needs the user-read-playback-position scope).\n"
  ))
}

# Episode metadata by ID (GET /v1/episodes, 50 IDs per call). Used to backfill
# export-only episodes the capped saved-episodes endpoint no longer returns.
# Unavailable or removed episodes come back as null and are skipped.
fetch_episode_metadata <- function(ids) {
  ids <- unique(ids[!is.na(ids) & nzchar(ids)])
  chunks <- split(ids, ceiling(seq_along(ids) / 50))
  out <- list()
  for (chunk in chunks) {
    resp <- httr2::request("https://api.spotify.com/v1/episodes") |>
      httr2::req_auth_bearer_token(access_token) |>
      httr2::req_url_query(ids = paste(chunk, collapse = ","), market = "US") |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    eps <- Filter(Negate(is.null), resp$episodes %||% list())
    out <- c(out, lapply(eps, normalize_episode))
  }
  out
}

# Merge API rows into the existing library. Returns the updated payload and
# whether the episode list changed. Meta is only touched when episodes change:
# stamping the refresh date every day would otherwise produce a daily commit and
# redeploy even when nothing new was saved, so `api_refreshed_at` means "last
# refresh that changed the library". `export_n_episodes` (set by
# import_library.py) is left alone, so n_episodes - export_n_episodes is the
# number of episodes appended by the API since the last Account Data export.
#
# `backfill` is a list of episode-metadata rows from GET /v1/episodes for
# export-only episodes the saved-episodes endpoint no longer returns. It only
# fills fields that are still missing and never sets `added_at`: that endpoint
# describes the episode, not when it was saved, so the save date stays unknown.
backfill_fields <- c("publisher", "dur_min", "release", "desc")

# `finished_at` is the first refresh date that saw the episode fully played.
# It is kept while the episode stays completed and cleared if Spotify later
# reports it as not fully played (e.g. progress reset). Episodes with unknown
# played state (null) are left alone.
stamp_finished <- function(episodes, today) {
  lapply(episodes, function(e) {
    played <- e$fully_played
    if (isTRUE(played) && is.null(e$finished_at)) {
      e$finished_at <- format(today)
    } else if (isFALSE(played) && !is.null(e$finished_at)) {
      e$finished_at <- NULL
    }
    e
  })
}

merge_saved_episodes <- function(existing, api_episodes, today = Sys.Date(),
                                 backfill = list()) {
  episodes <- existing$episodes %||% list()
  existing_ids <- vapply(episodes, function(e) e$id %||% NA_character_, character(1))
  api_ids <- vapply(api_episodes, function(e) e$id %||% NA_character_, character(1))
  backfill_ids <- vapply(backfill, function(e) e$id %||% NA_character_, character(1))
  is_missing <- function(x) is.null(x) || !length(x) || all(is.na(x))

  # Enrich existing library rows with fresh API metadata without dropping
  # fields that only exist in the Account Data export.
  for (i in seq_along(episodes)) {
    j <- match(existing_ids[[i]], api_ids)
    if (!is.na(j)) {
      fresh <- api_episodes[[j]]
      for (field in c("added_at", "publisher", "dur_min", "release", "url", "desc",
                      "fully_played")) {
        value <- fresh[[field]]
        if (!is.null(value) && length(value) && !all(is.na(value))) {
          episodes[[i]][[field]] <- value
        }
      }
    }
    k <- match(existing_ids[[i]], backfill_ids)
    if (!is.na(k)) {
      for (field in backfill_fields) {
        value <- backfill[[k]][[field]]
        if (is_missing(episodes[[i]][[field]]) && !is_missing(value)) {
          episodes[[i]][[field]] <- value
        }
      }
    }
  }

  # Newly saved API episodes may post-date the last Account Data export. Append
  # them so the dashboard grows between manual exports. API absence is NOT used
  # to delete anything because the endpoint is capped and does not represent
  # the complete library.
  new_idx <- which(!is.na(api_ids) & nzchar(api_ids) & !(api_ids %in% existing_ids))
  if (length(new_idx)) episodes <- c(episodes, api_episodes[new_idx])

  episodes <- stamp_finished(episodes, today)

  as_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null")
  changed <- !identical(as_json(episodes), as_json(existing$episodes %||% list()))
  meta <- existing$meta %||% list()
  if (changed) {
    meta$n_episodes <- length(episodes)
    meta$api_refreshed_at <- format(today)
    meta$api_saved_count <- length(api_episodes)
    meta$source <- "Spotify Account Data export + Spotify Web API incremental refresh"
  }
  list(
    payload = list(meta = meta, episodes = episodes),
    changed = changed, n_new = length(new_idx)
  )
}

existing <- jsonlite::fromJSON(out_path, simplifyDataFrame = FALSE)

# Only look up export rows the saved-episodes call did not just refresh and that
# are still missing metadata, so a fully backfilled library makes no extra
# calls. A lookup failure keeps today's refresh rather than failing the run.
api_ids <- vapply(api_episodes, function(e) e$id %||% NA_character_, character(1))
needs_backfill <- vapply(existing$episodes %||% list(), function(e) {
  !((e$id %||% NA_character_) %in% api_ids) &&
    any(vapply(backfill_fields, function(f) {
      v <- e[[f]]
      is.null(v) || !length(v) || all(is.na(v))
    }, logical(1)))
}, logical(1))
backfill_ids <- vapply(
  (existing$episodes %||% list())[needs_backfill],
  function(e) e$id %||% NA_character_, character(1)
)
backfill <- list()
if (length(backfill_ids)) {
  backfill <- tryCatch(fetch_episode_metadata(backfill_ids), error = function(e) {
    cat(paste0(
      "::warning title=Episode metadata backfill failed::",
      conditionMessage(e), "\n"
    ))
    list()
  })
}

merged <- merge_saved_episodes(existing, api_episodes, backfill = backfill)
payload <- merged$payload
json <- jsonlite::toJSON(
  payload, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null"
)
if (merged$changed) writeLines(json, out_path)

cat(sprintf(
  paste0(
    "Refreshed %d API saves (%d fully played); ",
    "backfilled metadata lookup for %d of %d episodes; ",
    "library now contains %d episodes (%d newly appended)%s\n"
  ),
  length(api_episodes), sum(api_played, na.rm = TRUE),
  length(backfill), length(backfill_ids),
  length(payload$episodes), merged$n_new,
  if (merged$changed) "" else "; no changes, JSON left untouched"
))
