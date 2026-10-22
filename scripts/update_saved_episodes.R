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
# "Your Episodes" in the app, and its paging is unreliable (see
# collect_saved_items). It refreshes metadata, appends new saves, and marks
# rows removed only after repeated absences (see track_api_removals). The
# Account Data export stays the membership source of truth.
#
# Output shape (must match what app.R reads):
#   { "meta": { generated_at, export_n_episodes, source, n_episodes,
#               n_removed, api_refreshed_at, api_saved_count, ... },
#     "episodes": [ { id, added_at, show, publisher, name, dur_min, release,
#                     url, desc, api_seen, api_misses, removed_at }, ... ] }

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

# /me/episodes paging is unreliable: `total` shrinks by the requested offset,
# results stop around the 150th row whatever `total` says, some rows come back
# with a null episode, and different page sizes return different subsets of
# the library. Paging once at several page sizes and keeping the union of
# episode IDs sees far more of the library than any single pass.
collect_saved_items <- function(fetch, limits = c(50L, 20L, 10L), max_rows = 1000L) {
  by_id <- list()
  pass_counts <- integer()
  for (limit in limits) {
    offset <- 0L
    n_pass <- 0L
    while (offset < max_rows) {
      page_items <- fetch(offset, limit)$items %||% list()
      if (!length(page_items)) break
      for (it in page_items) {
        id <- (it$episode %||% list())$id %||% NA_character_
        if (is.na(id) || !nzchar(id)) next
        n_pass <- n_pass + 1L
        # Keep the first copy seen; every pass describes the same save.
        if (is.null(by_id[[id]])) by_id[[id]] <- it
      }
      offset <- offset + limit
    }
    pass_counts[[as.character(limit)]] <- n_pass
  }
  list(items = unname(by_id), pass_counts = pass_counts)
}

collected <- collect_saved_items(fetch_page)
items <- collected$items
cat(sprintf(
  "Saved-episode passes (page size: episodes): %s; union %d unique\n",
  paste(names(collected$pass_counts), collected$pass_counts, sep = ": ", collapse = ", "),
  length(items)
))

if (!length(items)) {
  stop("Spotify returned 0 saved episodes; refusing to overwrite the JSON.")
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
  list(
    id = ep$id %||% NA_character_,
    added_at = added_at,
    show = show$name %||% NA_character_,
    publisher = show$publisher %||% NA_character_,
    name = ep$name %||% NA_character_,
    dur_min = if (is.na(dur_ms)) NA_real_ else round(dur_ms / 60000),
    release = ep$release_date %||% NA_character_,
    url = (ep$external_urls %||% list())$spotify %||% NA_character_,
    desc = ep$description %||% NA_character_
  )
}

api_episodes <- lapply(items, function(it) {
  normalize_episode(it$episode, substr(it$added_at %||% NA_character_, 1, 10))
})

# Episode metadata by ID (GET /v1/episodes, 50 IDs per call). Used to backfill
# export-only episodes the saved-episodes endpoint does not return.
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

# Finishing an episode (or unsaving it) removes it from "Your Episodes" and
# from /me/episodes. Paging is too unreliable for one absence to mean anything,
# so a row is only marked removed after the API has returned it at least once
# (`api_seen`) and then missed it on `removal_misses` refreshes in a row.
# A saved date (`added_at`) also counts as seen: only this API supplies it (the
# export has none and the metadata backfill never sets it), so rows the API
# returned before `api_seen` existed are tracked too.
# `removed_at` hides the row in the dashboard; the row stays in the JSON and
# is restored if the API returns it again. Export-only rows the API has never
# returned are never marked.
track_api_removals <- function(episodes, api_ids, today, removal_misses = 2L) {
  has_value <- function(x) !is.null(x) && length(x) && !all(is.na(x))
  lapply(episodes, function(e) {
    if ((e$id %||% NA_character_) %in% api_ids) {
      e$api_seen <- TRUE
      e$api_misses <- NULL
      e$removed_at <- NULL
    } else if (is.null(e$removed_at) && (isTRUE(e$api_seen) || has_value(e$added_at))) {
      misses <- (e$api_misses %||% 0L) + 1L
      if (misses >= removal_misses) {
        e$api_misses <- NULL
        e$removed_at <- format(today)
      } else {
        e$api_misses <- misses
      }
    }
    e
  })
}

merge_saved_episodes <- function(existing, api_episodes, today = Sys.Date(),
                                 backfill = list(), track_removals = TRUE) {
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
      for (field in c("added_at", "publisher", "dur_min", "release", "url", "desc")) {
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
  # them so the dashboard grows between manual exports.
  new_idx <- which(!is.na(api_ids) & nzchar(api_ids) & !(api_ids %in% existing_ids))
  if (length(new_idx)) episodes <- c(episodes, api_episodes[new_idx])

  if (track_removals) episodes <- track_api_removals(episodes, api_ids, today)

  as_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null")
  changed <- !identical(as_json(episodes), as_json(existing$episodes %||% list()))
  meta <- existing$meta %||% list()
  if (changed) {
    meta$n_episodes <- length(episodes)
    meta$n_removed <- sum(vapply(episodes, function(e) !is.null(e$removed_at), logical(1)))
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

# Removal tracking needs a healthy API pass. If today's union is under half of
# the rows the API is currently known to return, treat it as an API glitch:
# still refresh metadata, but don't count misses against anything.
n_tracked <- sum(vapply(existing$episodes %||% list(), function(e) {
  is.null(e$removed_at) && (isTRUE(e$api_seen) || !is.null(e$added_at))
}, logical(1)))
track_removals <- length(api_ids) >= n_tracked / 2
if (!track_removals) {
  cat(sprintf(paste0(
    "::warning title=Removal tracking skipped::API returned %d saves vs %d ",
    "tracked; not counting misses this run.\n"
  ), length(api_ids), n_tracked))
}

merged <- merge_saved_episodes(existing, api_episodes, backfill = backfill,
                               track_removals = track_removals)
payload <- merged$payload
json <- jsonlite::toJSON(
  payload, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null"
)
if (merged$changed) writeLines(json, out_path)

cat(sprintf(
  paste0(
    "Refreshed %d API saves; backfilled metadata lookup for %d of %d episodes; ",
    "library now contains %d episodes (%d newly appended, %d marked removed)%s\n"
  ),
  length(api_episodes), length(backfill), length(backfill_ids),
  length(payload$episodes), merged$n_new,
  sum(vapply(payload$episodes, function(e) !is.null(e$removed_at), logical(1))),
  if (merged$changed) "" else "; no changes, JSON left untouched"
))
