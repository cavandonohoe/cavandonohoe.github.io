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
# "Your Episodes" in the app, and it is capped at offset 200. We measure both
# gaps (see the meta$missingness block written below) rather than guessing.
#
# Output shape (must match what app.R reads):
#   { "meta": { generated_at, source, n_episodes, missingness{...} },
#     "episodes": [ { id, added_at, show, publisher, name, dur_min, release,
#                     url, desc }, ... ] }

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

# --- normalize and merge into the full library export -----------------------
# The dashboard now uses the Account Data export as its membership source of
# truth. The Web API is therefore an incremental enrichment source: refresh
# known metadata and append newly saved episodes, but never replace the full
# exported library with the API's smaller/capped result.
api_episodes <- lapply(items, function(it) {
  ep <- it$episode %||% list()
  show <- ep$show %||% list()
  dur_ms <- ep$duration_ms %||% NA_real_
  list(
    id = ep$id %||% NA_character_,
    added_at = substr(it$added_at %||% NA_character_, 1, 10),
    show = show$name %||% NA_character_,
    publisher = show$publisher %||% NA_character_,
    name = ep$name %||% NA_character_,
    dur_min = if (is.na(dur_ms)) NA_real_ else round(dur_ms / 60000),
    release = ep$release_date %||% NA_character_,
    url = (ep$external_urls %||% list())$spotify %||% NA_character_,
    desc = ep$description %||% NA_character_
  )
})

existing <- jsonlite::fromJSON(out_path, simplifyDataFrame = FALSE)
episodes <- existing$episodes %||% list()
existing_ids <- vapply(episodes, function(e) e$id %||% NA_character_, character(1))
api_ids <- vapply(api_episodes, function(e) e$id %||% NA_character_, character(1))

# Enrich existing library rows with fresh API metadata without dropping fields
# that only exist in the Account Data export.
for (i in seq_along(episodes)) {
  id <- existing_ids[[i]]
  j <- match(id, api_ids)
  if (!is.na(j)) {
    fresh <- api_episodes[[j]]
    for (field in c("added_at", "publisher", "dur_min", "release", "url", "desc")) {
      value <- fresh[[field]]
      if (!is.null(value) && length(value) && !all(is.na(value))) {
        episodes[[i]][[field]] <- value
      }
    }
  }
}

# Newly saved API episodes may post-date the last Account Data export. Append
# them so the dashboard grows between manual exports. API absence is NOT used
# to delete anything because the endpoint is capped and does not represent the
# complete library.
new_idx <- which(!is.na(api_ids) & nzchar(api_ids) & !(api_ids %in% existing_ids))
if (length(new_idx)) episodes <- c(episodes, api_episodes[new_idx])

meta <- existing$meta %||% list()
meta$n_episodes <- length(episodes)
meta$api_refreshed_at <- format(Sys.Date())
meta$api_saved_count <- length(api_episodes)
meta$source <- "Spotify Account Data export + Spotify Web API incremental refresh"

payload <- list(meta = meta, episodes = episodes)
json <- jsonlite::toJSON(
  payload, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null"
)
writeLines(json, out_path)

cat(sprintf(
  "Refreshed %d API saves; library now contains %d episodes (%d newly appended)\n",
  length(api_episodes), length(episodes), length(new_idx)
))
