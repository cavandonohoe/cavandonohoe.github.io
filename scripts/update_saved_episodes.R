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
# Endpoint: GET /v1/me/episodes (current_user_saved_episodes). NOTE: Spotify
# caps this at offset 200 and only serves the most recently saved episodes,
# so the JSON is a subset of the full "Your Episodes" list (documented in
# app.R). We page until the API stops returning items.
#
# Output shape (must match what app.R reads):
#   { "meta": { generated_at, source, n_episodes },
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
repeat {
  page <- fetch_page(offset, limit)
  page_items <- page$items %||% list()
  if (!length(page_items)) break
  items <- c(items, page_items)
  offset <- offset + limit
  # Spotify caps saved-episode paging at offset 200.
  if (is.null(page$`next`) || offset >= 200L) break
}

if (!length(items)) {
  stop("Spotify returned 0 saved episodes; refusing to overwrite the JSON.")
}

# --- normalize into the app's episode shape ---------------------------------
episodes <- lapply(items, function(it) {
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

meta <- list(
  generated_at = format(Sys.Date()),
  source = "Spotify Web API (current_user_saved_episodes)",
  n_episodes = length(episodes)
)

payload <- list(meta = meta, episodes = episodes)

json <- jsonlite::toJSON(
  payload,
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null"
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
writeLines(json, out_path)

cat(sprintf("Wrote %d episodes to %s\n", length(episodes), out_path))
