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

# --- missingness estimators -------------------------------------------------
# The saved-episodes endpoint only returns explicitly-saved (hearted)
# episodes, capped at offset 200. We can't recover the true "Your Episodes"
# count via the public API, but we can bound and characterise the gap.

# 1. Offset-cap headroom. Truncation is *active* only if the last page came
#    back full AND advertised a next page while we were at/over the cap.
saved_dates <- vapply(
  episodes, function(e) e$added_at %||% NA_character_, character(1)
)
saved_dates <- saved_dates[!is.na(saved_dates) & nzchar(saved_dates)]

cap <- 200L
truncation_active <- offset >= cap && last_page_had_next && last_page_size > 0
truncation_possible <- length(episodes) >= (cap - limit)

# 2. Calendar-gap detector. Months between the first and last save with zero
#    saved episodes are windows you likely listened via followed-show
#    auto-adds rather than hearting, i.e. present in "Your Episodes" but
#    invisible to this endpoint.
zero_save_months <- character(0)
if (length(saved_dates) > 1) {
  months_present <- unique(substr(saved_dates, 1, 7))
  rng <- range(as.Date(paste0(months_present, "-01")))
  all_months <- format(
    seq(rng[1], rng[2], by = "month"), "%Y-%m"
  )
  zero_save_months <- setdiff(all_months, months_present)
}

# 3. Followed-shows coverage. Pull followed shows and their episode totals so
#    the dashboard can express saved episodes as a share of everything
#    available across the shows you follow (an upper-bound denominator).
fetch_followed_shows <- function() {
  out <- list()
  after <- NULL
  repeat {
    req <- httr2::request("https://api.spotify.com/v1/me/shows") |>
      httr2::req_auth_bearer_token(access_token) |>
      httr2::req_url_query(limit = 50) |>
      httr2::req_retry(max_tries = 4)
    if (!is.null(after)) req <- httr2::req_url_query(req, after = after)
    page <- httr2::req_perform(req) |> httr2::resp_body_json()
    page_items <- page$items %||% list()
    if (!length(page_items)) break
    out <- c(out, page_items)
    after <- (page$cursors %||% list())$after
    if (is.null(after) || !nzchar(after)) break
  }
  out
}

followed_shows <- tryCatch(fetch_followed_shows(), error = function(e) list())
n_followed <- length(followed_shows)
total_available <- sum(vapply(followed_shows, function(s) {
  sh <- s$show %||% list()
  as.integer(sh$total_episodes %||% 0L)
}, integer(1)))

# Which shows you saved from are ones you actually follow?
followed_names <- vapply(followed_shows, function(s) {
  (s$show %||% list())$name %||% NA_character_
}, character(1))
saved_show_names <- unique(vapply(
  episodes, function(e) e$show %||% NA_character_, character(1)
))
saved_show_names <- saved_show_names[!is.na(saved_show_names)]
saved_from_unfollowed <- setdiff(saved_show_names, followed_names)

meta$missingness <- list(
  note = paste(
    "This endpoint returns only explicitly-saved (hearted) episodes and is",
    "capped at 200. Auto-added / followed-show episodes in \"Your Episodes\"",
    "are not exposed by the public API; the figures below bound the gap."
  ),
  offset_cap = list(
    cap = cap,
    n_returned = length(episodes),
    truncation_possible = truncation_possible,
    truncation_active = truncation_active
  ),
  zero_save_months = list(
    count = length(zero_save_months),
    months = as.list(zero_save_months)
  ),
  followed_shows = list(
    n_followed = n_followed,
    total_episodes_available = total_available,
    saved_share_of_available = if (total_available > 0) {
      round(length(episodes) / total_available, 4)
    } else {
      NA_real_
    },
    n_shows_saved_from = length(saved_show_names),
    n_saved_from_unfollowed = length(saved_from_unfollowed)
  )
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
