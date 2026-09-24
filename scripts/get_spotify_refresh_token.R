#!/usr/bin/env Rscript
# get_spotify_refresh_token.R
#
# ONE-TIME local helper to mint a long-lived Spotify refresh token for the
# scheduled `update_saved_episodes` workflow. The saved-episodes endpoint
# (current_user_saved_episodes) needs a user-authorized token with the
# `user-library-read` scope, and access tokens expire hourly, so CI must
# exchange a stored refresh token for a fresh access token on every run.
#
# You only run this once (or whenever you revoke the app). It walks the
# Authorization Code flow:
#   1. opens the Spotify consent page in your browser,
#   2. you approve and get redirected to the (local) redirect URI,
#   3. you paste the full redirected URL back here,
#   4. it prints the refresh token to store as a repo secret.
#
# Setup in the Spotify developer dashboard (https://developer.spotify.com):
#   - Create an app, note its Client ID / Client Secret.
#   - Add a Redirect URI of exactly http://127.0.0.1:8888/callback
#
# Usage:
#   SPOTIFY_CLIENT_ID=xxx SPOTIFY_CLIENT_SECRET=yyy \
#     Rscript scripts/get_spotify_refresh_token.R
#
# Then store as GitHub repo secrets (Settings > Secrets and variables >
# Actions):
#   SPOTIFY_CLIENT_ID, SPOTIFY_CLIENT_SECRET, SPOTIFY_REFRESH_TOKEN

client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
redirect_uri <- "http://127.0.0.1:8888/callback"

if (!nzchar(client_id) || !nzchar(client_secret)) {
  stop(
    "Set SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET in the environment ",
    "before running (see the header of this script)."
  )
}

auth_url <- httr2::url_modify(
  "https://accounts.spotify.com/authorize",
  query = list(
    client_id = client_id,
    response_type = "code",
    redirect_uri = redirect_uri,
    scope = "user-library-read",
    show_dialog = "true"
  )
)

cat("\n1. Open this URL in your browser and approve access:\n\n")
cat(auth_url, "\n\n")
try(utils::browseURL(auth_url), silent = TRUE)

cat(
  "2. After approving you'll be redirected to a ",
  redirect_uri, "?code=... URL that fails to load (that's expected).\n",
  "   Copy the FULL redirected URL from the address bar and paste it here.\n\n",
  sep = ""
)
redirected <- trimws(readline("Paste the redirected URL: "))

parsed <- httr2::url_parse(redirected)
code <- parsed$query$code
if (is.null(code) || !nzchar(code)) {
  stop("No ?code= found in the pasted URL. Re-run and paste the full URL.")
}

token_resp <- httr2::request("https://accounts.spotify.com/api/token") |>
  httr2::req_auth_basic(client_id, client_secret) |>
  httr2::req_body_form(
    grant_type = "authorization_code",
    code = code,
    redirect_uri = redirect_uri
  ) |>
  httr2::req_perform() |>
  httr2::resp_body_json()

if (is.null(token_resp$refresh_token)) {
  stop("Token exchange did not return a refresh_token. Response was:\n",
    jsonlite::toJSON(token_resp, auto_unbox = TRUE, pretty = TRUE))
}

cat("\nSuccess. Store these as GitHub Actions repo secrets:\n\n")
cat("  SPOTIFY_CLIENT_ID      =", client_id, "\n")
cat("  SPOTIFY_CLIENT_SECRET  = (the client secret you already have)\n")
cat("  SPOTIFY_REFRESH_TOKEN  =", token_resp$refresh_token, "\n\n")
cat("Keep the refresh token secret; it grants read access to your library.\n")
