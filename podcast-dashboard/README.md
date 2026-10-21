# Saved Podcast Episodes (Shiny)

An interactive dashboard of my saved Spotify podcast episodes: filter by
show, episode length, and date saved; search titles and descriptions; and
see episode counts, listening hours, and how saving activity trends over
time.

## Data

`data/saved_episodes.json` starts from the Account Data export's
`YourLibrary.json` for saved membership, titles, shows, and Spotify IDs. The
daily `update_saved_episodes` workflow then calls the Spotify Web API, refreshes
metadata (dates, descriptions, durations) for known episodes, and **appends**
newly saved ones. `meta.export_n_episodes` is the export's count; the rest were
added by the API. The API is capped and never removes episodes, so an unsave is
not reflected until the next export is imported. The dashboard footer shows
both the export import date and the latest API refresh that changed the data.
The API refresh only rewrites the file (and redeploys) when episodes change.

Finished episodes are dropped from the dashboard. The refresh stores Spotify's
`resume_point.fully_played` as `fully_played` (plus `finished_at`, the first
refresh that saw it completed), and `app.R` hides rows where it is `true`. The
rows stay in the JSON. This needs the `user-read-playback-position` scope on
the refresh token (see `scripts/get_spotify_refresh_token.R`); without it the
field stays null and nothing is hidden. Only episodes the capped API still
returns get a played state, so older export-only saves are never hidden.

Unknown lengths and saved dates remain null and are included in filters by
default. Disable "Include unknown lengths / saved dates" for complete metadata
only. Duration statistics use known values; the saved-date chart excludes unknown
dates. Listening history matches 103 current-library episodes.

To refresh from an Account Data ZIP, then rebuild listening aggregates:

```sh
python3 podcast-dashboard/tools/import_library.py "/path/to/my_spotify_data.zip"
python3 podcast-dashboard/tools/import_history.py "/path/to/Spotify Extended Streaming History"
```

The import date is not a saved date or an assertion about export generation time.
Only episode fields are imported; unrelated account data stays out of the repo.
No live credentials are used at runtime.

## Run locally

```r
shiny::runApp("podcast-dashboard")
```

## Deploy

Deployed to shinyapps.io:

```r
rsconnect::deployApp(
  appDir = "podcast-dashboard",
  appName = "podcast-dashboard",
  account = "cavandonohoe",
  server = "shinyapps.io"
)
```

Live: https://cavandonohoe.shinyapps.io/podcast-dashboard/

## Listening history

The dashboard joins `data/listening_history.json` to the saved snapshot by
Spotify episode ID. It adds recorded listening minutes, event counts, first/last
played dates (UTC), and playback start/end reason counts. All listening metrics
follow the existing filters and the listening-history filter. Saved duration
is labeled separately from actual recorded listening time.

Missing matches remain unknown, not zero or "unplayed". History can predate
saving; partial/repeat plays do not prove completion. The export coverage dates
are its outer bounds, not a guarantee of continuous history.

To regenerate the aggregate after replacing the saved snapshot or downloading
another extended-history export (Python 3.9+):

```sh
python3 podcast-dashboard/tools/import_history.py "/path/to/Spotify Extended Streaming History"
```

The importer reads audio and video files at the top level of the export folder
(not subfolders), counts a play once per timestamp, URI, and `ms_played`, and
writes only aggregates for IDs already in the saved snapshot. Raw exports, IP
addresses, devices, and unrelated listening history are not published. Start/end
reasons retain Spotify's raw codes and counts; they are not completion labels.
