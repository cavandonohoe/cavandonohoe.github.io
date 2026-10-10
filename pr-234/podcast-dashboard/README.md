# Saved Podcast Episodes (Shiny)

An interactive dashboard of my saved Spotify podcast episodes: filter by
show, episode length, and date saved; search titles and descriptions; and
see episode counts, listening hours, and how saving activity trends over
time.

## Data

`data/saved_episodes.json` uses the Account Data export's `YourLibrary.json`
for saved membership, titles, shows, and Spotify IDs. Known dates, descriptions,
and durations are retained from the previous API snapshot. The current export
contains 322 episodes (144 overlap the previous snapshot; 178 are additional).
The 42 previous-only IDs are not treated as currently saved.

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

The importer reads audio and video files, removes exact duplicate records, and
writes only aggregates for IDs already in the saved snapshot. Raw exports, IP
addresses, devices, and unrelated listening history are not published. Start/end
reasons retain Spotify's raw codes and counts; they are not completion labels.
