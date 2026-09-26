# Saved Podcast Episodes (Shiny)

An interactive dashboard of my saved Spotify podcast episodes: filter by
show, episode length, and date saved; search titles and descriptions; and
see episode counts, listening hours, and how saving activity trends over
time.

## Data

`data/saved_episodes.json` is a static snapshot pulled from the Spotify Web
API (`current_user_saved_episodes`). No live credentials are used at
runtime, so the app is fully self-contained.

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
