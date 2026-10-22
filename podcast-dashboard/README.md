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
added by the API. Spotify's saved-episodes paging is unreliable (each page size
returns a different partial slice), so the refresh pages at several sizes and
keeps the union. Finishing or unsaving an episode drops it from the API; once an
episode the API previously returned is missing on two runs in a row, it gets
`removed_at` and the dashboard hides it (the row stays in the JSON and comes
back if the API returns it again). Episodes the API has never returned are only
corrected by the next export import. The dashboard footer shows
both the export import date and the latest API refresh that changed the data.
The API refresh only rewrites the file (and redeploys) when episodes change.

Unknown lengths and saved dates remain null and are included in filters by
default. Disable "Include unknown lengths / saved dates" for complete metadata
only. Duration statistics use known values; the saved-date chart excludes unknown
dates. Listening history matches 103 current-library episodes.

To refresh from an Account Data ZIP, then rebuild listening aggregates:

```sh
python3 podcast-dashboard/tools/import_library.py "/path/to/my_spotify_data.zip" --as-of 2026-10-25
python3 podcast-dashboard/tools/import_history.py "/path/to/Spotify Extended Streaming History"
```

The export is the source of truth as of `--as-of`, the day Spotify generated
it (check the "your data is ready" email; without the flag it falls back to
the `YourLibrary.json` timestamp inside the ZIP). API evidence from after that
day still wins: episodes the refresh hid after `--as-of` (finished or unsaved
since) stay hidden, and episodes saved after it stay listed. Everything else
follows the export. `meta.export_as_of` records the snapshot date and
`meta.generated_at` the import date; neither is a saved date.
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
