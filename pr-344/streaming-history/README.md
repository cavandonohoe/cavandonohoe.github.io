# Spotify Streaming History (Shiny)

A dashboard of my full Spotify listening history: minutes per month (music
vs podcast), top artists, top podcast shows, and a searchable stream log.

## Data

Unlike the saved-episodes dashboard (which uses the Web API), the full
listening history is **not** available through any Spotify API. It only
comes from Spotify's GDPR data export:

1. Go to spotify.com > Account > **Privacy settings**.
2. Request **Extended streaming history** (not just "Account data").
3. Spotify emails a download link within ~30 days.
4. Unzip it; you'll get `Streaming_History_Audio_*.json` files.

Then ingest into the tidy CSVs this app reads:

```bash
Rscript scripts/ingest_streaming_history.R /path/to/unzipped_export \
  streaming-history/data
```

That writes `plays.csv`, `monthly.csv`, `top_artists.csv`, `top_shows.csv`,
and `meta.json` into `streaming-history/data/`. That folder holds your full
personal play log, so it is **gitignored**: it stays on your machine and is
bundled only when you deploy the app yourself.

Options and conventions:

- Point the script at the export folder itself. Subfolders are not read, so a
  second unzipped copy does not double count.
- The same play repeated across overlapping exports (same timestamp, item URI,
  and `ms_played`) is kept once.
- `--min-ms=30000` drops very short streams (Spotify counts a stream at 30
  seconds); the default keeps everything.
- Audiobook chapters (`audiobook_*` fields) are a separate `audiobook` kind.
- All dates and times are **UTC**, as recorded by Spotify.

The committed `sample-data/` folder holds a tiny **synthetic** sample. When
`data/` is absent the app falls back to it and shows a "Synthetic sample data"
banner, so the public repo never contains real listening history.

## Run locally

```r
shiny::runApp("streaming-history")
```

## Deploy

```r
rsconnect::deployApp(
  appDir = "streaming-history",
  appName = "streaming-history",
  account = "cavandonohoe",
  server = "shinyapps.io"
)
```
