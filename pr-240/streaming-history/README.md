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
and `meta.json` into `streaming-history/data/`.

The committed `data/` currently holds a tiny synthetic sample so the app
renders before a real export is ingested. Replace it by running the
ingestion script on your real export.

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
