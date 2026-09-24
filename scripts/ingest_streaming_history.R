#!/usr/bin/env Rscript
# ingest_streaming_history.R
#
# Ingests a Spotify "Extended Streaming History" GDPR export into a tidy
# dataset the streaming-history dashboard reads.
#
# The export is NOT available through the Spotify Web API. Request it at
#   spotify.com > Account > Privacy settings > "Extended streaming history"
# Spotify emails a ZIP within ~30 days. Unzip it and point this script at the
# folder of Streaming_History_Audio_*.json (and optional _Video_*.json) files.
#
# Usage:
#   Rscript scripts/ingest_streaming_history.R <export_dir> [out_dir]
#
#   <export_dir>  folder containing Streaming_History_*.json
#   [out_dir]     where to write outputs (default: streaming-history/data)
#
# Outputs (in out_dir):
#   plays.csv          one row per stream (tidy, the full log)
#   monthly.csv        minutes + plays per month, split music vs podcast
#   top_artists.csv    lifetime minutes/plays per artist
#   top_shows.csv      lifetime minutes/plays per podcast show
#   meta.json          coverage summary (date range, totals, file count)

suppressWarnings(suppressMessages({
  ok <- requireNamespace("jsonlite", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE) &&
    requireNamespace("lubridate", quietly = TRUE) &&
    requireNamespace("readr", quietly = TRUE)
}))
if (!ok) {
  stop("Needs the jsonlite, dplyr, lubridate and readr packages.")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("usage: Rscript scripts/ingest_streaming_history.R <export_dir> [out_dir]")
}
export_dir <- args[[1]]
out_dir <- if (length(args) >= 2) args[[2]] else "streaming-history/data"

if (!dir.exists(export_dir)) {
  stop("export_dir does not exist: ", export_dir)
}

files <- list.files(
  export_dir,
  pattern = "Streaming_History_(Audio|Video).*\\.json$",
  full.names = TRUE, recursive = TRUE
)
if (!length(files)) {
  stop(
    "No Streaming_History_*.json files found under ", export_dir,
    ". Point this at the unzipped Extended Streaming History folder."
  )
}

# --- read + bind all export files -------------------------------------------
# Extended history records use these keys (older exports may omit some, so we
# coalesce defensively):
#   ts, ms_played, master_metadata_track_name,
#   master_metadata_album_artist_name, master_metadata_album_album_name,
#   spotify_track_uri, episode_name, episode_show_name, spotify_episode_uri,
#   reason_start, reason_end, shuffle, skipped, platform, conn_country
read_one <- function(path) {
  recs <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  if (!length(recs)) return(NULL)
  col <- function(key, default = NA) {
    vapply(recs, function(r) {
      v <- r[[key]]
      if (is.null(v) || length(v) != 1) default else v
    }, FUN.VALUE = default)
  }
  tibble::tibble(
    ts = col("ts", NA_character_),
    ms_played = as.numeric(col("ms_played", NA_real_)),
    track = col("master_metadata_track_name", NA_character_),
    artist = col("master_metadata_album_artist_name", NA_character_),
    album = col("master_metadata_album_album_name", NA_character_),
    track_uri = col("spotify_track_uri", NA_character_),
    episode = col("episode_name", NA_character_),
    show = col("episode_show_name", NA_character_),
    episode_uri = col("spotify_episode_uri", NA_character_),
    reason_start = col("reason_start", NA_character_),
    reason_end = col("reason_end", NA_character_),
    shuffle = col("shuffle", NA),
    skipped = col("skipped", NA),
    platform = col("platform", NA_character_),
    country = col("conn_country", NA_character_)
  )
}

plays <- dplyr::bind_rows(lapply(files, read_one))

if (!nrow(plays)) {
  stop("Parsed 0 streams from the export files.")
}

# --- derive tidy fields -----------------------------------------------------
plays <- plays |>
  dplyr::mutate(
    played_at = lubridate::ymd_hms(ts, quiet = TRUE),
    date = as.Date(played_at),
    month = lubridate::floor_date(date, "month"),
    min_played = ms_played / 60000,
    kind = dplyr::case_when(
      !is.na(episode) & nzchar(episode) ~ "podcast",
      !is.na(track) & nzchar(track) ~ "music",
      TRUE ~ "other"
    ),
    title = dplyr::coalesce(track, episode),
    source_name = dplyr::coalesce(artist, show)
  ) |>
  dplyr::filter(!is.na(played_at)) |>
  dplyr::arrange(played_at)

# --- aggregates -------------------------------------------------------------
monthly <- plays |>
  dplyr::group_by(month, kind) |>
  dplyr::summarise(
    plays = dplyr::n(),
    minutes = round(sum(min_played, na.rm = TRUE), 1),
    .groups = "drop"
  )

top_artists <- plays |>
  dplyr::filter(kind == "music", !is.na(artist)) |>
  dplyr::group_by(artist) |>
  dplyr::summarise(
    plays = dplyr::n(),
    minutes = round(sum(min_played, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(minutes))

top_shows <- plays |>
  dplyr::filter(kind == "podcast", !is.na(show)) |>
  dplyr::group_by(show) |>
  dplyr::summarise(
    plays = dplyr::n(),
    minutes = round(sum(min_played, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(minutes))

meta <- list(
  generated_at = format(Sys.Date()),
  source = "Spotify Extended Streaming History (GDPR export)",
  files_ingested = length(files),
  n_streams = nrow(plays),
  first_stream = format(min(plays$date, na.rm = TRUE)),
  last_stream = format(max(plays$date, na.rm = TRUE)),
  total_minutes = round(sum(plays$min_played, na.rm = TRUE)),
  music_minutes = round(sum(plays$min_played[plays$kind == "music"], na.rm = TRUE)),
  podcast_minutes = round(
    sum(plays$min_played[plays$kind == "podcast"], na.rm = TRUE)
  )
)

# --- write ------------------------------------------------------------------
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(plays, file.path(out_dir, "plays.csv"))
readr::write_csv(monthly, file.path(out_dir, "monthly.csv"))
readr::write_csv(top_artists, file.path(out_dir, "top_artists.csv"))
readr::write_csv(top_shows, file.path(out_dir, "top_shows.csv"))
jsonlite::write_json(
  meta, file.path(out_dir, "meta.json"),
  auto_unbox = TRUE, pretty = TRUE
)

cat(sprintf(
  "Ingested %d streams from %d file(s), %s to %s. Wrote outputs to %s\n",
  meta$n_streams, meta$files_ingested, meta$first_stream, meta$last_stream,
  out_dir
))
