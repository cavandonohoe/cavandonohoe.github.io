# Synthetic export fixtures only; no real listening data.
run_ingest <- function(export_dir, out_dir, ...) {
  libs <- paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep))
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(normalizePath("../../scripts/ingest_streaming_history.R")),
      shQuote(export_dir), shQuote(out_dir), ...),
    env = libs, stdout = TRUE, stderr = TRUE
  )
  testthat::expect_null(attr(status, "status"))
  status
}

write_export <- function(path, records) {
  jsonlite::write_json(records, path, auto_unbox = TRUE, null = "null")
}

song <- function(ts, ms, uri = "spotify:track:t1", ip = "192.0.2.1") {
  list(ts = ts, ms_played = ms, master_metadata_track_name = "Song",
       master_metadata_album_artist_name = "Artist", spotify_track_uri = uri,
       episode_name = NULL, spotify_episode_uri = NULL, ip_addr = ip)
}

testthat::test_that("ingest dedups plays, skips subfolders, keeps audiobooks, applies min-ms", {
  export_dir <- tempfile("export")
  out_dir <- tempfile("out")
  dir.create(file.path(export_dir, "copy"), recursive = TRUE)
  on.exit(unlink(c(export_dir, out_dir), recursive = TRUE), add = TRUE)

  first <- list(
    song("2024-01-01T23:30:00Z", 200000),
    song("2024-01-02T10:00:00Z", 5000),
    list(ts = "2024-01-03T08:00:00Z", ms_played = 600000,
         audiobook_title = "Book", audiobook_uri = "spotify:audiobook:b1",
         audiobook_chapter_title = "Chapter 1",
         audiobook_chapter_uri = "spotify:chapter:c1")
  )
  write_export(file.path(export_dir, "Streaming_History_Audio_2024.json"), first)
  # Overlapping export repeats the first play with a different IP.
  write_export(
    file.path(export_dir, "Streaming_History_Audio_2024_1.json"),
    list(song("2024-01-01T23:30:00Z", 200000, ip = "192.0.2.2"))
  )
  # A re-unzipped copy in a subfolder must not be read at all.
  write_export(file.path(export_dir, "copy", "Streaming_History_Audio_2024.json"), first)

  run_ingest(export_dir, out_dir, "--min-ms=30000")
  plays <- readr::read_csv(file.path(out_dir, "plays.csv"), show_col_types = FALSE)
  meta <- jsonlite::fromJSON(file.path(out_dir, "meta.json"))

  testthat::expect_equal(nrow(plays), 2L)
  testthat::expect_setequal(plays$kind, c("music", "audiobook"))
  book <- plays[plays$kind == "audiobook", ]
  testthat::expect_equal(book$title, "Chapter 1")
  testthat::expect_equal(book$source_name, "Book")
  # 23:30 UTC stays on its UTC date (no local-time conversion).
  testthat::expect_equal(as.character(plays$date[plays$kind == "music"]), "2024-01-01")
  testthat::expect_equal(meta$files_ingested, 2L)
  testthat::expect_equal(meta$duplicates_removed, 1L)
  testthat::expect_equal(meta$below_min_ms_removed, 1L)
  testthat::expect_equal(meta$timezone, "UTC")
  testthat::expect_false(meta$synthetic)
  testthat::expect_equal(meta$audiobook_minutes, 10)
})
