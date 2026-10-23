testthat::test_that("refresh JSON preserves missing export metadata as null", {
  expressions <- parse("../../scripts/update_saved_episodes.R")
  writer <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("json"))
  }, as.list(expressions))
  env <- new.env()
  env$payload <- jsonlite::fromJSON(
    '{"episodes":[{"id":"a","added_at":null,"dur_min":null},{"id":"b","added_at":"2026-09-01","dur_min":30}]}',
    simplifyDataFrame = FALSE
  )
  eval(writer[[1]], envir = env)
  rows <- jsonlite::fromJSON(env$json)$episodes
  testthat::expect_equal(as.Date(rows$added_at), as.Date(c(NA, "2026-09-01")))
  testthat::expect_equal(as.numeric(rows$dur_min), c(NA, 30))
})

testthat::test_that("token helper reads a redirected URL under Rscript", {
  expressions <- as.list(parse("../../scripts/get_spotify_refresh_token.R"))
  input <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("redirected"))
  }, expressions)
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(unlist(lapply(input, deparse)), "cat(redirected)"), script)
  url <- "http://127.0.0.1:8888/callback?code=synthetic-code"
  result <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
                    input = url, stdout = TRUE)
  testthat::expect_identical(result, url)
})

load_merge_saved_episodes <- function() {
  expressions <- as.list(parse("../../scripts/update_saved_episodes.R"))
  wanted <- c("%||%", "backfill_fields", "track_api_removals", "mark_unavailable",
              "merge_saved_episodes")
  defs <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) && as.character(x[[2]]) %in% wanted
  }, expressions)
  env <- new.env()
  for (d in defs) eval(d, envir = env)
  env$merge_saved_episodes
}

testthat::test_that("an unchanged API refresh leaves meta (and the file) untouched", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(
    meta = list(generated_at = "2026-09-27", n_episodes = 1, export_n_episodes = 1,
                api_refreshed_at = "2026-10-01", api_saved_count = 1),
    episodes = list(list(id = "a", added_at = "2026-09-01", name = "A", dur_min = 30,
                         api_seen = TRUE))
  )
  same <- list(list(id = "a", added_at = "2026-09-01", name = "A", dur_min = 30))
  result <- merge_saved_episodes(existing, same, today = as.Date("2026-10-10"))
  testthat::expect_false(result$changed)
  testthat::expect_identical(result$payload$meta, existing$meta)
})

testthat::test_that("a new API save appends and stamps meta without moving the export count", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(
    meta = list(n_episodes = 1, export_n_episodes = 1, api_refreshed_at = "2026-10-01"),
    episodes = list(list(id = "a", added_at = NULL, name = "A", dur_min = NULL))
  )
  api <- list(
    list(id = "a", added_at = "2026-09-01", name = "A", dur_min = 30),
    list(id = "b", added_at = "2026-10-09", name = "B", dur_min = 45)
  )
  result <- merge_saved_episodes(existing, api, today = as.Date("2026-10-10"))
  testthat::expect_true(result$changed)
  testthat::expect_equal(result$n_new, 1L)
  meta <- result$payload$meta
  testthat::expect_equal(meta$n_episodes, 2L)
  testthat::expect_equal(meta$export_n_episodes, 1)
  testthat::expect_equal(meta$api_refreshed_at, "2026-10-10")
  testthat::expect_equal(result$payload$episodes[[1]]$dur_min, 30)
})

testthat::test_that("backfill fills missing metadata but never invents a saved date", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(
    meta = list(n_episodes = 2, export_n_episodes = 2),
    episodes = list(
      list(id = "old", added_at = NULL, name = "Old", show = "S", dur_min = NULL,
           release = NULL, publisher = NULL, desc = NULL),
      list(id = "kept", added_at = "2026-01-05", name = "Kept", show = "S",
           dur_min = 20, release = "2026-01-01", publisher = "P", desc = "orig")
    )
  )
  backfill <- list(
    list(id = "old", added_at = NA_character_, name = "Old", show = "S",
         dur_min = 42, release = "2024-03-01", publisher = "Pub", desc = "d"),
    list(id = "kept", added_at = NA_character_, name = "Kept", show = "S",
         dur_min = 99, release = "1999-01-01", publisher = "X", desc = "new")
  )
  result <- merge_saved_episodes(existing, list(), today = as.Date("2026-10-13"),
                                 backfill = backfill)
  old <- result$payload$episodes[[1]]
  kept <- result$payload$episodes[[2]]
  testthat::expect_true(result$changed)
  testthat::expect_equal(old$dur_min, 42)
  testthat::expect_equal(old$release, "2024-03-01")
  testthat::expect_equal(old$publisher, "Pub")
  testthat::expect_null(old$added_at)
  # Existing values are never overwritten by backfill.
  testthat::expect_equal(kept$dur_min, 20)
  testthat::expect_equal(kept$desc, "orig")
  testthat::expect_equal(result$n_new, 0L)
})

testthat::test_that("an empty backfill leaves an unchanged library untouched", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(
    meta = list(n_episodes = 1, api_refreshed_at = "2026-10-01"),
    episodes = list(list(id = "a", added_at = NULL, name = "A", dur_min = NULL))
  )
  result <- merge_saved_episodes(existing, list(), backfill = list())
  testthat::expect_false(result$changed)
  testthat::expect_identical(result$payload$meta, existing$meta)
})

load_collect_saved_items <- function() {
  expressions <- as.list(parse("../../scripts/update_saved_episodes.R"))
  wanted <- c("%||%", "collect_saved_items")
  defs <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) && as.character(x[[2]]) %in% wanted
  }, expressions)
  env <- new.env()
  for (d in defs) eval(d, envir = env)
  env$collect_saved_items
}

testthat::test_that("paging at several page sizes keeps the union of saved episodes", {
  collect_saved_items <- load_collect_saved_items()
  item <- function(id) list(added_at = "2026-10-01T00:00:00Z", episode = list(id = id))
  # Each page size sees a different slice, with a null episode mixed in.
  slices <- list(
    "2" = list(item("a"), item("b"), list(added_at = "2026-10-01", episode = NULL)),
    "1" = list(item("b"), item("c"))
  )
  fetch <- function(offset, limit) {
    rows <- slices[[as.character(limit)]]
    idx <- seq_len(limit) + offset
    list(items = rows[idx[idx <= length(rows)]])
  }
  result <- collect_saved_items(fetch, limits = c(2L, 1L))
  ids <- vapply(result$items, function(it) it$episode$id, character(1))
  testthat::expect_setequal(ids, c("a", "b", "c"))
  testthat::expect_identical(result$pass_counts, c("2" = 2L, "1" = 2L))
})

testthat::test_that("paging stops at max_rows even if the API never runs dry", {
  collect_saved_items <- load_collect_saved_items()
  calls <- 0L
  fetch <- function(offset, limit) {
    calls <<- calls + 1L
    list(items = list(list(episode = list(id = paste0("e", offset)))))
  }
  result <- collect_saved_items(fetch, limits = 1L, max_rows = 5L)
  testthat::expect_identical(calls, 5L)
  testthat::expect_length(result$items, 5L)
})

testthat::test_that("an API-seen episode is marked removed only after two misses in a row", {
  merge_saved_episodes <- load_merge_saved_episodes()
  ep <- function(id) list(id = id, name = toupper(id), dur_min = 30)
  existing <- list(meta = list(n_episodes = 2), episodes = list(ep("a"), ep("b")))
  # Run 1: both returned, so both become API-tracked.
  run1 <- merge_saved_episodes(existing, list(ep("a"), ep("b")), today = as.Date("2026-10-20"))
  testthat::expect_true(all(vapply(run1$payload$episodes, function(e) {
    isTRUE(e$api_seen)
  }, logical(1))))
  # Run 2: "b" missing once, not removed yet.
  run2 <- merge_saved_episodes(run1$payload, list(ep("a")), today = as.Date("2026-10-21"))
  b2 <- run2$payload$episodes[[2]]
  testthat::expect_equal(b2$api_misses, 1L)
  testthat::expect_null(b2$removed_at)
  # Run 3: missing again, now removed; meta counts it.
  run3 <- merge_saved_episodes(run2$payload, list(ep("a")), today = as.Date("2026-10-22"))
  b3 <- run3$payload$episodes[[2]]
  testthat::expect_equal(b3$removed_at, "2026-10-22")
  testthat::expect_null(b3$api_misses)
  testthat::expect_equal(run3$payload$meta$n_removed, 1L)
  testthat::expect_length(run3$payload$episodes, 2L)
  # Run 4: returned again, marker cleared.
  run4 <- merge_saved_episodes(run3$payload, list(ep("a"), ep("b")),
                               today = as.Date("2026-10-23"))
  testthat::expect_null(run4$payload$episodes[[2]]$removed_at)
  testthat::expect_equal(run4$payload$meta$n_removed, 0L)
})

testthat::test_that("a single miss followed by a hit resets the miss count", {
  merge_saved_episodes <- load_merge_saved_episodes()
  ep <- list(id = "a", name = "A", api_seen = TRUE, api_misses = 1L)
  existing <- list(meta = list(), episodes = list(ep))
  result <- merge_saved_episodes(existing, list(list(id = "a", name = "A")))
  testthat::expect_null(result$payload$episodes[[1]]$api_misses)
  testthat::expect_null(result$payload$episodes[[1]]$removed_at)
})

testthat::test_that("export-only episodes the API never returned are never marked removed", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(meta = list(), episodes = list(list(id = "old", name = "Old")))
  once <- merge_saved_episodes(existing, list(list(id = "new", name = "New")))
  twice <- merge_saved_episodes(once$payload, list(list(id = "new", name = "New")))
  old <- twice$payload$episodes[[1]]
  testthat::expect_null(old$removed_at)
  testthat::expect_null(old$api_misses)
})

testthat::test_that("track_removals = FALSE leaves miss counts alone", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(meta = list(), episodes = list(list(id = "a", name = "A", api_seen = TRUE)))
  result <- merge_saved_episodes(existing, list(), track_removals = FALSE)
  testthat::expect_null(result$payload$episodes[[1]]$api_misses)
  testthat::expect_false(result$changed)
})

testthat::test_that("rows with an API saved date are tracked even without api_seen", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(meta = list(), episodes = list(
    list(id = "dated", name = "D", added_at = "2026-10-01"),
    list(id = "undated", name = "U", added_at = NULL)
  ))
  api <- list(list(id = "other", name = "O"))
  once <- merge_saved_episodes(existing, api, today = as.Date("2026-10-22"))
  twice <- merge_saved_episodes(once$payload, api, today = as.Date("2026-10-23"))
  testthat::expect_equal(twice$payload$episodes[[1]]$removed_at, "2026-10-23")
  testthat::expect_null(twice$payload$episodes[[2]]$removed_at)
})

testthat::test_that("a 404 from the episode lookup marks the row unavailable", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(meta = list(), episodes = list(
    list(id = "gone", name = "Gone", release = NULL),
    list(id = "here", name = "Here", release = NULL)
  ))
  backfill <- list(list(id = "here", name = "Here", release = "2025-01-02"))
  result <- merge_saved_episodes(existing, list(), today = as.Date("2026-10-23"),
                                 backfill = backfill, unavailable = "gone")
  gone <- result$payload$episodes[[1]]
  here <- result$payload$episodes[[2]]
  testthat::expect_equal(gone$unavailable_at, "2026-10-23")
  testthat::expect_null(here$unavailable_at)
  testthat::expect_equal(here$release, "2025-01-02")
  testthat::expect_equal(result$payload$meta$n_unavailable, 1L)
  # Still gone a day later: the original date is kept and nothing changes.
  again <- merge_saved_episodes(result$payload, list(), today = as.Date("2026-10-24"),
                                unavailable = "gone")
  testthat::expect_equal(again$payload$episodes[[1]]$unavailable_at, "2026-10-23")
  testthat::expect_false(again$changed)
})

testthat::test_that("an unavailable episode Spotify returns again is restored", {
  merge_saved_episodes <- load_merge_saved_episodes()
  existing <- list(meta = list(), episodes = list(
    list(id = "a", name = "A", unavailable_at = "2026-10-20"),
    list(id = "b", name = "B", unavailable_at = "2026-10-20")
  ))
  result <- merge_saved_episodes(
    existing, list(list(id = "a", name = "A")), today = as.Date("2026-10-23"),
    backfill = list(list(id = "b", name = "B", release = "2025-05-05"))
  )
  testthat::expect_null(result$payload$episodes[[1]]$unavailable_at)
  testthat::expect_null(result$payload$episodes[[2]]$unavailable_at)
  testthat::expect_equal(result$payload$meta$n_unavailable, 0L)
})

testthat::test_that("episode lookups go one ID at a time and collect 404s", {
  expressions <- as.list(parse("../../scripts/update_saved_episodes.R"))
  wanted <- c("%||%", "normalize_episode", "fetch_episode_metadata")
  defs <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) && as.character(x[[2]]) %in% wanted
  }, expressions)
  env <- new.env()
  for (d in defs) eval(d, envir = env)
  calls <- character()
  fetch <- function(id) {
    calls <<- c(calls, id)
    if (id == "gone") return(NULL)
    list(id = id, name = toupper(id), duration_ms = 1800000, release_date = "2025-01-01",
         show = list(name = "S", publisher = "P"))
  }
  out <- env$fetch_episode_metadata(c("a", "gone", "a", NA, ""), fetch = fetch)
  testthat::expect_identical(calls, c("a", "gone"))
  testthat::expect_identical(out$unavailable, "gone")
  testthat::expect_length(out$rows, 1L)
  testthat::expect_equal(out$rows[[1]]$dur_min, 30)
  testthat::expect_equal(out$rows[[1]]$release, "2025-01-01")
})

load_before_cutoff <- function() {
  expressions <- as.list(parse("../../podcast-dashboard/app.R"))
  defs <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(as.character(x[[2]]), "before_cutoff")
  }, expressions)
  env <- new.env()
  for (d in defs) eval(d, envir = env)
  env$before_cutoff
}

testthat::test_that("the completion cutoff hides only day-precision releases before it", {
  before_cutoff <- load_before_cutoff()
  release <- c("2026-03-13", "2026-03-14", "2026-03-15", NA, "2025", "2026-02", "")
  testthat::expect_identical(
    before_cutoff(release, "2026-03-14"),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

testthat::test_that("no cutoff (null, NA or empty) hides nothing", {
  before_cutoff <- load_before_cutoff()
  release <- c("2001-01-01", NA)
  for (cutoff in list(NULL, NA, NA_character_, "")) {
    testthat::expect_identical(before_cutoff(release, cutoff), c(FALSE, FALSE))
  }
})

testthat::test_that("the refresh and import never write the cutoff file", {
  sources <- c("../../scripts/update_saved_episodes.R",
               "../../podcast-dashboard/tools/import_library.py",
               "../../podcast-dashboard/tools/import_history.py")
  for (path in sources) {
    testthat::expect_false(any(grepl("completion_cutoff", readLines(path))), info = path)
  }
})
