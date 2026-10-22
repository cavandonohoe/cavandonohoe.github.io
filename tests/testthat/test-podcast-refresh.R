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
  wanted <- c("%||%", "backfill_fields", "merge_saved_episodes")
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
    episodes = list(list(id = "a", added_at = "2026-09-01", name = "A", dur_min = 30))
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
