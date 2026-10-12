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
  wanted <- c("%||%", "merge_saved_episodes")
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
