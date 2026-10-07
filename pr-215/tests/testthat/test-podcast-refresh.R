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
