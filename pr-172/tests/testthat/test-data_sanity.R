test_that("data_sanity_verdict accepts a healthy refresh", {
  source("../../scripts/data_sanity.R")

  verdict <- data_sanity_verdict(
    new_rows = 105, new_cols = c("date", "close"),
    old_rows = 100, old_cols = c("date", "close")
  )
  expect_true(verdict$ok)
  expect_equal(verdict$problems, character())
})

test_that("data_sanity_verdict rejects an unreadable file", {
  source("../../scripts/data_sanity.R")

  verdict <- data_sanity_verdict(new_rows = NA_integer_, new_cols = character())
  expect_false(verdict$ok)
  expect_match(verdict$problems, "could not be read")
})

test_that("data_sanity_verdict rejects an empty refresh", {
  source("../../scripts/data_sanity.R")

  verdict <- data_sanity_verdict(new_rows = 0, new_cols = c("date", "close"), old_rows = 100)
  expect_false(verdict$ok)
  expect_true(any(grepl("expected at least", verdict$problems)))
})

test_that("data_sanity_verdict rejects a collapsed row count", {
  source("../../scripts/data_sanity.R")

  verdict <- data_sanity_verdict(new_rows = 50, new_cols = "date", old_rows = 100, old_cols = "date")
  expect_false(verdict$ok)
  expect_true(any(grepl("row count fell from 100 to 50", verdict$problems)))
})

test_that("data_sanity_verdict tolerates a drop within the threshold", {
  source("../../scripts/data_sanity.R")

  expect_true(data_sanity_verdict(new_rows = 85, new_cols = "date", old_rows = 100)$ok)
  expect_false(data_sanity_verdict(new_rows = 85, new_cols = "date", old_rows = 100, max_drop_pct = 5)$ok)
})

test_that("data_sanity_verdict flags dropped columns even when rows grow", {
  source("../../scripts/data_sanity.R")

  verdict <- data_sanity_verdict(
    new_rows = 200, new_cols = c("date", "close"),
    old_rows = 100, old_cols = c("date", "close", "volume")
  )
  expect_false(verdict$ok)
  expect_true(any(grepl("volume", verdict$problems)))
})

test_that("data_sanity_verdict allows added columns", {
  source("../../scripts/data_sanity.R")

  verdict <- data_sanity_verdict(
    new_rows = 100, new_cols = c("date", "close", "adj_close"),
    old_rows = 100, old_cols = c("date", "close")
  )
  expect_true(verdict$ok)
})

test_that("data_sanity_verdict treats a brand-new file as sane when non-empty", {
  source("../../scripts/data_sanity.R")

  expect_true(data_sanity_verdict(new_rows = 3, new_cols = "date")$ok)
  expect_false(data_sanity_verdict(new_rows = 0, new_cols = "date")$ok)
})

test_that("check_csv_sanity fails a missing file", {
  source("../../scripts/data_sanity.R")

  result <- check_csv_sanity(file.path(tempdir(), "definitely_not_here.csv"))
  expect_false(result$ok)
  expect_match(result$problems, "missing from the working tree")
})

test_that("check_csv_sanity reads a working-tree file with no committed counterpart", {
  source("../../scripts/data_sanity.R")

  path <- file.path(tempdir(), "data_sanity_new.csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(data.frame(date = c("2026-01-01", "2026-01-02"), close = c(1, 2)), path, row.names = FALSE)

  result <- check_csv_sanity(path)
  expect_true(result$ok)
  expect_equal(result$path, path)
})
