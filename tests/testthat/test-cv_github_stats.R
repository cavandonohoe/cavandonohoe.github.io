source("../../scripts/cv_github_stats.R")

testthat::test_that("round_stat floors to the step and adds a plus", {
  testthat::expect_equal(round_stat(233, 25), "225+")
  testthat::expect_equal(round_stat(46, 5), "45+")
  testthat::expect_equal(round_stat(1161, 50), "1,150+")
  testthat::expect_true(is.na(round_stat(NA, 5)))
})

testthat::test_that("fill_github_stats substitutes placeholders and keeps NAs", {
  formatted <- c(n_workflows = "45+", n_merged_prs = "225+")
  x <- c("Built {n_workflows} workflows", "Merged {n_merged_prs} PRs", NA, "No stats")
  testthat::expect_equal(
    fill_github_stats(x, formatted),
    c("Built 45+ workflows", "Merged 225+ PRs", NA, "No stats")
  )
})

testthat::test_that("fill_github_stats errors on unresolved placeholders", {
  testthat::expect_error(
    fill_github_stats("Made {n_stars} stars", c(n_workflows = "45+")),
    "Unresolved CV stat placeholders"
  )
})
