make_show <- function(season_ratings, eps_per_season = 4, year = 2000) {
  seasons <- seq_along(season_ratings)
  do.call(rbind, lapply(seasons, function(s) {
    n <- if (length(eps_per_season) > 1) eps_per_season[s] else eps_per_season
    data.frame(
      season = s,
      episode = seq_len(n),
      title = paste0("S", s, "E", seq_len(n)),
      rating = season_ratings[[s]],
      votes = 1000,
      episode_year = year + s - 1
    )
  }))
}

test_that("a single-season show yields no metrics", {
  source("../../scripts/tv_decline_metrics.R")

  expect_null(show_decline_metrics(make_show(list(8.0))))
})

test_that("peak and latest season averages drive the drop", {
  source("../../scripts/tv_decline_metrics.R")

  # Season averages: 9.0 then 6.0
  show <- make_show(list(rep(9.0, 4), rep(6.0, 4)))
  m <- show_decline_metrics(show)

  expect_equal(m$peak_season, 1)
  expect_equal(m$peak_avg, 9.0)
  expect_equal(m$latest_season, 2)
  expect_equal(m$latest_avg, 6.0)
  expect_equal(m$peak_to_latest_drop, 3.0)
  expect_true(m$latest_is_worst)
  expect_false(m$latest_is_peak)
})

test_that("a show ending on its best season is flagged as ending on peak", {
  source("../../scripts/tv_decline_metrics.R")

  m <- show_decline_metrics(make_show(list(rep(7.0, 4), rep(9.5, 4))))

  expect_true(m$latest_is_peak)
  expect_false(m$latest_is_worst)
  expect_equal(m$peak_to_latest_drop, 0)
  expect_gt(m$slope_per_season, 0)
})

test_that("short seasons are excluded from peak and latest", {
  source("../../scripts/tv_decline_metrics.R")

  # A 1-episode third season (mid-air) must not be treated as the latest season.
  show <- make_show(list(rep(8.0, 4), rep(9.0, 4), 2.0), eps_per_season = c(4, 4, 1))
  m <- show_decline_metrics(show, min_eps = 3)

  expect_equal(m$latest_season, 2)
  expect_equal(m$seasons, 3)
  expect_equal(m$seasons_scored, 2)
  expect_true(m$latest_is_peak)
})

test_that("slope is negative for a steadily declining show", {
  source("../../scripts/tv_decline_metrics.R")

  m <- show_decline_metrics(make_show(list(rep(9.0, 4), rep(8.0, 4), rep(7.0, 4))))

  expect_equal(m$slope_per_season, -1.0)
})

test_that("likely_ongoing is judged against the supplied current year", {
  source("../../scripts/tv_decline_metrics.R")

  show <- make_show(list(rep(8.0, 4), rep(8.0, 4)), year = 2019) # last season 2020

  expect_true(show_decline_metrics(show, current_year = 2021)$likely_ongoing)
  expect_false(show_decline_metrics(show, current_year = 2026)$likely_ongoing)
})
