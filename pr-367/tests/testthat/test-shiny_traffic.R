hourly <- function(start, counts) {
  data.frame(
    time = as.POSIXct(start, tz = "UTC") + 3600 * (seq_along(counts) - 1),
    connect_count = counts
  )
}

test_that("summarise_daily_traffic collapses hours into complete UTC days", {
  source("../../scripts/shiny_traffic.R")

  points <- hourly("2026-10-01 22:00", c(0, 0.5, 1, 0, NA))
  out <- summarise_daily_traffic(points, "app-a", today = as.Date("2026-10-03"))

  expect_equal(out$date, as.Date(c("2026-10-01", "2026-10-02")))
  expect_equal(out$active_hours, c(1L, 1L))
  expect_equal(out$connection_hours, c(0.5, 1))
  expect_equal(out$peak_connections, c(0.5, 1))
  expect_true(all(out$app == "app-a"))
})

test_that("summarise_daily_traffic drops the current partial day", {
  source("../../scripts/shiny_traffic.R")

  points <- hourly("2026-10-03 00:00", c(1, 1))
  out <- summarise_daily_traffic(points, "app-a", today = as.Date("2026-10-03"))
  expect_equal(nrow(out), 0)
  expect_equal(names(out), traffic_cols)
})

test_that("summarise_daily_traffic handles no data", {
  source("../../scripts/shiny_traffic.R")

  expect_equal(nrow(summarise_daily_traffic(NULL, "app-a")), 0)
})

test_that("merge_traffic_history keeps old days and lets fresh rows win", {
  source("../../scripts/shiny_traffic.R")

  history <- data.frame(
    date = c("2026-09-01", "2026-10-01"), app = "app-a",
    active_hours = c(3L, 1L), connection_hours = c(2, 0.5), peak_connections = c(1, 0.5)
  )
  fresh <- data.frame(
    date = as.Date(c("2026-10-01", "2026-10-02")), app = "app-a",
    active_hours = c(2L, 0L), connection_hours = c(1, 0), peak_connections = c(1, 0)
  )
  out <- merge_traffic_history(history, fresh)

  expect_equal(out$date, as.Date(c("2026-09-01", "2026-10-01", "2026-10-02")))
  expect_equal(out$active_hours, c(3L, 2L, 0L))
})

test_that("merge_traffic_history starts a new history", {
  source("../../scripts/shiny_traffic.R")

  fresh <- data.frame(
    date = as.Date("2026-10-02"), app = "app-b",
    active_hours = 0L, connection_hours = 0, peak_connections = 0
  )
  expect_equal(merge_traffic_history(NULL, fresh), fresh)
})

test_that("traffic_summary_md reports active days and last activity", {
  source("../../scripts/shiny_traffic.R")

  history <- data.frame(
    date = as.Date(c("2026-09-01", "2026-10-10", "2026-10-11")), app = "app-a",
    active_hours = c(4L, 0L, 2L), connection_hours = 0, peak_connections = 0
  )
  md <- traffic_summary_md(history, today = as.Date("2026-10-14"))
  expect_true(any(grepl("^\\| app-a \\| 1 \\| 2 \\| 1 \\| 2 \\| 2026-10-11 \\|$", md)))
})
