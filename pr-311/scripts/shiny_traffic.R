#!/usr/bin/env Rscript
# shiny_traffic.R
#
# Records daily traffic for every app on the shinyapps.io account so there is
# a long-running history to look back on. shinyapps.io only keeps a rolling
# window of metrics, so each run pulls the last `--days` days of hourly
# container metrics and merges them into data/shiny_traffic.csv (newer values
# replace older ones for the same app and day; days outside the window are
# kept as-is).
#
# shinyapps.io reports container connections, not individual visitors, so
# these numbers are a measure of activity (including the owner's own visits),
# not unique users.
#
# Columns:
#   date              UTC calendar day
#   app               shinyapps.io app name
#   active_hours      hours in the day with at least one open connection
#   connection_hours  sum of hourly mean connections (~ visitor-hours)
#   peak_connections  highest hourly mean connection count that day
#
# Needs rsconnect (and RCurl, which rsconnect uses for the metrics API) plus
# SHINYAPPS_NAME / SHINYAPPS_TOKEN / SHINYAPPS_SECRET in the environment.
#
# Usage:
#   Rscript scripts/shiny_traffic.R [--days 30] [--out data/shiny_traffic.csv]

traffic_cols <- c("date", "app", "active_hours", "connection_hours", "peak_connections")

#' Collapse hourly connect_count points into one row per UTC day.
#'
#' Only complete days (strictly before `today`) are returned, so a run never
#' commits a partial day that the next run would rewrite.
#'
#' @param points data.frame with `time` (POSIXct) and `connect_count` columns.
#' @param app App name to stamp on every row.
#' @param today The current UTC date.
#' @return data.frame with `traffic_cols`.
summarise_daily_traffic <- function(points, app, today = as.Date(Sys.time(), tz = "UTC")) {
  empty <- data.frame(
    date = as.Date(character()), app = character(), active_hours = integer(),
    connection_hours = numeric(), peak_connections = numeric()
  )
  if (is.null(points) || !nrow(points)) {
    return(empty)
  }
  count <- as.numeric(points$connect_count)
  count[is.na(count)] <- 0
  day <- as.Date(points$time, tz = "UTC")
  keep <- day < today
  if (!any(keep)) {
    return(empty)
  }
  count <- count[keep]
  day <- day[keep]
  days <- sort(unique(day))
  data.frame(
    date = days,
    app = app,
    active_hours = vapply(days, function(d) sum(count[day == d] > 0), integer(1)),
    connection_hours = vapply(days, function(d) round(sum(count[day == d]), 2), numeric(1)),
    peak_connections = vapply(days, function(d) round(max(count[day == d]), 2), numeric(1))
  )
}

#' Merge freshly pulled rows into the stored history.
#'
#' Fresh rows win for any (date, app) they cover; everything else in the
#' history is kept untouched. Output is sorted by date then app.
#'
#' @param history data.frame of previously stored rows (may be empty/NULL).
#' @param fresh data.frame of newly summarised rows.
#' @return data.frame with `traffic_cols`.
merge_traffic_history <- function(history, fresh) {
  if (is.null(history) || !nrow(history)) {
    out <- fresh
  } else {
    history$date <- as.Date(history$date)
    fresh_key <- paste(fresh$date, fresh$app)
    out <- rbind(history[!paste(history$date, history$app) %in% fresh_key, traffic_cols], fresh)
  }
  out <- out[order(out$date, out$app), traffic_cols]
  rownames(out) <- NULL
  out
}

#' Markdown summary of the last 7 and 30 days per app, for the job summary.
#'
#' @param history Merged history data.frame.
#' @param today The current UTC date.
#' @return Character vector of markdown lines.
traffic_summary_md <- function(history, today = as.Date(Sys.time(), tz = "UTC")) {
  lines <- c(
    "## shinyapps.io traffic", "",
    "| App | Active days (7d) | Active hours (7d) | Active days (30d) | Active hours (30d) | Last active |",
    "|---|---|---|---|---|---|"
  )
  for (app in sort(unique(history$app))) {
    h <- history[history$app == app, ]
    in7 <- h$date >= today - 7
    in30 <- h$date >= today - 30
    active <- h$active_hours > 0
    last <- if (any(active)) as.character(max(h$date[active])) else "never"
    lines <- c(lines, sprintf(
      "| %s | %d | %d | %d | %d | %s |", app,
      sum(active & in7), sum(h$active_hours[in7]),
      sum(active & in30), sum(h$active_hours[in30]), last
    ))
  }
  lines
}

parse_args <- function(args) {
  opts <- list(days = 30L, out = "data/shiny_traffic.csv")
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--days") {
      opts$days <- as.integer(args[i + 1])
      i <- i + 2
    } else if (args[i] == "--out") {
      opts$out <- args[i + 1]
      i <- i + 2
    } else {
      stop("Unknown argument: ", args[i])
    }
  }
  opts
}

main <- function(args) {
  opts <- parse_args(args)
  account <- Sys.getenv("SHINYAPPS_NAME")
  if (!nzchar(account)) {
    stop("SHINYAPPS_NAME is not set")
  }
  if (!account %in% rsconnect::accounts(server = "shinyapps.io")$name) {
    rsconnect::setAccountInfo(
      name = account,
      token = Sys.getenv("SHINYAPPS_TOKEN"),
      secret = Sys.getenv("SHINYAPPS_SECRET")
    )
  }

  apps <- rsconnect::applications(account = account, server = "shinyapps.io")
  if (!nrow(apps)) {
    stop("No applications found on shinyapps.io account ", account)
  }

  fresh <- do.call(rbind, lapply(apps$name, function(app) {
    points <- tryCatch(
      rsconnect::showMetrics(
        "container_status", "connect_count",
        appName = app, account = account, server = "shinyapps.io",
        from = paste0(opts$days, "d"), interval = "1h"
      ),
      error = function(e) {
        # showMetrics() errors with "No data." for an app with no points in
        # the window (e.g. archived or asleep); treat that as zero traffic.
        if (grepl("No data", conditionMessage(e))) {
          return(NULL)
        }
        stop(e)
      }
    )
    rows <- summarise_daily_traffic(points, app)
    cat(sprintf("%-24s %3d day(s), %3d active hour(s)\n", app, nrow(rows), sum(rows$active_hours)))
    rows
  }))

  history <- if (file.exists(opts$out)) utils::read.csv(opts$out, stringsAsFactors = FALSE) else NULL
  merged <- merge_traffic_history(history, fresh)
  utils::write.csv(merged, opts$out, row.names = FALSE)
  cat("Wrote", nrow(merged), "rows to", opts$out, "\n")

  summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
  md <- traffic_summary_md(merged)
  if (nzchar(summary_path)) {
    cat(md, sep = "\n", file = summary_path, append = TRUE)
  } else {
    cat(md, sep = "\n")
  }
}

if (sys.nframe() == 0) {
  main(commandArgs(trailingOnly = TRUE))
}
