# Metrics behind tv_decline_leaderboard.Rmd: does a show quit while it's ahead?
#
# Seasons with fewer than `min_eps` episodes are excluded from peak/finale/slope
# because a partially-aired current season would otherwise read as a collapse.

show_season_stats <- function(d) {
  dplyr::summarise(
    dplyr::group_by(d, season),
    episodes = dplyr::n(),
    avg_rating = mean(rating, na.rm = TRUE),
    last_year = suppressWarnings(max(episode_year, na.rm = TRUE)),
    .groups = "drop"
  )
}

show_decline_metrics <- function(d, min_eps = 3, current_year = as.integer(format(Sys.Date(), "%Y"))) {
  all_seasons <- show_season_stats(d)
  qualifying <- dplyr::filter(all_seasons, episodes >= min_eps, !is.na(avg_rating))

  if (nrow(qualifying) < 2) {
    return(NULL)
  }

  peak_row <- qualifying[which.max(qualifying$avg_rating), ]
  worst_row <- qualifying[which.min(qualifying$avg_rating), ]
  final_row <- qualifying[which.max(qualifying$season), ]

  peak_avg <- peak_row$avg_rating
  final_avg <- final_row$avg_rating

  # Slope of season average on season number: points lost (or gained) per season.
  slope <- unname(stats::coef(stats::lm(avg_rating ~ season, data = qualifying))[2])

  tibble::tibble(
    seasons = nrow(all_seasons),
    seasons_scored = nrow(qualifying),
    episodes = nrow(d),
    peak_season = peak_row$season,
    peak_avg = peak_avg,
    latest_season = final_row$season,
    latest_avg = final_avg,
    peak_to_latest_drop = peak_avg - final_avg,
    slope_per_season = slope,
    episode_sd = stats::sd(d$rating, na.rm = TRUE),
    latest_is_peak = final_row$season == peak_row$season,
    latest_is_worst = final_row$season == worst_row$season,
    last_year = max(all_seasons$last_year),
    # An airing show has no finale yet, so its "drop" is provisional, not a verdict.
    likely_ongoing = max(all_seasons$last_year) >= current_year - 1
  )
}

tv_decline_table <- function(manifest, min_eps = 3, ...) {
  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    metrics <- show_decline_metrics(read_show_ratings(manifest$slug[i]), min_eps = min_eps, ...)
    if (is.null(metrics)) {
      return(NULL)
    }
    dplyr::bind_cols(
      tibble::tibble(title = manifest$title[i], slug = manifest$slug[i]),
      metrics
    )
  })

  dplyr::bind_rows(rows)
}
