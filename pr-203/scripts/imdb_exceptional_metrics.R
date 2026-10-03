# Counts for the exceptional-episodes page, recomputed from the official snapshot.
imdb_exceptional_counts <- function(series, episodes, min_votes = 5000L) {
  stopifnot(!anyDuplicated(series$series_id), !anyDuplicated(episodes$episode_id))
  eligible <- series |>
    dplyr::filter(series_votes >= 25000, title_type %in% c("tvSeries", "tvMiniSeries"))
  hits <- episodes |>
    dplyr::filter(rating >= 9.7, votes >= min_votes, series_id %in% eligible$series_id) |>
    dplyr::count(series_id, name = "exceptional_episodes")
  eligible |>
    dplyr::left_join(hits, by = "series_id") |>
    dplyr::mutate(exceptional_episodes = dplyr::coalesce(exceptional_episodes, 0L)) |>
    dplyr::arrange(dplyr::desc(exceptional_episodes), dplyr::desc(series_votes), series_id)
}
