source(here::here("scripts", "imdb_exceptional_metrics.R"))

testthat::test_that("inclusive vote/rating cutoffs preserve shows with zero hits", {
  series <- tibble::tibble(
    series_id = c("a", "b", "c", "d"),
    series_votes = c(25000L, 24999L, 50000L, 100000L),
    title_type = c("tvSeries", "tvSeries", "tvMiniSeries", "movie"),
    linked_episodes = c(8L, 1L, 5L, 1L)
  )
  episodes <- tibble::tibble(
    episode_id = paste0("e", 1:6),
    series_id = c("a", "a", "a", "a", "b", "d"),
    rating = c(9.7, 9.7, 9.7, 9.6, 10, 10),
    votes = c(1000L, 5000L, 10000L, 10000L, 10000L, 10000L)
  )
  primary <- imdb_exceptional_counts(series, episodes)
  testthat::expect_equal(primary$series_id, c("a", "c"))
  testthat::expect_equal(primary$exceptional_episodes, c(2L, 0L))
  testthat::expect_equal(primary$percent_exceptional, c(25, 0))
  testthat::expect_equal(sum(imdb_exceptional_counts(series, episodes, 1000L)$exceptional_episodes), 3L)
  testthat::expect_equal(sum(imdb_exceptional_counts(series, episodes, 10000L)$exceptional_episodes), 1L)
})

testthat::test_that("duplicate episode IDs cannot inflate counts", {
  series <- tibble::tibble(series_id = "a", series_votes = 25000L,
                          title_type = "tvSeries", linked_episodes = 1L)
  episodes <- tibble::tibble(episode_id = c("e1", "e1"), series_id = "a", rating = 9.7, votes = 5000L)
  testthat::expect_error(imdb_exceptional_counts(series, episodes))
})

testthat::test_that("percentage uses all episodes and is undefined for an empty show", {
  series <- tibble::tibble(
    series_id = c("a", "b"), series_votes = 25000L, title_type = "tvSeries",
    linked_episodes = c(10L, 0L), rated_episodes = c(2L, 0L)
  )
  episodes <- tibble::tibble(episode_id = "e1", series_id = "a", rating = 9.7, votes = 5000L)
  result <- imdb_exceptional_counts(series, episodes)
  testthat::expect_equal(result$percent_exceptional[result$series_id == "a"], 10)
  testthat::expect_true(is.na(result$percent_exceptional[result$series_id == "b"]))
})
