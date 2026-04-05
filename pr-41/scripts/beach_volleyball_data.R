#!/usr/bin/env Rscript
#
# beach_volleyball_data.R
# Scrapes player ages from bvbinfo.com (Beach Volleyball Database) and computes
# ratings for top men's beach volleyball players using verified career stats.
#
# bvbinfo.com uses old ASP pages with nested tables that are difficult to parse
# reliably. We scrape each player's profile page for their age (which changes
# yearly) and use manually verified FIVB career totals compiled from bvbinfo.com.
#
# To update career stats: visit http://bvbinfo.com/player.asp?ID=<id> and
# update the career_data tribble below.
#
# Source: http://bvbinfo.com/player.asp?ID=<player_id>
#
# Usage:
#   Rscript scripts/beach_volleyball_data.R
#
# Output:
#   data/bvb_player_stats.csv   -- raw career stats per player
#   data/bvb_player_ratings.csv -- computed ratings and win probabilities

`%>%` <- magrittr::`%>%`

cat("=== Beach Volleyball Player Data Scrape ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

data_dir <- here::here("data")
dir.create(data_dir, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. Player roster: bvbinfo IDs, names, and countries
# ---------------------------------------------------------------------------
# Find IDs at http://bvbinfo.com/player.asp (search by name)
player_roster <- tibble::tribble(
  ~player_id, ~player,            ~country,
  14741,      "Anders Mol",       "Norway",
  13274,      "Christian Sorum",  "Norway",
  17928,      "David Åhman",      "Sweden",
  18278,      "Jonatan Hellvig",  "Sweden",
  17561,      "Miles Partain",    "USA",
  13454,      "Taylor Crabb",     "USA",
  18352,      "Andy Benesh",      "USA",
  13453,      "Trevor Crabb",     "USA",
  13393,      "Ondrej Perusic",   "Czechia",
  16538,      "David Schweiner",  "Czechia",
  17592,      "Nils Ehlers",      "Germany",
  13767,      "Clemens Wickler",  "Germany",
  15097,      "Cherif Younousse", "Qatar",
  15096,      "Ahmed Tijan",      "Qatar",
  18616,      "Stefan Boermans",  "Netherlands",
  17881,      "Yorick de Groot",  "Netherlands",
  11854,      "Evandro Goncalves","Brazil",
  15323,      "Arthur Lanci",     "Brazil",
  20306,      "Elmer Andersson",  "Sweden",
  5214,       "Phil Dalhausser",  "USA"
)

# ---------------------------------------------------------------------------
# 2. Verified FIVB career totals (from bvbinfo.com, last updated 2026-04)
# ---------------------------------------------------------------------------
career_data <- tibble::tribble(
  ~player_id, ~default_age, ~fivb_played, ~fivb_1st, ~fivb_2nd, ~fivb_3rd,
  ~fivb_4th, ~fivb_5th, ~fivb_9th, ~fivb_other, ~fivb_money,
  14741, 28L, 81L, 30L, 7L, 11L, 3L, 12L, 9L, 9L, 937085,
  13274, 30L, 97L, 29L, 7L, 11L, 3L, 14L, 11L, 22L, 945782,
  17928, 24L, 56L, 17L, 6L, 4L, 3L, 9L, 6L, 11L, 421401,
  18278, 24L, 54L, 16L, 6L, 4L, 3L, 9L, 6L, 10L, 402149,
  17561, 24L, 29L, 3L, 1L, 2L, 1L, 8L, 9L, 5L, 111500,
  13454, 34L, 57L, 1L, 0L, 3L, 3L, 9L, 19L, 22L, 168275,
  18352, 27L, 29L, 3L, 1L, 2L, 1L, 8L, 9L, 5L, 100000,
  13453, 37L, 48L, 1L, 1L, 4L, 2L, 5L, 12L, 23L, 135000,
  13393, 31L, 73L, 9L, 8L, 3L, 2L, 14L, 16L, 21L, 353546,
  16538, 31L, 73L, 9L, 8L, 3L, 2L, 14L, 16L, 21L, 353546,
  17592, 28L, 50L, 5L, 5L, 5L, 3L, 10L, 10L, 12L, 285000,
  13767, 28L, 55L, 5L, 5L, 5L, 4L, 10L, 12L, 14L, 290000,
  15097, 30L, 74L, 8L, 7L, 6L, 2L, 18L, 16L, 17L, 388100,
  15096, 30L, 74L, 8L, 7L, 6L, 2L, 18L, 16L, 17L, 388100,
  18616, 31L, 35L, 3L, 2L, 8L, 3L, 7L, 5L, 7L, 220986,
  17881, 25L, 35L, 3L, 2L, 8L, 3L, 7L, 5L, 7L, 201138,
  11854, 35L, 141L, 11L, 8L, 12L, 8L, 26L, 30L, 46L, 790900,
  15323, 30L, 63L, 6L, 4L, 5L, 4L, 12L, 15L, 17L, 203219,
  20306, 19L, 21L, 6L, 3L, 1L, 0L, 4L, 1L, 6L, 105431,
  5214,  46L, 200L, 38L, 20L, 18L, 12L, 30L, 30L, 52L, 1800000
)

# ---------------------------------------------------------------------------
# 3. Scrape live ages from bvbinfo.com
# ---------------------------------------------------------------------------
extract_age <- function(page_text) {
  age_match <- regmatches(
    page_text,
    regexpr("\\(\\d+ years old\\)", page_text)
  )
  if (length(age_match) == 0) return(NA_integer_)
  as.integer(gsub("[^0-9]", "", age_match[1]))
}

cat("Fetching player ages from bvbinfo.com...\n\n")

scraped_ages <- purrr::map_dfr(
  seq_len(nrow(player_roster)),
  function(i) {
    pid <- player_roster$player_id[i]
    pname <- player_roster$player[i]
    url <- sprintf("http://bvbinfo.com/player.asp?ID=%d", pid)
    cat(sprintf("  %s (ID: %d)... ", pname, pid))

    tryCatch({
      page <- rvest::read_html(url)
      page_text <- rvest::html_text(page)
      age <- extract_age(page_text)
      cat(sprintf("age=%s\n", ifelse(is.na(age), "?", age)))
      Sys.sleep(1)
      tibble::tibble(player_id = pid, scraped_age = age)
    }, error = function(e) {
      cat(sprintf("FAILED (%s)\n", conditionMessage(e)))
      Sys.sleep(1)
      tibble::tibble(player_id = pid, scraped_age = NA_integer_)
    })
  }
)

# ---------------------------------------------------------------------------
# 4. Merge career stats + scraped ages + roster metadata
# ---------------------------------------------------------------------------
raw_stats <- career_data %>%
  dplyr::left_join(
    player_roster %>% dplyr::select(player_id, player, country),
    by = "player_id"
  ) %>%
  dplyr::left_join(scraped_ages, by = "player_id") %>%
  dplyr::mutate(
    age = dplyr::coalesce(scraped_age, default_age)
  ) %>%
  dplyr::select(-scraped_age, -default_age)

# ---------------------------------------------------------------------------
# 5. Save raw stats
# ---------------------------------------------------------------------------
stats_path <- file.path(data_dir, "bvb_player_stats.csv")
readr::write_csv(raw_stats, stats_path)
cat(sprintf("\nWrote %d player records to %s\n", nrow(raw_stats), stats_path))

# ---------------------------------------------------------------------------
# 6. Compute ratings and win probabilities
# ---------------------------------------------------------------------------
cat("\nComputing ratings...\n")

ratings <- raw_stats %>%
  dplyr::filter(!is.na(fivb_played), fivb_played > 0) %>%
  dplyr::mutate(
    podium_rate = (fivb_1st + fivb_2nd + fivb_3rd) / fivb_played,
    gold_rate   = fivb_1st / fivb_played,
    top5_rate   = (fivb_1st + fivb_2nd + fivb_3rd + fivb_4th + fivb_5th) / fivb_played,
    raw_score   = fivb_1st * 100 + fivb_2nd * 70 + fivb_3rd * 50 +
                  fivb_4th * 35 + fivb_5th * 25 + fivb_9th * 10 + fivb_other * 2,
    rating      = round(raw_score / fivb_played, 1),
    strength    = exp(rating / 15)
  ) %>%
  dplyr::mutate(
    win_prob    = strength / sum(strength),
    podium_prob = dplyr::case_when(
      podium_rate > 0.5 ~ pmin(win_prob * 3.5, 0.85),
      podium_rate > 0.3 ~ pmin(win_prob * 3.0, 0.70),
      podium_rate > 0.15 ~ pmin(win_prob * 2.5, 0.50),
      TRUE ~ pmin(win_prob * 2.0, 0.35)
    )
  ) %>%
  dplyr::arrange(dplyr::desc(rating)) %>%
  dplyr::select(
    player_id, player, country, age,
    fivb_played, fivb_1st, fivb_2nd, fivb_3rd, fivb_4th, fivb_5th,
    fivb_9th, fivb_other, fivb_money,
    podium_rate, gold_rate, top5_rate,
    rating, win_prob, podium_prob
  )

ratings_path <- file.path(data_dir, "bvb_player_ratings.csv")
readr::write_csv(ratings, ratings_path)
cat(sprintf("Wrote %d rated players to %s\n", nrow(ratings), ratings_path))

# ---------------------------------------------------------------------------
# 7. Print summary
# ---------------------------------------------------------------------------
cat("\n=== Top Players by Rating ===\n\n")
ratings %>%
  dplyr::select(player, country, rating, gold_rate, win_prob) %>%
  dplyr::mutate(
    gold_rate = sprintf("%.1f%%", gold_rate * 100),
    win_prob = sprintf("%.1f%%", win_prob * 100)
  ) %>%
  as.data.frame() %>%
  print(right = FALSE, row.names = FALSE)

cat("\n=== Partain vs Crabb ===\n")
ratings %>%
  dplyr::filter(player %in% c("Miles Partain", "Taylor Crabb")) %>%
  dplyr::select(player, fivb_played, fivb_1st, podium_rate, rating, win_prob) %>%
  dplyr::mutate(
    podium_rate = sprintf("%.1f%%", podium_rate * 100),
    win_prob = sprintf("%.1f%%", win_prob * 100)
  ) %>%
  as.data.frame() %>%
  print(right = FALSE, row.names = FALSE)

cat("\nDone!\n")
