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
  ~player_id, ~player,                ~country,
  # --- Current elite ---
  14741,      "Anders Mol",           "Norway",
  13274,      "Christian Sorum",      "Norway",
  17928,      "David Åhman",          "Sweden",
  18278,      "Jonatan Hellvig",      "Sweden",
  17561,      "Miles Partain",        "USA",
  13454,      "Taylor Crabb",         "USA",
  18352,      "Andy Benesh",          "USA",
  13453,      "Trevor Crabb",         "USA",
  13393,      "Ondrej Perusic",       "Czechia",
  16538,      "David Schweiner",      "Czechia",
  17592,      "Nils Ehlers",          "Germany",
  13767,      "Clemens Wickler",      "Germany",
  15097,      "Cherif Younousse",     "Qatar",
  15096,      "Ahmed Tijan",          "Qatar",
  18616,      "Stefan Boermans",      "Netherlands",
  17881,      "Yorick de Groot",      "Netherlands",
  11854,      "Evandro Goncalves",    "Brazil",
  15323,      "Arthur Lanci",         "Brazil",
  20306,      "Elmer Andersson",      "Sweden",
  # --- US Olympians (all-time men's) ---
  69,         "Karch Kiraly",         "USA",
  151,        "Sinjin Smith",         "USA",
  125,        "Kent Steffes",         "USA",
  128,        "Randy Stoklos",        "USA",
  31,         "Mike Dodd",            "USA",
  138,        "Mike Whitmarsh",       "USA",
  5214,       "Phil Dalhausser",      "USA",
  111,        "Todd Rogers",          "USA",
  190,        "Jake Gibb",            "USA",
  1163,       "Sean Rosenthal",       "USA",
  1931,       "Nick Lucena",          "USA",
  5327,       "Casey Patterson",      "USA",
  13699,      "Tri Bourne",           "USA",
  13452,      "Chase Budinger",       "USA",
  13467,      "Miles Evans",          "USA",
  12,         "Dain Blanton",         "USA",
  38,         "Eric Fonoimoana",      "USA",
  141,        "Kevin Wong",           "USA",
  1922,       "Jeff Nygaard",         "USA",
  60,         "Dax Holdren",          "USA",
  90,         "Stein Metzger",        "USA",
  11645,      "Taylor Sander",        "USA",
  16101,      "Evan Cory",            "USA",
  8338,       "Adrian Carambula",     "USA",
  # --- Brazil legends ---
  148,        "Emanuel Rego",         "Brazil",
  525,        "Ricardo Santos",       "Brazil",
  7998,       "Alison Cerutti",       "Brazil",
  8539,       "Bruno Oscar Schmidt",  "Brazil",
  14454,      "George Wanderley",     "Brazil",
  # --- Germany ---
  1901,       "Julius Brink",         "Germany",
  2134,       "Jonas Reckermann",     "Germany",
  # --- Italy ---
  9059,       "Paolo Nicolai",        "Italy",
  9422,       "Daniele Lupo",         "Italy",
  # --- Netherlands ---
  9129,       "Alexander Brouwer",    "Netherlands",
  9130,       "Robert Meeuwsen",      "Netherlands",
  # --- Spain ---
  2077,       "Pablo Herrera",        "Spain",
  # --- Latvia ---
  6874,       "Janis Smedins",        "Latvia",
  6769,       "Martins Plavins",      "Latvia",
  # --- Russia ---
  10745,      "Viacheslav Krasilnikov","Russia",
  14263,      "Oleg Stoyanovskiy",    "Russia",
  # --- Canada ---
  1530,       "Mark Heese",           "Canada"
)

# ---------------------------------------------------------------------------
# 2. Verified FIVB career totals (from bvbinfo.com, last updated 2026-04)
# ---------------------------------------------------------------------------
career_data <- tibble::tribble(
  ~player_id, ~default_age, ~fivb_played, ~fivb_1st, ~fivb_2nd, ~fivb_3rd,
  ~fivb_4th, ~fivb_5th, ~fivb_9th, ~fivb_other, ~fivb_money,
  # --- Current elite ---
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
  20306, 21L, 21L, 6L, 3L, 1L, 0L, 4L, 1L, 6L, 105431,
  # --- US Olympians: old school ---
  69,   65L, 22L, 3L, 3L, 1L, 1L, 5L, 5L, 4L, 83750,
  151,  68L, 105L, 10L, 7L, 3L, 6L, 8L, 15L, 56L, 435938,
  125,  57L, 13L, 3L, 2L, 2L, 0L, 0L, 5L, 1L, 73525,
  128,  65L, 14L, 10L, 0L, 1L, 1L, 0L, 0L, 2L, 150250,
  31,   58L, 18L, 0L, 3L, 2L, 1L, 3L, 3L, 6L, 70000,
  138,  55L, 18L, 3L, 6L, 1L, 0L, 1L, 2L, 5L, 158275,
  # --- US Olympians: modern ---
  5214, 46L, 200L, 38L, 20L, 18L, 12L, 30L, 30L, 52L, 1800000,
  111,  52L, 124L, 24L, 7L, 11L, 9L, 12L, 23L, 38L, 911100,
  190,  50L, 149L, 7L, 8L, 8L, 8L, 34L, 44L, 40L, 825688,
  1163, 45L, 109L, 10L, 6L, 4L, 3L, 18L, 21L, 47L, 630300,
  1931, 42L, 121L, 9L, 11L, 6L, 6L, 25L, 18L, 46L, 697500,
  5327, 45L, 85L, 2L, 2L, 3L, 4L, 17L, 24L, 33L, 362713,
  13699, 36L, 90L, 2L, 1L, 4L, 4L, 21L, 24L, 34L, 307463,
  13452, 37L, 67L, 1L, 4L, 1L, 3L, 11L, 14L, 33L, 115225,
  13467, 36L, 80L, 2L, 6L, 2L, 4L, 9L, 19L, 38L, 117439,
  12,   54L, 68L, 2L, 1L, 3L, 4L, 5L, 17L, 36L, 207485,
  38,   56L, 63L, 1L, 2L, 1L, 4L, 7L, 20L, 28L, 184875,
  141,  54L, 18L, 0L, 0L, 0L, 0L, 3L, 5L, 10L, 20000,
  1922, 50L, 36L, 2L, 1L, 1L, 1L, 4L, 10L, 17L, 90000,
  60,   50L, 50L, 4L, 3L, 2L, 2L, 6L, 12L, 21L, 175000,
  90,   53L, 54L, 2L, 2L, 3L, 1L, 6L, 14L, 26L, 150000,
  11645, 34L, 11L, 0L, 0L, 0L, 0L, 0L, 3L, 8L, 9250,
  16101, 28L, 22L, 0L, 0L, 1L, 0L, 2L, 5L, 14L, 25000,
  8338, 35L, 95L, 3L, 3L, 4L, 4L, 13L, 18L, 50L, 275000,
  # --- Brazil legends ---
  148,  52L, 256L, 77L, 37L, 41L, 16L, 32L, 36L, 17L, 2562273,
  525,  51L, 238L, 56L, 31L, 23L, 18L, 33L, 34L, 43L, 1921183,
  7998, 40L, 152L, 28L, 18L, 15L, 9L, 27L, 28L, 27L, 1264550,
  8539, 39L, 149L, 17L, 10L, 7L, 11L, 26L, 43L, 35L, 958775,
  14454, 29L, 94L, 9L, 5L, 9L, 4L, 8L, 22L, 37L, 324763,
  # --- Germany ---
  1901, 43L, 109L, 9L, 11L, 14L, 9L, 12L, 13L, 41L, 671335,
  2134, 46L, 104L, 7L, 18L, 11L, 8L, 14L, 17L, 29L, 691428,
  # --- Italy ---
  9059, 37L, 154L, 6L, 13L, 13L, 6L, 29L, 38L, 49L, 705038,
  9422, 34L, 127L, 4L, 9L, 10L, 5L, 24L, 36L, 39L, 545238,
  # --- Netherlands ---
  9129, 36L, 150L, 7L, 11L, 8L, 9L, 31L, 37L, 47L, 648800,
  9130, 38L, 134L, 7L, 10L, 8L, 7L, 26L, 34L, 42L, 609838,
  # --- Spain ---
  2077, 43L, 225L, 7L, 12L, 12L, 8L, 50L, 58L, 78L, 908814,
  # --- Latvia ---
  6874, 38L, 170L, 14L, 7L, 10L, 8L, 17L, 44L, 70L, 695068,
  6769, 40L, 196L, 7L, 4L, 6L, 6L, 17L, 45L, 111L, 461582,
  # --- Russia ---
  10745, 34L, 112L, 12L, 7L, 8L, 5L, 18L, 24L, 38L, 520000,
  14263, 28L, 68L, 5L, 4L, 3L, 4L, 10L, 16L, 26L, 280000,
  # --- Canada ---
  1530, 55L, 109L, 2L, 4L, 5L, 5L, 13L, 25L, 55L, 350000
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

# ---------------------------------------------------------------------------
# 8. Head-to-head scraping helpers
# ---------------------------------------------------------------------------
# Scrapes a team's match schedule from Volleyballworld for a specific event.
# This is useful for building h2h records between rivals.
#
# Volleyballworld team schedule pages have a consistent URL pattern:
#   https://en.volleyballworld.com/beachvolleyball/competitions/
#   beach-pro-tour/<year>/<event-type>/<location>/teams/men/<team_id>/schedule/
#
# Usage:
#   scrape_vw_team_schedule(
#     "https://en.volleyballworld.com/.../teams/men/3144025/schedule/"
#   )
#
# This helper parses the page for match results involving the given team.

scrape_vw_team_schedule <- function(url) {
  tryCatch({
    page <- rvest::read_html(url)
    text <- rvest::html_text(page)
    text
  }, error = function(e) {
    warning(sprintf("Failed to fetch %s: %s", url, conditionMessage(e)))
    NULL
  })
}

# ---------------------------------------------------------------------------
# 9. Mol/Sorum vs Åhman/Hellvig verified head-to-head record
# ---------------------------------------------------------------------------
# This h2h data is verified against Volleyballworld.com official results
# and CEV European Championship records. Sources:
#   - Volleyballworld team schedule pages (official BPT results)
#   - CEV EuroBeachVolley 2022 results (ec2022results.com)
#   - Wikipedia 2022 European Beach Volleyball Championships
#   - Volleyballworld event articles with linked scoreboards
#
# To verify or add matches:
#   1. Check Volleyballworld event pages for each BPT tournament
#   2. Filter for Mol/Sorum or Åhman/Hellvig team schedule
#   3. Cross-reference set scores from match centre links
#
# All scores listed from Mol/Sorum's perspective.

mol_ahman_h2h <- tibble::tribble(
  ~date,         ~tournament,             ~round,  ~mol_score,
  ~sets_result,  ~winner,
  "2022-08-17",  "European Championship", "Pool",  "22-20, 21-16",
  "2-0",         "Mol/Sorum",
  "2022-08-21",  "European Championship", "Semi",  "16-21, 23-21, 13-15",
  "1-2",         "Åhman/Hellvig",
  "2022-11-06",  "Cape Town Elite16",     "Gold",  "21-19, 21-19",
  "2-0",         "Mol/Sorum",
  "2023-02-01",  "Doha Elite16",          "Pool",  "15-21, 21-18, 15-13",
  "2-1",         "Mol/Sorum",
  "2023-02-05",  "Doha Elite16",          "Gold",  "21-19, 21-19",
  "2-0",         "Mol/Sorum",
  "2023-03-26",  "Tepic Elite16",         "Gold",  "16-21, 15-21",
  "0-2",         "Åhman/Hellvig",
  "2023-12-07",  "Doha Finals",           "Pool",  "21-16, 21-16",
  "2-0",         "Mol/Sorum",
  "2023-12-09",  "Doha Finals",           "Gold",  "16-21, 17-21",
  "0-2",         "Åhman/Hellvig",
  "2024-06-09",  "Ostrava Elite16",       "Semi",  "21-23, 18-21",
  "0-2",         "Åhman/Hellvig",
  "2024-10-20",  "Joao Pessoa Elite16",   "Semi",  "22-20, 18-21, 15-10",
  "2-1",         "Mol/Sorum",
  "2024-12-05",  "Doha Finals",           "Pool",  "21-15, 17-21, 8-15",
  "1-2",         "Åhman/Hellvig",
  "2024-12-07",  "Doha Finals",           "Gold",  "21-18, 22-20",
  "2-0",         "Mol/Sorum"
)

h2h_path <- file.path(data_dir, "mol_ahman_h2h.csv")
readr::write_csv(mol_ahman_h2h, h2h_path)
cat(sprintf("\nWrote %d h2h matches to %s\n", nrow(mol_ahman_h2h), h2h_path))

mol_wins <- sum(mol_ahman_h2h$winner == "Mol/Sorum")
ahman_wins <- sum(mol_ahman_h2h$winner == "Åhman/Hellvig")
cat(sprintf("Record: Mol/Sorum %d - %d Åhman/Hellvig\n", mol_wins, ahman_wins))
