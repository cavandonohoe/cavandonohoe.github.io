#!/usr/bin/env Rscript
# refresh_director_filmographies.R
#
# Pull the directing filmography for a curated list of the 10
# highest-grossing film directors of all time, hitting the TMDb v3 API.
#
# Output:
#   data/director_filmographies.csv  - long format, one row per (director, film)
#   data/director_filmographies_meta.csv - small table of director-level metadata
#
# Requires:
#   TMDB_API_KEY env var (a v3 API key, NOT a v4 read access token).
#
# Usage:
#   TMDB_API_KEY=... Rscript scripts/refresh_director_filmographies.R

`%>%` <- magrittr::`%>%`

api_key <- Sys.getenv("TMDB_API_KEY", unset = "")
if (!nzchar(api_key)) {
  stop("TMDB_API_KEY is not set. Add it to repo secrets and pass it ",
       "into the workflow env.")
}

# Curated list of the 10 highest-grossing film directors of all time per
# Wikipedia / Statista (worldwide, not inflation-adjusted). Sources are
# updated periodically; ranks past ~2025 may shift as newer films release.
# The Russo brothers are listed as a duo because they always co-direct.
directors <- tibble::tribble(
  ~rank, ~display_name,        ~tmdb_query,            ~force_id,
  1,     "Steven Spielberg",   "Steven Spielberg",     488L,
  2,     "James Cameron",      "James Cameron",        2710L,
  3,     "Russo Brothers",     "Anthony Russo",        19271L,
  4,     "Peter Jackson",      "Peter Jackson",        108L,
  5,     "Michael Bay",        "Michael Bay",          865L,
  6,     "David Yates",        "David Yates",          11343L,
  7,     "Christopher Nolan",  "Christopher Nolan",    525L,
  8,     "Ridley Scott",       "Ridley Scott",         578L,
  9,     "Tim Burton",         "Tim Burton",           510L,
  10,    "J.J. Abrams",        "J.J. Abrams",          15344L
)

tmdb_get <- function(path, query = list()) {
  url <- paste0("https://api.themoviedb.org/3", path)
  req <- httr2::request(url) %>%
    httr2::req_url_query(!!!c(list(api_key = api_key), query)) %>%
    httr2::req_retry(max_tries = 4, backoff = ~ min(2 ^ .x, 30)) %>%
    httr2::req_throttle(rate = 35 / 10) %>%
    httr2::req_user_agent("cavandonohoe.github.io/director-filmographies")
  resp <- httr2::req_perform(req)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
}

resolve_person_id <- function(query, force_id = NA_integer_) {
  if (!is.na(force_id)) return(force_id)
  res <- tmdb_get("/search/person", list(query = query, include_adult = "false"))
  if (!is.null(res$results) && nrow(res$results) > 0) {
    # Prefer the most popular candidate whose primary department is Directing.
    # TMDb's default sort is by popularity, but a non-director with the same
    # name can outrank the director (e.g. "David Yates" id 11544 has zero
    # directing credits and was outranking the real director id 11343).
    directing <- res$results[res$results$known_for_department == "Directing", ,
                             drop = FALSE]
    if (nrow(directing) > 0) return(directing$id[1])
    return(res$results$id[1])
  }
  stop("No TMDb person match for: ", query)
}

fetch_directing_credits <- function(person_id) {
  res <- tmdb_get(paste0("/person/", person_id, "/movie_credits"))
  crew <- res$crew
  if (is.null(crew) || nrow(crew) == 0) {
    return(tibble::tibble())
  }
  crew %>%
    dplyr::filter(.data$job == "Director") %>%
    dplyr::select(dplyr::any_of(c("id", "title", "release_date", "popularity"))) %>%
    dplyr::distinct(.data$id, .keep_all = TRUE)
}

fetch_movie_details <- function(movie_id) {
  res <- tmdb_get(paste0("/movie/", movie_id))
  tibble::tibble(
    id = movie_id,
    title = res$title %||% NA_character_,
    release_date = res$release_date %||% NA_character_,
    runtime = res$runtime %||% NA_integer_,
    budget = res$budget %||% NA_real_,
    revenue = res$revenue %||% NA_real_,
    vote_average = res$vote_average %||% NA_real_,
    vote_count = res$vote_count %||% NA_integer_,
    original_language = res$original_language %||% NA_character_,
    imdb_id = res$imdb_id %||% NA_character_
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Annual US CPI-U (1913-2025). Used to convert nominal USD revenue to
# 2025-equivalent dollars so cross-era comparisons are honest. Source:
# BLS CPI-U All Urban Consumers, US City Average, annual averages.
# Refreshing this once a year is sufficient — new films move much more
# than CPI revisions.
cpi_table <- tibble::tribble(
  ~year, ~cpi,
  1970,   38.8, 1971, 40.5, 1972, 41.8, 1973, 44.4, 1974, 49.3,
  1975,   53.8, 1976, 56.9, 1977, 60.6, 1978, 65.2, 1979, 72.6,
  1980,   82.4, 1981, 90.9, 1982, 96.5, 1983, 99.6, 1984, 103.9,
  1985,  107.6, 1986, 109.6, 1987, 113.6, 1988, 118.3, 1989, 124.0,
  1990,  130.7, 1991, 136.2, 1992, 140.3, 1993, 144.5, 1994, 148.2,
  1995,  152.4, 1996, 156.9, 1997, 160.5, 1998, 163.0, 1999, 166.6,
  2000,  172.2, 2001, 177.1, 2002, 179.9, 2003, 184.0, 2004, 188.9,
  2005,  195.3, 2006, 201.6, 2007, 207.3, 2008, 215.3, 2009, 214.5,
  2010,  218.1, 2011, 224.9, 2012, 229.6, 2013, 233.0, 2014, 236.7,
  2015,  237.0, 2016, 240.0, 2017, 245.1, 2018, 251.1, 2019, 255.7,
  2020,  258.8, 2021, 271.0, 2022, 292.7, 2023, 304.7, 2024, 313.7,
  2025,  321.5
)
ref_year <- max(cpi_table$year)
ref_cpi <- cpi_table$cpi[cpi_table$year == ref_year]

inflate_to_ref <- function(usd, year) {
  cpi <- cpi_table$cpi[match(year, cpi_table$year)]
  out <- usd * (ref_cpi / cpi)
  out[is.na(cpi) | is.na(usd)] <- NA_real_
  out
}

message(sprintf(
  "[director-films] Resolving %d directors...", nrow(directors)
))

directors_resolved <- directors %>%
  dplyr::mutate(
    person_id = purrr::pmap_int(
      list(.data$tmdb_query, .data$force_id),
      function(q, f) resolve_person_id(q, f)
    )
  )

message("[director-films] Person IDs:")
print(directors_resolved %>% dplyr::select(rank, display_name, person_id))

all_films <- purrr::pmap_dfr(
  list(
    directors_resolved$person_id,
    directors_resolved$display_name,
    directors_resolved$rank
  ),
  function(person_id, display_name, rank) {
    message(sprintf("[director-films] Pulling credits for %s (id=%d)...",
                    display_name, person_id))
    credits <- fetch_directing_credits(person_id)
    if (nrow(credits) == 0) {
      stop(sprintf(
        paste0("No directing credits returned for %s (TMDb id=%d). ",
               "The id is probably wrong (TMDb name disambiguation). ",
               "Update the directors tribble with the correct force_id."),
        display_name, person_id
      ))
    }
    details <- purrr::map_dfr(credits$id, fetch_movie_details)
    dplyr::left_join(details, credits %>% dplyr::select(id, popularity),
                     by = "id") %>%
      dplyr::mutate(
        director_rank = rank,
        director_display_name = display_name,
        director_person_id = person_id
      )
  }
)

# The Russo brothers credit is pulled via Anthony Russo only (id 19271);
# Joe Russo's directing credits are the same set (they always co-direct).
# Pulling both would double-count, so we collapse under one display name.

films_clean <- all_films %>%
  dplyr::filter(!is.na(.data$release_date), .data$release_date != "") %>%
  dplyr::mutate(
    release_year = as.integer(stringr::str_sub(.data$release_date, 1, 4))
  ) %>%
  dplyr::filter(
    !is.na(.data$release_year),
    .data$release_year >= 1960,
    .data$release_year <= as.integer(format(Sys.Date(), "%Y"))
  ) %>%
  # Drop films that haven't released revenue data yet (TMDb defaults to 0
  # for unknowns, which would distort plots). Keep them only if they have
  # a non-zero vote_count, so vote-average plots can still include them.
  dplyr::mutate(
    revenue_usable = .data$revenue > 0,
    budget_usable = .data$budget > 0,
    revenue_2025 = inflate_to_ref(.data$revenue, .data$release_year),
    budget_2025 = inflate_to_ref(.data$budget, .data$release_year)
  ) %>%
  dplyr::group_by(.data$director_display_name) %>%
  dplyr::arrange(.data$release_date, .by_group = TRUE) %>%
  dplyr::mutate(
    career_film_number = dplyr::row_number()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.data$director_rank, .data$career_film_number) %>%
  dplyr::select(
    director_rank, director_display_name, director_person_id,
    career_film_number, tmdb_id = id, imdb_id, title, release_date,
    release_year, runtime, budget, revenue, budget_2025, revenue_2025,
    revenue_usable, budget_usable, vote_average, vote_count,
    original_language, popularity
  )

# Director-level rollup so the Rmd doesn't have to recompute it.
meta <- films_clean %>%
  dplyr::filter(.data$revenue_usable) %>%
  dplyr::group_by(.data$director_rank, .data$director_display_name) %>%
  dplyr::summarise(
    n_films = dplyr::n(),
    first_year = min(.data$release_year, na.rm = TRUE),
    last_year = max(.data$release_year, na.rm = TRUE),
    lifetime_revenue = sum(.data$revenue, na.rm = TRUE),
    lifetime_revenue_2025 = sum(.data$revenue_2025, na.rm = TRUE),
    avg_revenue_per_film = mean(.data$revenue, na.rm = TRUE),
    avg_revenue_per_film_2025 = mean(.data$revenue_2025, na.rm = TRUE),
    avg_vote = mean(.data$vote_average[.data$vote_count >= 100], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(.data$director_rank)

dir.create(here::here("data"), showWarnings = FALSE, recursive = TRUE)

readr::write_csv(films_clean, here::here("data", "director_filmographies.csv"))
readr::write_csv(meta, here::here("data", "director_filmographies_meta.csv"))

message(sprintf(
  "[director-films] Wrote %d films across %d directors. CPI ref year: %d.",
  nrow(films_clean), nrow(meta), ref_year
))
