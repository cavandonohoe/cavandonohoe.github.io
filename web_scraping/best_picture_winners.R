`%>%` <- magrittr::`%>%`
# best picture winners ------------------------------------------------------------------------

imdb_user_agent <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

read_imdb_html <- function(url) {
  all_empty <- TRUE
  for (attempt in seq_len(3)) {
    response <- tryCatch(
      httr::GET(
        url,
        httr::user_agent(imdb_user_agent),
        httr::add_headers(`Accept-Language` = "en-US,en;q=0.9"),
        httr::timeout(60)
      ),
      error = function(e) e
    )
    if (inherits(response, "error") || httr::http_error(response)) {
      all_empty <- FALSE
      if (attempt < 3) Sys.sleep(2 ^ attempt)
      next
    }
    html <- httr::content(response, as = "text", encoding = "UTF-8")
    if (is.null(html) || !nzchar(html) || nchar(html) < 200) {
      message(sprintf(
        "[best_picture] empty/short body for %s (%d chars), retrying...",
        url, nchar(if (is.null(html)) "" else html)
      ))
      if (attempt < 3) Sys.sleep(2 ^ attempt)
      next
    }
    parsed <- tryCatch(xml2::read_html(html), error = function(e) e)
    if (!inherits(parsed, "error")) {
      return(parsed)
    }
    all_empty <- FALSE
    message(sprintf(
      "[best_picture] parse error for %s: %s",
      url, conditionMessage(parsed)
    ))
    if (attempt < 3) Sys.sleep(2 ^ attempt)
  }
  if (all_empty && exists("signal_imdb_block", mode = "function")) {
    signal_imdb_block(url)
  }
  stop("Failed to fetch ", url, " after 3 attempts")
}

imdb_next_data <- function(url) {
  html <- read_imdb_html(url)
  json_text <- html %>%
    rvest::html_node("script#__NEXT_DATA__") %>%
    rvest::html_text()

  if (length(json_text) == 0) {
    stop("IMDb page did not include __NEXT_DATA__.")
  }

  jsonlite::fromJSON(json_text, simplifyVector = TRUE, flatten = TRUE)
}

url <- paste0(
  "https://www.imdb.com/search/title/?groups=best_picture_winner",
  "&sort=year,desc&count=250&view=simple"
)

best_pictures <- imdb_next_data(url)
title_items <- best_pictures$props$pageProps$searchResults$titleResults$titleListItems

oscar_winners_tib <- tibble::tibble(
  rank = seq_along(title_items$titleId),
  oscar_titles = title_items$titleText,
  movie_year = title_items$releaseYear,
  oscar_imdb_ratings = title_items$ratingSummary.aggregateRating,
  oscar_imdb_id = paste0("https://www.imdb.com/title/", title_items$titleId, "/"),
  tconst = title_items$titleId
)


# create a function for grabbing box office data from each imdb page
box_office_from_imdb_page <- function(url, retries = 3, pause_seconds = 1) {
  attempt <- 1
  while (attempt <= retries) {
    gross_worldwide <- tryCatch({
      page_data <- imdb_next_data(url)
      main_data <- page_data$props$pageProps$mainColumnData
      main_data$worldwideGross$total$amount
    }, error = function(err) {
      NA_real_
    })

    if (length(gross_worldwide) == 0 || is.null(gross_worldwide)) {
      gross_worldwide <- NA_real_
    }

    if (!is.na(gross_worldwide) || attempt == retries) {
      return(gross_worldwide)
    }

    Sys.sleep(pause_seconds)
    attempt <- attempt + 1
  }

  NA_real_
}

# grab box office data for each best picture winner
if (!exists("refresh_box_office")) {
  refresh_box_office <- FALSE
}
if (!exists("cache_ttl_days")) {
  cache_ttl_days <- 365
}
if (!exists("box_office_cache_path")) {
  box_office_cache_path <- file.path("data", "best_picture_box_office_cache.rds")
}

use_cache <- FALSE
if (file.exists(box_office_cache_path) && !refresh_box_office) {
  cache_age_days <- as.numeric(difftime(Sys.time(),
                                        file.info(box_office_cache_path)$mtime,
                                        units = "days"))
  use_cache <- !is.na(cache_age_days) && cache_age_days <= cache_ttl_days
}

if (use_cache) {
  oscar_box_office <- readRDS(box_office_cache_path)
} else {
  oscar_box_office <- oscar_winners_tib$oscar_imdb_id %>%
    lapply(box_office_from_imdb_page) %>%
    unlist() %>%
    tibble::as_tibble() %>%
    dplyr::rename(life_time_gross = value)

  saveRDS(oscar_box_office, box_office_cache_path)
}

# clean it up
#
# The ceremony year (oscar_year) is the calendar year in which the Academy
# Awards ceremony was held. A film released in year N is honored at the
# ceremony held in year N + 1, so oscar_year = movie_year + 1 for the vast
# majority of winners. A handful of films carry an IMDb releaseYear that does
# not match their Academy eligibility year (early festival premieres, the
# 1927/28 combined first ceremony, the shifted early-1940s eligibility window),
# so those ceremony years are corrected explicitly, keyed by IMDb title id
# (tconst) rather than the display title (which can drift, e.g. "Crash (I)").
oscar_year_overrides <- tibble::tribble(
  ~tconst,     ~oscar_year_fixed,
  "tt0018578", 1929L,  # Wings (movie 1927; 1st ceremony honored 1927/28)
  "tt0034583", 1944L,  # Casablanca (movie 1942; 16th ceremony, 1943 films)
  "tt0375679", 2006L,  # Crash (movie 2004; 78th ceremony, 2005 films)
  "tt0887912", 2010L   # The Hurt Locker (movie 2008; 82nd ceremony, 2009 films)
)

oscar_winners_clean <- oscar_winners_tib %>%
  dplyr::bind_cols(oscar_box_office) %>%
  # doesn't actually look like Sunrise won the oscar
  dplyr::filter(oscar_titles != "Sunrise") %>%
  dplyr::left_join(oscar_year_overrides, by = "tconst") %>%
  dplyr::mutate(
    oscar_year = dplyr::coalesce(.data$oscar_year_fixed, .data$movie_year + 1L)
  ) %>%
  dplyr::select(-"oscar_year_fixed")

# oscar_winners_clean %>% write.csv(here::here("data", "best_picture_winners.csv"))

# oscar_winners_clean %>% ggplot2::ggplot(ggplot2::aes(x=oscar_year, y = oscar_imdb_ratings)) +
#   ggplot2::geom_point()
