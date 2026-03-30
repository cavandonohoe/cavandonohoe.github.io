`%>%` <- magrittr::`%>%`
# best picture winners ------------------------------------------------------------------------

imdb_user_agent <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

read_imdb_html <- function(url) {
  response <- httr::GET(
    url,
    httr::user_agent(imdb_user_agent),
    httr::add_headers(`Accept-Language` = "en-US,en;q=0.9")
  )
  httr::stop_for_status(response)
  xml2::read_html(response)
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
latest_oscar_year <- max(oscar_winners_tib$movie_year, na.rm = TRUE) + 1

oscar_winners_clean <- oscar_winners_tib %>%
  dplyr::bind_cols(oscar_box_office) %>%
  # doesn't actually look like Sunrise won the oscar
  dplyr::filter(oscar_titles != "Sunrise") %>%
  dplyr::mutate(oscar_year = latest_oscar_year - dplyr::row_number() + 1) %>%
  dplyr::mutate(oscar_year = dplyr::case_when(oscar_titles == "The Hurt Locker" ~ 2010,
                                oscar_titles == "Slumdog Millionaire" ~ 2009,
                                oscar_titles == "Crash (I)" ~ 2006,
                                oscar_titles == "Million Dollar Baby" ~ 2005,
                                oscar_titles == "Mrs. Miniver" ~ 1943,
                                oscar_titles == "Casablanca" ~ 1944,
                                TRUE ~ oscar_year))

# oscar_winners_clean %>% write.csv(here::here("data", "best_picture_winners.csv"))

# oscar_winners_clean %>% ggplot2::ggplot(ggplot2::aes(x=oscar_year, y = oscar_imdb_ratings)) +
#   ggplot2::geom_point()
