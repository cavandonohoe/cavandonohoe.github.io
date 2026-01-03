`%>%` <- magrittr::`%>%`
# Top 1000 Box Office -------------------------------------------------------------------------

# box office mojo from imdb has a table listed on this website:
# https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW
# and continues
# https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW&offset=200
# and continues till 800
# let's get web scraping

imdb_user_agent <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

read_imdb_html <- function(url) {
  response <- httr::GET(
    url,
    httr::user_agent(imdb_user_agent),
    httr::add_headers("Accept-Language" = "en-US,en;q=0.9")
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

url <- "https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW"

links <- rvest::read_html(url) %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")


offset <- seq(from = 200, to = 800, by = 200)

all_urls <- c(url, paste0(url, "&offset=", offset))

rankings_tib <- tibble::tibble()
for (url_index in all_urls) {
  rankings <- rvest::read_html(url_index) %>%
    rvest::html_node("table") %>%
    rvest::html_table(fill=TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Rank = readr::parse_number(Rank))

  # funny enough, before doing this, I just read in a list of imdb movies titles 
  # and got the imdb id that way. but apparently multiple versions of beauty and the beast
  # were released in 2017, so that plan kinda backfired
  links <- rvest::read_html(url_index) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  link_tib <- tibble::tibble(links) %>%
    dplyr::filter(grepl(x=links, pattern="cso") &
                                        grepl(x=links, pattern = "title")) %>%
    dplyr::mutate(id = gsub(x=links, pattern = "/title/|/\\?ref.*", replacement = ""))


  rankings_id <- rankings %>%
    dplyr::bind_cols(link_tib)

  rankings_tib <- rankings_tib %>%
    dplyr::bind_rows(rankings_id)
}

rankings_tib_final <- rankings_tib %>%
  dplyr::mutate(Rank = ifelse(is.na(Rank) & dplyr::row_number() == 1000, 1000, Rank))


# now include user rating ---------------------------------------------------------------------


rankings_tib_final2 <- rankings_tib_final %>%
  dplyr::mutate(imdb_link = paste0("https://www.imdb.com/title/", id))

if (!exists("refresh_ratings")) {
  refresh_ratings <- FALSE
}

if (!exists("ratings_cache_path")) {
  ratings_cache_path <- file.path("data", "top1000_imdb_ratings_cache.rds")
}

get_rating <- function(title_id) {
  url <- paste0("https://www.imdb.com/title/", title_id, "/")
  rating <- tryCatch({
    page_data <- imdb_next_data(url)
    rating_value <- page_data$props$pageProps$aboveTheFoldData$ratingsSummary$aggregateRating
    if (is.null(rating_value) || length(rating_value) == 0) {
      NA_real_
    } else {
      as.numeric(rating_value)
    }
  }, error = function(err) {
    NA_real_
  })

  rating
}


# this one takes a while
# look through 1000 websites and find each rating
# luckily imdb has unique id's for each movie unlike a different movie rating website
# *cough* rotten tomatoes *cough*
ratings_cache <- tibble::tibble(id = character(), imdb_rating = numeric())
if (file.exists(ratings_cache_path) && !refresh_ratings) {
  ratings_cache <- readRDS(ratings_cache_path)
}

ratings_lookup <- setNames(ratings_cache$imdb_rating, ratings_cache$id)
title_lookup <- setNames(rankings_tib_final2$Title, rankings_tib_final2$id)
missing_ids <- setdiff(rankings_tib_final2$id, names(ratings_lookup))

if (length(missing_ids) > 0) {
  recent_titles <- character()
  for (i in seq_along(missing_ids)) {
    title_id <- missing_ids[[i]]
    title_name <- title_lookup[[title_id]]
    ratings_lookup[[title_id]] <- get_rating(title_id)
    recent_titles <- c(recent_titles, sprintf("%s (%s)", title_name, title_id))

    if (i %% 10 == 0 || i == length(missing_ids)) {
      message(sprintf("Fetched %d/%d:", i, length(missing_ids)))
      message(paste(recent_titles, collapse = "\n"))
      message("")
      recent_titles <- character()
    }

    if (i %% 25 == 0) {
      saveRDS(
        tibble::tibble(id = names(ratings_lookup),
                       imdb_rating = as.numeric(ratings_lookup)),
        ratings_cache_path
      )
    }
  }
}

saveRDS(
  tibble::tibble(id = names(ratings_lookup),
                 imdb_rating = as.numeric(ratings_lookup)),
  ratings_cache_path
)

all_user_ratings <- as.numeric(ratings_lookup[rankings_tib_final2$id])



imdb_rank_ratings <-
  rankings_tib_final2 %>%
  dplyr::mutate(imdb_rating = as.numeric(unlist(all_user_ratings)))


imdb_rank_ratings_clean <- imdb_rank_ratings %>%
  dplyr::mutate(`Lifetime Gross` = as.numeric(gsub('\\$|,', '', `Lifetime Gross`)),
         imdb_rating = as.numeric(imdb_rating)) %>%
  dplyr::rename(box_office_rank = Rank)

# imdb_rank_ratings_clean %>% write.csv(here::here("data", "top1000_box_office.csv"))
