
`%||%` <- function(x, y) if (is.null(x)) y else x

extract_imdb_id <- function(input) {
  # already an ID
  if (grepl("^tt\\d+$", input)) {
    return(input)
  }
  
  # try to pull from URL or messy string
  id <- stringr::str_match(input, "tt\\d+")[, 1]
  
  if (is.na(id)) {
    stop("Could not extract IMDb ID from input: ", input)
  }
  
  id
}

get_imdb_season_episodes <- function(imdb_input, season) {
  imdb_id <- extract_imdb_id(imdb_input)
  
  url <- paste0(
    "https://www.imdb.com/title/",
    imdb_id,
    "/episodes/?season=",
    season
  )
  
  page <- rvest::read_html(url)
  
  next_data_txt <- page %>%
    rvest::html_element("script#__NEXT_DATA__") %>%
    rvest::html_text2()
  
  next_data <- jsonlite::fromJSON(next_data_txt, simplifyVector = FALSE)
  
  episodes_items <- next_data$props$pageProps$contentData$section$episodes$items
  
  if (is.null(episodes_items) || length(episodes_items) == 0) {
    return(
      tibble::tibble(
        season       = integer(),
        episode      = integer(),
        title        = character(),
        rating       = numeric(),
        votes        = integer(),
        release_year = integer()
      )
    )
  }
  
  purrr::map_dfr(episodes_items, function(item) {
    tibble::tibble(
      season       = as.integer(item$season),
      episode      = as.integer(item$episode),
      title        = item$titleText,
      rating       = item$aggregateRating,
      votes        = item$voteCount,
      release_year = item$releaseYear %||% NA_integer_
    )
  })
}


get_imdb_all_episodes <- function(imdb_input, max_seasons = 50) {
  imdb_id <- extract_imdb_id(imdb_input)
  
  out <- list()
  
  for (s in seq_len(max_seasons)) {
    df <- get_imdb_season_episodes(imdb_id, s)
    
    if (nrow(df) == 0) {
      break
    }
    
    out[[length(out) + 1]] <- df
  }
  
  if (length(out) == 0) {
    return(
      tibble::tibble(
        imdb_id      = character(),
        season       = integer(),
        episode      = integer(),
        title        = character(),
        rating       = numeric(),
        votes        = integer()
      )
    )
  }
  
  dplyr::bind_rows(out) %>%
    dplyr::mutate(imdb_id = imdb_id, .before = 1)
}

# recursively collect title nodes that look like tv series/miniseries
collect_title_nodes <- function(x) {
  out <- list()
  
  if (is.list(x)) {
    nms <- names(x)
    
    # candidate node if it has an ID and looks like a tv show
    if (!is.null(nms) &&
        "id" %in% nms &&
        is.character(x$id) &&
        length(x$id) == 1 &&
        grepl("^tt\\d+$", x$id)) {
      
      # get title text if present
      title <- NA_character_
      if ("titleText" %in% nms) {
        tt <- x$titleText
        if (is.list(tt) && !is.null(tt$text)) {
          title <- tt$text
        } else if (is.character(tt)) {
          title <- tt
        }
      }
      
      type <- NA_character_
      if ("titleType" %in% nms) {
        tt <- x$titleType
        if (is.list(tt) && !is.null(tt$id)) {
          type <- tt$id
        } else if (is.character(tt)) {
          type <- tt
        }
      }
      
      out[[length(out) + 1]] <- tibble::tibble(
        imdb_id = x$id,
        title   = title,
        type    = type
      )
    }
    
    # recurse into children
    for (i in seq_along(x)) {
      out <- c(out, collect_title_nodes(x[[i]]))
    }
  }
  
  out
}

get_top250_tv_shows <- function() {
  url <- "https://www.imdb.com/chart/toptv/"
  
  page <- rvest::read_html(url)
  
  next_data_txt <- page %>%
    rvest::html_element("script#__NEXT_DATA__") %>%
    rvest::html_text2()
  
  next_data <- jsonlite::fromJSON(next_data_txt, simplifyVector = FALSE)
  
  nodes <- collect_title_nodes(next_data)
  
  titles_df <- nodes %>%
    dplyr::bind_rows() %>%
    dplyr::filter(type %in% c("tvSeries", "tvMiniSeries")) %>%
    dplyr::distinct(imdb_id, .keep_all = TRUE) %>%
    dplyr::slice_head(n = 250)
  
  titles_df
}

get_top250_tv_episodes <- function() {
  top250 <- get_top250_tv_shows()
  
  purrr::map2_dfr(
    .x = top250$imdb_id,
    .y = top250$title,
    .f = function(id, show_title) {
      message("Fetching ", show_title, " (", id, ")")
      
      eps <- get_imdb_all_episodes(id)
      
      if (nrow(eps) == 0) {
        return(
          tibble::tibble(
            imdb_id      = character(),
            series_title = character(),
            season       = integer(),
            episode      = integer(),
            title        = character(),
            rating       = numeric(),
            votes        = integer()
          )
        )
      }
      
      eps <- eps %>%
        dplyr::mutate(
          series_title = show_title,
          .before = 2
        )
      
      # small pause between series
      Sys.sleep(runif(1, 0.3, 0.8))
      
      eps
    }
  )
}

top250_episodes <- get_top250_tv_episodes()

# uncomment to override csv
# top250_episodes %>% write.csv(here::here("data", "top250_imdb_series.csv"))
