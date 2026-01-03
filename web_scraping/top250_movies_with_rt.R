`%>%` <- magrittr::`%>%`
# alright working on imdb top 250 movies now
# maybe I can get the rotten tomatoes as well
# I think check the director(s) and that might be your unique identifier

url <- "https://www.imdb.com/chart/top/"

imdb_user_agent <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/123.0.0.0 Safari/537.36"
)

cache_dir <- here::here("web_scraping", "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

make_cache_key <- function(text) {
  tmp <- tempfile()
  writeLines(text, tmp)
  key <- as.character(tools::md5sum(tmp))
  unlink(tmp)
  key
}

safe_fetch_html <- function(page_url, label = "page", max_attempts = 3, use_cache = TRUE) {
  cache_path <- file.path(cache_dir, paste0(make_cache_key(page_url), ".html"))
  if (use_cache && file.exists(cache_path)) {
    return(rvest::read_html(cache_path))
  }

  for (attempt in seq_len(max_attempts)) {
    resp <- tryCatch(
      httr::GET(
        page_url,
        httr::user_agent(imdb_user_agent),
        httr::add_headers(`Accept-Language` = "en-US,en;q=0.9"),
        httr::timeout(60)
      ),
      error = function(e) e
    )

    if (!inherits(resp, "error") && !httr::http_error(resp)) {
      html <- httr::content(resp, as = "text", encoding = "UTF-8")
      if (use_cache) {
        writeLines(html, cache_path)
      }
      return(rvest::read_html(html))
    }

    if (attempt < max_attempts) {
      Sys.sleep(2 ^ attempt)
    }
  }

  message("Failed to fetch ", label, ": ", page_url)
  NULL
}

fetch_imdb_html <- function(page_url) {
  html <- safe_fetch_html(page_url, label = "IMDb")
  if (is.null(html)) {
    stop("Failed to fetch IMDb page: ", page_url)
  }
  html
}

parse_json_ld <- function(html, type) {
  scripts <- html %>%
    rvest::html_nodes("script[type='application/ld+json']") %>%
    rvest::html_text()
  parsed <- lapply(
    scripts,
    function(text) tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  )
  matches <- Filter(
    function(x) !is.null(x) && !is.null(x[["@type"]]) && x[["@type"]] == type,
    parsed
  )
  if (length(matches) == 0) {
    return(NULL)
  }
  matches[[1]]
}

parse_imdb_next_data <- function(html) {
  node <- rvest::html_node(html, "script#__NEXT_DATA__")
  if (is.null(node)) {
    return(NULL)
  }
  text <- rvest::html_text(node)
  tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
}

extract_director_name <- function(director) {
  if (is.null(director)) {
    return(NA_character_)
  }
  if (is.data.frame(director)) {
    return(director$name[1])
  }
  if (is.list(director)) {
    return(director[[1]]$name %||% NA_character_)
  }
  if (is.character(director)) {
    return(director[1])
  }
  NA_character_
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

top_250_html <- fetch_imdb_html(url)

item_list <- parse_json_ld(top_250_html, "ItemList")
if (is.null(item_list)) {
  stop("Could not find the IMDb Top 250 list on the chart page.")
}

items <- item_list$itemListElement
item_df <- items$item
if (is.null(item_df) || !is.data.frame(item_df)) {
  stop("Unexpected IMDb schema: item list does not include a data.frame of items.")
}

top_250_base <- tibble::tibble(
  rank = seq_len(nrow(items)),
  title = ifelse(
    !is.na(item_df$alternateName) & item_df$alternateName != "",
    item_df$alternateName,
    item_df$name
  ),
  site = item_df$url,
  imdb_rating = as.numeric(item_df$aggregateRating$ratingValue),
  id = sub(".*/title/(tt\\d+)/.*", "\\1", item_df$url)
)

top_250_base <- top_250_base %>%
  dplyr::mutate(title = gsub("&apos;|&#39;", "'", title))

safe_post_json <- function(url, body, headers, label = "request", max_attempts = 3) {
  cache_key <- make_cache_key(paste0(url, jsonlite::toJSON(body, auto_unbox = TRUE)))
  cache_path <- file.path(cache_dir, paste0(cache_key, ".json"))
  if (file.exists(cache_path)) {
    return(jsonlite::fromJSON(readChar(cache_path, file.info(cache_path)$size), simplifyVector = FALSE))
  }

  for (attempt in seq_len(max_attempts)) {
    resp <- tryCatch(
      httr::POST(url, headers, encode = "json", body = body, httr::timeout(60)),
      error = function(e) e
    )

    if (!inherits(resp, "error") && !httr::http_error(resp)) {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      writeLines(txt, cache_path)
      return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
    }

    if (attempt < max_attempts) {
      Sys.sleep(2 ^ attempt)
    }
  }

  message("Failed to fetch ", label)
  NULL
}

extract_credit_names <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(NA_character_)
  }
  names <- vapply(items, function(x) x$rowTitle %||% NA_character_, character(1))
  names <- names[!is.na(names) & names != ""]
  if (length(names) == 0) {
    return(NA_character_)
  }
  paste(unique(names), collapse = "|")
}

get_company_names <- function(html_object, section_id) {
  if (is.null(html_object)) {
    return(NA_character_)
  }
  section <- rvest::html_node(
    html_object,
    xpath = paste0("//*[@id='", section_id, "']/ancestor::section[1]")
  )
  if (is.null(section)) {
    return(NA_character_)
  }
  names <- rvest::html_nodes(section, "a.ipc-metadata-list-item__label") %>%
    rvest::html_text(trim = TRUE)
  names <- names[names != ""]
  if (length(names) == 0) {
    return(NA_character_)
  }
  paste(unique(names), collapse = "|")
}

get_rt_algolia_config <- function() {
  search_html <- safe_fetch_html(
    "https://www.rottentomatoes.com/search?search=shawshank",
    label = "Rotten Tomatoes search"
  )
  if (is.null(search_html)) {
    return(NULL)
  }

  scripts <- search_html %>%
    rvest::html_nodes("script") %>%
    rvest::html_text()
  script <- scripts[stringr::str_detect(scripts, "root.RottenTomatoes.thirdParty")]
  if (length(script) == 0) {
    return(NULL)
  }

  json <- stringr::str_match(script[1], "root.RottenTomatoes.thirdParty = (\\{.*\\});")[, 2]
  if (is.na(json) || json == "") {
    return(NULL)
  }

  jsonlite::fromJSON(json)
}

rt_algolia_search <- function(query, config) {
  if (is.null(config) || is.null(config$algoliaSearch)) {
    return(list())
  }

  app_id <- config$algoliaSearch$aId
  api_key <- config$algoliaSearch$sId
  url <- paste0("https://", app_id, "-dsn.algolia.net/1/indexes/*/queries")
  body <- list(requests = list(list(
    indexName = "content_rt",
    params = paste0("query=", URLencode(query, reserved = TRUE), "&hitsPerPage=5")
  )))

  resp <- safe_post_json(
    url,
    body,
    httr::add_headers(
      `X-Algolia-API-Key` = api_key,
      `X-Algolia-Application-Id` = app_id
    ),
    label = "Rotten Tomatoes search"
  )

  if (is.null(resp) || length(resp$results) == 0) {
    return(list())
  }

  resp$results[[1]]$hits
}

pick_rt_hit <- function(hits, year = NA_real_) {
  if (length(hits) == 0) {
    return(NULL)
  }
  movie_hits <- Filter(function(h) !is.null(h$type) && h$type == "movie", hits)
  if (length(movie_hits) == 0) {
    return(NULL)
  }
  if (!is.na(year)) {
    year_hits <- Filter(function(h) !is.null(h$releaseYear) && h$releaseYear == year, movie_hits)
    if (length(year_hits) > 0) {
      return(year_hits[[1]])
    }
  }
  movie_hits[[1]]
}

rt_lookup_movie <- function(title, year, config) {
  hits <- rt_algolia_search(title, config)
  hit <- pick_rt_hit(hits, year)
  if (is.null(hit)) {
    return(list(
      vanity = NA_character_,
      audience_score = NA_real_,
      critics_score = NA_real_,
      director = NA_character_,
      studios = NA_character_,
      producers = NA_character_
    ))
  }

  director <- NA_character_
  if (!is.null(hit$crew)) {
    director_hit <- Filter(function(x) !is.null(x$role) && x$role == "Director", hit$crew)
    if (length(director_hit) > 0) {
      director <- director_hit[[1]]$name %||% NA_character_
    }
  }

  producer <- NA_character_
  if (!is.null(hit$crew)) {
    producer_hit <- Filter(function(x) !is.null(x$role) && x$role == "Producer", hit$crew)
    if (length(producer_hit) > 0) {
      producer <- paste(
        unique(vapply(producer_hit, function(x) x$name %||% NA_character_, character(1))),
        collapse = "|"
      )
    }
  }

  studios <- NA_character_
  if (!is.null(hit$studios)) {
    studios <- paste(unique(unlist(hit$studios)), collapse = "|")
  }

  list(
    vanity = hit$vanity %||% NA_character_,
    audience_score = hit$rottenTomatoes$audienceScore %||% NA_real_,
    critics_score = hit$rottenTomatoes$criticsScore %||% NA_real_,
    director = director,
    studios = studios,
    producers = producer
  )
}

get_imdb_title_meta <- function(page_url) {
  html <- safe_fetch_html(page_url, label = "IMDb title")
  if (is.null(html)) {
    return(list(year = NA_real_, director = NA_character_))
  }
  movie_ld <- parse_json_ld(html, "Movie")
  date_published <- movie_ld$datePublished
  year <- if (!is.null(date_published)) {
    suppressWarnings(as.numeric(substr(date_published, 1, 4)))
  } else {
    NA_real_
  }

  imdb_director <- NA_character_
  next_data <- parse_imdb_next_data(html)
  if (!is.null(next_data)) {
    credits <- next_data$props$pageProps$aboveTheFoldData$principalCreditsV2
    if (!is.null(credits)) {
      groups <- vapply(credits, function(x) x$grouping$text %||% NA_character_, character(1))
      idx <- which(groups == "Director")
      if (length(idx) > 0) {
        credit_list <- credits[[idx[1]]]$credits
        if (!is.null(credit_list) && length(credit_list) > 0) {
          imdb_director <- credit_list[[1]]$name$nameText$text %||% NA_character_
        }
      }
    }
  }
  if (is.na(imdb_director)) {
    imdb_director <- extract_director_name(movie_ld$director)
  }
  list(year = year, director = imdb_director)
}

title_meta <- lapply(top_250_base$site, get_imdb_title_meta)

top_250_tib_clean <- top_250_base %>%
  dplyr::mutate(
    year = vapply(title_meta, function(x) x$year, numeric(1)),
    director = vapply(title_meta, function(x) x$director, character(1)),
    producer_site = paste0(site, "companycredits?ref_=ttfc_sa_3"),
    fullcredits_site = paste0(site, "fullcredits")
  ) %>%
  dplyr::select(rank, title, director, year, imdb_rating, id, site, producer_site, fullcredits_site)


# next step is to grab each rotten tomato section
# so it's in the format shawshank_redemption
# they drop 'the' and turn it into snakecase
# so
# https://www.rottentomatoes.com/m/shawshank_redemption
# and any duplicates get a year
# so we'll do some error handling when that happens
# also, I said they drop the 'the', but I mean... not always


rt_config <- get_rt_algolia_config()
rt_lookup <- mapply(
  rt_lookup_movie,
  title = top_250_tib_clean$title,
  year = top_250_tib_clean$year,
  MoreArgs = list(config = rt_config),
  SIMPLIFY = FALSE
)

rt_vanity <- vapply(rt_lookup, function(x) x$vanity, character(1))
rt_audience_scores <- vapply(rt_lookup, function(x) x$audience_score, numeric(1))
rt_critics_scores <- vapply(rt_lookup, function(x) x$critics_score, numeric(1))
rt_directors <- vapply(rt_lookup, function(x) x$director, character(1))
rt_studios <- vapply(rt_lookup, function(x) x$studios, character(1))
rt_producers <- vapply(rt_lookup, function(x) x$producers, character(1))

rotten_tomatoes_url <- top_250_tib_clean %>%
  dplyr::select(title, imdb_director = director) %>%
  dplyr::mutate(rotten_tomatoes_title =
           dplyr::case_when((title == "The Dark Knight" & imdb_director == "Christopher Nolan") |
                       (title == "The Dark Knight Rises" & imdb_director == "Christopher Nolan") |
                       (title == "The Gold Rush" & imdb_director == "Charles Chaplin") |
                       (title == "The Grand Budapest Hotel" & imdb_director == "Wes Anderson") ~
                       snakecase::to_any_case(string = title, case = "snake"),
                     title == "12 Angry Men" & imdb_director == "Sidney Lumet" ~
                       "1000013_12_angry_men",
                     title == "Seven Samurai" & imdb_director == "Akira Kurosawa" ~
                       "seven_samurai_1956",
                     title == "Se7en" & imdb_director == "David Fincher" ~
                       "seven",
                     title == "Life Is Beautiful" & imdb_director == "Roberto Benigni" ~
                       "1084398-life_is_beautiful",
                     title == "Star Wars: Episode IV - A New Hope" &
                       imdb_director == "George Lucas" ~
                       "star_wars",
                     title == "Star Wars: Episode V - The Empire Strikes Back" &
                       imdb_director == "Irvin Kershner" ~
                       "empire_strikes_back",
                     title == "Hara-Kiri" & imdb_director == "Masaki Kobayashi" ~
                       "harakiri",
                     title == "Whiplash" & imdb_director == "Damien Chazelle" ~
                       "whiplash_2014",
                     title == "Casablanca" & imdb_director == "Michael Curtiz" ~
                       "1003707-casablanca",
                     title == "The Lives of Others" &
                       imdb_director == "Florian Henckel von Donnersmarck" ~
                       "the_lives_of_others",
                     title == "Sunset Blvd." & imdb_director == "Billy Wilder" ~
                       "sunset_boulevard",
                     title == paste0(
                       "Dr. Strangelove or: How I Learned to Stop Worrying and ",
                       "Love the Bomb"
                     ) &
                       imdb_director == "Stanley Kubrick" ~
                       "dr_strangelove",
                     title == "Princess Mononoke" & imdb_director == "Hayao Miyazaki" ~
                       "princess_mononoke_1999",
                     title == "Your Name." & imdb_director == "Makoto Shinkai" ~
                       "your_name_2017",
                     title == "Aliens" & imdb_director == "James Cameron" ~
                       "1000617-aliens",
                     title == "Capharnaüm" & imdb_director == "Nadine Labaki" ~
                       "capernaum",
                     title == "Hamilton" & imdb_director == "Thomas Kail" ~
                       "hamilton_2020",
                     title == "Braveheart" & imdb_director == "Mel Gibson" ~
                       "1065684-braveheart",
                     title == "M" & imdb_director == "Fritz Lang" ~
                       "1012928-m",
                     title == "Come and See" & imdb_director == "Elem Klimov" ~
                       "1036052-come_and_see",
                     title == "The Kid" & imdb_director == "Charles Chaplin" ~
                       "1052609-kid",
                     title == "Logan" & imdb_director == "James Mangold" ~
                       "logan_2017",
                     title == "The Apartment" & imdb_director == "Billy Wilder" ~
                       "1001115-apartment",
                     title == "Metropolis" & imdb_director == "Fritz Lang" ~
                       "1013775-metropolis",
                     title == "The Sting" & imdb_director == "George Roy Hill" ~
                       "1020130-sting",
                     title == "1917" & imdb_director == "Sam Mendes" ~
                       "1917_2019",
                     title == "Heat" & imdb_director == "Michael Mann" ~
                       "heat_1995",
                     title == "All About Eve" & imdb_director == "Joseph L. Mankiewicz" ~
                       "1000626-all_about_eve",
                     title == "Unforgiven" & imdb_director == "Clint Eastwood" ~
                       "1041911-unforgiven",
                     title == "The Wolf of Wall Street" & imdb_director == "Martin Scorsese" ~
                       "the_wolf_of_wall_street_2013",
                     title == "Shutter Island" & imdb_director == "Martin Scorsese" ~
                       "1198124-shutter_island",
                     title == "Warrior" & imdb_director == "Gavin O'Connor" ~
                       "1212910-warrior",
                     title == "My Father and My Son" & imdb_director == "Cagan Irmak" ~
                       "babam-ve-oglum-my-father-and-my-son",
                     title == "The General" & imdb_director == "Clyde Bruckman" ~
                       "1008166-general",
                     title == "Mary and Max" & imdb_director == "Adam Elliot" ~
                       "1209767-mary_and_max",
                     title == "Room" & imdb_director == "Lenny Abrahamson" ~
                       "room_2015",
                     title == "Ben-Hur" & imdb_director == "William Wyler" ~
                       "benhur",
                     title == "Stand by Me" & imdb_director == "Rob Reiner" ~
                       "stand_by_me_1986",
                     title == "The Bandit" & imdb_director == "Yavuz Turgul" ~
                       "the_bandit_2016",
                     title == "Spotlight" & imdb_director == "Tom McCarthy" ~
                       "spotlight_2015",
                     title == "A Silent Voice: The Movie" & imdb_director == "Naoko Yamada" ~
                       "a_silent_voice",
                     title == "In the Mood for Love" & imdb_director == "Kar-Wai Wong" ~
                       "in_the_mood_for_love_2001",
                     title == "Love's a Bitch" & imdb_director == "Alejandro G. Iñárritu" ~
                       "amores_perros",
                     title == "Demon Slayer: Mugen Train" & imdb_director == "Haruo Sotozaki" ~
                       "demon_slayer_kimetsu_no_yaiba_the_movie_mugen_train",
                     title == "Three Colors: Red" & imdb_director == "Krzysztof Kieslowski" ~
                       "1058966-red",
                     title == "Raatchasan" & imdb_director == "Ram Kumar" ~
                       "tumbbad",
                     title == "The Thing" & imdb_director == "John Carpenter" ~
                       "1021244-thing",
                     title == "Rebecca" & imdb_director == "Alfred Hitchcock" ~
                       "1017293-rebecca",
                     title == "Parasite" & imdb_director == "Bong Joon Ho" ~
                       "parasite_2019",
                     title == "Joker" & imdb_director == "Todd Phillips" ~
                       "joker_2019",
                     title == "Coco" & imdb_director == "Lee Unkrich" ~
                       "coco_2017",
                     title == "The Hunt" & imdb_director == "Thomas Vinterberg" ~
                       "the_hunt_2013",
                     title == "Dune" & imdb_director == "Denis Villeneuve" ~
                       "dune_2021",
                     title == "The Father" & imdb_director == "Florian Zeller" ~
                       "the_father_2021",
                     title == "A Separation" & imdb_director == "Asghar Farhadi" ~
                       "a_separation_2011",
                     title == "Casino" & imdb_director == "Martin Scorsese" ~
                       "1067987-casino",
                     title == "The Elephant Man" & imdb_director == "David Lynch" ~
                       "1006527-elephant_man",
                     title == "Inside Out" & imdb_director == "Pete Docter" ~
                       "inside_out_2015",
                     title == "Memories of Murder" & imdb_director == "Bong Joon Ho" ~
                       "memories_of_murder_2003",
                     title == "Stalker" & imdb_director == "Andrei Tarkovsky" ~
                       "1043378-stalker",
                     title == "Prisoners" & imdb_director == "Denis Villeneuve" ~
                       "prisoners_2013",
                     title == "Rush" & imdb_director == "Ron Howard" ~
                       "rush_2013",
                     title == "Rocky" & imdb_director == "John G. Avildsen" ~
                       "1017776-rocky",
                     title == "Hera Pheri" & imdb_director == "Priyadarshan" ~
                       "hera-pheri",
                     title == "The Wizard of Oz" & imdb_director == "Victor Fleming" ~
                       "the_wizard_of_oz_1939",
                     title == "Life of Brian" & imdb_director == "Terry Jones" ~
                       "monty_pythons_life_of_brian",
                     title == "The Handmaiden" & imdb_director == "Park Chan-wook" ~
                       "the_handmaiden",
                     TRUE ~ gsub(x = snakecase::to_any_case(
                       string = gsub(x = title, pattern = "'|\\.", replacement = ""),
                       case = "snake"),
                       pattern = "^the_|^a_", replacement = "")),
         rotten_tomatoes_title = stringi::stri_trans_general(rotten_tomatoes_title, "Latin-ASCII"),
         rotten_tomatoes_url_guess = paste0("https://www.rottentomatoes.com/m/", rotten_tomatoes_title),
         rotten_tomatoes_url = ifelse(
           !is.na(rt_vanity) & rt_vanity != "",
           paste0("https://www.rottentomatoes.com/m/", rt_vanity),
           rotten_tomatoes_url_guess
         ))


html_list_rot <- list()
index <- 1
for (rot_url in rotten_tomatoes_url$rotten_tomatoes_url) {
  html_list_rot[index] <- list(safe_fetch_html(rot_url, label = "Rotten Tomatoes"))
  index <- index + 1
}
# rotten_tomatoes_url %>% dplyr::filter(dplyr::row_number() == index)

# let's go again
# let's grab the imdb html data
# unreliable producers are in rotten tomatoes
# can definitely get more data from imdb afterwards too
html_imdb_list <- list()
index <- 1
for (imdb_url in top_250_tib_clean$site) {
  html_imdb_list[index] <- list(safe_fetch_html(imdb_url, label = "IMDb title"))
  index <- index + 1
}

# I've found it might be easiest to do this in a for loop
# would love to mclapply this sometime for speed purposes

# production companies

# fucking this isn't here anymore
# fucking imdb...


html_imdb_prod_list <- list()
index <- 1
for (imdb_url in top_250_tib_clean$producer_site) {
  html_imdb_prod_list[index] <- list(safe_fetch_html(imdb_url, label = "IMDb company credits"))
  index <- index + 1
}

html_imdb_fullcredits_list <- list()
index <- 1
for (imdb_url in top_250_tib_clean$fullcredits_site) {
  html_imdb_fullcredits_list[index] <- list(safe_fetch_html(imdb_url, label = "IMDb full credits"))
  index <- index + 1
}

imdb_producers <- function(html_object) {
  if (is.null(html_object)) {
    return(NA_character_)
  }
  next_data <- parse_imdb_next_data(html_object)
  if (is.null(next_data)) {
    return(NA_character_)
  }
  categories <- next_data$props$pageProps$contentData$categories
  if (is.null(categories)) {
    return(NA_character_)
  }
  names_vec <- vapply(categories, function(x) x$name %||% NA_character_, character(1))
  idx <- which(names_vec == "Producers")
  if (length(idx) == 0) {
    return(NA_character_)
  }
  items <- categories[[idx[1]]]$section$items
  extract_credit_names(items)
}

# production_companies_imdb_fun = function(html_object) {
#   html_object %>%
#     rvest::html_nodes("#__next > main > div") %>% 
#     rvest::html_nodes(paste0(
#       "section.ipc-page-background.ipc-page-background--base.",
#       "TitlePage__StyledPageBackground-wzlr49-0.dDUGgO"
#     )) %>% 
#     rvest::html_nodes("div > section > div") %>% 
#     rvest::html_nodes(paste0(
#       "div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.",
#       "btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2"
#     )) %>% 
#     # rvest::html_nodes(paste0(
#     #   "section:nth-child(44) > div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ",
#     #   "ul > li:nth-child(7)"
#     # ))
#     rvest::html_nodes("section") %>%
#     # rvest::html_attr("data-testid")
#     rvest::html_nodes(xpath = "//*[@data-testid='Details']") %>%
#     rvest::html_nodes("div") %>% 
#     rvest::html_nodes(xpath = "//*[@data-testid='title-details-companies']") %>%
#     rvest::html_nodes("div > ul > li > a") %>% 
#     rvest::html_text() %>% 
#     paste(collapse = "|")
# }



audience_score_fun <- function(html_object){
  if (is.null(html_object)) {
    return(NA_character_)
  }
  score <- html_object %>%
    rvest::html_nodes("#topSection > div.thumbnail-scoreboard-wrap > score-board") %>%
    rvest::html_attr("audiencescore")
  if (length(score) < 1) {
    return(NA_character_)
  }
  score[1]
}

tomato_meter_score_fun <- function(html_object){
  if (is.null(html_object)) {
    return(NA_character_)
  }
  score <- html_object %>%
    rvest::html_nodes("#topSection > div.thumbnail-scoreboard-wrap > score-board") %>%
    rvest::html_attr("tomatometerscore")
  if (length(score) < 1) {
    return(NA_character_)
  }
  score[1]
}

directors_rot <- function(html_object){
  if (is.null(html_object)) {
    return(NA_character_)
  }
  director <- html_object %>%
    rvest::html_nodes("section.panel.panel-rt.panel-box.movie_info.media > div > div > ul >li") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::as_tibble() %>%
    tidyr::separate(col = value, sep = ": ", into = c("title", "name")) %>%
    dplyr::filter(title == "Director") %>%
      dplyr::pull(name)

  if (length(director) < 1) {
    director <- NA_character_
  }
  return(as.character(director))
}

movie_info_rot <- function(html_object){
  if (is.null(html_object)) {
    return(tibble::tibble(category = NA_character_, name = NA_character_))
  }
  movie_info <- html_object %>%
    rvest::html_nodes("section.panel.panel-rt.panel-box.movie_info.media > div > div > ul >li") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    tibble::as_tibble() %>%
    tidyr::separate(col = value, sep = ": ", into = c("category", "name"))

  return(movie_info)

}

# run html node functions

audience_scores_html <- vapply(
  html_list_rot,
  audience_score_fun,
  character(1)
)

tomato_meter_scores_html <- vapply(
  html_list_rot,
  tomato_meter_score_fun,
  character(1)
)

director_rot_tom_html <- vapply(
  html_list_rot,
  directors_rot,
  character(1)
)

audience_scores <- ifelse(is.na(audience_scores_html), rt_audience_scores, audience_scores_html)
tomato_meter_scores <- ifelse(is.na(tomato_meter_scores_html), rt_critics_scores, tomato_meter_scores_html)
director_rot_tom <- ifelse(is.na(director_rot_tom_html), rt_directors, director_rot_tom_html)

movie_info <- html_list_rot %>%
  lapply(movie_info_rot)
names(movie_info) = top_250_tib_clean$title

movie_info_tib <- movie_info %>%
  dplyr::bind_rows(.id = "title")

imdb_production_co <- vapply(
  html_imdb_prod_list,
  get_company_names,
  character(1),
  section_id = "production"
)

imdb_distributor <- vapply(
  html_imdb_prod_list,
  get_company_names,
  character(1),
  section_id = "distribution"
)

producers_imdb <- vapply(
  html_imdb_fullcredits_list,
  imdb_producers,
  character(1)
)

producers_imdb <- producers_imdb %>%
  stringr::str_remove_all("\\s*\\([^\\)]+\\)")

producers_imdb <- tibble::tibble(id = top_250_tib_clean$id, producers_imdb)

top_250_imdb_rot_tom <- top_250_tib_clean %>%
  dplyr::mutate(audience_scores = as.numeric(audience_scores),
         tomato_meter_scores = as.numeric(tomato_meter_scores),
         director_rot_tom = director_rot_tom,
         director = ifelse(is.na(director), rt_directors, director),
         distributor = ifelse(is.na(imdb_distributor), rt_studios, imdb_distributor),
         production_co = ifelse(is.na(imdb_production_co), rt_studios, imdb_production_co),
         produced_by = ifelse(is.na(distributor), production_co, distributor)) %>%
  dplyr::left_join(producers_imdb) %>%
  dplyr::mutate(producers_imdb = ifelse(is.na(producers_imdb), rt_producers, producers_imdb))

top_250_imdb_rot_tom <- top_250_imdb_rot_tom %>%
  dplyr::select(-fullcredits_site)

top_250_imdb_rot_tom <- top_250_imdb_rot_tom %>%
  dplyr::mutate(
    production_company = dplyr::coalesce(production_co, distributor)
  )


write_output <- TRUE
send_email_notification <- TRUE
if (write_output) {
  readr::write_csv(top_250_imdb_rot_tom, here::here("data", "top250_movies_with_rt.csv"))
}

if (send_email_notification) {
  source(here::here("scripts", "gmail_notify.R"))
  send_gmail_notification(
    to = "cavandonohoe@gmail.com",
    subject = "Top 250 with RT: refresh complete",
    body = "Finished generating data/top250_movies_with_rt.csv."
  )
}
