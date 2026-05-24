`%>%` <- magrittr::`%>%`
# Load necessary libraries

extract_imdb_id <- function(input) {
  # If user already gives "tt1234567"
  if (grepl("^tt\\d+$", input)) {
    return(input)
  }

  # If input is a URL, extract ttXXXXXXX
  id <- stringr::str_match(input, "tt\\d+")[, 1]

  if (is.na(id)) {
    stop("Could not extract IMDb ID from input: ", input)
  }

  return(id)
}

# Query IMDb's public GraphQL endpoint for one season of episodes.
# We use this instead of scraping the HTML pages because IMDb's WAF
# blocks server-side requests to www.imdb.com (e.g. from GitHub
# Actions), but the caching.graphql.imdb.com endpoint is unauthenticated
# and unblocked. Pagination handles long seasons (Family Guy, Simpsons,
# etc.).
get_imdb_season_episodes <- function(imdb_input, season) {
  imdb_id <- extract_imdb_id(imdb_input)
  endpoint <- "https://caching.graphql.imdb.com/"
  ua <- paste0(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/124.0.0.0 Safari/537.36"
  )
  query <- paste0(
    "query Eps($id: ID!, $season: String!, $first: Int!, $after: ID) {",
    "  title(id: $id) {",
    "    episodes {",
    "      episodes(",
    "        first: $first,",
    "        after: $after,",
    "        filter: { includeSeasons: [$season] }",
    "      ) {",
    "        edges {",
    "          node {",
    "            series {",
    "              displayableEpisodeNumber {",
    "                episodeNumber { text }",
    "                displayableSeason { text }",
    "              }",
    "            }",
    "            titleText { text }",
    "            ratingsSummary { aggregateRating voteCount }",
    "          }",
    "        }",
    "        pageInfo { endCursor hasNextPage }",
    "      }",
    "    }",
    "  }",
    "}"
  )

  empty <- tibble::tibble(
    season  = integer(),
    episode = integer(),
    title   = character(),
    rating  = numeric(),
    votes   = integer()
  )

  all_edges <- list()
  after <- NULL
  repeat {
    body <- list(
      query = query,
      variables = list(
        id = imdb_id,
        season = as.character(season),
        first = 50L,
        after = after
      )
    )
    resp <- httr2::request(endpoint) %>%
      httr2::req_method("POST") %>%
      httr2::req_headers(
        `User-Agent` = ua,
        `Content-Type` = "application/json"
      ) %>%
      httr2::req_body_raw(
        jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
        type = "application/json"
      ) %>%
      httr2::req_perform()
    parsed <- jsonlite::fromJSON(
      httr2::resp_body_string(resp),
      simplifyVector = FALSE
    )
    eps_block <- parsed$data$title$episodes$episodes
    if (is.null(eps_block) || length(eps_block$edges) == 0) break
    all_edges <- c(all_edges, eps_block$edges)
    if (isTRUE(eps_block$pageInfo$hasNextPage)) {
      after <- eps_block$pageInfo$endCursor
    } else {
      break
    }
  }

  if (length(all_edges) == 0) return(empty)

  purrr::map_dfr(all_edges, function(edge) {
    node <- edge$node
    season_text <- node$series$displayableEpisodeNumber$displayableSeason$text
    episode_text <- node$series$displayableEpisodeNumber$episodeNumber$text
    rating <- node$ratingsSummary$aggregateRating
    votes <- node$ratingsSummary$voteCount
    tibble::tibble(
      season  = suppressWarnings(as.integer(season_text)),
      episode = suppressWarnings(as.integer(episode_text)),
      title   = if (is.null(node$titleText$text)) NA_character_ else node$titleText$text,
      rating  = if (is.null(rating)) NA_real_ else as.numeric(rating),
      votes   = if (is.null(votes)) 0L else as.integer(votes)
    )
  })
}

get_imdb_all_episodes <- function(imdb_id, max_seasons = 50) {
  out <- list()

  for (s in seq_len(max_seasons)) {
    df <- get_imdb_season_episodes(imdb_id, s)

    if (nrow(df) == 0) {
      # we've gone past the last real season
      break
    }

    out[[length(out) + 1]] <- df
  }

  if (length(out) == 0) {
    return(
      tibble::tibble(
        season  = integer(),
        episode = integer(),
        title   = character(),
        rating  = numeric(),
        votes   = integer()
      )
    )
  }

  dplyr::bind_rows(out)
}



# game of thrones
game_of_thrones_ratings <- get_imdb_all_episodes("https://www.imdb.com/title/tt0944947/")
game_of_thrones_ratings %>%
  readr::write_csv(here::here("data", "game_of_thrones_ep_ratings.csv"))
# breaking bad
breaking_bad_rating <- get_imdb_all_episodes("https://www.imdb.com/title/tt0903747/")
breaking_bad_rating %>%
  readr::write_csv(here::here("data", "breaking_bad_ep_ratings.csv"))
# always sunny
always_sunny <- get_imdb_all_episodes("https://www.imdb.com/title/tt0472954/")
always_sunny %>%
  readr::write_csv(here::here("data", "always_sunny_ep_ratings.csv"))
# himym
himym <- get_imdb_all_episodes("https://www.imdb.com/title/tt0460649/")
himym %>%
  readr::write_csv(here::here("data", "himym_ep_ratings.csv"))
# better call saul
better_call_saul <- get_imdb_all_episodes("https://www.imdb.com/title/tt3032476/")
better_call_saul %>%
  readr::write_csv(here::here("data", "better_call_saul_ep_ratings.csv"))
# zerozerozero
zerozerozero <- get_imdb_all_episodes("https://www.imdb.com/title/tt8332438/")
zerozerozero %>%
  readr::write_csv(here::here("data", "zerozerozero_ep_ratings.csv"))
# hotd
hotd <- get_imdb_all_episodes("https://www.imdb.com/title/tt11198330/")
hotd %>%
  readr::write_csv(here::here("data", "hotd_ep_ratings.csv"))
# bluey
bluey <- get_imdb_all_episodes("https://www.imdb.com/title/tt7678620/")
bluey %>%
  readr::write_csv(here::here("data", "bluey_ep_ratings.csv"))
# westworld
westworld <- get_imdb_all_episodes("https://www.imdb.com/title/tt0475784/")
westworld %>%
  readr::write_csv(here::here("data", "westworld_ep_ratings.csv"))
# paris hilton bff
paris_hilton_bff <- get_imdb_all_episodes("https://www.imdb.com/title/tt1292967/")
paris_hilton_bff %>%
  readr::write_csv(here::here("data", "paris_hilton_bff_ep_ratings.csv"))
# stranger things
stranger_things <- get_imdb_all_episodes("https://www.imdb.com/title/tt4574334/")
stranger_things %>%
  readr::write_csv(here::here("data", "stranger_things_ep_ratings.csv"))
# bojack
bojack <- get_imdb_all_episodes("https://www.imdb.com/title/tt3398228/")
bojack %>%
  readr::write_csv(here::here("data", "bojack_ep_ratings.csv"))
# velma
velma <- get_imdb_all_episodes("https://www.imdb.com/title/tt14153790/")
velma %>%
  readr::write_csv(here::here("data", "velma_ep_ratings.csv"))
# suits
suits <- get_imdb_all_episodes("https://www.imdb.com/title/tt1632701/")
suits %>%
  readr::write_csv(here::here("data", "suits_ep_ratings.csv"))
# vampire diaries
vampire_diaries <- get_imdb_all_episodes("https://www.imdb.com/title/tt1405406/")
vampire_diaries %>%
  readr::write_csv(here::here("data", "vampire_diaries_ep_ratings.csv"))
# greys anatomy
greys_anatomy <- get_imdb_all_episodes("https://www.imdb.com/title/tt0413573/")
greys_anatomy %>%
  readr::write_csv(here::here("data", "greys_anatomy_ep_ratings.csv"))
# friends
friends <- get_imdb_all_episodes("https://www.imdb.com/title/tt0108778/")
friends %>%
  readr::write_csv(here::here("data", "friends_ep_ratings.csv"))
# house
house <- get_imdb_all_episodes("https://www.imdb.com/title/tt0412142/")
house %>%
  readr::write_csv(here::here("data", "house_ep_ratings.csv"))
# simpsons
simpsons <- get_imdb_all_episodes("https://www.imdb.com/title/tt0096697/")
simpsons %>%
  readr::write_csv(here::here("data", "simpsons_ep_ratings.csv"))
# pretty little liars
pretty_little_liars <- get_imdb_all_episodes("https://www.imdb.com/title/tt1578873/")
pretty_little_liars %>%
  readr::write_csv(here::here("data", "pretty_little_liars_ep_ratings.csv"))
# the crown
crown <- get_imdb_all_episodes("https://www.imdb.com/title/tt4786824/")
crown %>%
  readr::write_csv(here::here("data", "crown_ep_ratings.csv"))
# love island (uk)
love_island <- get_imdb_all_episodes("https://www.imdb.com/title/tt4770018/")
love_island %>%
  readr::write_csv(here::here("data", "love_island_ep_ratings.csv"))
# south park
south_park <- get_imdb_all_episodes("https://www.imdb.com/title/tt0121955/")
south_park %>%
  readr::write_csv(here::here("data", "south_park_ep_ratings.csv"))
# family guy
family_guy <- get_imdb_all_episodes("https://www.imdb.com/title/tt0182576/")
family_guy %>%
  readr::write_csv(here::here("data", "family_guy_ep_ratings.csv"))
# invincible
invincible <- get_imdb_all_episodes("https://www.imdb.com/title/tt6741278/")
invincible %>%
  readr::write_csv(here::here("data", "invincible_ep_ratings.csv"))
# clone wars
clone_wars <- get_imdb_all_episodes("https://www.imdb.com/title/tt0458290/")
clone_wars %>%
  readr::write_csv(here::here("data", "clone_wars_ep_ratings.csv"))
# mr robot
mr_robot <- get_imdb_all_episodes("https://www.imdb.com/title/tt4158110/")
mr_robot %>%
  readr::write_csv(here::here("data", "mr_robot_ep_ratings.csv"))
# sparticus
sparticus <- get_imdb_all_episodes("https://www.imdb.com/title/tt1442449/")
sparticus %>%
  readr::write_csv(here::here("data", "sparticus_ep_ratings.csv"))
# attack on titan
attack_on_titan <- get_imdb_all_episodes("https://www.imdb.com/title/tt2560140/")
attack_on_titan %>%
  readr::write_csv(here::here("data", "attack_on_titan_ep_ratings.csv"))
# mad men
mad_men <- get_imdb_all_episodes("https://www.imdb.com/title/tt0804503/")
mad_men %>%
  readr::write_csv(here::here("data", "mad_men_ep_ratings.csv"))
# blue eye samurai
blue_eye_samurai <- get_imdb_all_episodes("https://www.imdb.com/title/tt13309742/")
blue_eye_samurai %>%
  readr::write_csv(here::here("data", "blue_eye_samurai_ep_ratings.csv"))
# six feet under
six_feet_under <- get_imdb_all_episodes("https://www.imdb.com/title/tt0248654/")
six_feet_under %>%
  readr::write_csv(here::here("data", "six_feet_under_ep_ratings.csv"))
# hannibal
hannibal <- get_imdb_all_episodes("https://www.imdb.com/title/tt2243973/")
hannibal %>%
  readr::write_csv(here::here("data", "hannibal_ep_ratings.csv"))
# the wire
the_wire <- get_imdb_all_episodes("https://www.imdb.com/title/tt0306414/")
the_wire %>%
  readr::write_csv(here::here("data", "the_wire_ep_ratings.csv"))
# person of interest
person_of_interest <- get_imdb_all_episodes("https://www.imdb.com/title/tt1839578/")
person_of_interest %>%
  readr::write_csv(here::here("data", "person_of_interest_ep_ratings.csv"))
# chernobyl
chernobyl <- get_imdb_all_episodes("https://www.imdb.com/title/tt7366338/")
chernobyl %>%
  readr::write_csv(here::here("data", "chernobyl_ep_ratings.csv"))
# fleabag
fleabag <- get_imdb_all_episodes("https://www.imdb.com/title/tt5687612/")
fleabag %>%
  readr::write_csv(here::here("data", "fleabag_ep_ratings.csv"))
# parks and recreation
parks_and_rec <- get_imdb_all_episodes("https://www.imdb.com/title/tt1266020/")
parks_and_rec %>%
  readr::write_csv(here::here("data", "parks_and_rec_ep_ratings.csv"))
# new girl
new_girl <- get_imdb_all_episodes("https://www.imdb.com/title/tt1826940/")
new_girl %>%
  readr::write_csv(here::here("data", "new_girl_ep_ratings.csv"))
# brooklyn nine-nine
brooklyn_nine_nine <- get_imdb_all_episodes("https://www.imdb.com/title/tt2467372/")
brooklyn_nine_nine %>%
  readr::write_csv(here::here("data", "brooklyn_nine_nine_ep_ratings.csv"))
# avatar
avatar <- get_imdb_all_episodes("https://www.imdb.com/title/tt0417299/")
avatar %>%
  readr::write_csv(here::here("data", "avatar_ep_ratings.csv"))
# the office us
the_office <- get_imdb_all_episodes("https://www.imdb.com/title/tt0386676/")
the_office %>%
  readr::write_csv(here::here("data", "the_office_ep_ratings.csv"))
# death note
death_note <- get_imdb_all_episodes("https://www.imdb.com/title/tt0877057/")
death_note %>%
  readr::write_csv(here::here("data", "death_note_ep_ratings.csv"))
# andor
andor <- get_imdb_all_episodes("https://www.imdb.com/title/tt9253284/")
andor %>%
  readr::write_csv(here::here("data", "andor_ep_ratings.csv"))
# modern love
modern_love <- get_imdb_all_episodes("https://www.imdb.com/title/tt8543390/")
modern_love %>%
  readr::write_csv(here::here("data", "modern_love_ep_ratings.csv"))
# my hero academia
my_hero_academia <- get_imdb_all_episodes("https://www.imdb.com/title/tt5626028/")
my_hero_academia %>%
  readr::write_csv(here::here("data", "my_hero_academia_ep_ratings.csv"))
# the boys
the_boys <- get_imdb_all_episodes("https://www.imdb.com/title/tt1190634/")
the_boys %>%
  readr::write_csv(here::here("data", "the_boys_ep_ratings.csv"))

