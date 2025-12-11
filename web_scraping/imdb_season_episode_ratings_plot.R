


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(rvest)
library(tidyverse)

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
  
  # if this season doesn't exist or has no episodes
  if (is.null(episodes_items) || length(episodes_items) == 0) {
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
  
  purrr::map_dfr(episodes_items, function(item) {
    tibble::tibble(
      season  = as.integer(item$season),
      episode = as.integer(item$episode),
      title   = item$titleText,
      rating  = item$aggregateRating,
      votes   = item$voteCount
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
game_of_thrones_ratings = get_imdb_all_episodes("https://www.imdb.com/title/tt0944947/")
game_of_thrones_ratings %>% write_csv(here::here("data", "game_of_thrones_ep_ratings.csv"))
# breaking bad
breaking_bad_rating = get_imdb_all_episodes("https://www.imdb.com/title/tt0903747/")
breaking_bad_rating %>% write_csv(here::here("data", "breaking_bad_ep_ratings.csv"))
# always sunny
always_sunny = get_imdb_all_episodes("https://www.imdb.com/title/tt0472954/")
always_sunny %>% write_csv(here::here("data", "always_sunny_ep_ratings.csv"))
# himym
himym = get_imdb_all_episodes("https://www.imdb.com/title/tt0460649/")
himym %>% write_csv(here::here("data", "himym_ep_ratings.csv"))
# better call saul
better_call_saul = get_imdb_all_episodes("https://www.imdb.com/title/tt3032476/")
better_call_saul %>% write_csv(here::here("data", "better_call_saul_ep_ratings.csv"))
# zerozerozero
zerozerozero = get_imdb_all_episodes("https://www.imdb.com/title/tt8332438/")
zerozerozero %>% write_csv(here::here("data", "zerozerozero_ep_ratings.csv"))
# hotd
hotd = get_imdb_all_episodes("https://www.imdb.com/title/tt11198330/")
hotd %>% write_csv(here::here("data", "hotd_ep_ratings.csv"))
# bluey
bluey = get_imdb_all_episodes("https://www.imdb.com/title/tt7678620/")
bluey %>% write_csv(here::here("data", "bluey_ep_ratings.csv"))
# westworld
westworld = get_imdb_all_episodes("https://www.imdb.com/title/tt0475784/")
westworld %>% write_csv(here::here("data", "westworld_ep_ratings.csv"))
# paris hilton bff
paris_hilton_bff = get_imdb_all_episodes("https://www.imdb.com/title/tt1292967/")
paris_hilton_bff %>% write_csv(here::here("data", "paris_hilton_bff_ep_ratings.csv"))
# stranger things
stranger_things = get_imdb_all_episodes("https://www.imdb.com/title/tt4574334/")
stranger_things %>% write_csv(here::here("data", "stranger_things_ep_ratings.csv"))
# bojack
bojack = get_imdb_all_episodes("https://www.imdb.com/title/tt3398228/")
bojack %>% write_csv(here::here("data", "bojack_ep_ratings.csv"))
# velma
velma = get_imdb_all_episodes("https://www.imdb.com/title/tt14153790/")
velma %>% write_csv(here::here("data", "velma_ep_ratings.csv"))
# suits
suits = get_imdb_all_episodes("https://www.imdb.com/title/tt1632701/")
suits %>% write_csv(here::here("data", "suits_ep_ratings.csv"))
# vampire diaries
vampire_diaries = get_imdb_all_episodes("https://www.imdb.com/title/tt1405406/")
vampire_diaries %>% write_csv(here::here("data", "vampire_diaries_ep_ratings.csv"))
# greys anatomy
greys_anatomy = get_imdb_all_episodes("https://www.imdb.com/title/tt0413573/")
greys_anatomy %>% write_csv(here::here("data", "greys_anatomy_ep_ratings.csv"))
# friends
friends = get_imdb_all_episodes("https://www.imdb.com/title/tt0108778/")
friends %>% write_csv(here::here("data", "friends_ep_ratings.csv"))
# house
house = get_imdb_all_episodes("https://www.imdb.com/title/tt0412142/")
house %>% write_csv(here::here("data", "house_ep_ratings.csv"))
# simpsons
simpsons = get_imdb_all_episodes("https://www.imdb.com/title/tt0096697/")
simpsons %>% write_csv(here::here("data", "simpsons_ep_ratings.csv"))
# pretty little liars
pretty_little_liars = get_imdb_all_episodes("https://www.imdb.com/title/tt1578873/")
pretty_little_liars %>% write_csv(here::here("data", "pretty_little_liars_ep_ratings.csv"))
# the crown
crown = get_imdb_all_episodes("https://www.imdb.com/title/tt4786824/")
crown %>% write_csv(here::here("data", "crown_ep_ratings.csv"))
# love island (uk)
love_island = get_imdb_all_episodes("https://www.imdb.com/title/tt4770018/")
love_island %>% write_csv(here::here("data", "love_island_ep_ratings.csv"))
# south park
south_park = get_imdb_all_episodes("https://www.imdb.com/title/tt0121955/")
south_park %>% write_csv(here::here("data", "south_park_ep_ratings.csv"))
# family guy
family_guy = get_imdb_all_episodes("https://www.imdb.com/title/tt0182576/")
family_guy %>% write_csv(here::here("data", "family_guy_ep_ratings.csv"))
# invincible
invincible = get_imdb_all_episodes("https://www.imdb.com/title/tt6741278/")
invincible %>% write_csv(here::here("data", "invincible_ep_ratings.csv"))
# clone wars
clone_wars = get_imdb_all_episodes("https://www.imdb.com/title/tt0458290/")
clone_wars %>% write_csv(here::here("data", "clone_wars_ep_ratings.csv"))
# mr robot
mr_robot = get_imdb_all_episodes("https://www.imdb.com/title/tt4158110/")
mr_robot %>% write_csv(here::here("data", "mr_robot_ep_ratings.csv"))
# sparticus
sparticus = get_imdb_all_episodes("https://www.imdb.com/title/tt1442449/")
sparticus %>% write_csv(here::here("data", "sparticus_ep_ratings.csv"))
# attack on titan
attack_on_titan = get_imdb_all_episodes("https://www.imdb.com/title/tt2560140/")
attack_on_titan %>% write_csv(here::here("data", "attack_on_titan_ep_ratings.csv"))
# mad men
mad_men = get_imdb_all_episodes("https://www.imdb.com/title/tt0804503/")
mad_men %>% write_csv(here::here("data", "mad_men_ep_ratings.csv"))
# blue eye samurai
blue_eye_samurai = get_imdb_all_episodes("https://www.imdb.com/title/tt13309742/")
blue_eye_samurai %>% write_csv(here::here("data", "blue_eye_samurai_ep_ratings.csv"))
# six feet under
six_feet_under = get_imdb_all_episodes("https://www.imdb.com/title/tt0248654/")
six_feet_under %>% write_csv(here::here("data", "six_feet_under_ep_ratings.csv"))
# hannibal
hannibal = get_imdb_all_episodes("https://www.imdb.com/title/tt2243973/")
hannibal %>% write_csv(here::here("data", "hannibal_ep_ratings.csv"))
# the wire
the_wire = get_imdb_all_episodes("https://www.imdb.com/title/tt0306414/")
the_wire %>% write_csv(here::here("data", "the_wire_ep_ratings.csv"))
# person of interest
person_of_interest = get_imdb_all_episodes("https://www.imdb.com/title/tt1839578/")
person_of_interest %>% write_csv(here::here("data", "person_of_interest_ep_ratings.csv"))
# chernobyl
chernobyl = get_imdb_all_episodes("https://www.imdb.com/title/tt7366338/")
chernobyl %>% write_csv(here::here("data", "chernobyl_ep_ratings.csv"))
# fleabag
fleabag = get_imdb_all_episodes("https://www.imdb.com/title/tt5687612/")
fleabag %>% write_csv(here::here("data", "fleabag_ep_ratings.csv"))
# parks and recreation
parks_and_rec = get_imdb_all_episodes("https://www.imdb.com/title/tt1266020/")
parks_and_rec %>% write_csv(here::here("data", "parks_and_rec_ep_ratings.csv"))
# new girl
new_girl = get_imdb_all_episodes("https://www.imdb.com/title/tt1826940/")
new_girl %>% write_csv(here::here("data", "new_girl_ep_ratings.csv"))
# brooklyn nine-nine
brooklyn_nine_nine = get_imdb_all_episodes("https://www.imdb.com/title/tt2467372/")
brooklyn_nine_nine %>% write_csv(here::here("data", "brooklyn_nine_nine_ep_ratings.csv"))
# avatar
avatar = get_imdb_all_episodes("https://www.imdb.com/title/tt0417299/")
avatar %>% write_csv(here::here("data", "avatar_ep_ratings.csv"))
# the office us
the_office = get_imdb_all_episodes("https://www.imdb.com/title/tt0386676/")
the_office %>% write_csv(here::here("data", "the_office_ep_ratings.csv"))
# death note
death_note = get_imdb_all_episodes("https://www.imdb.com/title/tt0877057/")
death_note %>% write_csv(here::here("data", "death_note_ep_ratings.csv"))


