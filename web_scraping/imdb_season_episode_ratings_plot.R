


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(rvest)
library(tidyverse)

# Function to extract episode details from each season
extract_episodes <- function(season_url) {
  season_page <- read_html(season_url)
  
  episodes <- season_page %>%
    html_nodes("article") %>%
    lapply(function(x) {
      episode_number_raw <- html_node(x, "div.sc-ccd6e31b-4.hddQZU > div.sc-ccd6e31b-5.eEIQUx > h4 > div > a > div") %>% html_text(trim = TRUE)
      episode_number <- str_extract(episode_number_raw, "(?<=E)\\d+") %>% as.numeric()
      rating_raw <- html_node(x, "div.sc-ccd6e31b-4.hddQZU > div.sc-ccd6e31b-12.ldTSvW > div > span") %>% html_text(trim = TRUE)
      rating <- str_extract(rating_raw, "^[0-9.]+") %>% as.numeric()
      
      tibble(
        episode_name_number = episode_number_raw,
        episode = episode_number,
        imdb_rating = rating
      )
    }) %>%
    bind_rows()
  
  season_number <- str_extract(season_url, "\\d+$")
  episodes <- episodes %>%
    mutate(season = as.numeric(season_number),
           season_ep_name = gsub(" ∙ ", ": ", episode_name_number))
  
  return(episodes)
}

# Helper function to create the data frame for episode data
create_episode_data <- function(season_urls) {
  episode_data <- lapply(season_urls, extract_episodes) %>%
    bind_rows() %>%
    filter(episode != 0) %>%
    filter(!is.na(imdb_rating))
  
  return(episode_data)
}


# Function to scrape IMDb series episode data
scrape_imdb_episode_data <- function(url) {
  url <- url %>% stringr::str_remove_all("/$") %>% file.path("episodes")
  webpage <- read_html(url)
  
  seasons <- webpage %>%
    html_node("main > div > section > div > section > div > div.sc-4b498b6e-1.jDpRYv.ipc-page-grid__item.ipc-page-grid__item--span-2 > section:nth-child(2) > section.sc-6d19272a-0.bUgwdr > div.ipc-tabs.ipc-tabs--base.ipc-tabs--align-left.ipc-tabs--display-chip.ipc-tabs--inherit > ul") %>%
    html_nodes("a") %>%
    html_text()
  
  season_urls <- webpage %>%
    html_node("#__next") %>%
    html_node("main > div > section > div > section > div > div.sc-4b498b6e-1.jDpRYv.ipc-page-grid__item.ipc-page-grid__item--span-2 > section:nth-child(2) > section.sc-6d19272a-0.bUgwdr > div.ipc-tabs.ipc-tabs--base.ipc-tabs--align-left.ipc-tabs--display-chip.ipc-tabs--inherit > ul") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    paste0("https://www.imdb.com", .)
  
  # Create the episode data frame
  episode_data <- create_episode_data(season_urls)
  
  return(episode_data)
}



# game of thrones
game_of_thrones_ratings = scrape_imdb_episode_data("https://www.imdb.com/title/tt0944947/")
game_of_thrones_ratings %>% write_csv("data/game_of_thrones_ep_ratings.csv")
# breaking bad
breaking_bad_rating = scrape_imdb_episode_data("https://www.imdb.com/title/tt0903747/")
breaking_bad_rating %>% write_csv("data/breaking_bad_ep_ratings.csv")
# always sunny
always_sunny = scrape_imdb_episode_data("https://www.imdb.com/title/tt0472954/")
always_sunny %>% write_csv("data/always_sunny_ep_ratings.csv")
# himym
himym = scrape_imdb_episode_data("https://www.imdb.com/title/tt0460649/")
himym %>% write_csv("data/himym_ep_ratings.csv")
# better call saul
better_call_saul = scrape_imdb_episode_data("https://www.imdb.com/title/tt3032476/")
better_call_saul %>% write_csv("data/better_call_saul_ep_ratings.csv")
# zerozerozero
zerozerozero = scrape_imdb_episode_data("https://www.imdb.com/title/tt8332438/")
zerozerozero %>% write_csv("data/zerozerozero_ep_ratings.csv")
# hotd
hotd = scrape_imdb_episode_data("https://www.imdb.com/title/tt11198330/")
hotd %>% write_csv("data/hotd_ep_ratings.csv")
# bluey
bluey = scrape_imdb_episode_data("https://www.imdb.com/title/tt7678620/")
bluey %>% write_csv("data/bluey_ep_ratings.csv")
# westworld
westworld = scrape_imdb_episode_data("https://www.imdb.com/title/tt0475784/")
westworld %>% write_csv("data/westworld_ep_ratings.csv")
# paris hilton bff
paris_hilton_bff = scrape_imdb_episode_data("https://www.imdb.com/title/tt1292967/")
paris_hilton_bff %>% write_csv("data/paris_hilton_bff_ep_ratings.csv")
# stranger things
stranger_things = scrape_imdb_episode_data("https://www.imdb.com/title/tt4574334/")
stranger_things %>% write_csv("data/stranger_things_ep_ratings.csv")
# bojack
bojack = scrape_imdb_episode_data("https://www.imdb.com/title/tt3398228/")
bojack %>% write_csv("data/bojack_ep_ratings.csv")
# velma
velma = scrape_imdb_episode_data("https://www.imdb.com/title/tt14153790/")
velma %>% write_csv("data/velma_ep_ratings.csv")
# suits
suits = scrape_imdb_episode_data("https://www.imdb.com/title/tt1632701/")
suits %>% write_csv("data/suits_ep_ratings.csv")
# vampire diaries
vampire_diaries = scrape_imdb_episode_data("https://www.imdb.com/title/tt1405406/")
vampire_diaries %>% write_csv("data/vampire_diaries_ep_ratings.csv")
# greys anatomy
greys_anatomy = scrape_imdb_episode_data("https://www.imdb.com/title/tt0413573/")
greys_anatomy %>% write_csv("data/greys_anatomy_ep_ratings.csv")
