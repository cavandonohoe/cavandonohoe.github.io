


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(rvest)

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
