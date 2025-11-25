

# get our helper function
source("get_all_episodes.R")

# import the data from imdb and export to csv

url = "https://www.imdb.com/chart/toptv/"

top_series_html = url %>% read_html()

# grab the raw table from imdb top tv
top_series_tbl = top_series_html %>%
  html_nodes("#main > div > span > div > div > div.lister > table") %>% 
  html_table()

# grab the long site
top_series_id_site = top_series_html %>%
  html_nodes("#main > div > span > div > div > div.lister > table > tbody") %>%
  html_nodes("td.titleColumn > a") %>% 
  html_attr("href")

# get only the imdb unique ids
top_series_id = top_series_id_site %>% gsub(pattern = "/title/", replacement = "") %>%
  gsub(pattern = "/.*", replacement = "")


# clean up the table
top_series_tib = top_series_tbl[[1]] %>% as_tibble(.name_repair = "universal") %>%
  mutate(id = top_series_id,
         rank = row_number(),
         # extract text between last set of parentheses
         year = gsub(x = Rank...Title, pattern = ".*\\((.*)\\).*", replacement = "\\1") %>%
           as.numeric(),
         # extract text from first and last space
         title = gsub(x = Rank...Title, pattern = "^\\S+\\s+|\\s+[^ ]+$", replacement = ""),
         site = paste0("https://www.imdb.com/title/", id)) %>% 
  select(rank, title, year, imdb_rating = IMDb.Rating, id, site)

# before running this parallel::mclapply, make sure the
# get_all_episodes.R script is up to date
# so you're going to have to look at the entire for loop to check where
# this script may fail
# remember one piece? or random series that just enjoy messing with you?
# for (imdb_id in top_series_tib$id) {
#   get_all_episodes(imdb_id)
# }
# top_series_tib %>% filter(id == imdb_id)

# this runs a little faster than a normal lapply, but an lapply works too
# took less than 9 mins on my mac
all_eps_ratings = parallel::mclapply(top_series_tib$id, get_all_episodes,
                                     mc.cores = parallel::detectCores() - 1)


all_eps_ratings_tib = all_eps_ratings %>% 
  bind_rows() %>% 
  mutate(user_rating = as.numeric(user_rating)) %>%
  left_join(top_series_tib %>% select(-title), by = c("imdb_id" = "id")) %>% 
  rename(episode_rating = user_rating,
         episode_votes = user_votes,
         series_rating = imdb_rating,
         series_url = site,
         episode_url = url)

# uncomment to override csv
# all_eps_ratings_tib %>% write.csv(here::here("data", "top250_imdb_series.csv"))



