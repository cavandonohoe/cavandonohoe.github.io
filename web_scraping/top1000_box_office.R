


# Top 1000 Box Office -------------------------------------------------------------------------

# box office mojo from imdb has a table listed on this website:
# https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW
# and continues
# https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW&offset=200
# and continues till 800
# let's get web scraping

url = "https://www.boxofficemojo.com/chart/top_lifetime_gross/?area=XWW"

links = read_html(url) %>% 
  html_nodes("a") %>% 
  html_attr("href")

tibble(links) %>% filter(grepl(x=links, pattern="cso") &
                           grepl(x=links, pattern = "title")) %>%
  mutate(id = gsub(x=links, pattern = "/title/|/\\?ref.*", replacement = ""))

offset = seq(from = 200, to = 800, by = 200)

all_urls =c(url, paste0(url, "&offset=", offset))

rankings_tib = tibble()
for (url_index in all_urls) {
  rankings = read_html(url_index) %>% 
    html_node("table") %>% 
    html_table(fill=TRUE) %>% 
    as_tibble() %>% 
    mutate(Rank = as.numeric(Rank))
  
  # funny enough, before doing this, I just read in a list of imdb movies titles 
  # and got the imdb id that way. but apparently multiple versions of beauty and the beast
  # were released in 2017, so that plan kinda backfired
  links = read_html(url_index) %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  link_tib = tibble(links) %>% filter(grepl(x=links, pattern="cso") &
                                        grepl(x=links, pattern = "title")) %>%
    mutate(id = gsub(x=links, pattern = "/title/|/\\?ref.*", replacement = ""))
  
  
  rankings_id = rankings %>% bind_cols(link_tib)
  
  rankings_tib = rankings_tib %>%
    bind_rows(rankings_id)
}

rankings_tib_final = rankings_tib %>%
  mutate(Rank = ifelse(is.na(Rank) & row_number() == 1000, 1000, Rank))

# imdb_title_basics = read_tsv("https://datasets.imdbws.com/title.basics.tsv.gz")
# 
# imdb_movies = imdb_title_basics %>% filter(titleType == "movie")
# 
# rankings_with_id = rankings_tib_final %>%
#   left_join(imdb_movies %>%
#               select(Title = primaryTitle, Year = startYear, tconst))
# 
# # tt2771200 is the right one for beauty and the beast
# imdb_movies %>%
#   filter(grepl(x=primaryTitle, pattern = "beauty and the beast", ignore.case = TRUE)) %>%
#   filter(startYear == 2017)
# 
# # lion king 2019 isnt listed
# # should be tt6105098
# imdb_movies %>%
#   filter(grepl(x=primaryTitle, pattern = "lion king", ignore.case = TRUE))
# 
# # note: look up the director of these movies cuz some of them are kinda bs
# 
# rankings_with_id %>% group_by(Title, Year) %>% filter(n() >1) %>% ungroup() %>% arrange(Title)


rankings_tib_final



# now include user rating ---------------------------------------------------------------------


rankings_tib_final2 = rankings_tib_final %>% mutate(imdb_link = paste0("https://www.imdb.com/title/", id))

# grab_user_rating = function(html_object) {
#   html_object %>% read_html() %>%
#     html_nodes(xpath = "//*[@id='__next']/main/div/section[1]/section/div[3]/section/section/div[1]/div[2]/div/div[1]/a/div/div/div[2]/div[1]/span[1]") %>%
#     html_text()
# }

get_rating = function(link){
  link %>% read_html() %>% 
    html_nodes(xpath = "//*[@id='__next']/main/div/section[1]/section/div[3]/section/section/div[1]/div[2]/div/div[1]/a/div/div/div[2]/div[1]/span[1]") %>%
    html_text()
}


all_user_ratings = rankings_tib_final2$imdb_link %>% 
  parallel::mclapply(get_rating, mc.cores = parallel::detectCores() - 1)



rankings_tib_final2 %>% mutate(imdb_rating = as.numeric(unlist(all_user_ratings)))




imdb_rank_ratings =
  # rankings_tib_final2 %>% 
  # # slice(1:5) %>% 
  # rowwise() %>%
  # mutate(rating = get_rating(imdb_link)) %>% 
  # ungroup()
  rankings_tib_final2 %>%
  mutate(imdb_rating = as.numeric(unlist(all_user_ratings)))

imdb_rank_ratings

imdb_rank_ratings_clean = imdb_rank_ratings %>%
  mutate(`Lifetime Gross` = as.numeric(gsub('\\$|,', '', `Lifetime Gross`)),
         imdb_rating = as.numeric(imdb_rating)) %>% 
  rename(box_office_rank = Rank)

# imdb_rank_ratings_clean %>% write.csv("data/top1000_box_office.csv")


