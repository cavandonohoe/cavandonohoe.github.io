


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


# now include user rating ---------------------------------------------------------------------


rankings_tib_final2 = rankings_tib_final %>% mutate(imdb_link = paste0("https://www.imdb.com/title/", id))


get_rating = function(link){
  link %>% read_html() %>% 
    # old version
    # html_nodes(xpath = "//*[@id='__next']/main/div/section[1]/section/div[3]/section/section/div[1]/div[2]/div/div[1]/a/div/div/div[2]/div[1]/span[1]") %>%
    # new version
    html_nodes("#__next > main > div > section.ipc-page-background.ipc-page-background--base.sc-ca85a21c-0.efoFqn > section > div:nth-child(4) > section > section > div.sc-80d4314-0.fjPRnj > div.sc-db8c1937-0.eGmDjE.sc-80d4314-3.iBtAhY > div > div:nth-child(1) > a") %>% 
    html_nodes("div > div") %>% 
    html_nodes("div.sc-7ab21ed2-0.fAePGh > div.sc-7ab21ed2-2.kYEdvH > span.sc-7ab21ed2-1.jGRxWM") %>%
    html_text()
}


# this one takes a while
# look through 1000 websites and find each rating
# luckily imdb has unique id's for each movie unlike a different movie rating website
# *cough* rotten tomatoes *cough*
all_user_ratings = rankings_tib_final2$imdb_link %>% 
  parallel::mclapply(get_rating, mc.cores = parallel::detectCores() - 1)



imdb_rank_ratings =
  rankings_tib_final2 %>%
  mutate(imdb_rating = as.numeric(unlist(all_user_ratings)))


imdb_rank_ratings_clean = imdb_rank_ratings %>%
  mutate(`Lifetime Gross` = as.numeric(gsub('\\$|,', '', `Lifetime Gross`)),
         imdb_rating = as.numeric(imdb_rating)) %>% 
  rename(box_office_rank = Rank)

# imdb_rank_ratings_clean %>% write.csv("data/top1000_box_office.csv")


