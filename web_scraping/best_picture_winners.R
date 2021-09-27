


# best picture winners ------------------------------------------------------------------------

url = "https://www.imdb.com/search/title/?groups=best_picture_winner&sort=year,desc&count=100&view=simple"

best_pictures = url %>% read_html()

oscar_winners = best_pictures %>% 
  html_nodes("#main > div > div.lister.list.detail.sub-list > div") %>%
  html_nodes("div.lister-item-content > div > div.col-title > span") %>% 
  html_text() %>%
  gsub(pattern ="\n", replacement = "") %>%
  str_squish()


oscar_imdb_ratings = best_pictures %>% 
  html_nodes("#main > div > div.lister.list.detail.sub-list > div") %>%
  html_nodes("div.lister-item-content > div > div.col-imdb-rating > strong") %>% 
  html_text() %>% 
  gsub(pattern ="\n", replacement = "") %>%
  str_squish()

oscar_imdb_id = best_pictures %>% 
  html_nodes("#main > div > div.lister.list.detail.sub-list > div") %>% 
  html_nodes("div.lister-item-content > div > div.col-title > span") %>% 
  html_nodes("a") %>% 
  html_attr("href")


oscar_winners = tibble(oscar_winners, oscar_imdb_ratings, oscar_imdb_id) %>% 
  mutate(rank = gsub(x = oscar_winners, pattern = "\\s.*", replacement = ""),
         # extract text between first and last space
         oscar_titles = gsub(x = oscar_winners, pattern = "^\\S+\\s+|\\s+[^ ]+$", replacement = ""),
         oscar_imdb_id = paste0("https://www.imdb.com", oscar_imdb_id))

"#__next > main > div > section.ipc-page-background.ipc-page-background--base.TitlePage__StyledPageBackground-wzlr49-0.dDUGgO > div > section > div > div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2 > section:nth-child(48) > div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ul > li:nth-child(3)"

check = oscar_winners$oscar_imdb_id[1] %>% 
  read_html() %>% 
  html_nodes(paste("#__next > main > div", 
                   "section.ipc-page-background.ipc-page-background--base.TitlePage__StyledPageBackground-wzlr49-0.dDUGgO",
                   "div > section > div",
                   "div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2",
                   sep = " > ")) %>%
  html_nodes("div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ul") %>% 
  # html_nodes("div > ul > li > span") %>%
  html_text()
  
check %>% as_tibble() %>% filter(grepl(x=value, pattern = "Gross worldwide")) %>% 
  mutate(value = gsub(x=value, pattern = ".*Gross worldwide", replacement = "")) %>% pull
  
box_office_from_imdb_page = function(url) {
  gross_worldwide = url %>% read_html() %>% 
    html_nodes(paste("#__next > main > div", 
                     "section.ipc-page-background.ipc-page-background--base.TitlePage__StyledPageBackground-wzlr49-0.dDUGgO",
                     "div > section > div",
                     "div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2",
                     sep = " > ")) %>%
    html_nodes("div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ul") %>% 
    html_text() %>% 
    as_tibble() %>% filter(grepl(x=value, pattern = "Gross worldwide")) %>% 
    mutate(value = gsub(x=value, pattern = ".*Gross worldwide", replacement = "")) %>% pull
  
  if (length(gross_worldwide) == 0) {
    gross_worldwide = NA
  }
  return(gross_worldwide)
}

oscar_box_office = oscar_winners$oscar_imdb_id %>% 
  parallel::mclapply(box_office_from_imdb_page, mc.cores = parallel::detectCores() - 1) %>%
  unlist() %>% 
  as_tibble() %>% 
  rename(life_time_gross = value)

oscar_winners

oscar_box_office

oscar_winners_clean = oscar_winners %>%
  bind_cols(oscar_box_office) %>% 
  # doesn't actually look like Sunrise won the oscar
  filter(oscar_titles != "Sunrise") %>%
  mutate(movie_year = as.numeric(gsub(x = oscar_winners, pattern = ".*\\((.*)\\).*",
                                      replacement = "\\1")),
         # remove 202
         oscar_year = 2022 - row_number()) %>%
  mutate(oscar_year = case_when(oscar_titles == "The Hurt Locker" ~ 2010,
                                oscar_titles == "Slumdog Millionaire" ~ 2009,
                                oscar_titles == "Crash (I)" ~ 2006,
                                oscar_titles == "Million Dollar Baby" ~ 2005,
                                oscar_titles == "Mrs. Miniver" ~ 1943,
                                oscar_titles == "Casablanca" ~ 1944,
                                TRUE ~ oscar_year)) %>% 
  mutate(tconst = gsub(x=oscar_imdb_id, pattern = "https://www.imdb.com/title/|/\\?ref_=adv_li_tt",
                       replacement = ""))

# oscar_winners_clean %>% write.csv("data/best_picture_winners.csv")

# oscar_winners_clean %>% ggplot(aes(x=oscar_year, y = oscar_imdb_ratings)) +
#   geom_point()


