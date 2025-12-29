`%>%` <- magrittr::`%>%`



# best picture winners ------------------------------------------------------------------------

url = "https://www.imdb.com/search/title/?groups=best_picture_winner&sort=year,desc&count=100&view=simple"

best_pictures = url %>% rvest::read_html()

# grab the movies
oscar_winners = best_pictures %>% 
  rvest::html_nodes("#main > div > div.lister.list.detail.sub-list > div") %>%
  rvest::html_nodes("div.lister-item-content > div > div.col-title > span") %>% 
  rvest::html_text() %>%
  gsub(pattern ="\n", replacement = "") %>%
  stringr::str_squish()

# grab the ratings
oscar_imdb_ratings = best_pictures %>% 
  rvest::html_nodes("#main > div > div.lister.list.detail.sub-list > div") %>%
  rvest::html_nodes("div.lister-item-content > div > div.col-imdb-rating > strong") %>% 
  rvest::html_text() %>% 
  gsub(pattern ="\n", replacement = "") %>%
  stringr::str_squish()

# grab the unique imdb id (important for looking at the imdb page for these movies for more data)
oscar_imdb_id = best_pictures %>% 
  rvest::html_nodes("#main > div > div.lister.list.detail.sub-list > div") %>% 
  rvest::html_nodes("div.lister-item-content > div > div.col-title > span") %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href")

# make a table from all the vectors
oscar_winners_tib = tibble::tibble(oscar_winners, oscar_imdb_ratings, oscar_imdb_id) %>% 
  dplyr::mutate(rank = gsub(x = oscar_winners, pattern = "\\s.*", replacement = ""),
         # extract text between first and last space
         oscar_titles = gsub(x = oscar_winners, pattern = "^\\S+\\s+|\\s+[^ ]+$", replacement = ""),
         oscar_imdb_id = paste0("https://www.imdb.com", oscar_imdb_id))


# create a function for grabbing box office data from each imdb page
box_office_from_imdb_page = function(url) {
  gross_worldwide = url %>% rvest::read_html() %>% 
    rvest::html_nodes(paste("#__next > main > div", 
                     "section.ipc-page-background.ipc-page-background--base.TitlePage__StyledPageBackground-wzlr49-0.dDUGgO",
                     "div > section > div",
                     "div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2",
                     sep = " > ")) %>%
    rvest::html_nodes("div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ul") %>% 
    rvest::html_text() %>% 
    tibble::as_tibble() %>% dplyr::filter(grepl(x=value, pattern = "Gross worldwide")) %>% 
    dplyr::mutate(value = gsub(x=value, pattern = ".*Gross worldwide", replacement = "")) %>% dplyr::pull
  
  if (length(gross_worldwide) == 0) {
    gross_worldwide = NA
  }
  return(gross_worldwide)
}

# grab box office data for each best picture winner
oscar_box_office = oscar_winners_tib$oscar_imdb_id %>% 
  parallel::mclapply(box_office_from_imdb_page, mc.cores = parallel::detectCores() - 1) %>%
  unlist() %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(life_time_gross = value)

# clean it up
oscar_winners_clean = oscar_winners_tib %>%
  dplyr::bind_cols(oscar_box_office) %>% 
  # doesn't actually look like Sunrise won the oscar
  dplyr::filter(oscar_titles != "Sunrise") %>%
  dplyr::mutate(movie_year = as.numeric(gsub(x = oscar_winners, pattern = ".*\\((.*)\\).*",
                                      replacement = "\\1")),
         # remove 202
         oscar_year = 2022 - dplyr::row_number()) %>%
  dplyr::mutate(oscar_year = dplyr::case_when(oscar_titles == "The Hurt Locker" ~ 2010,
                                oscar_titles == "Slumdog Millionaire" ~ 2009,
                                oscar_titles == "Crash (I)" ~ 2006,
                                oscar_titles == "Million Dollar Baby" ~ 2005,
                                oscar_titles == "Mrs. Miniver" ~ 1943,
                                oscar_titles == "Casablanca" ~ 1944,
                                TRUE ~ oscar_year)) %>% 
  dplyr::mutate(tconst = gsub(x=oscar_imdb_id, pattern = "https://www.imdb.com/title/|/\\?ref_=adv_li_tt",
                       replacement = ""))

# oscar_winners_clean %>% write.csv(here::here("data", "best_picture_winners.csv"))

# oscar_winners_clean %>% ggplot2::ggplot(ggplot2::aes(x=oscar_year, y = oscar_imdb_ratings)) +
#   ggplot2::geom_point()


