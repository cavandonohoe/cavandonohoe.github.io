

## Web Scraping All Oscar Nominees

library(rvest)

url = "https://www.imdb.com/event/ev0000003/2021/1/?ref_=ev_eh"

year_html = read_html(url)


# there is a lot of javascript in these imdb pages, so everything is a little messy until it isn't
years_messy = year_html %>%
  # html_nodes("#sidebar") %>% 
  html_nodes("#sidebar > div") %>% 
  html_nodes("span") %>%
  # html_nodes("href")
  # html_attr("a") %>% 
  html_text(trim = TRUE)

year_vec = strsplit(x = years_messy, split = ",") %>% unlist

year_tib = tibble(year_vec) %>% filter(grepl(x=year_vec, pattern = "year")) %>%
  mutate(year = gsub(x = year_vec, pattern = "\"year\":", replacement = ""),
         year_url = paste0("https://www.imdb.com/event/ev0000003/", year, "/1/?ref_=ev_eh"))


## let's start this for loop

oscar_html_list = list()
i = 1
for (url in year_tib$year_url) {
  oscar_html_list[[i]] = url %>% read_html()
  i= i+1
}

# create a function that grabs the messy jscript with all the nominees of every category
# this won't take that long since we're at less than 100 oscar award years
oscar_jscript_raw = function(html_object) {
  html_object %>% html_nodes("body > div > div > div > div > div > div > div > span") %>%
    html_text() %>% 
    strsplit(split = ",(?![ Production])", perl = TRUE) %>% unlist() %>%
    as_tibble() %>%
    rename(messy_jscript_text = value)
}

oscar_jscript = oscar_html_list %>%
  lapply(oscar_jscript_raw)

names(oscar_jscript) = year_tib$year


# now create a function that looks at just the best picture nominees

best_pic_noms_from_imdb = function(tib) {
  tib2 = tib %>% 
    filter(grepl(x = messy_jscript_text,
                 # we want to keep the nominees, the const (id) and the Best Picture label
                 # the Best Picture label will be a good wrapper for text we are looking for
                 pattern = paste("Best Motion Picture of the Year|Best Picture",
                                 "primaryNominees", "const\":\"tt", sep = "|"))) %>% 
    mutate(text =
             case_when(
               grepl(x = messy_jscript_text,
                     # keep it standard
                     pattern = "Best Motion Picture of the Year|Best Picture") ~ 
                 "Best Motion Picture of the Year",
               TRUE ~ messy_jscript_text),
           text2 = gsub(x = text, pattern = "\"const\":\"|.*name\":\"|}]|\"", replacement = "")) %>% 
    group_by(text2) %>% mutate(row_num = row_number()) %>% 
    ungroup()
  
  # create a nice looking tibble with title; id; and nominee or winner column
  gsub(x = paste(tib2$text2, collapse = "|"),
       pattern = ".*?Best Motion Picture of the Year(.*)Best Motion Picture of the Year.*",
       replacement = "\\1") %>%
    gsub(pattern = "Best Motion Picture of the Year|\\|$|^\\|", replacement = "") %>%
    gsub(pattern = "\\|\\|\\|", replacement = "||") %>% 
    strsplit(split = "\\|\\|") %>% unlist %>% 
    as_tibble() %>% 
    separate(col = value, sep = "\\|", into = c("title", "id")) %>%
    mutate(nominees_or_winner = ifelse(row_number() == 1, "winner", "nominee"))
}

best_pic_noms_list = oscar_jscript %>%
  lapply(best_pic_noms_from_imdb)



best_pic_noms = best_pic_noms_list %>% bind_rows(.id = "year") %>%
  filter(grepl(x = id, pattern = "^tt")) %>%
  mutate(imdb_url = paste0("https://www.imdb.com/title/", id))

# now this portion will take a while
# we want to look through all the nominees and return their own html object
imdb_html_list = list()
i = 1
for (url in best_pic_noms$imdb_url) {
  imdb_html_list[[i]] = url %>% read_html()
  i = i+1
}


production_companies_imdb_fun = function(html_object) {
  html_object %>%
    html_nodes("#__next > main > div") %>% 
    html_nodes("section.ipc-page-background.ipc-page-background--base.TitlePage__StyledPageBackground-wzlr49-0.dDUGgO") %>% 
    html_nodes("div > section > div") %>% 
    html_nodes("div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2") %>% 
    # html_nodes("section:nth-child(44) > div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ul > li:nth-child(7)")
    html_nodes("section") %>%
    # html_attr("data-testid")
    html_nodes(xpath = "//*[@data-testid='Details']") %>%
    html_nodes("div") %>% 
    html_nodes(xpath = "//*[@data-testid='title-details-companies']") %>%
    html_nodes("div > ul > li > a") %>% 
    html_text() %>% 
    paste(collapse = "|")
}


producers = imdb_html_list %>%
  lapply(production_companies_imdb_fun) %>% 
  unlist()



best_pic_noms_final = best_pic_noms %>% mutate(producers = producers) %>%
  # idk why 1930 got copied twice
  distinct()

# best_pic_noms_final %>% write.csv("data/best_picture_nominees.csv")





