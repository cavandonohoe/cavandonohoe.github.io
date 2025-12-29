`%>%` <- magrittr::`%>%`
## Web Scraping All Oscar Nominees

url <- "https://www.imdb.com/event/ev0000003/2021/1/?ref_=ev_eh"

year_html <- rvest::read_html(url)


# there is a lot of javascript in these imdb pages, so everything is a little messy until it isn't
years_messy <- year_html %>%
  
  # rvest::html_nodes("#sidebar") %>% 
  rvest::html_nodes("#sidebar > div") %>%
    
  rvest::html_nodes("span") %>%
    
  # rvest::html_nodes("href")
  # rvest::html_attr("a") %>% 
  rvest::html_text(trim = TRUE)

year_vec <- strsplit(x = years_messy, split = ",") %>%
  unlist

year_tib <- tibble::tibble(year_vec) %>%
  dplyr::filter(grepl(x=year_vec, pattern = "year")) %>%
  
  dplyr::mutate(year = gsub(x = year_vec, pattern = "\"year\":", replacement = ""),
         year_url = paste0("https://www.imdb.com/event/ev0000003/", year, "/1/?ref_=ev_eh"))


## let's start this for loop

oscar_html_list <- list()
i <- 1
for (url in year_tib$year_url) {
  oscar_html_list[[i]] = url %>%
    rvest::read_html()
  i <- i+1
}

# create a function that grabs the messy jscript with all the nominees of every category
# this won't take that long since we're at less than 100 oscar award years
oscar_jscript_raw <- function(html_object) {
  html_object %>%
    rvest::html_nodes("body > div > div > div > div > div > div > div > span") %>%
    
    rvest::html_text() %>%
      
    strsplit(split = ",(?![ Production])", perl = TRUE) %>%
      unlist() %>%
      
    tibble::as_tibble() %>%
      
    dplyr::rename(messy_jscript_text = value)
}

oscar_jscript <- oscar_html_list %>%
  
  lapply(oscar_jscript_raw)

names(oscar_jscript) = year_tib$year


# now create a function that looks at just the best picture nominees

best_pic_noms_from_imdb <- function(tib) {
  tib2 <- tib %>%
    
    dplyr::filter(grepl(x = messy_jscript_text,
                 # we want to keep the nominees, the const (id) and the Best Picture label
                 # the Best Picture label will be a good wrapper for text we are looking for
                 pattern = paste("Best Motion Picture of the Year|Best Picture",
                                 "primaryNominees", "const\":\"tt", sep = "|"))) %>%
                                   
    dplyr::mutate(text =
             dplyr::case_when(
               grepl(x = messy_jscript_text,
                     # keep it standard
                     pattern = "Best Motion Picture of the Year|Best Picture") ~
                 "Best Motion Picture of the Year",
               TRUE ~ messy_jscript_text),
           text2 = gsub(x = text, pattern = "\"const\":\"|.*name\":\"|}]|\"", replacement = "")) %>%
             
    dplyr::group_by(text2) %>%
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      
    dplyr::ungroup()

  # create a nice looking tibble with title; id; and nominee or winner column
  gsub(x = paste(tib2$text2, collapse = "|"),
       pattern = ".*?Best Motion Picture of the Year(.*)Best Motion Picture of the Year.*",
       replacement = "\\1") %>%
         
    gsub(pattern = "Best Motion Picture of the Year|\\|$|^\\|", replacement = "") %>%
      
    gsub(pattern = "\\|\\|\\|", replacement = "||") %>%
      
    strsplit(split = "\\|\\|") %>%
      unlist %>%
      
    tibble::as_tibble() %>%
      
    tidyr::separate(col = value, sep = "\\|", into = c("title", "id")) %>%
      
    dplyr::mutate(nominees_or_winner = ifelse(dplyr::row_number() == 1, "winner", "nominee"))
}

best_pic_noms_list <- oscar_jscript %>%
  
  lapply(best_pic_noms_from_imdb)



best_pic_noms <- best_pic_noms_list %>%
  dplyr::bind_rows(.id = "year") %>%
  
  dplyr::filter(grepl(x = id, pattern = "^tt")) %>%
    
  dplyr::mutate(imdb_url = paste0("https://www.imdb.com/title/", id))

# now this portion will take a while
# we want to look through all the nominees and return their own html object
imdb_html_list <- list()
i <- 1
for (url in best_pic_noms$imdb_url) {
  imdb_html_list[[i]] = url %>%
    rvest::read_html()
  i <- i+1
}


production_companies_imdb_fun <- function(html_object) {
  html_object %>%
    
    rvest::html_nodes("#__next > main > div") %>%
      
    rvest::html_nodes(paste0(
      "section.ipc-page-background.ipc-page-background--base.",
      "TitlePage__StyledPageBackground-wzlr49-0.dDUGgO"
    )) %>%
      
    rvest::html_nodes("div > section > div") %>%
      
    rvest::html_nodes(paste0(
      "div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.",
      "btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2"
    )) %>%
      
    # rvest::html_nodes(paste0(
    #   "section:nth-child(44) > div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ",
    #   "ul > li:nth-child(7)"
    # ))
    rvest::html_nodes("section") %>%
      
    # rvest::html_attr("data-testid")
    rvest::html_nodes(xpath = "//*[@data-testid='Details']") %>%
      
    rvest::html_nodes("div") %>%
      
    rvest::html_nodes(xpath = "//*[@data-testid='title-details-companies']") %>%
      
    rvest::html_nodes("div > ul > li > a") %>%
      
    rvest::html_text() %>%
      
    paste(collapse = "|")
}

# looking deeper into the credits
# imdb had an update to remove it from the home screen
# those douchebags
# making it slightly harder to webscrape
imdb_prod_html_list <- list()
i <- 1
for (url in best_pic_noms$imdb_url) {
  imdb_prod_html_list[[i]] = url %>%
    file.path("companycredits?ref_=ttfc_sa_3") %>%
    rvest::read_html()
  i <- i+1
}


imdb_producers <- function(html_object) {
  html_object %>%
    
    rvest::html_nodes("#company_credits_content > ul") %>%
      
    rvest::html_text() %>%
      
    .[1] %>%
      
    stringr::str_split(pattern = "\n") %>%
      
    unlist %>%
      stringr::str_trim() %>%
      stringr::str_squish() %>%
      
    `[` (.!="") %>%
      
    paste(collapse = "|") %>%
      
    stringr::str_remove_all("\\s*\\([^\\)]+\\)")
}

# producers = imdb_html_list %>%
#   lapply(production_companies_imdb_fun) %>% 
#   unlist()

producers <- imdb_prod_html_list %>%
  
  lapply(imdb_producers) %>%
    
  unlist()


best_pic_noms_final <- best_pic_noms %>%
  dplyr::mutate(producers = producers) %>%
  
  # idk why 1930 got copied twice
  dplyr::distinct()

# best_pic_noms_final %>% write.csv(here::here("data", "best_picture_nominees.csv"))



