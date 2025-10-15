

# alright working on imdb top 250 movies now
# maybe I can get the rotten tomatoes as well
# I think check the director(s) and that might be your unique identifier

library(tidyverse)
library(rvest)

url = "https://www.imdb.com/chart/top/"

top_250_html = url %>% read_html()

top_250_tib = top_250_html %>% 
  html_node("#main > div > span > div > div > div.lister > table") %>% 
  html_table() %>% as_tibble(.name_repair = "universal")

top_250_ids = top_250_html %>% 
  html_nodes("#main > div > span > div > div > div.lister > table > tbody") %>% 
  html_nodes("td.titleColumn > a") %>% 
  html_attr("href") %>%
  gsub(pattern = "/title/", replacement = "") %>%
  gsub(pattern = "/.*", replacement = "")


# the nice thing about this imdb list, is that it shows the director
# so we can check if the rotten tomatoes directors match up with the imdb directors
# our qc (quality check) will set our control group as the imdb group since I trust imdb more

directors = top_250_html %>% 
  html_nodes("#main > div > span > div > div > div.lister > table > tbody") %>% 
  html_nodes("td.titleColumn > a") %>%
  html_attr("title") %>%
  gsub(pattern = " \\(dir\\.\\).*", replacement = "")


top_250_tib_clean = top_250_tib %>% 
  mutate(id = top_250_ids,
         rank = row_number(),
         # extract text between last set of parentheses
         year = gsub(x = Rank...Title, pattern = ".*\\((.*)\\).*", replacement = "\\1") %>%
           as.numeric(),
         # extract text from first and last space
         title = gsub(x = Rank...Title, pattern = "^\\S+\\s+|\\s+[^ ]+$", replacement = ""),
         site = paste0("https://www.imdb.com/title/", id),
         director = directors) %>%
  select(rank, title, director, year, imdb_rating = IMDb.Rating, id, site) %>%
  mutate(producer_site = file.path(site, "companycredits?ref_=ttfc_sa_3"))


# next step is to grab each rotten tomato section
# so it's in the format shawshank_redemption
# they drop 'the' and turn it into snakecase
# so
# https://www.rottentomatoes.com/m/shawshank_redemption
# and any duplicates get a year
# so we'll do some error handling when that happens
# also, I said they drop the 'the', but I mean... not always


rotten_tomatoes_url = top_250_tib_clean %>% 
  select(title, imdb_director = director) %>%
  mutate(rotten_tomatoes_title = 
           case_when((title == "The Dark Knight" & imdb_director == "Christopher Nolan") |
                       (title == "The Dark Knight Rises" & imdb_director == "Christopher Nolan") |
                       (title == "The Gold Rush" & imdb_director == "Charles Chaplin") |
                       (title == "The Grand Budapest Hotel" & imdb_director == "Wes Anderson") ~ 
                       snakecase::to_any_case(string = title, case = "snake"),
                     title == "12 Angry Men" & imdb_director == "Sidney Lumet" ~
                       "1000013_12_angry_men",
                     title == "Seven Samurai" & imdb_director == "Akira Kurosawa" ~
                       "seven_samurai_1956",
                     title == "Se7en" & imdb_director == "David Fincher" ~
                       "seven",
                     title == "Life Is Beautiful" & imdb_director == "Roberto Benigni" ~
                       "1084398-life_is_beautiful",
                     title == "Star Wars: Episode IV - A New Hope" & imdb_director == "George Lucas" ~
                       "star_wars",
                     title == "Star Wars: Episode V - The Empire Strikes Back" & imdb_director == "Irvin Kershner" ~
                       "empire_strikes_back",
                     title == "Hara-Kiri" & imdb_director == "Masaki Kobayashi" ~
                       "harakiri",
                     title == "Whiplash" & imdb_director == "Damien Chazelle" ~
                       "whiplash_2014",
                     title == "Casablanca" & imdb_director == "Michael Curtiz" ~
                       "1003707-casablanca",
                     title == "The Lives of Others" & imdb_director == "Florian Henckel von Donnersmarck" ~
                       "the_lives_of_others",
                     title == "Sunset Blvd." & imdb_director == "Billy Wilder" ~
                       "sunset_boulevard",
                     title == "Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb" & imdb_director == "Stanley Kubrick" ~
                       "dr_strangelove",
                     title == "Princess Mononoke" & imdb_director == "Hayao Miyazaki" ~
                       "princess_mononoke_1999",
                     title == "Your Name." & imdb_director == "Makoto Shinkai" ~
                       "your_name_2017",
                     title == "Aliens" & imdb_director == "James Cameron" ~
                       "1000617-aliens",
                     title == "Capharnaüm" & imdb_director == "Nadine Labaki" ~
                       "capernaum",
                     title == "Hamilton" & imdb_director == "Thomas Kail" ~
                       "hamilton_2020",
                     title == "Braveheart" & imdb_director == "Mel Gibson" ~
                       "1065684-braveheart",
                     title == "M" & imdb_director == "Fritz Lang" ~
                       "1012928-m",
                     title == "Come and See" & imdb_director == "Elem Klimov" ~
                       "1036052-come_and_see",
                     title == "The Kid" & imdb_director == "Charles Chaplin" ~
                       "1052609-kid",
                     title == "Logan" & imdb_director == "James Mangold" ~
                       "logan_2017",
                     title == "The Apartment" & imdb_director == "Billy Wilder" ~
                       "1001115-apartment",
                     title == "Metropolis" & imdb_director == "Fritz Lang" ~
                       "1013775-metropolis",
                     title == "The Sting" & imdb_director == "George Roy Hill" ~
                       "1020130-sting",
                     title == "1917" & imdb_director == "Sam Mendes" ~
                       "1917_2019",
                     title == "Heat" & imdb_director == "Michael Mann" ~
                       "heat_1995",
                     title == "All About Eve" & imdb_director == "Joseph L. Mankiewicz" ~
                       "1000626-all_about_eve",
                     title == "Unforgiven" & imdb_director == "Clint Eastwood" ~
                       "1041911-unforgiven",
                     title == "The Wolf of Wall Street" & imdb_director == "Martin Scorsese" ~
                       "the_wolf_of_wall_street_2013",
                     title == "Shutter Island" & imdb_director == "Martin Scorsese" ~
                       "1198124-shutter_island",
                     title == "Warrior" & imdb_director == "Gavin O'Connor" ~
                       "1212910-warrior",
                     title == "My Father and My Son" & imdb_director == "Cagan Irmak" ~
                       "babam-ve-oglum-my-father-and-my-son",
                     title == "The General" & imdb_director == "Clyde Bruckman" ~
                       "1008166-general",
                     title == "Mary and Max" & imdb_director == "Adam Elliot" ~
                       "1209767-mary_and_max",
                     title == "Room" & imdb_director == "Lenny Abrahamson" ~
                       "room_2015",
                     title == "Ben-Hur" & imdb_director == "William Wyler" ~
                       "benhur",
                     title == "Stand by Me" & imdb_director == "Rob Reiner" ~
                       "stand_by_me_1986",
                     title == "The Bandit" & imdb_director == "Yavuz Turgul" ~
                       "the_bandit_2016",
                     title == "Spotlight" & imdb_director == "Tom McCarthy" ~
                       "spotlight_2015",
                     title == "A Silent Voice: The Movie" & imdb_director == "Naoko Yamada" ~
                       "a_silent_voice",
                     title == "In the Mood for Love" & imdb_director == "Kar-Wai Wong" ~
                       "in_the_mood_for_love_2001",
                     title == "Love's a Bitch" & imdb_director == "Alejandro G. Iñárritu" ~
                       "amores_perros",
                     title == "Demon Slayer: Mugen Train" & imdb_director == "Haruo Sotozaki" ~
                       "demon_slayer_kimetsu_no_yaiba_the_movie_mugen_train",
                     title == "Three Colors: Red" & imdb_director == "Krzysztof Kieslowski" ~
                       "1058966-red",
                     title == "Raatchasan" & imdb_director == "Ram Kumar" ~
                       "tumbbad",
                     title == "The Thing" & imdb_director == "John Carpenter" ~
                       "1021244-thing",
                     title == "Rebecca" & imdb_director == "Alfred Hitchcock" ~
                       "1017293-rebecca",
                     title == "Parasite" & imdb_director == "Bong Joon Ho" ~
                       "parasite_2019",
                     title == "Joker" & imdb_director == "Todd Phillips" ~
                       "joker_2019",
                     title == "Coco" & imdb_director == "Lee Unkrich" ~
                       "coco_2017",
                     title == "The Hunt" & imdb_director == "Thomas Vinterberg" ~
                       "the_hunt_2013",
                     title == "Dune" & imdb_director == "Denis Villeneuve" ~
                       "dune_2021",
                     title == "The Father" & imdb_director == "Florian Zeller" ~
                       "the_father_2021",
                     title == "A Separation" & imdb_director == "Asghar Farhadi" ~
                       "a_separation_2011",
                     title == "Casino" & imdb_director == "Martin Scorsese" ~
                       "1067987-casino",
                     title == "The Elephant Man" & imdb_director == "David Lynch" ~
                       "1006527-elephant_man",
                     title == "Inside Out" & imdb_director == "Pete Docter" ~
                       "inside_out_2015",
                     title == "Memories of Murder" & imdb_director == "Bong Joon Ho" ~
                       "memories_of_murder_2003",
                     title == "Stalker" & imdb_director == "Andrei Tarkovsky" ~
                       "1043378-stalker",
                     title == "Prisoners" & imdb_director == "Denis Villeneuve" ~
                       "prisoners_2013",
                     title == "Rush" & imdb_director == "Ron Howard" ~
                       "rush_2013",
                     title == "Rocky" & imdb_director == "John G. Avildsen" ~
                       "1017776-rocky",
                     title == "Hera Pheri" & imdb_director == "Priyadarshan" ~
                       "hera-pheri",
                     title == "The Wizard of Oz" & imdb_director == "Victor Fleming" ~
                       "the_wizard_of_oz_1939",
                     title == "Life of Brian" & imdb_director == "Terry Jones" ~
                       "monty_pythons_life_of_brian",
                     title == "The Handmaiden" & imdb_director == "Park Chan-wook" ~
                       "the_handmaiden",
                     TRUE ~ gsub(x = snakecase::to_any_case(
                       string = gsub(x = title, pattern = "'|\\.", replacement = ""),
                       case = "snake"),
                       pattern = "^the_|^a_", replacement = "")),
         rotten_tomatoes_title = stringi::stri_trans_general(rotten_tomatoes_title, "Latin-ASCII"),
         rotten_tomatoes_url = paste0("https://www.rottentomatoes.com/m/", rotten_tomatoes_title))


html_list_rot = list()
index = 1
for (rot_url in rotten_tomatoes_url$rotten_tomatoes_url) {
  html_list_rot[[index]] = read_html(rot_url)
  index = index + 1
}
# rotten_tomatoes_url %>% filter(row_number() == index)

# let's go again
# let's grab the imdb html data
# unreliable producers are in rotten tomatoes
# can definitely get more data from imdb afterwards too
html_imdb_list = list()
index = 1
for (imdb_url in top_250_tib_clean$site) {
  html_imdb_list[[index]] = read_html(imdb_url)
  index = index + 1
}

# I've found it might be easiest to do this in a for loop
# would love to mclapply this sometime for speed purposes

# production companies

# fucking this isn't here anymore
# fucking imdb...


html_imdb_prod_list = list()
index = 1
for (imdb_url in top_250_tib_clean$producer_site) {
  html_imdb_prod_list[[index]] = read_html(imdb_url)
  index = index + 1
}


imdb_producers <- function(html_object) {
  html_object %>%
    html_nodes("#company_credits_content > ul") %>%
    html_text() %>%
    .[1] %>%
    str_split(pattern = "\n") %>%
    unlist %>% str_trim() %>% str_squish() %>%
    `[` (.!="") %>%
    paste(collapse = "|")
}

# production_companies_imdb_fun = function(html_object) {
#   html_object %>%
#     html_nodes("#__next > main > div") %>% 
#     html_nodes("section.ipc-page-background.ipc-page-background--base.TitlePage__StyledPageBackground-wzlr49-0.dDUGgO") %>% 
#     html_nodes("div > section > div") %>% 
#     html_nodes("div.TitleMainBelowTheFoldGroup__TitleMainPrimaryGroup-sc-1vpywau-1.btXiqv.ipc-page-grid__item.ipc-page-grid__item--span-2") %>% 
#     # html_nodes("section:nth-child(44) > div.styles__MetaDataContainer-sc-12uhu9s-0.cgqHBf > ul > li:nth-child(7)")
#     html_nodes("section") %>%
#     # html_attr("data-testid")
#     html_nodes(xpath = "//*[@data-testid='Details']") %>%
#     html_nodes("div") %>% 
#     html_nodes(xpath = "//*[@data-testid='title-details-companies']") %>%
#     html_nodes("div > ul > li > a") %>% 
#     html_text() %>% 
#     paste(collapse = "|")
# }



audience_score_fun = function(html_object){
  html_object %>% html_nodes("#topSection > div.thumbnail-scoreboard-wrap > score-board") %>%
    html_attr("audiencescore")
}

tomato_meter_score_fun = function(html_object){
  html_object %>% html_nodes("#topSection > div.thumbnail-scoreboard-wrap > score-board") %>%
    html_attr("tomatometerscore")
}

directors_rot = function(html_object){
  director = html_object %>%
    html_nodes("section.panel.panel-rt.panel-box.movie_info.media > div > div > ul >li") %>% 
    html_text() %>%
    str_squish() %>% 
    as_tibble() %>%
    separate(col = value, sep = ": ", into = c("title", "name")) %>% 
    filter(title == "Director") %>% pull(name)
  
  if (length(director) < 1) {
    director = NA
  }
  return(director)
}

movie_info_rot = function(html_object){
  movie_info = html_object %>%
    html_nodes("section.panel.panel-rt.panel-box.movie_info.media > div > div > ul >li") %>% 
    html_text() %>%
    str_squish() %>% 
    as_tibble() %>%
    separate(col = value, sep = ": ", into = c("category", "name"))
  
  return(movie_info)
  
}

# run html node functions

audience_scores = html_list_rot %>% 
  lapply(audience_score_fun) %>% unlist()


tomato_meter_scores = html_list_rot %>% 
  lapply(tomato_meter_score_fun) %>% unlist()

director_rot_tom = html_list_rot %>% 
  lapply(directors_rot) %>% unlist()

movie_info = html_list_rot %>% 
  lapply(movie_info_rot)
names(movie_info) = top_250_tib_clean$title

movie_info_tib = movie_info %>% bind_rows(.id = "title")

producers_imdb = html_imdb_prod_list %>% 
  lapply(imdb_producers) %>% 
  unlist()

producers_imdb <- producers_imdb %>%
  # remove all parentheses and insides of parentheses
  str_remove_all("\\s*\\([^\\)]+\\)")

producers_imdb = tibble(id = top_250_tib_clean$id, producers_imdb)

top_250_imdb_rot_tom = top_250_tib_clean %>% 
  mutate(audience_scores = as.numeric(audience_scores),
         tomato_meter_scores = as.numeric(tomato_meter_scores),
         director_rot_tom = director_rot_tom) %>% 
  left_join(movie_info_tib %>% filter(category == "Director") %>% select(title, director = name)) %>% 
  left_join(movie_info_tib %>% filter(category == "Distributor") %>% select(title, distributor = name)) %>%
  left_join(movie_info_tib %>% filter(category == "Production Co") %>% select(title, production_co = name)) %>% 
  mutate(produced_by = ifelse(is.na(distributor), production_co, distributor)) %>% 
  left_join(producers_imdb)


# top_250_imdb_rot_tom %>% write.csv("data/top250_movies_with_rt.csv")

