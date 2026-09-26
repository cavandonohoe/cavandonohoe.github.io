# Shared manifest of TV shows with episode-rating data in data/<slug>_ep_ratings.csv.
# Sourced by imdb_rating_plot.Rmd and tv_decline_leaderboard.Rmd so the two pages
# cannot drift apart. Commented-out rows are deliberately excluded from the site.
#
#   title      - heading shown on the page (also drives the PNG file name)
#   slug       - data/<slug>_ep_ratings.csv
#   fig_width  - knitr figure width (inches) for the on-page interactive plot
#   fig_height - knitr figure height (inches)
#   png_width  - width (inches) used for the exported PNG (defaults to fig_width)
#   png_height - height (inches) used for the exported PNG (defaults to fig_height)
# Leave dims as NA to use the document defaults.

tv_shows_manifest <- function() {
  manifest <- tibble::tribble(
    ~title, ~slug, ~fig_width, ~fig_height, ~png_width, ~png_height,
    "Alice in Borderland", "alice_in_borderland", NA, NA, NA, NA,
    "Always Sunny in Philadelphia", "always_sunny", NA, NA, NA, NA,
    "Andor", "andor", NA, NA, NA, NA,
    "Archer", "archer", NA, NA, NA, NA,
    "Attack on Titan", "attack_on_titan", NA, 9, NA, NA,
    "Avatar: The Last Airbender", "avatar", NA, NA, NA, NA,
    "Barry", "barry", NA, NA, NA, NA,
    "Batman: Caped Crusader", "batman_caped_crusader", NA, NA, NA, NA,
    "Batman: The Animated Series", "batman_animated_series", 7, 16, NA, NA,
    "Better Call Saul", "better_call_saul", NA, NA, NA, NA,
    "Blue Eye Samurai", "blue_eye_samurai", NA, NA, NA, NA,
    "Bluey", "bluey", 4, 9, NA, NA,
    "Bojack Horseman", "bojack", NA, NA, NA, NA,
    "Breaking Bad", "breaking_bad", NA, NA, NA, NA,
    "Brooklyn Nine-Nine", "brooklyn_nine_nine", NA, NA, NA, NA,
    "Chernobyl", "chernobyl", NA, NA, NA, NA,
    "Clone Wars", "clone_wars", NA, NA, NA, NA,
    "Community", "community", NA, NA, NA, NA,
    "Daredevil", "daredevil", NA, NA, NA, NA,
    "Death Note", "death_note", NA, 12, NA, NA,
    "Family Guy", "family_guy", 15, 9, 16, 9,
    "Fleabag", "fleabag", NA, NA, NA, NA,
    "Friends", "friends", 7, 7, NA, NA,
    "Futurama", "futurama", NA, NA, NA, NA,
    "Game of Thrones", "game_of_thrones", NA, NA, NA, NA,
    "Gravity Falls", "gravity_falls", NA, 8, NA, NA,
    "Grey's Anatomy", "greys_anatomy", 9, 9, NA, NA,
    "Hannibal", "hannibal", NA, NA, NA, NA,
    "Heated Rivalry", "heated_rivalry", NA, NA, NA, NA,
    "House MD", "house", 7, 7, NA, NA,
    "House of the Dragon", "hotd", NA, NA, NA, NA,
    "How I Met Your Mother", "himym", NA, NA, NA, NA,
    "Invincible", "invincible", NA, NA, NA, NA,
    "The Last of Us", "the_last_of_us", NA, NA, NA, NA,
    #   "Love Island (UK)", "love_island", NA, 11, NA, NA,
    "Mad Men", "mad_men", NA, NA, NA, NA,
    #   "Modern Love", "modern_love", NA, NA, NA, NA,
    #   "Mr Robot", "mr_robot", NA, NA, NA, NA,
    "My Hero Academia", "my_hero_academia", NA, NA, NA, NA,
    "New Girl", "new_girl", NA, NA, NA, NA,
    #   "Paris Hilton's My New BFF", "paris_hilton_bff", NA, NA, NA, NA,
    "Parks and Recreation", "parks_and_rec", NA, NA, NA, NA,
    "Person of Interest", "person_of_interest", NA, NA, NA, NA,
    "Pretty Little Liars", "pretty_little_liars", NA, NA, NA, NA,
    "The Punisher", "the_punisher", NA, NA, NA, NA,
    "Rick and Morty", "rick_and_morty", NA, NA, NA, NA,
    "Schitt's Creek", "schitts_creek", NA, NA, NA, NA,
    "Scrubs", "scrubs", NA, NA, NA, NA,
    "Seinfeld", "seinfeld", NA, NA, NA, NA,
    "Sherlock", "sherlock", NA, NA, NA, NA,
    "Six Feet Under", "six_feet_under", NA, NA, NA, NA,
    "South Park", "south_park", 15, 6, 16, 6,
    "Sparticus", "sparticus", NA, NA, NA, NA,
    "Stranger Things", "stranger_things", NA, NA, NA, NA,
    "Suits", "suits", NA, NA, NA, NA,
    "Ted Lasso", "ted_lasso", NA, NA, NA, NA,
    "The Boys", "the_boys", NA, NA, NA, NA,
    #   "The Crown", "crown", NA, NA, NA, NA,
    "The Mandalorian", "the_mandalorian", NA, NA, NA, NA,
    "The Office (US)", "the_office", NA, NA, NA, NA,
    "The Owl House", "the_owl_house", NA, NA, NA, NA,
    "The Simpsons", "simpsons", 15, 6, 16, 6,
    "The Wire", "the_wire", NA, NA, NA, NA,
    "Vampire Diaries", "vampire_diaries", NA, NA, NA, NA,
    "Velma", "velma", NA, NA, NA, NA,
    "Westworld", "westworld", NA, NA, NA, NA,
    "X-Men '97", "x_men_97", NA, NA, NA, NA
    # "ZeroZeroZero", "zerozerozero", NA, NA, NA, NA
  )

  dplyr::arrange(manifest, stringr::str_remove(tolower(title), "^the "))
}

# Turn a title into the same anchor slug R Markdown auto-generates for a "## Title"
# heading, so other pages can deep-link to a show's section.
anchor_id <- function(x) {
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "-")
  stringr::str_replace_all(x, "^-+|-+$", "")
}

read_show_ratings <- function(slug) {
  readr::read_csv(
    here::here("data", paste0(slug, "_ep_ratings.csv")),
    show_col_types = FALSE
  )
}
