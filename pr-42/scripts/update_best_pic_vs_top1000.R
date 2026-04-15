to <- "cavandonohoe@gmail.com"
sleep_after <- FALSE
refresh_box_office <- FALSE
refresh_ratings <- FALSE

message("Starting Best Picture scrape...")
source(here::here("web_scraping", "best_picture_winners.R"))
readr::write_csv(oscar_winners_clean, here::here("data", "best_picture_winners.csv"))
message("Best Picture scrape complete.")

message("Starting Top 1000 Box Office scrape...")
source(here::here("web_scraping", "top1000_box_office.R"))
readr::write_csv(imdb_rank_ratings_clean, here::here("data", "top1000_box_office.csv"))
message("Top 1000 Box Office scrape complete.")

source(here::here("scripts", "gmail_notify.R"))
send_gmail_notification(
  to = to,
  subject = "Best Picture vs Top 1000 refresh complete",
  body = paste(
    "Best Picture and Top 1000 Box Office scrapes finished.",
    sprintf("Time: %s", Sys.time()),
    sep = "\n"
  )
)

if (sleep_after) {
  system("pmset sleepnow")
}
