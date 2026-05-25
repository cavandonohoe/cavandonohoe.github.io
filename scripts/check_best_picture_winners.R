`%>%` <- magrittr::`%>%`

csv_path <- here::here("data", "best_picture_winners.csv")
cache_path <- here::here("data", "best_picture_box_office_cache.rds")

old_winners <- if (file.exists(csv_path)) {
  readr::read_csv(csv_path, show_col_types = FALSE)
} else {
  tibble::tibble()
}

refresh_box_office <- TRUE
box_office_cache_path <- cache_path

message("Refreshing Best Picture winners from IMDb...")
source(here::here("web_scraping", "best_picture_winners.R"))

new_winners <- oscar_winners_clean
readr::write_csv(new_winners, csv_path)

old_latest <- old_winners %>%
  dplyr::arrange(dplyr::desc(oscar_year)) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::select(oscar_year, oscar_titles, movie_year, oscar_imdb_ratings)

new_latest <- new_winners %>%
  dplyr::arrange(dplyr::desc(oscar_year)) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::select(oscar_year, oscar_titles, movie_year, oscar_imdb_ratings)

summary_lines <- c(
  "## Best Picture Winners Check",
  "",
  sprintf("Generated at: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "### Latest checked-in rows",
  "",
  paste(capture.output(print(old_latest, n = Inf)), collapse = "\n"),
  "",
  "### Latest refreshed rows",
  "",
  paste(capture.output(print(new_latest, n = Inf)), collapse = "\n")
)

summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_path)) {
  cat(paste(summary_lines, collapse = "\n"), "\n", file = summary_path, append = TRUE)
} else {
  cat(paste(summary_lines, collapse = "\n"), "\n")
}

message("Wrote refreshed winners to ", csv_path)
