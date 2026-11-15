# Quick IMDb season x episode heatmap for one show, using the exact same
# fetch (web_scraping/imdb_season_episode_ratings_plot.R) and plot
# (scripts/episode_plot.R) code as imdb_rating_plot.Rmd. Takes seconds instead
# of waiting on CI + the full site build.
#
# Usage (from the repo root):
#   Rscript scripts/quick_rating_plot.R <imdb id or url> "<Show Title>" [slug] [--pr]
#
# Examples:
#   Rscript scripts/quick_rating_plot.R tt0903747 "Breaking Bad"
#   Rscript scripts/quick_rating_plot.R https://www.imdb.com/title/tt2085059/ \
#     "Black Mirror" --pr
#
# Always: fetches ratings, writes data/<slug>_ep_ratings.csv, writes the PNG to
# imdb_rating_plot_files/<title>.png, and opens it.
# With --pr: also adds the show to the scraper config and the shows manifest,
# then commits on a new branch off origin/main and opens a PR. The PNG is a
# local preview; the PR adds the show to the published page.

`%>%` <- magrittr::`%>%`

args <- commandArgs(trailingOnly = TRUE)
make_pr <- "--pr" %in% args
args <- setdiff(args, "--pr")
if (length(args) < 2) {
  stop(
    "Usage: Rscript scripts/quick_rating_plot.R <imdb id or url> ",
    "\"<Show Title>\" [slug] [--pr]",
    call. = FALSE
  )
}

source(here::here("web_scraping", "imdb_season_episode_ratings_plot.R"))
source(here::here("scripts", "episode_plot.R"))
source(here::here("scripts", "shows_manifest.R"))

imdb_id <- extract_imdb_id(args[[1]])
show_title <- args[[2]]
slug <- if (length(args) >= 3) {
  args[[3]]
} else {
  show_title %>%
    tolower() %>%
    stringr::str_replace_all("'", "") %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

cat(sprintf("Fetching %s (%s) ...\n", show_title, imdb_id))
episodes <- get_imdb_all_episodes(imdb_id)
if (nrow(episodes) == 0) stop("No rated episodes returned for ", imdb_id, call. = FALSE)
cat(sprintf(
  "  %d episodes across %d season(s)\n",
  nrow(episodes), dplyr::n_distinct(episodes$season)
))

csv_path <- here::here("data", paste0(slug, "_ep_ratings.csv"))
readr::write_csv(episodes, csv_path)

png_path <- episode_png_path(show_title)
create_episode_plot(episodes, show_title, png_path = png_path)
cat("PNG:", png_path, "\n")
if (Sys.getenv("QUICK_PLOT_NO_OPEN") != "1") {
  opener <- if (Sys.info()[["sysname"]] == "Darwin") "open" else "xdg-open"
  system2(opener, shQuote(png_path), wait = FALSE)
}

if (!make_pr) quit(status = 0)

# --- PR mode -----------------------------------------------------------------

run <- function(cmd, ...) {
  status <- system2(cmd, c(...))
  if (status != 0) stop(cmd, " ", paste(c(...), collapse = " "), " failed", call. = FALSE)
}

if (slug %in% tv_shows_manifest()$slug) {
  cat("Already in the shows manifest; CSV refreshed but no PR needed.\n")
  quit(status = 0)
}

# Insert one line before a closing ")" of a tribble, found via an anchor line.
insert_before_close <- function(path, anchor_regex, new_line) {
  lines <- readLines(path)
  anchor <- grep(anchor_regex, lines)[[1]]
  close <- anchor + which(grepl("^\\s*\\)\\s*$", lines[(anchor + 1):length(lines)]))[[1]]
  last <- max(which(grepl("^\\s*\"", lines[anchor:(close - 1)]))) + anchor - 1
  if (!grepl(",\\s*$", lines[[last]])) lines[[last]] <- paste0(lines[[last]], ",")
  lines <- append(lines, new_line, after = last)
  writeLines(lines, path)
}

title_r <- gsub("\"", "\\\\\"", show_title)
insert_before_close(
  here::here("web_scraping", "imdb_season_episode_ratings_plot.R"),
  "^shows <- tibble::tribble\\(",
  sprintf("  \"%s\", \"%s\", \"%s\"", tolower(title_r), imdb_id, slug)
)
insert_before_close(
  here::here("scripts", "shows_manifest.R"),
  "~title, ~slug, ~fig_width",
  sprintf("    \"%s\", \"%s\", NA, NA, NA, NA", title_r, slug)
)

branch <- paste0("cdonohoe/add-", gsub("_", "-", slug), "-ratings")
run("git", "fetch", "-q", "origin", "main")
run("git", "switch", "-c", branch, "origin/main")
run(
  "git", "add", shQuote(csv_path),
  "web_scraping/imdb_season_episode_ratings_plot.R", "scripts/shows_manifest.R"
)
run("git", "commit", "-q", "-m", shQuote(paste("Add", show_title, "to IMDb rating plots")))
run("git", "push", "-q", "-u", "origin", branch)
body <- paste0(
  "Adds **", show_title, "** ([", imdb_id, "](https://www.imdb.com/title/", imdb_id,
  "/)) to the IMDb rating plot page and the weekly ratings scraper.\n\n",
  "Preview generated locally with `scripts/quick_rating_plot.R` ",
  "(same plotting code as the page)."
)
run(
  "gh", "pr", "create", "--assignee", "@me", "--base", "main",
  "--title", shQuote(paste("Add", show_title, "to IMDb rating plots")),
  "--body", shQuote(body)
)
