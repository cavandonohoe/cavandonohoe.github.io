# CI helper for .github/workflows/quick-rating-plot.yml.
#
# Finds shows added to scripts/shows_manifest.R relative to a base commit,
# fetches ratings for any whose data/<slug>_ep_ratings.csv is missing (IMDb id
# comes from the scraper's show list), and renders each show's heatmap PNG with
# the same create_episode_plot() the page uses.
#
# Usage: Rscript scripts/quick_plot_new_shows.R <base sha> <png out dir>
# Writes <png out dir>/manifest.csv with one row per new show:
#   title, slug, imdb_id, png, fetched, episodes, seasons

`%>%` <- magrittr::`%>%`

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript scripts/quick_plot_new_shows.R <base sha> <png out dir>", call. = FALSE)
}
base_sha <- args[[1]]
out_dir <- args[[2]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

source(here::here("web_scraping", "imdb_season_episode_ratings_plot.R"))
source(here::here("scripts", "episode_plot.R"))

# Manifest as of the base commit (the PR's merge base with main).
base_file <- tempfile(fileext = ".R")
status <- system2(
  "git", c("show", paste0(base_sha, ":scripts/shows_manifest.R")),
  stdout = base_file
)
if (status != 0) stop("Could not read base manifest at ", base_sha, call. = FALSE)
base_env <- new.env()
sys.source(base_file, envir = base_env)
base_slugs <- base_env$tv_shows_manifest()$slug

source(here::here("scripts", "shows_manifest.R"))
new_shows <- tv_shows_manifest() %>%
  dplyr::filter(!slug %in% base_slugs)

empty <- tibble::tibble(
  title = character(), slug = character(), imdb_id = character(),
  png = character(), fetched = logical(), episodes = integer(),
  seasons = integer()
)
if (nrow(new_shows) == 0) {
  cat("No new shows in the manifest.\n")
  readr::write_csv(empty, file.path(out_dir, "manifest.csv"))
  quit(status = 0)
}

# `shows` (scraper config) is defined by the scraper script sourced above.
ids <- shows %>% dplyr::select(slug, imdb_id)
missing_ids <- setdiff(new_shows$slug, ids$slug)
if (length(missing_ids) > 0) {
  stop(
    "New manifest slug(s) missing from the scraper's show list in ",
    "web_scraping/imdb_season_episode_ratings_plot.R: ",
    paste(missing_ids, collapse = ", "),
    call. = FALSE
  )
}

results <- purrr::map_dfr(seq_len(nrow(new_shows)), function(i) {
  row <- new_shows[i, ]
  imdb_id <- ids$imdb_id[ids$slug == row$slug][[1]]
  csv_path <- here::here("data", paste0(row$slug, "_ep_ratings.csv"))
  fetched <- !file.exists(csv_path)
  if (fetched) {
    cat(sprintf("Fetching %s (%s) ...\n", row$title, imdb_id))
    episodes <- get_imdb_all_episodes(imdb_id)
    if (nrow(episodes) == 0) {
      stop("No rated episodes returned for ", imdb_id, call. = FALSE)
    }
    readr::write_csv(episodes, csv_path)
  }
  episodes <- readr::read_csv(csv_path, show_col_types = FALSE)

  png_w <- if (is.na(row$png_width)) {
    if (is.na(row$fig_width)) 8 else row$fig_width
  } else {
    row$png_width
  }
  png_h <- if (is.na(row$png_height)) {
    if (is.na(row$fig_height)) 6 else row$fig_height
  } else {
    row$png_height
  }
  png_path <- file.path(out_dir, basename(episode_png_path(row$title)))
  create_episode_plot(
    episodes, row$title,
    png_width = png_w, png_height = png_h, png_path = png_path
  )
  cat("PNG:", png_path, "\n")

  n_episodes <- nrow(episodes)
  n_seasons <- dplyr::n_distinct(episodes$season)
  tibble::tibble(
    title = row$title, slug = row$slug, imdb_id = imdb_id,
    png = basename(png_path), fetched = fetched,
    episodes = n_episodes,
    seasons = n_seasons
  )
})

readr::write_csv(results, file.path(out_dir, "manifest.csv"))
