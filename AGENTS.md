# Agent notes

## "Make a rating plot for <show>"

Do not wait for the site build. Pick the path that fits your environment.

### Cloud agent / no IMDb network access (e.g. ChatGPT, Codex)

Open a PR off `main` that adds two rows and nothing else:

1. `scripts/shows_manifest.R`: add a row to the `tribble` (order doesn't
   matter, the function sorts it):

   ```r
   "Black Mirror", "black_mirror", NA, NA, NA, NA,
   ```

2. `web_scraping/imdb_season_episode_ratings_plot.R`: add a row to the
   `shows` tribble with the same slug and the show's IMDb id (`tt...`):

   ```r
   "black mirror", "tt2085059", "black_mirror",
   ```

Watch the trailing commas: the last row in each tribble has none. The slug is
lowercase snake_case of the title. Do **not** create the CSV yourself.

The `Quick rating plot preview` workflow then fetches the ratings on GitHub
Actions, commits `data/<slug>_ep_ratings.csv` to the PR, and posts the heatmap
as a PR comment within a few minutes. Link the user to that comment.

### Local, with R and network access

```bash
Rscript scripts/quick_rating_plot.R <tt id or IMDb url> "<Show Title>" --pr
```

Show the user the PNG it prints (`imdb_rating_plot_files/<title>.png`) right
away, then link the PR. Drop `--pr` for a preview only.

### Either way

Both paths use `scripts/episode_plot.R` (the same `create_episode_plot()` the
page uses) and the scraper in `web_scraping/imdb_season_episode_ratings_plot.R`,
so the preview matches what the page publishes. Never fork the plotting code;
edit `scripts/episode_plot.R` instead.
