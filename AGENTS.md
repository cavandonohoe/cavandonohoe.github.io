# Agent notes

## "Make a rating plot for <show>"

Use the fast path. Do not wait for CI or the site build to show the plot.

1. Find the show's IMDb ID (`tt...`).
2. Run from the repo root:

   ```bash
   Rscript scripts/quick_rating_plot.R <tt id or IMDb url> "<Show Title>" --pr
   ```

   Drop `--pr` for a preview only (no branch, no PR).
3. Show the user the PNG it prints
   (`imdb_rating_plot_files/<title>.png`) right away, then link the PR.

The script reuses `scripts/episode_plot.R` (the same `create_episode_plot()`
the page uses) and the scraper in
`web_scraping/imdb_season_episode_ratings_plot.R`, so the preview matches
what the page will publish. Never fork the plotting code; edit
`scripts/episode_plot.R` instead.
