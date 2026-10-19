
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cavan’s Website

<!-- badges: start -->

[![Site
build](https://github.com/cavandonohoe/cavandonohoe.github.io/actions/workflows/pages-rmarkdown.yml/badge.svg)](https://github.com/cavandonohoe/cavandonohoe.github.io/actions/workflows/pages-rmarkdown.yml)
<!-- badges: end -->

The goal of my website is to display some fun side projects I have been
working on. Movie data intrigues me, so I scraped IMDb to use some of
their clean data. I also have my résumé here since it’s easier to share
a link than an attachment.

Check out my website at <https://cavandonohoe.github.io>

## Pages

<!-- Keep these lists in the same order and wording as the navbar in _site.yml. -->

### About

- [About Me](https://cavandonohoe.github.io/about.html)
- [Services](https://cavandonohoe.github.io/services.html)
- [CV](https://cavandonohoe.github.io/cv.html)
- [Publications](https://cavandonohoe.github.io/publications.html)

### Movies & TV

- [Exceptional IMDb
  Episodes](https://cavandonohoe.github.io/imdb_exceptional_episodes.html)
- [IMDb TV
  Series](https://cavandonohoe.github.io/imdb_top_250_tv_series.html)
- [IMDb Rating Plot by Season and
  Episode](https://cavandonohoe.github.io/imdb_rating_plot.html)
- [Did They Quit While They Were
  Ahead?](https://cavandonohoe.github.io/tv_decline_leaderboard.html)
- [Top 250 IMDb Movies with Rotten Tomatoes
  Scores](https://cavandonohoe.github.io/top_250_imdb_with_rt.html)
- [Best Picture
  Nominees](https://cavandonohoe.github.io/best_picture_nominees.html)
- [Best Picture and Top 1000 Box
  Office](https://cavandonohoe.github.io/best_pic_vs_top_1000.html)
- [Director Box Office
  Trajectory](https://cavandonohoe.github.io/director_box_office_trajectory.html)
- [My IMDb Ratings](https://cavandonohoe.github.io/my_imdb.html)

### Sports & Personal

- [AVCA Men’s Volleyball
  All-Americans](https://cavandonohoe.github.io/avca_all_americans.html)
- [Jason, Owen, and Wyett’s Firefighter
  Schedule](https://cavandonohoe.github.io/firefighter_schedule.html)
- [Google Maps: Want To Go +
  Favorites](https://cavandonohoe.github.io/google_maps_saved_places.html)
- [Get Bloggy With It (travel
  blog)](https://cavandonohoe.github.io/my_travels.html)
- [TOHS Class of 2012
  Reunion](https://cavandonohoe.github.io/tohs_reunion.html)

### Visualizations

- [Saved Podcast
  Episodes](https://cavandonohoe.shinyapps.io/podcast-dashboard/)
  (Shiny)
- [Detour
  Finder](https://cavandonohoe.github.io/detour-finder/index.html)
- [When Were Confederate Statues
  Erected?](https://cavandonohoe.github.io/confederate_statues.html)
- [Animated Procedural
  Maze](https://cavandonohoe.github.io/maze-art.html)
- [Sierpinski
  Triangle](https://cavandonohoe.github.io/sierpinski_triangle.html)
- [Airplane Boarding
  Simulation](https://cavandonohoe.github.io/airplane_boarding.html)

### Real Estate & Finance

- [US Rental Investment
  Markets](https://cavandonohoe.github.io/us_rental_markets.html)
- [LA Neighborhoods: Price per Sq
  Ft](https://cavandonohoe.github.io/la_real_estate.html)
- [LA Housing Prices: Animated Heat
  Map](https://cavandonohoe.github.io/la_housing_heatmap.html)
- [Rent vs. Buy
  Calculator](https://cavandonohoe.shinyapps.io/rent-vs-buy/) (Shiny,
  [source](https://github.com/cavandonohoe/rent-vs-buy))

### Data Science & Clinical

- [SDTM with R](https://cavandonohoe.github.io/sdtm_with_r.html)

### Learn R

- [Course Overview](https://cavandonohoe.github.io/learn_r.html)
  1.  [Introduction to
      R](https://cavandonohoe.github.io/intro_to_r.html)
  2.  [Basic
      Operations](https://cavandonohoe.github.io/basic_operations_r.html)
  3.  [Data
      Manipulation](https://cavandonohoe.github.io/data_manipulation_r.html)
  4.  [Data
      Visualization](https://cavandonohoe.github.io/data_visualization_r.html)
  5.  [Statistics with
      R](https://cavandonohoe.github.io/statistics_with_r.html)
  6.  [Reproducible Reports with R
      Markdown](https://cavandonohoe.github.io/rmarkdown_r.html)
  7.  [Capstone Project](https://cavandonohoe.github.io/capstone_r.html)

### Archive

- [S&P 500 Perspective](https://cavandonohoe.github.io/sp500.html)
- [Covid Cases: Then &
  Now](https://cavandonohoe.github.io/covid_cases_nostalgia.html)

## Automation

The repo runs the following on a schedule via GitHub Actions. Anything
manual is a click away on the Actions tab.

### Site build & previews

- `pages-branch-previews.yml` – main deploy + per-PR preview deploy and
  cleanup. This is what publishes the site (pushes to the `gh-pages`
  branch).
- `pages-rmarkdown.yml` – daily safety-net build with DESCRIPTION /
  `library()` validation and gs4 guard checks. Does **not** deploy
  (deploy is owned by `pages-branch-previews.yml` to avoid Pages
  deployment races).

### Scheduled refreshes (commit to `main`)

- `update_imdb_ratings.yml` – weekly IMDb episode ratings refresh.
- `update_top1000_box_office.yml` – monthly Best Picture + Top 1000 box
  office refresh.
- `update_top250_with_rt.yml` – monthly Top 250 + Rotten Tomatoes
  refresh.
- `update_director_filmographies.yml` – monthly TMDb director
  filmography refresh for the Director Box Office Trajectory page.
- `update_sp500.yml` – weekly VOO price refresh.
- `update_us_rentals.yml` – monthly Zillow ZHVI/ZORI refresh.
- `update_mymaps_export.yml` – weekly Google My Maps export.
- `update_watched_episodes.yml` – weekly episode list for every rated
  series in my IMDb export.
- `update_imdb_exceptional_episodes.yml` – weekly Exceptional IMDb
  Episodes refresh.
- `update_confederate_statues_data.yml` – monthly Confederate statues
  data refresh.
- `update_movie_ranker_personal.yml` – every 6 hours, pulls my personal
  Elo ratings from the movie-ranker Supabase project for My IMDb
  Ratings.
- `update_saved_episodes.yml` – daily Spotify saved-episodes refresh,
  then redeploys the podcast dashboard on shinyapps.io.
- `update_tohs_reunion.yml` – hourly TOHS reunion sheet snapshot.
- `update_page_views.yml` – daily GA4 pull that reorders the homepage
  Projects grid by traffic.
- `shiny-traffic.yml` – weekly snapshot of shinyapps.io traffic for
  every app on the account.
- `year-in-review.yml` – daily current-year developer review, plus a Jan
  1 snapshot of the year that just ended.
- `activity-art.yml` – daily 30-day commit activity SVG.
- `lately.yml` – daily summary of the last 30 days of my commits
  (`data/lately.json`).
- `personal-changelog.yml` – daily changelog of PRs merged in the last
  30 days (`data/changelog.json`).
- `weekly-wrapped.yml` – weekly 7-day activity snapshot
  (`data/weekly/`).
- `project-case-studies.yml` – on push, rebuilds project case-study
  metadata (`data/project_case_studies.json`).
- `refresh-commit-activity.yml` – weekly re-knit of this README’s
  commit-activity charts.

The uniform ones (`update_sp500`, `update_us_rentals`,
`update_top250_with_rt`, `update_watched_episodes`) are thin callers of
`refresh-data.yml`, which owns the shared run/guard/commit/redeploy
logic. The rest stay standalone because they build dynamic commit
messages or need extra fetch steps. Every refresh that commits to `main`
runs `scripts/data_sanity.R` first, so an empty or collapsed scrape
fails the run instead of landing on `main`.

### Data refreshes (open PR)

- `update_best_picture_winners.yml` – annual Oscar refresh.
- `rebuild_cv.yml` – weekly CV PDF/DOCX rebuild from Google Sheet, opens
  a PR if anything changed.
- `refresh-sitemap.yml` – weekly `sitemap.xml` regeneration.

### Site health

- `link-check.yml` – weekly + on-PR broken-link check (lychee).
- `pa11y.yml` – weekly accessibility audit (pa11y-ci).
- `lighthouse.yml` – two modes: on-PR audit of the deployed preview URL
  with a score comment, plus a post-build audit of the production
  artifact after each successful main build.
- `repo-size.yml` – weekly + on-PR repo-size report.
- `scheduled-failure-alert.yml` – opens an issue when a scheduled
  workflow fails and closes it on the next green run.

### Code quality

- `lint.yml` – on-PR lintr scan of changed R/Rmd files.
- `typos.yml` – on-PR spell check.
- `pr-smoke.yml` – on-PR fast render of only the changed Rmds.
- `test.yml` – R test suite (`scripts/run_tests.R`) on PRs and pushes.

### Side apps

- `movie-ranker-build.yml` – on-PR build of the Next.js movie-ranker
  app.
- `backfill-movie-ranker-posters.yml` – weekly TMDB poster backfill for
  new movie-ranker titles.
- `ping-movie-ranker-supabase.yml` – weekly read query so the free-tier
  Supabase project never auto-pauses.
- `tohs-apps-script.yml` – validates and deploys the TOHS reunion Google
  Apps Script.

### Repo hygiene

- `.github/dependabot.yml` – weekly bump of pinned GitHub Actions.
- `dependabot-automerge.yml` – squash-merges minor/patch Dependabot PRs
  once all other checks pass.
- `labeler.yml` – auto-label PRs by changed paths.
- `prune-previews.yml` – weekly cleanup of stale PR preview deploys.
- `data-question.yml` – acknowledges issues labeled `data-question`.

## Activity over time

The chart below summarizes commit activity on this repo: monthly commit
count and lines changed (insertions + deletions), split by my own
commits vs. automated commits from the scheduled data refresh workflows
above. Regenerated each time `README.Rmd` is knit.

<img src="images/readme/commit-activity-1.png" alt="Two bar charts showing monthly commits and monthly lines changed (insertions + deletions) for this repository, split by Cavan's own commits vs. scheduled-bot commits."  />

<img src="images/readme/commit-activity-cumulative-1.png" alt="Two cumulative line charts showing total commits and total lines changed (insertions + deletions) over time, split by Cavan's own commits vs. scheduled-bot commits."  />

*1,052 commits from Dec 2020 to Oct 2026 (651 by me, 401 by scheduled
bots), touching 3,110,589 lines in total.*

## Setup and Local Development

Local development, prerequisites, build steps, lint/test commands, and
PR conventions all live in [CONTRIBUTING.md](CONTRIBUTING.md).

### Chase Statement Parser

The Chase statement parser is a local utility for turning Chase credit
card statement PDFs into a transaction workbook. It is designed so the
repo can keep reusable code without committing private finance data.

Privacy rules:

- Do not commit statement PDFs, generated workbooks, exported
  transaction CSVs, or Google Sheet URLs.
- Keep personal owner rules in `config/chase_owner_rules.csv`; that file
  is ignored by git.
- Use `config/chase_owner_rules.example.csv` as the tracked starting
  rules file for non-sensitive patterns and labels.

Parse statements locally:

``` bash
Rscript scripts/parse_chase_statements.R \
  --rules config/chase_owner_rules.csv \
  --out data/chase_tx.xlsx \
  statements/*.pdf
```

Upload an already-generated workbook to a Google Sheet:

``` bash
Rscript scripts/upload_chase_tx_to_gsheet.R \
  data/chase_tx.xlsx \
  "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit"
```

The R parser is the canonical workflow. The Python parser supports the
same rules CSV format, but the R path is what this repo validates with
synthetic tests.

Parsing real PDFs with the R script requires the optional `pdftools`
package and its system Poppler libraries. The synthetic tests do not
require PDF parsing dependencies.

Run the synthetic parser smoke test:

``` bash
Rscript tests/test_chase_parser.R
```

### Project Structure

- `*.Rmd` files: Individual pages of the website
- `_site.yml`: Site configuration and navigation
- `_common.R`: Common R code (Google auth setup for CI)
- `header/header.html`: HTML header with meta tags and favicons
- `include_footer.html`: Footer with contact links
- `css/footer.css`: Footer styling
- `.github/workflows/`: GitHub Actions workflows for CI/CD
- `data/`: Data files used by various pages
- `scripts/`: Utility scripts (e.g., ensuring gs4 guards)
- `tests/`: R tests run by `test.yml`
- `podcast-dashboard/`, `movie-ranker/`, `detour-finder/`,
  `streaming-history/`: self-contained side apps
- `tools/tohs-apps-script/`: Google Apps Script for the TOHS reunion

### Deployment

The site is automatically deployed to GitHub Pages via GitHub Actions
when changes are pushed to the `main` branch. See the Automation section
above for the full list of workflows.

## License

This repository is dual-licensed to cover both its code and its written
content:

- **Code** (R, R Markdown chunks, scripts, workflows, and configuration)
  is licensed under the [MIT License](LICENSE).
- **Content** (prose, articles, images, and the CV) is licensed under
  [Creative Commons Attribution 4.0 International (CC BY
  4.0)](LICENSE-CONTENT.md).

In short: reuse the code freely with attribution and no warranty, and
share or adapt the writing and images as long as you give credit.
