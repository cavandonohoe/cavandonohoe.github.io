# Contributing

Thanks for your interest in this repo. It's primarily a personal site, but bug reports, link fixes, and accessibility improvements are welcome.

## Local development

### Prerequisites

- R (>= 4.2). The repo pins R via `.R-version` for CI; locally any 4.2+ should work.
- Pandoc (installed by RStudio, or via Homebrew on macOS).
- The packages listed in `DESCRIPTION`. Install them in one shot:

  ```r
  desc <- read.dcf("DESCRIPTION")
  pkgs <- trimws(unlist(strsplit(desc[, "Imports"], ",\\s*")))
  install.packages(setdiff(pkgs, rownames(installed.packages())))
  ```

### Build the site

```r
rmarkdown::render_site()
```

Output goes to `_site/`. Open `_site/index.html` in a browser to preview.

To render a single page (much faster during iteration):

```r
rmarkdown::render("airplane_boarding.Rmd")
```

### Google Sheets auth (CV only)

The CV page reads from a Google Sheet. For local development:

```r
googlesheets4::gs4_auth()
```

CI uses a service-account key stored as the `GCP_SA_KEY` GitHub secret. Most other pages do not need auth.

## Running checks locally

The same checks CI runs, in increasing order of cost:

```r
# Lint a single file
lintr::lint("scripts/generate_sitemap.R")

# Lint a directory
lintr::lint_dir("scripts/")

# Run the test suite (when tests are present)
testthat::test_dir("tests/testthat")
```

For typo / link / accessibility checks see the corresponding workflows in `.github/workflows/`. These run automatically on PR.

## Pull request conventions

- Branch off `origin/main`. Don't rebase published PR branches without good reason.
- One concern per PR. Smaller PRs land faster.
- PR titles use imperative voice (`Add ...`, `Fix ...`, `Pin ...`). No JIRA-style prefixes.
- Reference the issue you're closing with `Closes #N` in the PR body.
- CI must be green before merge. The data-refresh workflows don't run on PRs from forks (they need secrets); that's expected.

## Adding a new page

1. Create `your_page.Rmd` at the repo root.
2. Add an entry to `_site.yml` under `navbar:` if it should appear in the top nav, or just under a `Project` card in `index.Rmd` for project pages.
3. Render locally and confirm `pr-smoke.yml` will pick it up (it auto-discovers changed top-level `.Rmd` files).
4. If the page needs a scheduled data refresh, add a workflow under `.github/workflows/update_*.yml` modeled on the existing ones.

## Adding a new test

1. Pure functions go in a sourceable file under `scripts/`.
2. Tests go in `tests/testthat/test-<name>.R` and `source("../../scripts/<file>.R")` at the top.
3. Run `testthat::test_dir("tests/testthat")` locally before pushing.

## Reporting issues

Open an issue with:

- What page or workflow is affected
- What you expected vs. what happened
- A link to the relevant page or commit if applicable
