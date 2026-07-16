# Contributing

Thanks for your interest in this repo. It's primarily a personal site, but bug reports, link fixes, and accessibility improvements are welcome.

## Local development

### Prerequisites

- R (>= 4.2). The repo pins R via `.R-version` for CI; locally any 4.2+ should work.
- Pandoc (installed by RStudio, or via Homebrew on macOS).

### Dependencies (renv)

The repo uses [renv](https://rstudio.github.io/renv/) to pin all R
package versions in `renv.lock`. On first checkout, restore the locked
package versions into a project-local library:

```r
renv::restore()
```

After that, anything you `install.packages()` lands in the project
library only and won't pollute your global R installation. The first
restore is slow (it builds ~200 packages from source); subsequent
restores are near-instant thanks to renv's cache.

If you'd rather not use renv locally, every R package the site needs
is also declared in `DESCRIPTION`. You can install them all in one
shot:

```r
desc <- read.dcf("DESCRIPTION")
pkgs <- trimws(unlist(strsplit(desc[, "Imports"], ",\\s*")))
install.packages(setdiff(pkgs, rownames(installed.packages())))
```

The lockfile is the source of truth for CI; `DESCRIPTION` mainly
documents the high-level package surface for humans.

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

## Git hooks (pre-push)

The repo ships a pre-push hook (`.githooks/pre-push`) that runs the fast,
deterministic slice of CI locally so red checks are caught before the push
instead of after a round-trip to GitHub. Enable it once after cloning:

```bash
./scripts/install-git-hooks.sh
```

That points `core.hooksPath` at the tracked `.githooks/` directory, so the
hook is versioned and shared. On each `git push` it runs, against only the
files changed in the range being pushed:

- **typos** — spell check via `.typos.toml` (mirrors `typos.yml`)
- **lintr** — lints changed `.R` / `.Rmd` files via `.lintr` (mirrors `lint.yml`)
- **testthat** — runs `tests/testthat` when `scripts/` or `tests/` changed (mirrors `test.yml`)
- **DESCRIPTION** — checks every `library()` call in an Rmd is declared (mirrors the build sanity check)

Heavy jobs (site render, Lighthouse, pa11y, link-check) stay on CI; they
need Chrome / a built `_site` / network and are too slow for a push gate.

Bypass when you need to:

```bash
git push --no-verify          # skip all hooks
SKIP_HOOKS=1 git push         # same, explicit
SKIP_LINT=1 git push          # skip just lintr (also SKIP_TYPOS / SKIP_TESTS / SKIP_DESC)
```

## Bumping pinned dependencies

The renv lockfile (`renv.lock`) is the source of truth for what
versions CI installs. To pull in newer versions of one or all packages:

```r
# Update everything to the latest CRAN versions
renv::update()

# Or just specific packages
renv::update(c("rmarkdown", "knitr"))

# Re-snapshot the lockfile so the updates are tracked
renv::snapshot()
```

Then open a PR with the resulting `renv.lock` diff. CI will exercise
the new versions on every workflow.

To bump the R version itself, edit `.R-version` and update the `R.Version` field in `renv.lock` to match. CI reads R version from `.R-version` directly; the field in `renv.lock` is documentation.

## Pull request conventions

- Branch off `origin/main`. Don't rebase published PR branches without good reason.
- One concern per PR. Smaller PRs land faster.
- PR titles use imperative voice (`Add ...`, `Fix ...`, `Pin ...`, `Update ...`). No JIRA-style prefixes.
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
