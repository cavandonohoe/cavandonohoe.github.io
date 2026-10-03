# IMDb exceptional episodes — 3 October 2026

Official downloads qualify **1,340 shows** at **25,000 or more series votes**: 1,132 TV series and 208 miniseries. All have linked episodes and at least one rated episode. There are no language, country, animation, documentary, or genre exclusions. Other title types are excluded. Series votes are votes for the show itself, not a sum of episode votes.

An exceptional episode has IMDb weighted rating **at least 9.7**, with the episode-vote thresholds below. Inclusive comparisons use the one-decimal rating supplied by IMDb. These are descriptive popularity and rating filters, not a statistical guarantee of quality.

| Episode votes | Qualifying shows | Share of 1,340 | Exceptional episodes |
|---|---:|---:|---:|
| ≥1,000 | 89 | 6.64% | 250 |
| **≥5,000 — primary** | **78** | **5.82%** | **229** |
| ≥10,000 | 57 | 4.25% | 169 |

## Cohort comparison — provisional historical comparison

**The current Top 250 cohort could not be obtained.** The official live chart and public endpoint returned HTTP 403. Search-index snippets were insufficient to verify all 250 IDs as of the retrieval date. No list was reconstructed by sorting bulk ratings, and the saved CSV was not labelled current.

The repository CSV `data/top250_imdb_series.csv` contains 250 distinct show IDs. Only **218** meet the 25,000-series-vote cutoff today. Results below use current official episode ratings for those historical cohort IDs, not the old CSV ratings.

| Episode votes | Saved cohort: all 250, shows / episodes | Saved cohort: 218 eligible, shows / episodes | Outside saved cohort: 1,122 eligible, shows / episodes |
|---|---:|---:|---:|
| ≥1,000 | 52 / 200 | 50 / 177 | 39 / 73 |
| ≥5,000 | 48 / 168 | 47 / 167 | 31 / 62 |
| ≥10,000 | 37 / 136 | 37 / 136 | 20 / 33 |

At the primary threshold, the saved eligible cohort has 47/218 shows (21.56%) with exceptional episodes, versus 31/1,122 (2.76%) outside it. Thus a saved-cohort-only analysis would omit 31 of the 78 qualifying popular shows and 62 of the 229 exceptional episodes. These findings cannot establish the exact omissions from today's Top 250 until that cohort is verified.

## Leading shows — primary episode count

| Show | Exceptional episodes |
|---|---:|
| One Piece | 40 |
| Attack on Titan | 13 |
| Game of Thrones | 11 |
| My Hero Academia | 8 |
| Naruto: Shippuden | 8 |
| Better Call Saul | 7 |
| Mr. Robot | 6 |
| Star Wars: The Clone Wars | 6 |
| Breaking Bad | 5 |
| Invincible | 5 |

Examples outside the saved cohort include My Hero Academia, Star Wars: The Clone Wars, Person of Interest, Buffy the Vampire Slayer, Community, Heated Rivalry, Lost, and Scrubs. Membership refers solely to the repository's saved cohort.

## Validation and data limits

Validation uses repository commit `448604a20ae1669cec4b6a08b544949ffdd54e2a`. Existing CSVs are checks, not the universe or rating source. The saved Top 250 CSV has 15,519 rows: 15,405 match a unique official episode using series ID + season + episode; 7 have missing/ambiguous keys; 107 have no official rating. Of the matched rows, 83 have invalid/missing CSV rating or votes; 12,225 have the same rating and 30 the same votes. Twenty matched rows change their primary exceptional classification between snapshots. The episode numbers match, but because the CSV lacks episode IDs, this is a provisional identity check; renumbering can also create differences. These differences need not indicate errors: the official daily data and historical CSVs describe different times.

All 69 individual episode CSVs are checked separately. Detailed validation results are in `validation_summary.csv` and `validation_episode_matches.csv`. Star Wars: Visions has zero votes in the stored CSV; those zeros cannot be trusted for episode-vote eligibility. Its show ID was resolved from official title basics.

Official episode IDs uniquely define episode counts. Missing season/episode numbers remain in the official universe; specials and season-zero entries are not excluded. A rating record is required; no exact air-date field is available in these downloads, so no exact release-date filter is applied. A show with no exceptional episodes remains in the denominator. Long-running shows have more opportunities for high-rated episodes; raw episode counts are not a normalized quality ranking. Vote cutoffs do not eliminate organized voting or fandom effects.

## Files and reproducibility

- `results/eligible_series.csv`: all 1,340 eligible shows, series ratings/votes, linked/rated episode coverage, and counts at all three thresholds.
- `results/primary_exceptional_episodes.csv`: the 229 primary episodes in eligible shows.
- `results/exceptional_episodes.csv`: ≥9.7 / ≥1,000 episodes across the broader universe and saved validation cohort; filter `eligible_series=True` for the broader analysis.
- `results/cohort_summary.csv`: counts and denominators for each threshold and historical comparison.
- `results/validation_summary.csv` and `results/validation_episode_matches.csv`: CSV comparisons; these can overlap across files and should not be summed as unique episodes.
- `results/manifest.json`: dataset SHA-256 hashes, HTTP metadata, retrieval time, and repository commit.

Run with Python 3.10+ from the repository root (no third-party packages required):

```sh
python analysis/imdb-exceptional-episodes/download.py
python analysis/imdb-exceptional-episodes/analyze.py
```

The input downloads are ignored by Git. Output CSVs and metadata go to this directory's `results/` folder; validation reads existing CSVs from the repository's `data/` directory. `analyze.py --data-dir PATH --output-dir PATH` supports an existing snapshot and a separate output directory. The checked-in report is a dated snapshot and must be updated when fresh outputs are committed.

For the original validation snapshot, use repository commit `448604a20ae1669cec4b6a08b544949ffdd54e2a`. Later repository CSV edits may change validation results independently of official dataset updates.

Downloads change daily; reruns against a later snapshot can change counts. The original downloads were retrieved 3 October 2026 UTC; their individual Last-Modified and x-amz-meta-run-date headers are in the manifest. Raw downloads are not bundled.

Official sources: https://data.imdb.com/non-commercial-datasets/ and https://datasets.imdbws.com/ . Current comparison target: https://www.imdb.com/chart/toptv/ .

## Website page

`imdb_exceptional_episodes.Rmd` renders this snapshot into the website, with a show leaderboard, searchable show and episode tables, vote-threshold tabs, and a historical cohort comparison. `scripts/imdb_exceptional_metrics.R` recomputes counts in R and the page checks them against the saved sensitivity summary. Rendering does not download bulk data. The page is linked from the homepage and Projects menu.
