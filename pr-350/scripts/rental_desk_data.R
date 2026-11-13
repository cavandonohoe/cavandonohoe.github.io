#!/usr/bin/env Rscript
# rental_desk_data.R
#
# Copies the rental desk's live tracker output from the private
# cavandonohoe/rental-desk repo into data/rental_desk/ for the scoreboard page.
#
# Auth: FETCH_TOKEN (a fine-grained, read-only "Contents" token scoped to
# rental-desk, stored as the RENTAL_DESK_TOKEN repo secret), else `gh auth token`
# for local runs. With neither, it leaves the committed snapshot alone and exits 0.
#
# Run locally: Rscript scripts/rental_desk_data.R
# Output: data/rental_desk/live_performance.csv, live_v1.csv, live_v2.csv

repo <- "cavandonohoe/rental-desk"
out_dir <- here::here("data", "rental_desk")

token <- Sys.getenv("FETCH_TOKEN")
if (!nzchar(token) && nzchar(Sys.which("gh"))) {
  token <- tryCatch(
    suppressWarnings(system2("gh", c("auth", "token"), stdout = TRUE, stderr = FALSE)),
    error = function(e) ""
  )
  token <- if (length(token)) token[1] else ""
}
if (!nzchar(token)) {
  message("No FETCH_TOKEN or gh auth; keeping the committed rental desk snapshot.")
  quit(status = 0)
}

gh_get <- function(path, raw = FALSE) {
  req <- httr2::request(paste0("https://api.github.com/repos/", repo, "/contents/", path)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_headers(
      Accept = if (raw) "application/vnd.github.raw+json" else "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28"
    ) |>
    httr2::req_retry(max_tries = 3)
  resp <- httr2::req_perform(req)
  if (raw) httr2::resp_body_string(resp) else httr2::resp_body_json(resp)
}

#' Latest picks/live file matching a prefix, e.g. "live_" or "live_v2_"
#' @param files Names in picks/.
#' @param prefix File prefix before the YYYY-MM stamp.
#' @return The newest matching file name.
latest_live <- function(files, prefix) {
  hits <- sort(grep(paste0("^", prefix, "\\d{4}-\\d{2}\\.csv$"), files, value = TRUE))
  if (!length(hits)) stop("no picks/", prefix, "YYYY-MM.csv in ", repo)
  utils::tail(hits, 1)
}

picks <- vapply(gh_get("picks"), function(x) x$name, character(1))
targets <- c(
  live_performance.csv = "picks/live_performance.csv",
  live_v1.csv = file.path("picks", latest_live(picks, "live_")),
  live_v2.csv = file.path("picks", latest_live(picks, "live_v2_"))
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
for (nm in names(targets)) {
  body <- gh_get(targets[[nm]], raw = TRUE)
  parsed <- readr::read_csv(I(body), show_col_types = FALSE)
  if (nrow(parsed) == 0) stop(targets[[nm]], " came back empty")
  writeLines(sub("\n$", "", body), file.path(out_dir, nm))
  cat("Saved", nrow(parsed), "rows from", targets[[nm]], "to data/rental_desk/", nm, "\n")
}
