# Live GitHub stats for the CV.
#
# The resume Google Sheet can contain placeholders such as `{n_workflows}`,
# `{n_merged_prs}`, and `{n_commits}`. cv.Rmd and cv_doc.Rmd source this file
# and call fill_github_stats() so the numbers stay current every time the CV
# is rebuilt (see .github/workflows/rebuild_cv.yml).
#
# Counts are floored to a round step and suffixed with "+" (e.g. "225+") so
# the rendered CV only changes when a threshold is crossed, not every commit.

cv_github_owner <- "cavandonohoe"
cv_github_repo <- "cavandonohoe.github.io"

# Floor `n` to a multiple of `step` and append "+".
round_stat <- function(n, step) {
  if (length(n) != 1 || is.na(n)) {
    return(NA_character_)
  }
  paste0(format(floor(n / step) * step, big.mark = ","), "+")
}

# Resolve a GitHub token: GITHUB_TOKEN / GITHUB_PAT, else the gh CLI.
cv_github_token <- function() {
  token <- Sys.getenv("GITHUB_TOKEN", Sys.getenv("GITHUB_PAT", ""))
  if (!nzchar(token) && nzchar(Sys.which("gh"))) {
    token <- tryCatch(
      suppressWarnings(system2("gh", c("auth", "token"), stdout = TRUE, stderr = FALSE)),
      error = function(e) ""
    )
    token <- if (length(token) == 0) "" else token[[1]]
  }
  token
}

# Fetch raw counts. Only work authored by the owner is counted (bot PRs and
# scheduled data-refresh commits are excluded).
get_github_stats <- function(owner = cv_github_owner, repo = cv_github_repo,
                             workflow_dir = ".github/workflows") {
  n_workflows <- length(list.files(workflow_dir, pattern = "\\.ya?ml$"))

  query <- paste0(
    "query($owner: String!, $q: String!) {",
    "  user(login: $owner) { id }",
    "  search(query: $q, type: ISSUE) { issueCount }",
    "}"
  )
  api <- function(body) {
    out <- httr2::request("https://api.github.com/graphql") |>
      httr2::req_auth_bearer_token(cv_github_token()) |>
      httr2::req_body_json(body) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    if (!is.null(out$errors)) {
      stop("GitHub GraphQL error: ", out$errors[[1]]$message)
    }
    out
  }

  res <- api(list(
    query = query,
    variables = list(
      owner = owner,
      q = sprintf("repo:%s/%s is:pr is:merged author:%s", owner, repo, owner)
    )
  ))
  user_id <- res$data$user$id

  commits <- api(list(
    query = paste0(
      "query($owner: String!, $repo: String!, $id: ID!) {",
      "  repository(owner: $owner, name: $repo) {",
      "    defaultBranchRef { target { ... on Commit {",
      "      history(author: {id: $id}) { totalCount }",
      "    } } }",
      "  }",
      "}"
    ),
    variables = list(owner = owner, repo = repo, id = user_id)
  ))

  list(
    n_workflows = n_workflows,
    n_merged_prs = res$data$search$issueCount,
    n_commits = commits$data$repository$defaultBranchRef$target$history$totalCount
  )
}

# Format raw counts into the strings substituted into the CV.
format_github_stats <- function(stats) {
  c(
    n_workflows = round_stat(stats$n_workflows, 5),
    n_merged_prs = round_stat(stats$n_merged_prs, 25),
    n_commits = round_stat(stats$n_commits, 50)
  )
}

# Replace `{name}` placeholders in `x` with values from `formatted`.
fill_github_stats <- function(x, formatted) {
  for (nm in names(formatted)) {
    if (!is.na(formatted[[nm]])) {
      x <- gsub(paste0("{", nm, "}"), formatted[[nm]], x, fixed = TRUE)
    }
  }
  leftover <- regmatches(x, gregexpr("\\{n_[a-z_]+\\}", x))
  leftover <- unique(unlist(leftover))
  if (length(leftover) > 0) {
    stop("Unresolved CV stat placeholders: ", paste(leftover, collapse = ", "))
  }
  x
}

# Convenience wrapper used by the CV Rmds.
cv_github_stats <- function() {
  format_github_stats(get_github_stats())
}
