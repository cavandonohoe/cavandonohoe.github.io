# Public snapshot: never persist email addresses from the private sheet.
tohs_public_rows <- function(sheet) {
  required <- c("First Name", "Last Name", "Email")
  if (!all(required %in% names(sheet))) stop("Reunion sheet columns changed")
  # The existing page skips the first sheet row (a non-graduate row).
  sheet <- sheet[-1, , drop = FALSE]
  first <- trimws(as.character(sheet[["First Name"]]))
  last <- trimws(as.character(sheet[["Last Name"]]))
  email <- trimws(as.character(sheet[["Email"]]))
  keep <- !is.na(first) & nzchar(first) & !is.na(last) & nzchar(last)
  result <- data.frame(
    first_name = first[keep], last_name = last[keep],
    email_bool = !is.na(email[keep]) & nzchar(email[keep]),
    stringsAsFactors = FALSE
  )
  if (!nrow(result)) stop("Reunion sheet returned no graduates")
  result
}

refresh_tohs_reunion <- function(path = "data/tohs_reunion.csv") {
  key <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  if (!nzchar(key) || !file.exists(key)) stop("Google service-account key is required")
  googlesheets4::gs4_auth(path = key, cache = FALSE)
  sheet <- googlesheets4::read_sheet(
    "1JwWeBjwwQHzmGgh8HPuO_0pghzemC3ikpLx_pXlQvsI",
    sheet = "Full Grad Class", col_types = "c"
  )
  rows <- tohs_public_rows(sheet)
  changed <- TRUE
  if (file.exists(path)) {
    previous <- read.csv(path, stringsAsFactors = FALSE)
    if (nrow(rows) < 0.8 * nrow(previous)) stop("Graduate count dropped by more than 20%; review sheet")
    changed <- !identical(rows, previous[names(rows)])
  }
  if (changed) {
    rows$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    write.csv(rows, path, row.names = FALSE, na = "")
  }
  summary <- sprintf(
    "Reunion: %d/%d emails collected; %d missing. Snapshot %s.",
    sum(rows$email_bool), nrow(rows), sum(!rows$email_bool),
    if (changed) "updated" else "unchanged"
  )
  message(summary)
  summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(summary_path)) cat(summary, "\n", file = summary_path, append = TRUE)
  invisible(changed)
}

if (sys.nframe() == 0L) refresh_tohs_reunion()
