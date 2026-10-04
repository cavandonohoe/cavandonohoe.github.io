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

tohs_validate_public <- function(sheet, check_freshness = TRUE) {
  allowed <- c("first_name", "last_name", "email_bool", "updated_at")
  if (!identical(names(sheet), allowed)) stop("Public export schema changed; publication refused")
  if (!nrow(sheet)) stop("Public export returned no graduates")
  for (field in c("first_name", "last_name")) {
    values <- trimws(as.character(sheet[[field]]))
    if (anyNA(values) || any(!nzchar(values)) || any(nchar(values) > 100) ||
        any(grepl("@|[<>\\r\\n]|https?:|[0-9]{5}", values, perl = TRUE)))
      stop("Unsafe public name; publication refused")
    sheet[[field]] <- values
  }
  flags <- toupper(as.character(sheet$email_bool))
  if (anyNA(flags) || any(!flags %in% c("TRUE", "FALSE"))) stop("Invalid email coverage flag")
  sheet$email_bool <- flags == "TRUE"
  timestamps <- unique(as.character(sheet$updated_at))
  if (length(timestamps) != 1L || is.na(timestamps) || !nzchar(timestamps)) stop("Invalid export timestamp")
  if (check_freshness) {
    stamp <- as.POSIXct(timestamps, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    age <- as.numeric(difftime(Sys.time(), stamp, units = "hours"))
    if (is.na(age) || age > 3 || age < -0.1) stop("Public export is stale; keeping last good snapshot")
  }
  sheet
}

refresh_tohs_reunion <- function(path = "data/tohs_reunion.csv") {
  key <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  if (!nzchar(key) || !file.exists(key)) stop("Google service-account key is required")
  googlesheets4::gs4_auth(path = key, cache = FALSE)
  public_id <- Sys.getenv("TOHS_PUBLIC_SPREADSHEET_ID")
  if (nzchar(public_id)) {
    # V2 reads only the safe export, never raw responses or Master Contacts.
    sheet <- suppressMessages(googlesheets4::read_sheet(
      public_id, sheet = "Public Export", col_types = "c"
    ))
    rows <- tohs_validate_public(sheet)[c("first_name", "last_name", "email_bool")]
  } else {
    # Existing workflow remains operational until the explicit V2 cutover.
    sheet <- suppressMessages(googlesheets4::read_sheet(
      "1JwWeBjwwQHzmGgh8HPuO_0pghzemC3ikpLx_pXlQvsI",
      sheet = "Full Grad Class", col_types = "c"
    ))
    rows <- tohs_public_rows(sheet)
    probe <- rows
    probe$updated_at <- "legacy"
    tohs_validate_public(probe, check_freshness = FALSE)
  }
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
  form_url <- Sys.getenv("TOHS_FORM_URL")
  if (nzchar(form_url)) {
    if (!grepl("^https://(docs\\.google\\.com/forms/d/[A-Za-z0-9_/-]+|forms\\.gle/[A-Za-z0-9_-]+)$", form_url))
      stop("Invalid public Form URL")
    dir.create("config", showWarnings = FALSE)
    writeLines(form_url, "config/tohs_form_url.txt")
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
