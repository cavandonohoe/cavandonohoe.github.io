# _common.R
# Auto-auth for Google on CI (GitHub Actions). Locally you keep using user OAuth.
if (identical(tolower(Sys.getenv("CI")), "true") && file.exists("sa.json")) {
  googledrive::drive_auth(path = "sa.json", cache = FALSE)
  googlesheets4::gs4_auth(path = "sa.json", cache = FALSE)
}
