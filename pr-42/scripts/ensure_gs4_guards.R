#!/usr/bin/env Rscript

# ensure_gs4_guards.R
# Fails CI if any Rmd that touches Google APIs lacks a guard chunk.
# Accepts ANY chunk whose label *starts with* "ci-skip-gs4" (e.g., ci-skip-gs4_cv).

args <- commandArgs(trailingOnly = TRUE)
fix  <- any(args %in% c("--fix")) || identical(Sys.getenv("FIX"), "1")

# Heuristic: an Rmd "needs a guard" if it references these APIs/verbs.
NEEDS_PAT <- "\\b(googlesheets4|googledrive|read_sheet|drive_get|gs4_|drive_)\\b"

needs_guard <- function(path) {
  lines <- readLines(path, warn = FALSE)
  any(grepl(NEEDS_PAT, lines))
}

# A file "has a guard" if it contains a chunk whose label starts with "ci-skip-gs4"
# Examples this matches:
#   ```{r ci-skip-gs4}
#   ```{r ci-skip-gs4, include=FALSE}
#   ```{r ci-skip-gs4_cv, include=FALSE}
HAS_GUARD_PAT <- "^```\\s*\\{[^}]*\\bci-skip-gs4[^}\\s]*"

has_guard <- function(path) {
  lines <- readLines(path, warn = FALSE)
  any(grepl(HAS_GUARD_PAT, lines))
}

insert_guard <- function(path) {
  lines <- readLines(path, warn = FALSE)
  guard_chunk <- c(
    "```{r ci-skip-gs4, include=FALSE}",
    "is_ci <- identical(Sys.getenv(\"CI\"), \"true\")",
    "# Avoid Google API calls on CI",
    "if (is_ci) googlesheets4::gs4_deauth()",
    "```",
    ""
  )
  writeLines(c(guard_chunk, lines), path)
}

# Discover Rmds (recursive). Adjust include/exclude patterns as needed.
rmds <- dir(".", pattern = "[.](R|r)md$", recursive = TRUE, full.names = TRUE)

violations <- character()
for (f in rmds) {
  if (needs_guard(f) && !has_guard(f)) {
    if (fix) {
      insert_guard(f)
      message(sprintf("Inserted gs4 guard into %s", f))
    } else {
      violations <- c(violations, f)
    }
  }
}

if (length(violations)) {
  cat("Files needing gs4 guards (label can be prefixed, e.g. ci-skip-gs4_cv):\n")
  for (v in violations) cat(" - ", v, ": ci-skip-gs4*\n", sep = "")
  quit(status = 1)
} else {
  cat("All good: gs4 guards present (prefixes allowed).\n")
}
