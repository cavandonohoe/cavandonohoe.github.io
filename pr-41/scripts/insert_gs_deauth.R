has_gs4 <- function(txt) {
  any(grepl("\\blibrary\\(\\s*googlesheets4\\s*\\)", txt)) ||
    any(grepl("googlesheets4::", txt))
}
has_deauth <- function(txt) any(grepl("\\bgs4_deauth\\s*\\(", txt))

insert_after_yaml <- function(txt, line_to_insert) {
  # Detect YAML front matter (--- ... ---) at top
  if (length(txt) >= 1 && grepl("^\\s*---\\s*$", txt[1])) {
    # find closing ---
    end <- which(grepl("^\\s*---\\s*$", txt[-1]))[1] + 1
    if (!is.na(end)) {
      return(c(txt[1:end], line_to_insert, txt[(end+1):length(txt)]))
    }
  }
  c(line_to_insert, txt)
}

rmds <- Sys.glob("*.Rmd")
changed <- character()

for (f in rmds) {
  txt <- readLines(f, warn = FALSE)
  if (!has_gs4(txt) || has_deauth(txt)) next

  line_to_insert <- "```{r gs4-auth, include=FALSE}\ngooglesheets4::gs4_deauth()\n```\n"
  newtxt <- insert_after_yaml(txt, line_to_insert)

  if (!identical(txt, newtxt)) {
    writeLines(newtxt, f)
    changed <- c(changed, f)
  }
}

if (length(changed)) {
  cat("Inserted googlesheets4::gs4_deauth() in:\n", paste0(" - ", changed), sep = "\n")
} else {
  cat("No files needed googlesheets4::gs4_deauth() (either not using gs4 or already present).\n")
}
