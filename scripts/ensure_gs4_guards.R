args <- commandArgs(trailingOnly = TRUE)
fix  <- any(args %in% c("--fix", "-f")) || identical(Sys.getenv("FIX"), "1")

uses_gs4 <- function(txt) {
  any(grepl("\\blibrary\\(\\s*googlesheets4\\s*\\)", txt)) ||
    any(grepl("googlesheets4::", txt))
}
has_ci_skip <- function(txt) any(grepl("^```\\{r\\s+ci-skip-gs4\\b", txt))
has_deauth  <- function(txt) any(grepl("\\bgs4_deauth\\s*\\(", txt))

insert_after_yaml <- function(txt, block) {
  if (length(txt) >= 1 && grepl("^\\s*---\\s*$", txt[1])) {
    # find closing --- (second fence)
    end <- which(grepl("^\\s*---\\s*$", txt[-1]))[1] + 1
    if (!is.na(end)) return(c(txt[1:end], block, txt[(end+1):length(txt)]))
  }
  c(block, txt)
}

# blocks
ci_skip_block <- c(
  "```{r ci-skip-gs4, include=FALSE}",
  "if (Sys.getenv(\"CI\") == \"true\") knitr::knit_exit()",
  "```",
  ""
)
deauth_block <- c(
  "```{r gs4-auth, include=FALSE}",
  "googlesheets4::gs4_deauth()",
  "```",
  ""
)

rmds <- list.files(".", pattern="\\.Rmd$", full.names=TRUE)
needs <- list()

for (f in rmds) {
  txt <- readLines(f, warn = FALSE)
  if (!uses_gs4(txt)) next
  
  need_skip   <- !has_ci_skip(txt)
  need_deauth <- !has_deauth(txt)
  
  if (need_skip || need_deauth) {
    needs[[f]] <- c(if (need_skip) "ci-skip-gs4", if (need_deauth) "gs4_deauth")
    if (fix) {
      # insert in stable order: CI skip first, then deauth
      if (need_skip)   txt <- insert_after_yaml(txt, ci_skip_block)
      if (need_deauth) txt <- insert_after_yaml(txt, deauth_block)
      writeLines(txt, f)
    }
  }
}

if (length(needs)) {
  cat("Files needing gs4 guards:\n")
  for (f in names(needs)) cat(sprintf(" - %s: %s\n", f, paste(needs[[f]], collapse=", ")))
  if (!fix) {
    cat("\nRun with --fix (or FIX=1) to apply changes.\n")
    quit(status = 1)
  } else {
    cat("\nApplied fixes.\n")
  }
} else {
  cat("All gs4-using Rmds already have ci-skip-gs4 and gs4_deauth.\n")
}
