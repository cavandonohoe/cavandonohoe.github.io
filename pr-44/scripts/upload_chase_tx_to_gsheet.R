#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Install googlesheets4: install.packages('googlesheets4')")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install readxl: install.packages('readxl')")
  }
})

read_workbook_sheet <- function(xlsx_path, sheet_name) {
  readxl::read_xlsx(xlsx_path, sheet = sheet_name)
}

upload_sheet <- function(ss, sheet_name, data) {
  existing_sheets <- googlesheets4::sheet_names(ss)
  if (!(sheet_name %in% existing_sheets)) {
    googlesheets4::sheet_add(ss, sheet = sheet_name)
  }
  googlesheets4::range_clear(ss = ss, sheet = sheet_name)
  googlesheets4::sheet_write(data = data, ss = ss, sheet = sheet_name)
}

main <- function(args) {
  if (length(args) != 2) {
    cat("Usage: Rscript upload_chase_tx_to_gsheet.R <workbook.xlsx> <google-sheet-url>\n")
    quit(status = 1)
  }

  xlsx_path <- args[1]
  ss <- args[2]

  googlesheets4::gs4_auth(cache = TRUE)

  tx <- read_workbook_sheet(xlsx_path, "Transactions")
  summary <- read_workbook_sheet(xlsx_path, "Monthly B Summary")
  metadata <- read_workbook_sheet(xlsx_path, "Metadata")

  upload_sheet(ss, "Transactions", tx)
  upload_sheet(ss, "Monthly B Summary", summary)
  upload_sheet(ss, "Metadata", metadata)

  cat("Uploaded workbook tabs to Google Sheet:\n")
  cat(" - Transactions\n")
  cat(" - Monthly B Summary\n")
  cat(" - Metadata\n")
}

if (sys.nframe() == 0) {
  main(commandArgs(trailingOnly = TRUE))
}
