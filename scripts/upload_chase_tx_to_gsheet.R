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

shared_transactions <- function(tx) {
  tx[!is.na(tx$owner) & tx$owner == "b", , drop = FALSE]
}

upload_sheet <- function(ss, sheet_name, data) {
  existing_sheets <- googlesheets4::sheet_names(ss)
  if (!(sheet_name %in% existing_sheets)) {
    googlesheets4::sheet_add(ss, sheet = sheet_name)
  }
  googlesheets4::range_clear(ss = ss, sheet = sheet_name)
  googlesheets4::sheet_write(data = data, ss = ss, sheet = sheet_name)
  googlesheets4::range_autofit(ss = ss, sheet = sheet_name, dimension = "columns")
}

summary_formula <- function(transactions_sheet_name = "Transactions") {
  months_expr <- paste0(
    "SORT(UNIQUE(FILTER(TEXT(",
    transactions_sheet_name,
    "!B2:B,\"yyyy-mm\"),",
    transactions_sheet_name,
    "!B2:B<>\"\")))"
  )
  month_range_expr <- paste0(
    "TEXT(",
    transactions_sheet_name,
    "!B2:B,\"yyyy-mm\")"
  )
  amount_range_expr <- paste0(transactions_sheet_name, "!D2:D")

  paste0(
    "=ARRAYFORMULA({",
    months_expr, ",",
    "SUMIF(", month_range_expr, ",", months_expr, ",", amount_range_expr, "),",
    "COUNTIF(", month_range_expr, ",", months_expr, "),",
    "SUMIF(", month_range_expr, ",", months_expr, ",", amount_range_expr, ")*2/3,",
    "SUMIF(", month_range_expr, ",", months_expr, ",", amount_range_expr, ")/3",
    "})"
  )
}

upload_formula_summary_sheet <- function(
  ss,
  sheet_name,
  transactions_sheet_name = "Transactions"
) {
  existing_sheets <- googlesheets4::sheet_names(ss)
  if (!(sheet_name %in% existing_sheets)) {
    googlesheets4::sheet_add(ss, sheet = sheet_name)
  }

  formula_cell <- data.frame(
    value = googlesheets4::gs4_formula(summary_formula(transactions_sheet_name))
  )
  header_row <- data.frame(
    month = "month",
    b_total = "b_total",
    b_count = "b_count",
    c_share = "c_share",
    v_share = "v_share"
  )

  googlesheets4::range_clear(ss = ss, sheet = sheet_name)
  googlesheets4::range_write(
    ss = ss,
    sheet = sheet_name,
    range = "A1",
    data = header_row,
    col_names = FALSE,
    reformat = FALSE
  )
  googlesheets4::range_write(
    ss = ss,
    sheet = sheet_name,
    range = "A2",
    data = formula_cell,
    col_names = FALSE,
    reformat = FALSE
  )
  googlesheets4::range_autofit(ss = ss, sheet = sheet_name, dimension = "columns")
}

main <- function(args) {
  if (length(args) != 2) {
    cat("Usage: Rscript upload_chase_tx_to_gsheet.R <workbook.xlsx> <google-sheet-url>\n")
    quit(status = 1)
  }

  xlsx_path <- args[1]
  ss <- args[2]

  googlesheets4::gs4_auth(cache = TRUE)

  tx <- shared_transactions(read_workbook_sheet(xlsx_path, "Transactions"))
  metadata <- read_workbook_sheet(xlsx_path, "Metadata")

  upload_sheet(ss, "Transactions", tx)
  upload_formula_summary_sheet(ss, "Monthly B Summary", "Transactions")
  upload_sheet(ss, "Metadata", metadata)

  cat("Uploaded workbook tabs to Google Sheet:\n")
  cat(" - Transactions\n")
  cat(" - Monthly B Summary\n")
  cat(" - Metadata\n")
}

if (sys.nframe() == 0) {
  main(commandArgs(trailingOnly = TRUE))
}
