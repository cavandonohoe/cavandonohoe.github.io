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

sheet_id_by_name <- function(ss, sheet_name) {
  sheets <- googlesheets4::gs4_get(ss)$sheets
  match_idx <- match(sheet_name, sheets$name)
  if (is.na(match_idx)) {
    stop("Could not find sheet named ", sheet_name)
  }
  sheets$id[[match_idx]]
}

set_owner_b_filter <- function(ss, sheet_name, n_cols) {
  owner_col_idx <- 6
  criteria <- setNames(
    list(
      list(
        hiddenValues = list("", "c")
      )
    ),
    as.character(owner_col_idx)
  )
  request <- googlesheets4::request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = googlesheets4::as_sheets_id(ss),
      requests = list(
        list(
          setBasicFilter = list(
            filter = list(
              range = list(
                sheetId = sheet_id_by_name(ss, sheet_name),
                startRowIndex = 0,
                startColumnIndex = 0,
                endColumnIndex = n_cols
              ),
              criteria = criteria
            )
          )
        )
      )
    )
  )
  googlesheets4::request_make(request)
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
  paste0(
    "=QUERY({",
    "TEXT(", transactions_sheet_name, "!B2:B,\"yyyy-mm\"),",
    transactions_sheet_name, "!D2:D,",
    transactions_sheet_name, "!F2:F",
    "},",
    "\"select Col1, sum(Col2), count(Col2), sum(Col2) * 2 / 3, sum(Col2) / 3 ",
    "where Col3 = 'b' and Col1 is not null ",
    "group by Col1 order by Col1 ",
    "label Col1 'month', ",
    "sum(Col2) 'b_total', ",
    "count(Col2) 'b_count', ",
    "sum(Col2) * 2 / 3 'c_share', ",
    "sum(Col2) / 3 'v_share'\"",
    ",0)"
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

  googlesheets4::range_clear(ss = ss, sheet = sheet_name)
  googlesheets4::range_write(
    ss = ss,
    sheet = sheet_name,
    range = "A1",
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

  tx <- read_workbook_sheet(xlsx_path, "Transactions")
  metadata <- read_workbook_sheet(xlsx_path, "Metadata")

  upload_sheet(ss, "Transactions", tx)
  set_owner_b_filter(ss, "Transactions", ncol(tx))
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
