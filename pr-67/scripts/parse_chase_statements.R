#!/usr/bin/env Rscript

# Parse Chase credit card statement PDFs and extract transactions.
#
# Usage:
#   Rscript parse_chase_statements.R <pdf> [<pdf> ...]
#   Rscript parse_chase_statements.R --out transactions.xlsx ~/Downloads/Statements*.pdf
#
# Output columns: file, date, merchant, amount, type, owner
#   - date    : Date (inferred year from statement Opening/Closing Date)
#   - merchant: merchant name / transaction description
#   - amount  : numeric; positive = purchase, negative = payment/credit
#   - type    : "PAYMENT"/"PURCHASE"/"CASH ADVANCE"/"FEE"/"INTEREST" (section header)
#   - owner   : "c" for Cavan-only purchases, "b" for shared purchases, blank for payments

suppressPackageStartupMessages({
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Install pdftools: install.packages('pdftools')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Install dplyr: install.packages('dplyr')")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Install readr: install.packages('readr')")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Install openxlsx: install.packages('openxlsx')")
  }
})

classify_owner <- function(merchant, amount, type) {
  if (is.na(amount) || amount <= 0 || identical(type, "PAYMENT")) {
    return("")
  }

  c_patterns <- c("CHATGPT", "OPENAI", "\\bALO\\b", "MAMMOTH", "SOLIDCORE", "CLASSPASS", "HIGHLINE")
  if (any(grepl(paste(c_patterns, collapse = "|"), merchant, ignore.case = TRUE))) {
    return("c")
  }

  "b"
}

summary_path <- function(out_file) {
  dir_name <- dirname(out_file)
  stem <- tools::file_path_sans_ext(basename(out_file))
  ext <- tools::file_ext(out_file)
  ext <- if (nzchar(ext)) paste0(".", ext) else ""
  file.path(dir_name, paste0(stem, "_b_monthly_summary", ext))
}

write_b_summary <- function(all_tx, out_file) {
  summary_file <- summary_path(out_file)

  summary_table(all_tx) |>
    readr::write_csv(summary_file)

  summary_file
}

summary_table <- function(all_tx) {
  all_tx |>
    dplyr::filter(owner == "b") |>
    dplyr::mutate(month = format(date, "%Y-%m")) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      b_total = round(sum(amount), 2),
      b_count = dplyr::n(),
      c_share = round(b_total * 2 / 3, 2),
      v_share = round(b_total / 3, 2),
      .groups = "drop"
    )
}

metadata_table <- function(all_tx, source_files) {
  data.frame(
    field = c(
      "generated_at",
      "source_files",
      "statement_file_count",
      "transaction_row_count",
      "purchase_row_count",
      "payment_row_count",
      "owner_c_row_count",
      "owner_b_row_count",
      "owner_c_rules",
      "shared_split"
    ),
    value = c(
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      paste(basename(source_files), collapse = ", "),
      length(source_files),
      nrow(all_tx),
      sum(all_tx$amount > 0, na.rm = TRUE),
      sum(all_tx$amount <= 0, na.rm = TRUE),
      sum(all_tx$owner == "c", na.rm = TRUE),
      sum(all_tx$owner == "b", na.rm = TRUE),
      "CHATGPT/OPENAI, ALO, MAMMOTH, SOLIDCORE, CLASSPASS, HIGHLINE",
      "c=2/3, v=1/3"
    ),
    stringsAsFactors = FALSE
  )
}

write_xlsx_workbook <- function(all_tx, out_file, source_files) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Transactions")
  openxlsx::writeData(wb, "Transactions", all_tx)

  openxlsx::addWorksheet(wb, "Monthly B Summary")
  openxlsx::writeData(wb, "Monthly B Summary", summary_table(all_tx))

  openxlsx::addWorksheet(wb, "Metadata")
  openxlsx::writeData(wb, "Metadata", metadata_table(all_tx, source_files))

  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
}

parse_chase_statement <- function(pdf_path) {
  pages <- pdftools::pdf_text(pdf_path)
  full_text <- paste(pages, collapse = "\n")

  # Statement period: "Opening/Closing Date   MM/DD/YY - MM/DD/YY"
  period_match <- regmatches(
    full_text,
    regexpr(
      "Opening/Closing Date[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{2})[[:space:]]*-[[:space:]]*([0-9]{2}/[0-9]{2}/[0-9]{2})",
      full_text
    )
  )
  if (length(period_match) == 0) {
    stop("Could not find Opening/Closing Date in ", pdf_path)
  }
  dates <- regmatches(
    period_match,
    gregexpr("[0-9]{2}/[0-9]{2}/[0-9]{2}", period_match)
  )[[1]]
  open_date <- as.Date(dates[1], format = "%m/%d/%y")
  close_date <- as.Date(dates[2], format = "%m/%d/%y")
  open_year <- as.integer(format(open_date, "%Y"))
  close_year <- as.integer(format(close_date, "%Y"))

  lines <- unlist(strsplit(pages, "\n"))

  # Section headers that group transactions inside ACCOUNT ACTIVITY.
  section_regex <- paste0(
    "^[[:space:]]*(PAYMENTS AND OTHER CREDITS|PURCHASE[S]?|",
    "CASH ADVANCES?|FEES CHARGED|INTEREST CHARGED|",
    "BALANCE TRANSFERS)[[:space:]]*$"
  )

  # Transaction lines start with MM/DD and end with a $ amount.
  tx_regex <- paste0(
    "^[[:space:]]*([0-9]{2}/[0-9]{2})[[:space:]]+",
    "(.+?)[[:space:]]+",
    "(-?[0-9][0-9,]*\\.[0-9]{2})[[:space:]]*$"
  )

  in_activity <- FALSE
  current_section <- NA_character_
  records <- list()

  for (ln in lines) {
    stripped <- trimws(ln)

    if (grepl("^ACCOUNT ACTIVITY", stripped)) {
      in_activity <- TRUE
      next
    }
    if (!in_activity) next

    # End of transaction area: YTD totals, interest tables, etc.
    if (grepl("^20[0-9]{2} Totals Year-to-Date|^INTEREST CHARGES|^Totals Year-to-Date",
              stripped)) {
      in_activity <- FALSE
      next
    }

    if (grepl(section_regex, stripped)) {
      current_section <- sub("[[:space:]]+$", "", stripped)
      next
    }

    m <- regmatches(ln, regexec(tx_regex, ln))[[1]]
    if (length(m) == 4) {
      mmdd <- m[2]
      merchant <- trimws(gsub("[[:space:]]+", " ", m[3]))
      # Leading "& " appears on some payment lines; strip it.
      merchant <- sub("^& ", "", merchant)
      amount <- as.numeric(gsub(",", "", m[4]))

      # Year inference: if month >= opening month use open_year, else close_year.
      mm <- as.integer(substr(mmdd, 1, 2))
      yr <- if (mm >= as.integer(format(open_date, "%m"))) open_year else close_year
      # Handle calendar year rollover across Dec->Jan.
      if (open_year != close_year) {
        open_mm <- as.integer(format(open_date, "%m"))
        yr <- if (mm >= open_mm) open_year else close_year
      }
      tx_date <- as.Date(sprintf("%04d-%02d-%02d", yr, mm, as.integer(substr(mmdd, 4, 5))))

      type <- switch(
        toupper(current_section %||% ""),
        "PAYMENTS AND OTHER CREDITS" = "PAYMENT",
        "PURCHASE" = "PURCHASE",
        "PURCHASES" = "PURCHASE",
        "CASH ADVANCE" = "CASH ADVANCE",
        "CASH ADVANCES" = "CASH ADVANCE",
        "FEES CHARGED" = "FEE",
        "INTEREST CHARGED" = "INTEREST",
        "BALANCE TRANSFERS" = "BALANCE TRANSFER",
        NA_character_
      )
      owner <- classify_owner(merchant, amount, type)

      records[[length(records) + 1]] <- data.frame(
        file = basename(pdf_path),
        date = tx_date,
        merchant = merchant,
        amount = amount,
        type = type,
        owner = owner,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(records) == 0) {
    return(data.frame(
      file = character(), date = as.Date(character()),
      merchant = character(), amount = numeric(), type = character(), owner = character()
    ))
  }
  do.call(rbind, records)
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

main <- function(args) {
  out_file <- NULL
  if (length(args) >= 2 && args[1] == "--out") {
    out_file <- args[2]
    args <- args[-(1:2)]
  }
  if (length(args) == 0) {
    cat("Usage: Rscript parse_chase_statements.R [--out out.xlsx] <pdf> [<pdf> ...]\n")
    quit(status = 1)
  }

  all_tx <- do.call(rbind, lapply(args, parse_chase_statement))
  all_tx <- all_tx[order(all_tx$date, all_tx$file), ]

  if (!is.null(out_file)) {
    if (tolower(tools::file_ext(out_file)) == "xlsx") {
      write_xlsx_workbook(all_tx, out_file, args)
      cat("Wrote", nrow(all_tx), "transactions to workbook", out_file, "\n")
      cat("Workbook tabs: Transactions, Monthly B Summary, Metadata\n")
    } else {
      utils::write.csv(all_tx, out_file, row.names = FALSE)
      summary_file <- write_b_summary(all_tx, out_file)
      cat("Wrote", nrow(all_tx), "transactions to", out_file, "\n")
      cat("Wrote monthly shared summary to", summary_file, "\n")
    }
  } else {
    # Print as a tidy table.
    old <- options(width = 200)
    on.exit(options(old), add = TRUE)
    print(all_tx, row.names = FALSE)
  }

  invisible(all_tx)
}

if (sys.nframe() == 0) {
  main(commandArgs(trailingOnly = TRUE))
}
