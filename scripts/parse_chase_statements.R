#!/usr/bin/env Rscript

# Parse Chase credit card statement PDFs and extract transactions.
#
# Usage:
#   Rscript parse_chase_statements.R <pdf> [<pdf> ...]
#   Rscript parse_chase_statements.R --out transactions.csv ~/Downloads/Statements*.pdf
#
# Output columns: file, date, merchant, amount, type
#   - date    : Date (inferred year from statement Opening/Closing Date)
#   - merchant: merchant name / transaction description
#   - amount  : numeric; positive = purchase, negative = payment/credit
#   - type    : "PAYMENT"/"PURCHASE"/"CASH ADVANCE"/"FEE"/"INTEREST" (section header)

suppressPackageStartupMessages({
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Install pdftools: install.packages('pdftools')")
  }
})

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

      records[[length(records) + 1]] <- data.frame(
        file = basename(pdf_path),
        date = tx_date,
        merchant = merchant,
        amount = amount,
        type = type,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(records) == 0) {
    return(data.frame(
      file = character(), date = as.Date(character()),
      merchant = character(), amount = numeric(), type = character()
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
    cat("Usage: Rscript parse_chase_statements.R [--out out.csv] <pdf> [<pdf> ...]\n")
    quit(status = 1)
  }

  all_tx <- do.call(rbind, lapply(args, parse_chase_statement))
  all_tx <- all_tx[order(all_tx$date, all_tx$file), ]

  if (!is.null(out_file)) {
    utils::write.csv(all_tx, out_file, row.names = FALSE)
    cat("Wrote", nrow(all_tx), "transactions to", out_file, "\n")
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
