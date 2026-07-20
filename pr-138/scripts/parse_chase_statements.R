#!/usr/bin/env Rscript

# Parse Chase credit card statement PDFs and extract transactions.
#
# Usage:
#   Rscript parse_chase_statements.R <pdf> [<pdf> ...]
#   Rscript parse_chase_statements.R --rules config/chase_owner_rules.csv --out transactions.xlsx ~/Downloads/Statements*.pdf
#
# Output columns: file, date, merchant, amount, type, owner
#   - date    : Date (inferred year from statement Opening/Closing Date)
#   - merchant: merchant name / transaction description
#   - amount  : numeric; positive = purchase, negative = payment/credit
#   - type    : "PAYMENT"/"PURCHASE"/"CASH ADVANCE"/"FEE"/"INTEREST" (section header)
#   - owner   : rule-matched owner; positive unmatched purchases default to "b"; payments blank

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install ", package, ": install.packages('", package, "')", call. = FALSE)
  }
}

empty_rules <- function() {
  data.frame(
    pattern = character(),
    owner = character(),
    label = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )
}

read_owner_rules <- function(path = NULL) {
  require_package("readr")

  if (is.null(path) || !nzchar(path)) {
    return(empty_rules())
  }
  if (!file.exists(path)) {
    stop("Rules file does not exist: ", path, call. = FALSE)
  }

  rules <- readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  required <- c("pattern", "owner", "label", "notes")
  missing <- setdiff(required, names(rules))
  if (length(missing) > 0) {
    stop("Rules file is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  rules <- as.data.frame(rules[required], stringsAsFactors = FALSE)
  rules$pattern <- trimws(rules$pattern)
  rules$owner <- trimws(rules$owner)
  rules <- rules[nzchar(rules$pattern), , drop = FALSE]
  invalid_owner <- !rules$owner %in% c("", "b", "c")
  if (any(invalid_owner)) {
    stop("Rules file owner values must be blank, 'b', or 'c'", call. = FALSE)
  }
  rules
}

classify_owner <- function(merchant, amount, type, owner_rules = empty_rules()) {
  if (is.na(amount) || amount <= 0 || identical(type, "PAYMENT")) {
    return("")
  }

  for (i in seq_len(nrow(owner_rules))) {
    if (grepl(owner_rules$pattern[[i]], merchant, ignore.case = TRUE)) {
      return(owner_rules$owner[[i]])
    }
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
  require_package("readr")

  summary_file <- summary_path(out_file)

  summary_table(all_tx) |>
    readr::write_csv(summary_file)

  summary_file
}

summary_table <- function(all_tx) {
  shared <- all_tx[!is.na(all_tx$owner) & all_tx$owner == "b", , drop = FALSE]
  if (nrow(shared) == 0) {
    return(data.frame(
      month = character(),
      b_total = numeric(),
      b_count = integer(),
      c_share = numeric(),
      v_share = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  shared$month <- format(shared$date, "%Y-%m")
  totals <- aggregate(amount ~ month, shared, sum)
  counts <- aggregate(amount ~ month, shared, length)
  names(totals)[names(totals) == "amount"] <- "b_total"
  names(counts)[names(counts) == "amount"] <- "b_count"

  summary <- merge(totals, counts, by = "month", sort = TRUE)
  summary$b_total <- round(summary$b_total, 2)
  summary$c_share <- round(summary$b_total * 2 / 3, 2)
  summary$v_share <- round(summary$b_total / 3, 2)
  summary[, c("month", "b_total", "b_count", "c_share", "v_share")]
}

rule_summary <- function(owner_rules) {
  if (nrow(owner_rules) == 0) {
    return("none")
  }
  labels <- owner_rules$label[nzchar(owner_rules$label)]
  if (length(labels) == 0) {
    return(paste0(nrow(owner_rules), " rule(s)"))
  }
  paste(unique(labels), collapse = ", ")
}

metadata_table <- function(all_tx, source_files, owner_rules = empty_rules()) {
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
      "owner_rule_count",
      "owner_rule_labels",
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
      nrow(owner_rules),
      rule_summary(owner_rules),
      "c=2/3, v=1/3"
    ),
    stringsAsFactors = FALSE
  )
}

write_xlsx_workbook <- function(all_tx, out_file, source_files, owner_rules = empty_rules()) {
  require_package("openxlsx")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Transactions")
  openxlsx::writeData(wb, "Transactions", all_tx)

  openxlsx::addWorksheet(wb, "Monthly B Summary")
  openxlsx::writeData(wb, "Monthly B Summary", summary_table(all_tx))

  openxlsx::addWorksheet(wb, "Metadata")
  openxlsx::writeData(wb, "Metadata", metadata_table(all_tx, source_files, owner_rules))

  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
}

parse_chase_statement_text <- function(text, source_file = "statement.txt", owner_rules = empty_rules()) {
  # Statement period: "Opening/Closing Date   MM/DD/YY - MM/DD/YY"
  period_match <- regmatches(
    text,
    regexpr(
      "Opening/Closing Date[[:space:]]+([0-9]{2}/[0-9]{2}/[0-9]{2})[[:space:]]*-[[:space:]]*([0-9]{2}/[0-9]{2}/[0-9]{2})",
      text
    )
  )
  if (length(period_match) == 0) {
    stop("Could not find Opening/Closing Date in ", source_file)
  }
  dates <- regmatches(
    period_match,
    gregexpr("[0-9]{2}/[0-9]{2}/[0-9]{2}", period_match)
  )[[1]]
  open_date <- as.Date(dates[1], format = "%m/%d/%y")
  close_date <- as.Date(dates[2], format = "%m/%d/%y")
  open_year <- as.integer(format(open_date, "%Y"))
  close_year <- as.integer(format(close_date, "%Y"))

  lines <- unlist(strsplit(text, "\n"))

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
      owner <- classify_owner(merchant, amount, type, owner_rules)

      records[[length(records) + 1]] <- data.frame(
        file = basename(source_file),
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

parse_chase_statement <- function(pdf_path, owner_rules = empty_rules()) {
  require_package("pdftools")

  parse_chase_statement_text(
    paste(pdftools::pdf_text(pdf_path), collapse = "\n"),
    source_file = pdf_path,
    owner_rules = owner_rules
  )
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

parse_cli_args <- function(args) {
  parsed <- list(out_file = NULL, rules_file = NULL, pdfs = character())
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--out") {
      if (i == length(args)) stop("--out requires a value", call. = FALSE)
      parsed$out_file <- args[[i + 1]]
      i <- i + 2
    } else if (arg == "--rules") {
      if (i == length(args)) stop("--rules requires a value", call. = FALSE)
      parsed$rules_file <- args[[i + 1]]
      i <- i + 2
    } else {
      parsed$pdfs <- c(parsed$pdfs, arg)
      i <- i + 1
    }
  }
  parsed
}

main <- function(args) {
  parsed <- parse_cli_args(args)
  if (length(parsed$pdfs) == 0) {
    cat("Usage: Rscript parse_chase_statements.R [--rules rules.csv] [--out out.xlsx] <pdf> [<pdf> ...]\n")
    quit(status = 1)
  }

  owner_rules <- read_owner_rules(parsed$rules_file)
  all_tx <- do.call(rbind, lapply(parsed$pdfs, parse_chase_statement, owner_rules = owner_rules))
  all_tx <- all_tx[order(all_tx$date, all_tx$file), ]

  if (!is.null(parsed$out_file)) {
    if (tolower(tools::file_ext(parsed$out_file)) == "xlsx") {
      write_xlsx_workbook(all_tx, parsed$out_file, parsed$pdfs, owner_rules)
      cat("Wrote", nrow(all_tx), "transactions to workbook", parsed$out_file, "\n")
      cat("Workbook tabs: Transactions, Monthly B Summary, Metadata\n")
    } else {
      utils::write.csv(all_tx, parsed$out_file, row.names = FALSE)
      summary_file <- write_b_summary(all_tx, parsed$out_file)
      cat("Wrote", nrow(all_tx), "transactions to", parsed$out_file, "\n")
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
