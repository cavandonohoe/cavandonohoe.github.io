#!/usr/bin/env Rscript

source("scripts/parse_chase_statements.R")
source("scripts/upload_chase_tx_to_gsheet.R")

fixture <- paste(readLines("tests/fixtures/chase_statement_synthetic.txt"), collapse = "\n")
rules <- data.frame(
  pattern = c("SOLO COFFEE", "PRIVATE TRAIN", "SYNTHETIC ANNUAL FEE"),
  owner = c("c", "c", "c"),
  label = c("solo", "transit", "fee"),
  notes = c("Synthetic personal merchant", "Synthetic transit merchant", "Synthetic fee"),
  stringsAsFactors = FALSE
)

tx <- parse_chase_statement_text(fixture, "synthetic-statement.txt", rules)

stopifnot(nrow(tx) == 6)
stopifnot(all(c("file", "date", "merchant", "amount", "type", "owner") %in% names(tx)))

payment <- tx[tx$type == "PAYMENT", ]
stopifnot(nrow(payment) == 1)
stopifnot(identical(payment$owner[[1]], ""))

shared <- tx[tx$merchant == "HOUSEHOLD MARKET EXAMPLE CA", ]
stopifnot(nrow(shared) == 1)
stopifnot(identical(shared$owner[[1]], "b"))

unmatched <- tx[tx$merchant == "UNMATCHED BOOKSHOP EXAMPLE CA", ]
stopifnot(nrow(unmatched) == 1)
stopifnot(identical(unmatched$owner[[1]], "b"))

solo <- tx[tx$merchant == "SOLO COFFEE EXAMPLE CA", ]
stopifnot(nrow(solo) == 1)
stopifnot(identical(solo$owner[[1]], "c"))

fee <- tx[tx$merchant == "SYNTHETIC ANNUAL FEE", ]
stopifnot(nrow(fee) == 1)
stopifnot(identical(fee$owner[[1]], "c"))

summary <- summary_table(tx)
stopifnot(nrow(summary) == 1)
stopifnot(summary$month[[1]] == "2026-04")
stopifnot(summary$b_count[[1]] == 2)
stopifnot(summary$b_total[[1]] == 58.50)
stopifnot(summary$c_share[[1]] == 39.00)
stopifnot(summary$v_share[[1]] == 19.50)

formula <- summary_formula("Transactions")
stopifnot(grepl("where Col3 = 'b'", formula, fixed = TRUE))
stopifnot(grepl("QUERY", formula, fixed = TRUE))

cat("Synthetic Chase parser tests passed.\n")
