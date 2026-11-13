#!/usr/bin/env Rscript
# scripts/build_la_times_101.R
#
# Rebuilds data/la_times_101.json (the L.A. Times 101 tracker) in two stages:
#
#   1. scrape: read the restaurant names from each edition's published list
#      and snapshot them in data/la_times_101_source_names.json
#   2. derive: turn the snapshot plus the hand-kept data/la_times_101_overrides.csv
#      into the per-edition rows (first appearance, comeback, previous edition)
#
# Usage (from the repo root):
#   Rscript scripts/build_la_times_101.R            # derive from the snapshot
#   Rscript scripts/build_la_times_101.R --scrape   # re-fetch pages first
#   Rscript scripts/build_la_times_101.R --scrape --cache-dir /tmp/la101
#
# Adding an edition: add one row to la101_editions() (use the "itemlist" parser
# if the page is a latimes.com/food/list page), run with --scrape, then add any
# alias / match rows the new names need to the overrides CSV and the geography
# JSON. The build stops with a clear message when either is incomplete.

la101_source_path <- "data/la_times_101_source_names.json"
la101_overrides_path <- "data/la_times_101_overrides.csv"
la101_geography_path <- "data/la_times_101_geography.json"
la101_output_path <- "data/la_times_101.json"
la101_slots <- 101L
la101_statuses <- c("First appearance", "Comeback", "Previous edition", "Other")

la101_editions <- function() {
  ballots <- "https://ballots.latimes.com/lists/"
  projects <- "https://www.latimes.com/projects/"
  lists <- "https://www.latimes.com/food/list/"
  data.frame(
    year = 2013:2026,
    parser = c(
      rep("ballots", 5), "slides", rep("review", 2), rep("itemlist", 6)
    ),
    # Direction the published ranking runs down the page: "asc" lists No. 1 first,
    # "desc" counts down to No. 1. NA marks the unranked 2018, 2020 and 2021 editions.
    ranking = c(
      "desc", "desc", "asc", "asc", "asc", NA, "asc", NA, NA,
      "asc", "asc", "desc", "desc", "desc"
    ),
    source = c(
      paste0(ballots, "101-best-restaurants-jonathan-gold-2013-list/"),
      paste0(ballots, "101-best-restaurants-jonathan-gold-2014/"),
      paste0(ballots, "jonathan-golds-101-best-restaurants-2015/"),
      paste0(ballots, "jonathan-golds-101-best-restaurants-2016/"),
      paste0(ballots, "jonathan-golds-101-best-restaurants-2017/"),
      paste0(projects, "la-fo-101-restaurants-we-love-2018/"),
      paste0(projects, "101-best-la-restaurants-2019/"),
      paste0(projects, "101-best-la-restaurants-2020/"),
      paste0(lists, "best-los-angeles-restaurants-2021"),
      paste0(lists, "101-best-los-angeles-restaurants-ranked-2022"),
      paste0(lists, "101-best-los-angeles-restaurants-ranked-2023"),
      paste0(lists, "101-best-restaurants-los-angeles-2024"),
      paste0(lists, "101-best-restaurants-los-angeles-2025"),
      paste0(lists, "101-best-restaurants-los-angeles")
    ),
    stringsAsFactors = FALSE
  )
}

# ---- Stage 1: scrape ---------------------------------------------------------

la101_user_agent <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

la101_fetch_html <- function(url, cache_file = NULL, retries = 3, pause_seconds = 2) {
  if (!is.null(cache_file) && file.exists(cache_file)) {
    return(paste(readLines(cache_file, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
  }
  for (attempt in seq_len(retries)) {
    resp <- tryCatch(
      httr::GET(url, httr::user_agent(la101_user_agent), httr::timeout(60)),
      error = function(e) NULL
    )
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      html <- httr::content(resp, as = "text", encoding = "UTF-8")
      if (!is.null(cache_file)) {
        dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
        writeLines(html, cache_file, useBytes = TRUE)
      }
      return(html)
    }
    Sys.sleep(pause_seconds * attempt)
  }
  stop("Could not fetch ", url, " after ", retries, " attempts")
}

# Names as published: collapse whitespace (incl. non-breaking), drop "12. " prefixes.
la101_clean_name <- function(x) {
  x <- gsub("[\\s\u00a0]+", " ", x, perl = TRUE)
  x <- trimws(x)
  sub("^[0-9]+\\.\\s+", "", x)
}

la101_itemlist_names <- function(doc) {
  scripts <- xml2::xml_text(
    xml2::xml_find_all(doc, "//script[@type='application/ld+json']")
  )
  for (txt in scripts) {
    parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.list(parsed) && identical(parsed[["@type"]], "ItemList")) {
      return(vapply(parsed$itemListElement, function(it) it$item$name, character(1)))
    }
  }
  character(0)
}

# One parser per page template the Times has used.
la101_parse_names <- function(html, parser) {
  doc <- xml2::read_html(html)
  raw <- switch(
    parser,
    ballots = xml2::xml_text(xml2::xml_find_all(doc, "//h2/span[@itemprop='name']")),
    slides = xml2::xml_attr(
      xml2::xml_find_all(doc, "//div[contains(@class,'slide')]"), "data-name"
    ),
    review = xml2::xml_text(xml2::xml_find_all(doc, "//h2[contains(@class,'review__name')]")),
    itemlist = la101_itemlist_names(doc),
    stop("Unknown parser: ", parser)
  )
  la101_clean_name(raw[!is.na(raw)])
}

la101_scrape <- function(editions = la101_editions(), cache_dir = NULL, pause_seconds = 1) {
  out <- list()
  for (i in seq_len(nrow(editions))) {
    year <- editions$year[i]
    cache_file <- if (is.null(cache_dir)) NULL else file.path(cache_dir, paste0(year, ".html"))
    message("Fetching ", year, ": ", editions$source[i])
    html <- la101_fetch_html(editions$source[i], cache_file)
    names <- la101_parse_names(html, editions$parser[i])
    if (length(names) != la101_slots) {
      stop(year, ": expected ", la101_slots, " names, parsed ", length(names),
           ". The page template may have changed; update la101_parse_names().")
    }
    out[[as.character(year)]] <- list(source = editions$source[i], names = as.list(names))
    Sys.sleep(pause_seconds)
  }
  out
}

# Report names that changed against the previous snapshot (the Times edits lists).
la101_snapshot_diff <- function(old, new) {
  for (year in names(new)) {
    before <- unlist(old[[year]]$names)
    after <- unlist(new[[year]]$names)
    if (is.null(before)) {
      message(year, ": new edition")
    } else if (!identical(sort(before), sort(after))) {
      message(year, ": removed [", paste(setdiff(before, after), collapse = "; "),
              "] added [", paste(setdiff(after, before), collapse = "; "), "]")
    }
  }
}

# ---- Stage 2: derive ---------------------------------------------------------

# Lower-case with accents removed. Code points are escapes so it works in any locale.
la101_fold <- function(x) {
  accented <- c(
    0xe1, 0xe0, 0xe2, 0xe4, 0xe3, 0xe5, 0xe9, 0xe8, 0xea, 0xeb, 0xed, 0xec, 0xee, 0xef,
    0xf3, 0xf2, 0xf4, 0xf6, 0xf5, 0xfa, 0xf9, 0xfb, 0xfc, 0xfd, 0xff, 0xf1, 0xe7
  )
  plain <- "aaaaaaeeeeiiiiooooouuuuyync"
  lower <- intToUtf8(accented)
  upper <- intToUtf8(accented[accented != 0xff] - 0x20)
  x <- chartr(lower, plain, enc2utf8(x))
  x <- chartr(upper, sub("y", "", plain, fixed = TRUE), x)
  tolower(x)
}

# Spelling-insensitive identity: folds accents, quotes, "&"/"and", a leading "The".
la101_key <- function(x) {
  x <- gsub("&", " and ", la101_fold(x), fixed = TRUE)
  x <- sub("^the ", "", trimws(x))
  gsub("[^a-z0-9]", "", x)
}

la101_override_columns <- c(
  "year", "source_name", "name", "links_to", "match_label", "status", "note", "reason"
)

la101_read_overrides <- function(path = la101_overrides_path) {
  ov <- utils::read.csv(path, colClasses = "character", na.strings = "",
                        encoding = "UTF-8", stringsAsFactors = FALSE)
  if (!identical(names(ov), la101_override_columns)) {
    stop("Overrides columns must be: ", toString(la101_override_columns))
  }
  if (anyDuplicated(ov[c("year", "source_name")])) stop("Duplicate (year, source_name) overrides")
  ov
}

# "A; B; +C" -> existing = c("A", "B"), created = "C". A leading "+" declares a component
# that first appears inside this (shared) slot. "!new" marks a different restaurant that
# happens to share a name with an earlier entry (separately reviewed branches).
la101_split_links <- function(x) {
  parts <- if (is.na(x)) character(0) else trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  is_new <- startsWith(parts, "+")
  fresh <- parts == "!new"
  list(
    existing = parts[!is_new & !fresh],
    created = trimws(sub("^\\+", "", parts[is_new])),
    fresh = any(fresh)
  )
}

# Published rank for each scraped name, in page order (NA when the edition is unranked).
la101_ranks <- function(year, n, editions = la101_editions()) {
  dir <- editions$ranking[match(year, editions$year)]
  if (length(dir) == 0 || is.na(dir)) return(rep(NA_integer_, n))
  if (dir == "asc") seq_len(n) else rev(seq_len(n))
}

# Turns the scraped names into rows. Identity is a spelling-insensitive name key;
# overrides handle renames (links_to) and shared slots (several names in links_to,
# the first being the one later spellings attach to). A row's first/previous come from
# every entity it links to: earliest first year, latest previous year.
la101_derive <- function(source_names, overrides) {
  reg <- list() # entity id -> list(first, last, names = list(year -> names))
  look <- list() # identity key -> entity id
  result <- list()
  used <- character(0)

  for (year in sort(as.integer(names(source_names)))) {
    ykey <- as.character(year)
    snap <- reg
    look0 <- look
    src <- unlist(source_names[[ykey]]$names)
    ov <- overrides[overrides$year == year, , drop = FALSE]
    plans <- vector("list", length(src))

    for (i in seq_along(src)) {
      o <- ov[match(src[i], ov$source_name), , drop = FALSE]
      has_o <- nrow(o) == 1 && !is.na(o$source_name)
      if (has_o) used <- c(used, paste(year, src[i], sep = "|"))
      name <- if (has_o && !is.na(o$name)) o$name else src[i]
      note <- if (has_o) o$note else NA_character_
      links <- la101_split_links(if (has_o) o$links_to else NA_character_)

      if (has_o && identical(o$status, "Other")) {
        plans[[i]] <- list(name = name, other = TRUE, note = note)
        next
      }
      own <- if (links$fresh) NULL else look0[[la101_key(name)]]
      linked <- vapply(links$existing, function(n) {
        id <- look0[[la101_key(n)]]
        if (is.null(id)) stop(year, " ", name, ": links_to '", n, "' is not in an earlier edition")
        id
      }, character(1))
      members <- unique(c(own, unname(linked)))
      members <- members[members %in% names(snap)]

      row <- list(name = name, note = note, first = year, previous = NA, match = NA_character_,
                  status = "First appearance")
      if (length(members)) {
        firsts <- vapply(members, function(m) snap[[m]]$first, numeric(1))
        lasts <- vapply(members, function(m) snap[[m]]$last, numeric(1))
        prev <- max(lasts)
        prior <- unlist(lapply(members[lasts == prev], function(m) {
          snap[[m]]$names[[as.character(prev)]]
        }))
        same <- la101_key(prior) == la101_key(name)
        row$first <- min(firsts)
        row$previous <- prev
        row$match <- if (has_o && !is.na(o$match_label)) o$match_label else
          if (any(same)) prior[same][1] else prior[1]
        row$status <- if (prev == year - 1L) "Previous edition" else "Comeback"
      }
      plans[[i]] <- list(row = row, own = own, members = members, created = links$created)
    }

    rows <- vector("list", length(src))
    ranks <- la101_ranks(year, length(src))
    for (i in seq_along(src)) {
      plan <- plans[[i]]
      if (isTRUE(plan$other)) {
        rows[[i]] <- list(name = plan$name, status = "Other", first = NA, previous = NA,
                          match = NA_character_, note = plan$note, rank = ranks[i])
        next
      }
      rows[[i]] <- c(plan$row[c("name", "status", "first", "previous", "match", "note")],
                     rank = ranks[i])
      # Register the row: members and created components are seen this edition.
      ids <- plan$members
      for (n in plan$created) {
        id <- look[[la101_key(n)]]
        if (is.null(id)) {
          id <- as.character(length(reg) + 1L)
          reg[[id]] <- list(first = year, last = year, names = list())
          look[[la101_key(n)]] <- id
        }
        ids <- c(ids, id)
      }
      primary <- if (!is.null(plan$own)) plan$own else if (length(plan$members)) plan$members[1]
      if (is.null(primary)) {
        primary <- as.character(length(reg) + 1L)
        reg[[primary]] <- list(first = year, last = year, names = list())
        ids <- c(ids, primary)
      }
      look[[la101_key(plan$row$name)]] <- primary
      for (id in unique(c(ids, primary))) {
        entry <- reg[[id]]
        entry$last <- year
        entry$names[[ykey]] <- c(entry$names[[ykey]], plan$row$name)
        reg[[id]] <- entry
      }
    }

    sort_key <- gsub("[\u2018\u2019]", "'", la101_fold(
      vapply(rows, function(r) r$name, character(1))
    ))
    rows <- rows[order(sort_key, method = "radix")]
    statuses <- vapply(rows, function(r) r$status, character(1))
    result[[ykey]] <- list(
      source = source_names[[ykey]]$source,
      counts = as.integer(table(factor(statuses, levels = la101_statuses))),
      rows = rows
    )
  }

  stale <- setdiff(paste(overrides$year, overrides$source_name, sep = "|"), used)
  if (length(stale)) {
    stop("Overrides that match no scraped name (stale or misspelled): ", toString(stale))
  }
  result
}

la101_validate <- function(editions) {
  total <- 0L
  for (year in names(editions)) {
    rows <- editions[[year]]$rows
    nm <- vapply(rows, function(r) r$name, character(1))
    st <- vapply(rows, function(r) r$status, character(1))
    if (length(rows) != la101_slots) stop(year, ": ", length(rows), " rows, expected ", la101_slots)
    if (anyDuplicated(nm)) stop(year, ": duplicate names: ", toString(nm[duplicated(nm)]))
    if (!all(st %in% la101_statuses)) stop(year, ": unknown status")
    if (sum(editions[[year]]$counts) != la101_slots) stop(year, ": counts do not sum to 101")
    total <- total + length(rows)
  }
  if (total != la101_slots * length(editions)) stop("Total slots do not match editions")
  invisible(total)
}

la101_check_geography <- function(editions, path = la101_geography_path) {
  geo <- jsonlite::fromJSON(path, simplifyVector = FALSE)$editions
  for (year in names(editions)) {
    nm <- vapply(editions[[year]]$rows, function(r) r$name, character(1))
    missing <- setdiff(nm, names(geo[[year]]))
    if (length(missing)) {
      stop(year, ": no neighborhood in ", path, " for: ", paste(missing, collapse = "; "))
    }
  }
  invisible(TRUE)
}

# ---- Output (same layout as the committed file, so diffs stay readable) ------

la101_json_value <- function(x) {
  if (length(x) == 0 || is.na(x)) return("null")
  if (is.numeric(x)) return(as.character(as.integer(x)))
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
}

la101_to_json <- function(editions) {
  blocks <- vapply(names(editions), function(year) {
    ed <- editions[[year]]
    rows <- vapply(ed$rows, function(r) {
      sprintf(
        paste0(
          '      {"name":%s,"status":%s,"first":%s,"previous":%s,"match":%s,"note":%s,',
          '"rank":%s}'
        ),
        la101_json_value(r$name), la101_json_value(r$status), la101_json_value(r$first),
        la101_json_value(r$previous), la101_json_value(r$match), la101_json_value(r$note),
        la101_json_value(r$rank)
      )
    }, character(1))
    paste0(
      '  "', year, '": {\n',
      '    "source": ', la101_json_value(ed$source), ",\n",
      '    "counts": [', paste(ed$counts, collapse = ","), "],\n",
      '    "rows": [\n', paste(rows, collapse = ",\n"), "\n    ]\n  }"
    )
  }, character(1))
  paste0("{\n", paste(blocks, collapse = ",\n"), "\n}\n")
}

la101_read_snapshot <- function(path = la101_source_path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

la101_write_snapshot <- function(snapshot, path = la101_source_path) {
  lines <- vapply(names(snapshot), function(year) {
    nm <- vapply(snapshot[[year]]$names, la101_json_value, character(1))
    paste0('  "', year, '": {\n    "source": ', la101_json_value(snapshot[[year]]$source),
           ',\n    "names": [\n      ', paste(nm, collapse = ",\n      "), "\n    ]\n  }")
  }, character(1))
  writeLines(paste0("{\n", paste(lines, collapse = ",\n"), "\n}"), path, useBytes = TRUE)
}

la101_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if ("--scrape" %in% args) {
    cache_dir <- NULL
    if ("--cache-dir" %in% args) cache_dir <- args[which(args == "--cache-dir") + 1L]
    old <- if (file.exists(la101_source_path)) la101_read_snapshot() else list()
    fresh <- la101_scrape(cache_dir = cache_dir)
    la101_snapshot_diff(old, fresh)
    la101_write_snapshot(fresh)
  }
  editions <- la101_derive(la101_read_snapshot(), la101_read_overrides())
  la101_validate(editions)
  la101_check_geography(editions)
  writeLines(la101_to_json(editions), la101_output_path, sep = "", useBytes = TRUE)
  message("Wrote ", la101_output_path, " (", length(editions), " editions)")
  invisible(editions)
}

if (sys.nframe() == 0L) la101_main()
