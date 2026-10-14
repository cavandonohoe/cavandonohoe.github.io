# Helpers for ordering the homepage project cards by recent traffic.
#
# scripts/update_page_views.R pulls GA4 page views (site pages) and outbound
# click counts (externally hosted apps such as the shinyapps.io calculators)
# and stores only the resulting rank order of page keys in
# data/page_views.json. index.Rmd reads that ranking and calls order_cards()
# so the pinned cards stay first and the rest follow what people visit most.

# Normalize a site path or card href to a comparable key.
#
# "/imdb_rating_plot.html", "/imdb_rating_plot", and "imdb_rating_plot.html"
# all map to "imdb_rating_plot"; "/detour-finder/index.html" and
# "/detour-finder/" map to "detour-finder/"; the homepage maps to "".
# Absolute http(s) URLs keep their scheme and host (lowercased host, no
# trailing slash). PR preview paths ("/pr-123/...") return NA so preview
# traffic never influences production ordering.
card_key <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  x <- sub("[?#].*$", "", x[ok])
  is_url <- grepl("^https?://", x, ignore.case = TRUE)

  url <- x[is_url]
  host <- tolower(sub("^(https?://[^/]+).*$", "\\1", url, ignore.case = TRUE))
  rest <- sub("^https?://[^/]+", "", url, ignore.case = TRUE)
  url <- sub("/+$", "", paste0(host, rest))

  path <- sub("^/+", "", x[!is_url])
  path <- sub("(^|/)index(\\.html)?$", "\\1", path)
  path <- sub("\\.html$", "", path)
  path[grepl("^pr-[0-9]+(/|$)", path)] <- NA_character_

  res <- character(length(x))
  res[is_url] <- url
  res[!is_url] <- path
  out[ok] <- res
  out
}

# Combine GA4 report rows into a ranking of keys, most visited first.
#
# `pages` and `clicks` are data frames with columns `path` / `url` and
# `count`. Rows that normalize to the same key are summed. Keys with no
# traffic, PR-preview paths, and the homepage are dropped. Ties keep the
# alphabetical key order so the output is deterministic.
rank_card_keys <- function(pages, clicks = NULL) {
  frames <- list(
    data.frame(key = card_key(pages$path), count = as.numeric(pages$count))
  )
  if (!is.null(clicks) && nrow(clicks) > 0) {
    frames[[2]] <- data.frame(key = card_key(clicks$url), count = as.numeric(clicks$count))
  }
  hits <- do.call(rbind, frames)
  hits <- hits[!is.na(hits$key) & hits$key != "" & !is.na(hits$count), , drop = FALSE]
  if (nrow(hits) == 0) {
    return(character())
  }
  totals <- tapply(hits$count, hits$key, sum)
  totals <- totals[totals > 0]
  keys <- names(totals)
  keys[order(-totals, keys)]
}

# Read the committed ranking. Returns character() when the file is missing
# or unreadable, which leaves the manual card order untouched.
read_card_ranking <- function(path = "data/page_views.json") {
  if (!file.exists(path)) {
    return(character())
  }
  data <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
  ranking <- data$ranking
  if (is.null(ranking)) character() else as.character(ranking)
}

# Order cards: pinned hrefs first (in the given order), then everything else
# by traffic rank. Cards without traffic data keep their original relative
# order after the ranked ones. Each card is a list whose first element is its
# href.
order_cards <- function(cards, ranking = character(), pinned = character()) {
  if (length(cards) == 0) {
    return(cards)
  }
  keys <- card_key(vapply(cards, function(card) card[[1]], character(1)))
  pinned_idx <- unique(stats::na.omit(match(card_key(pinned), keys)))
  rest <- setdiff(seq_along(cards), pinned_idx)
  rank <- match(keys[rest], ranking)
  rank[is.na(rank)] <- Inf
  cards[c(pinned_idx, rest[order(rank, rest)])]
}
