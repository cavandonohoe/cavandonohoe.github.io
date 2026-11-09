source("../../scripts/build_la_times_101.R")

repo_path <- function(path) file.path("../../", path)

committed_snapshot <- function() la101_read_snapshot(repo_path(la101_source_path))
committed_overrides <- function() la101_read_overrides(repo_path(la101_overrides_path))

edition <- function(...) list(source = "https://example.com", names = as.list(c(...)))

no_overrides <- function() {
  ov <- data.frame(matrix(character(0), 0, length(la101_override_columns)))
  names(ov) <- la101_override_columns
  ov
}

override <- function(year, source_name, ...) {
  row <- as.list(setNames(rep(NA_character_, length(la101_override_columns)),
                          la101_override_columns))
  row$year <- as.character(year)
  row$source_name <- source_name
  row$name <- source_name
  extra <- list(...)
  for (field in names(extra)) row[[field]] <- extra[[field]]
  as.data.frame(row, stringsAsFactors = FALSE)
}

find_row <- function(result, year, name) {
  rows <- result[[as.character(year)]]$rows
  hit <- Filter(function(r) identical(r$name, name), rows)
  if (length(hit) != 1) stop("expected one row named ", name, " in ", year)
  hit[[1]]
}

testthat::test_that("the committed snapshot and overrides rebuild the committed tracker data", {
  built <- la101_derive(committed_snapshot(), committed_overrides())
  la101_validate(built)
  committed <- paste(
    readLines(repo_path(la101_output_path), encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )
  testthat::expect_identical(paste0(committed, "\n"), la101_to_json(built))
})

testthat::test_that("every rebuilt restaurant has a neighborhood in the geography file", {
  built <- la101_derive(committed_snapshot(), committed_overrides())
  testthat::expect_true(la101_check_geography(built, repo_path(la101_geography_path)))
})

testthat::test_that("every edition lists 101 distinct slots and 1,414 slots overall", {
  built <- la101_derive(committed_snapshot(), committed_overrides())
  testthat::expect_identical(names(built), as.character(2013:2026))
  testthat::expect_identical(la101_validate(built), 14L * 101L)
})

testthat::test_that("clean_name drops rank prefixes and odd whitespace only", {
  testthat::expect_identical(
    la101_clean_name(c("4. Sushi Kisen", "  Danbi  ", "101 Noodle Express", "A\n  B")),
    c("Sushi Kisen", "Danbi", "101 Noodle Express", "A B")
  )
})

testthat::test_that("identity keys ignore accents, quotes, ampersands and a leading The", {
  same <- c("Park's BBQ", "Park’s BBQ", "PARK'S bbq")
  testthat::expect_length(unique(la101_key(same)), 1)
  testthat::expect_identical(la101_key("Bäco Mercat"), la101_key("Baco Mercat"))
  testthat::expect_identical(la101_key("Church & State"), la101_key("Church and State"))
  testthat::expect_identical(la101_key("The Tasting Kitchen"), la101_key("Tasting Kitchen"))
  testthat::expect_false(identical(la101_key("Q"), la101_key("Q Sushi")))
})

testthat::test_that("each page template parser returns clean names", {
  ballots <- paste0(
    '<h2><span class="rank">2</span><span itemprop="name">Alma</span></h2>',
    '<h2><span itemprop="name">1. Bestia</span></h2><h2>Not a restaurant</h2>'
  )
  testthat::expect_identical(la101_parse_names(ballots, "ballots"), c("Alma", "Bestia"))

  slides <- paste0(
    '<div class="slide price_2" data-name="Adana"></div>',
    '<div class="row" data-name="skip me"></div>',
    '<div class="slide" data-name="Bavel"></div>'
  )
  testthat::expect_identical(la101_parse_names(slides, "slides"), c("Adana", "Bavel"))

  review <- paste0(
    '<h2 class="review__name">A.O.C.</h2>',
    '<h2 class="review__name" id="x"><em>Jitlada</em></h2>'
  )
  testthat::expect_identical(la101_parse_names(review, "review"), c("A.O.C.", "Jitlada"))

  itemlist <- paste0(
    '<script type="application/ld+json">{"@type":"NewsArticle","name":"skip"}</script>',
    '<script type="application/ld+json">{"@type":"ItemList","itemListElement":[',
    '{"@type":"ListItem","item":{"@type":"Restaurant","name":"1. Holbox "}},',
    '{"@type":"ListItem","item":{"@type":"Restaurant","name":"Kato"}}]}</script>'
  )
  testthat::expect_identical(la101_parse_names(itemlist, "itemlist"), c("Holbox", "Kato"))
  testthat::expect_error(la101_parse_names("<p/>", "unknown"), "Unknown parser")
})

testthat::test_that("statuses follow the previous appearance, spelling aside", {
  src <- list(
    "2020" = edition("Alpha", "Bravo", "Charlie"),
    "2021" = edition("The Alpha", "Delta"),
    "2022" = edition("Alpha", "Bravo", "Delta", "Echo")
  )
  built <- la101_derive(src, no_overrides())
  testthat::expect_identical(find_row(built, 2020, "Alpha")$status, "First appearance")

  renamed <- find_row(built, 2021, "The Alpha")
  testthat::expect_identical(
    renamed[c("status", "first", "previous", "match")],
    list(status = "Previous edition", first = 2020, previous = 2020, match = "Alpha")
  )
  testthat::expect_identical(find_row(built, 2021, "Delta")$status, "First appearance")

  back <- find_row(built, 2022, "Bravo")
  testthat::expect_identical(back$status, "Comeback")
  testthat::expect_identical(c(back$first, back$previous), c(2020, 2020))
  testthat::expect_identical(find_row(built, 2022, "Alpha")$match, "The Alpha")
  testthat::expect_identical(built[["2022"]]$counts, c(1L, 1L, 2L, 0L))
})

testthat::test_that("overrides rename, split branches, and mark non-restaurants", {
  src <- list(
    "2020" = edition("Delta", "Mayor Pat", "Old Name"),
    "2021" = edition("Delta West", "New Name", "Mayor Pat")
  )
  ov <- rbind(
    override(2020, "Mayor Pat", status = "Other"),
    override(2021, "Mayor Pat", status = "Other"),
    override(2021, "Delta West", name = "Delta", links_to = "!new"),
    override(2021, "New Name", links_to = "Old Name", match_label = "Old Name (closed)")
  )
  built <- la101_derive(src, ov)
  testthat::expect_identical(find_row(built, 2020, "Mayor Pat")$status, "Other")
  testthat::expect_true(is.na(find_row(built, 2020, "Mayor Pat")$first))
  testthat::expect_identical(find_row(built, 2021, "Delta")$status, "First appearance")
  linked <- find_row(built, 2021, "New Name")
  testthat::expect_identical(c(linked$status, linked$match),
                             c("Previous edition", "Old Name (closed)"))
  testthat::expect_identical(built[["2021"]]$counts, c(1L, 0L, 1L, 1L))
})

testthat::test_that("a shared slot takes the earliest first and latest previous year", {
  src <- list(
    "2020" = edition("Alpha", "Bravo"),
    "2021" = edition("Alpha"),
    "2022" = edition("Alpha / Bravo"),
    "2023" = edition("Foxtrot")
  )
  ov <- rbind(
    override(2022, "Alpha / Bravo", links_to = "Alpha; Bravo; +Foxtrot"),
    override(2023, "Foxtrot")
  )
  built <- la101_derive(src, ov)
  slot <- find_row(built, 2022, "Alpha / Bravo")
  testthat::expect_identical(
    slot[c("status", "first", "previous", "match")],
    list(status = "Previous edition", first = 2020, previous = 2021, match = "Alpha")
  )
  child <- find_row(built, 2023, "Foxtrot")
  testthat::expect_identical(
    child[c("status", "first", "previous", "match")],
    list(status = "Previous edition", first = 2022, previous = 2022, match = "Alpha / Bravo")
  )
})

testthat::test_that("stale or misspelled overrides stop the build", {
  src <- list("2020" = edition("Alpha"), "2021" = edition("Alpha"))
  testthat::expect_error(
    la101_derive(src, override(2021, "Not scraped", name = "Alpha")),
    "match no scraped name"
  )
  testthat::expect_error(
    la101_derive(src, override(2021, "Alpha", links_to = "Nowhere")),
    "not in an earlier edition"
  )
})

testthat::test_that("validation rejects short editions and unknown geography", {
  built <- la101_derive(committed_snapshot(), committed_overrides())
  short <- built
  short[["2020"]]$rows <- short[["2020"]]$rows[-1]
  testthat::expect_error(la101_validate(short), "expected 101")

  geography <- tempfile(fileext = ".json")
  jsonlite::write_json(list(editions = list("2013" = list(Alma = "Downtown L.A."))), geography,
                       auto_unbox = TRUE)
  testthat::expect_error(la101_check_geography(built["2013"], geography), "no neighborhood")
})
