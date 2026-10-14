source("../../scripts/project_cards.R")

test_that("card_key normalizes site paths and card hrefs to the same key", {
  expect_equal(
    card_key(c("/imdb_rating_plot.html", "/imdb_rating_plot", "imdb_rating_plot.html")),
    rep("imdb_rating_plot", 3)
  )
  expect_equal(
    card_key(c("/detour-finder/index.html", "/detour-finder/", "detour-finder/index.html")),
    rep("detour-finder/", 3)
  )
  expect_equal(card_key(c("/", "/index.html", "")), c("", "", ""))
  expect_equal(card_key("/tohs_reunion.html?utm_source=x#top"), "tohs_reunion")
})

test_that("card_key keeps absolute URLs comparable to outbound click URLs", {
  expect_equal(
    card_key(c(
      "https://cavandonohoe.shinyapps.io/rent-vs-buy/",
      "https://CavanDonohoe.shinyapps.io/rent-vs-buy",
      "https://cavandonohoe.shinyapps.io/rent-vs-buy/?tab=2"
    )),
    rep("https://cavandonohoe.shinyapps.io/rent-vs-buy", 3)
  )
})

test_that("card_key drops PR preview paths and passes NA through", {
  expect_equal(card_key(c("/pr-283/index.html", "/pr-12/", NA)), rep(NA_character_, 3))
  expect_equal(card_key("/pricing.html"), "pricing")
})

test_that("rank_card_keys sums duplicate keys and merges outbound clicks", {
  pages <- data.frame(
    path = c("/a.html", "/a", "/b.html", "/", "/pr-9/a.html", "/c.html"),
    count = c(3, 4, 5, 100, 50, 0)
  )
  clicks <- data.frame(url = "https://app.example.com/x/", count = 6)
  expect_equal(
    rank_card_keys(pages, clicks),
    c("a", "https://app.example.com/x", "b")
  )
})

test_that("rank_card_keys breaks ties alphabetically and handles empty input", {
  pages <- data.frame(path = c("/z.html", "/m.html"), count = c(2, 2))
  expect_equal(rank_card_keys(pages), c("m", "z"))
  empty <- data.frame(path = character(), count = numeric())
  expect_equal(rank_card_keys(empty, empty[0, ]), character())
})

test_that("order_cards pins first, then ranks, then keeps unranked order", {
  cards <- list(
    list("a.html", "", "A", ""),
    list("b.html", "", "B", ""),
    list("c.html", "", "C", ""),
    list("https://app.example.com/x/", "", "X", ""),
    list("d.html", "", "D", "")
  )
  titles <- function(x) vapply(x, function(card) card[[3]], character(1))

  ordered <- order_cards(cards, ranking = c("d", "https://app.example.com/x"), pinned = "c.html")
  expect_equal(titles(ordered), c("C", "D", "X", "A", "B"))

  # A pinned card that also ranks is not duplicated; unknown pins are ignored.
  ordered <- order_cards(cards, ranking = c("c", "b"), pinned = c("missing.html", "c.html"))
  expect_equal(titles(ordered), c("C", "B", "A", "X", "D"))
})

test_that("order_cards with no ranking keeps the manual order", {
  cards <- list(list("a.html"), list("b.html"), list("c.html"))
  expect_identical(order_cards(cards), cards)
  expect_identical(order_cards(list()), list())
})

test_that("read_card_ranking tolerates missing or malformed files", {
  expect_equal(read_card_ranking(tempfile()), character())
  bad <- tempfile(fileext = ".json")
  writeLines("{not json", bad)
  expect_equal(read_card_ranking(bad), character())
  good <- tempfile(fileext = ".json")
  jsonlite::write_json(list(ranking = c("b", "a")), good)
  expect_equal(read_card_ranking(good), c("b", "a"))
})
