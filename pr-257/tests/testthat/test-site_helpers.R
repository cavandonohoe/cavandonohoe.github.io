test_that("extract_hrefs returns empty for non-list input", {
  source("../../scripts/site_helpers.R")

  expect_equal(extract_hrefs("not a list"), character())
  expect_equal(extract_hrefs(42), character())
  expect_equal(extract_hrefs(NULL), character())
})

test_that("extract_hrefs finds a top-level href", {
  source("../../scripts/site_helpers.R")

  result <- extract_hrefs(list(href = "index.html"))
  expect_equal(result, "index.html")
})

test_that("extract_hrefs walks nested lists", {
  source("../../scripts/site_helpers.R")

  nav <- list(
    navbar = list(
      left = list(
        list(text = "Home", href = "index.html"),
        list(text = "About", href = "about.html"),
        list(
          text = "Projects",
          menu = list(
            list(text = "Foo", href = "foo.html"),
            list(text = "Bar", href = "bar.html")
          )
        )
      ),
      right = list(
        list(icon = "github", href = "https://github.com/example")
      )
    )
  )
  result <- extract_hrefs(nav)
  expect_setequal(
    result,
    c(
      "index.html",
      "about.html",
      "foo.html",
      "bar.html",
      "https://github.com/example"
    )
  )
})

test_that("extract_hrefs handles entries without href", {
  source("../../scripts/site_helpers.R")

  nav <- list(
    list(text = "Section header (no link)"),
    list(text = "Real link", href = "page.html")
  )
  expect_equal(extract_hrefs(nav), "page.html")
})

test_that("extract_hrefs preserves duplicates", {
  source("../../scripts/site_helpers.R")

  nav <- list(
    list(href = "page.html"),
    list(href = "page.html")
  )
  expect_equal(extract_hrefs(nav), c("page.html", "page.html"))
})
