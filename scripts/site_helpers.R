#!/usr/bin/env Rscript
# site_helpers.R
#
# Pure helper functions used by other site-build scripts. These are
# kept separate so they can be unit-tested in isolation
# (see tests/testthat/test-site_helpers.R).

#' Recursively extract all `href` values from a (possibly nested) list.
#'
#' Walks a list (typically the parsed contents of `_site.yml`) and returns
#' every value found under an `href` key, at any depth, in document order.
#'
#' @param node A list, named list, or atomic value.
#' @return A character vector of `href` values (zero-length if none found).
extract_hrefs <- function(node) {
  if (is.list(node)) {
    direct <- if (!is.null(node$href)) node$href else character()
    nested <- unlist(lapply(node, extract_hrefs), use.names = FALSE)
    c(direct, nested)
  } else {
    character()
  }
}
