#!/usr/bin/env Rscript
# generate_sitemap.R
#
# Regenerates sitemap.xml.
#
# Strategy:
#   1. Discover the universe of pages from _site.yml's navbar entries plus
#      any *.html in the rendered _site/ directory (if available).
#   2. Preserve <changefreq> and <priority> for any URL that already exists
#      in sitemap.xml. New URLs get sensible defaults (weekly / 0.7).
#   3. Update <lastmod> to today for every URL.
#
# Usage:
#   Rscript scripts/generate_sitemap.R [base_url]

`%>%` <- magrittr::`%>%`

args <- commandArgs(trailingOnly = TRUE)
base_url <- if (length(args) >= 1 && nzchar(args[[1]])) {
  args[[1]]
} else {
  "https://cavandonohoe.github.io"
}
base_url <- sub("/$", "", base_url)

repo_root <- here::here()
site_yml <- file.path(repo_root, "_site.yml")
site_dir <- file.path(repo_root, "_site")
out_path <- file.path(repo_root, "sitemap.xml")

today <- format(Sys.Date(), "%Y-%m-%d")

extract_hrefs <- function(node) {
  if (is.list(node)) {
    direct <- if (!is.null(node$href)) node$href else character()
    nested <- unlist(lapply(node, extract_hrefs), use.names = FALSE)
    c(direct, nested)
  } else {
    character()
  }
}

hrefs_yml <- character()
if (file.exists(site_yml)) {
  yml <- yaml::read_yaml(site_yml)
  hrefs_yml <- extract_hrefs(yml)
}

hrefs_yml <- hrefs_yml[grepl("\\.html$", hrefs_yml, ignore.case = TRUE)]
hrefs_yml <- hrefs_yml[!grepl("^https?://", hrefs_yml, ignore.case = TRUE)]

hrefs_built <- character()
if (dir.exists(site_dir)) {
  htmls <- list.files(site_dir, pattern = "\\.html$", recursive = TRUE)
  htmls <- htmls[!grepl("^pr-[0-9]+/", htmls)]
  hrefs_built <- htmls
}

all_paths <- unique(c("", hrefs_yml, hrefs_built))
all_paths <- all_paths[nzchar(all_paths) | all_paths == ""]
all_paths <- sort(unique(all_paths))

if (length(all_paths) == 0) {
  stop("No pages discovered for sitemap; check _site.yml or render the site first.")
}

# Parse existing sitemap.xml (if any) to preserve per-URL metadata.
existing <- list()
if (file.exists(out_path)) {
  existing_xml <- tryCatch(xml2::read_xml(out_path), error = function(e) NULL)
  if (!is.null(existing_xml)) {
    ns <- c(d1 = "http://www.sitemaps.org/schemas/sitemap/0.9")
    url_nodes <- xml2::xml_find_all(existing_xml, ".//d1:url", ns = ns)
    for (n in url_nodes) {
      loc <- xml2::xml_text(xml2::xml_find_first(n, "./d1:loc", ns = ns))
      cf  <- xml2::xml_text(xml2::xml_find_first(n, "./d1:changefreq", ns = ns))
      pr  <- xml2::xml_text(xml2::xml_find_first(n, "./d1:priority", ns = ns))
      if (length(loc) && nzchar(loc)) {
        existing[[loc]] <- list(
          changefreq = if (length(cf) && nzchar(cf)) cf else NA_character_,
          priority   = if (length(pr) && nzchar(pr)) pr else NA_character_
        )
      }
    }
  }
}

build_url_block <- function(loc) {
  meta <- existing[[loc]]
  changefreq <- if (!is.null(meta) && !is.na(meta$changefreq)) meta$changefreq else "weekly"
  priority   <- if (!is.null(meta) && !is.na(meta$priority))   meta$priority   else "0.7"
  paste0(
    "  <url>\n",
    "    <loc>", loc, "</loc>\n",
    "    <lastmod>", today, "</lastmod>\n",
    "    <changefreq>", changefreq, "</changefreq>\n",
    "    <priority>", priority, "</priority>\n",
    "  </url>"
  )
}

build_loc <- function(p) {
  if (!nzchar(p) || identical(p, "/")) {
    paste0(base_url, "/")
  } else {
    paste0(base_url, "/", p)
  }
}

locs <- vapply(all_paths, build_loc, character(1))
locs <- unique(locs)

# Preserve any locs that exist in the old sitemap but aren't reachable from
# our discovery (rare — manually-added entries).
extra <- setdiff(names(existing), locs)
locs <- c(locs, extra)
locs <- unique(locs)
locs <- sort(locs)

urls <- vapply(locs, build_url_block, character(1))

xml <- c(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
  "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
  urls,
  "</urlset>"
)

writeLines(xml, out_path)
cat("Wrote", length(locs), "URLs to", out_path, "\n")
