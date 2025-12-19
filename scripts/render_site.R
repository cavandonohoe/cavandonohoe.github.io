args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(rmarkdown)
})

render_root <- function(encoding = "UTF-8") {
  render_site(encoding = encoding)
}

render_subdir <- function(dir, encoding = "UTF-8") {
  files <- list.files(dir, pattern = "\\.Rmd$", full.names = TRUE)
  if (!length(files)) return(invisible(NULL))

  out_dir <- file.path("_site", dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for (file in files) {
    render(
      input = file,
      output_dir = out_dir,
      encoding = encoding,
      output_options = list(
        lib_dir = "../site_libs",
        self_contained = FALSE
      )
    )
  }
}

encoding <- if (length(args) && nzchar(args[[1]])) args[[1]] else "UTF-8"

render_root(encoding = encoding)
render_subdir("projects", encoding = encoding)
render_subdir("learn_r", encoding = encoding)
