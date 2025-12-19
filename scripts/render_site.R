args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages({
  library(rmarkdown)
})

render_root <- function(encoding = "UTF-8") {
  render_site(encoding = encoding)
}

render_subdir <- function(dir, root, encoding = "UTF-8") {
  files <- list.files(dir, pattern = "\\.Rmd$", full.names = TRUE)
  if (!length(files)) return(invisible(NULL))

  out_root <- file.path(root, "_site")
  out_dir <- file.path(out_root, dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for (file in files) {
    out_file <- paste0(tools::file_path_sans_ext(basename(file)), ".html")
    render(
      input = file,
      output_dir = out_dir,
      output_file = out_file,
      encoding = encoding,
      output_options = list(
        lib_dir = file.path(out_dir, "site_libs"),
        self_contained = FALSE
      )
    )
  }
}

encoding <- if (length(args) && nzchar(args[[1]])) args[[1]] else "UTF-8"
root <- normalizePath(getwd(), mustWork = TRUE)

render_root(encoding = encoding)
render_subdir("projects", root = root, encoding = encoding)
render_subdir("learn_r", root = root, encoding = encoding)
