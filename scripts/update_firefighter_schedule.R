args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]

script_path <- if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath(file.path("scripts", "update_firefighter_schedule.R"), mustWork = TRUE)
}

site_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
output_dir <- file.path(site_root, "_site")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
setwd(site_root)

if (!nzchar(Sys.getenv("TZ"))) {
  Sys.setenv(TZ = "America/Los_Angeles")
}

rmarkdown::render(
  input = file.path(site_root, "firefighter_schedule.Rmd"),
  output_file = "firefighter_schedule.html",
  output_dir = output_dir,
  output_options = list(self_contained = FALSE),
  encoding = "UTF-8",
  quiet = FALSE
)
