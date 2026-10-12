site_root <- getwd()
if (basename(site_root) == "_site") {
  site_root <- dirname(site_root)
}
docx_output_dir <- file.path(site_root, "_site")
docx_path <- file.path(docx_output_dir, "cv_doc.docx")

rmarkdown::render(
  file.path(site_root, "cv_doc.Rmd"),
  output_file = "cv_doc.docx",
  output_dir = docx_output_dir,
  quiet = TRUE
)

if (file.exists(docx_path)) {
  file.copy(docx_path, file.path(site_root, "cv_doc.docx"), overwrite = TRUE)
}
