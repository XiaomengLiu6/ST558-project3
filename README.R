# render the "project3.RMD" file as a .md file called README.md.

rmarkdown::render(
  input = "project3.rmd",
  output_format = "github_document",
  output_file = "README.md",
  runtime = "static",
  clean = TRUE,
  params = NULL,
  encoding = "UTF-8"
)
