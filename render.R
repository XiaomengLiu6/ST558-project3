#ed_levels <- c("2","3","4","5","6")
#output_file <- paste0("ed_level_eq_",ed_levels,".md")
#params = lapply(ed_levels, FUN = function(x){list(ed_level = x)})
#reports <- tibble(output_file, params)

#apply(reports, MARGIN = 1,
#  FUN = function(x){
#  render(input = "project3-el.Rmd", output_file = x[[1]], params = x[[2]])
#  })

rmarkdown::render("project3-el.Rmd", output_file = "elementary.md",
  params = list(ed_level = "2"))

rmarkdown::render("project3-el.Rmd", output_file = "some-hs.md",
  params = list(ed_level = "3"))

rmarkdown::render("project3-el.Rmd", output_file = "hs-cred.md",
  params = list(ed_level = "4"))

rmarkdown::render("project3-el.Rmd", output_file = "some-college.md",
  params = list(ed_level = "5"))

rmarkdown::render("project3-el.Rmd", output_file = "four-yr-deg-or-higher.md",
  params = list(ed_level = "6"))