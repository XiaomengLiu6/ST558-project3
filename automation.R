ed_levels <- c("2","3","4","5","6")
output_file <- paste0("ed_level_eq_",ed_levels,".md")
params = lapply(ed_levels, FUN = function(x){list(ed_level = x)})
reports <- tibble(output_file, params)

library(rmarkdown)
apply(reports, MARGIN = 1,
  FUN = function(x){
  render(input = "project3.Rmd", output_file = x[[1]], params = x[[2]])
  })