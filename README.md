This is a brief description of the purpose of the repo:

The repo is about ST558 Project 3. We analyzed a Diabetes Health Indicator Dataset. We were doing a separate analysis 
(basic EDA and the fitting/selection of predictive models) for each of these five Education groups using a single 
.Rmd file. In the file, we had a parameter corresponding to which Education level we were looking at and we subset the data to only use those observations 
for that analysis. We created render() code similar to the lecture to make the process of creating the five separate analysis simple to do.

Here is a list of R packages used:
```
readr
dplyr
ggplot2
corrplot
caret
ModelMetrics
rotationForest
```

This is the code used to create the analyses from a single .Rmd file (i.e. the render() code)
```
ed_levels <- c("2","3","4","5","6")
output_file <- paste0("ed_level_eq_",ed_levels,".md")
params = lapply(ed_levels, FUN = function(x){list(ed_level = x)})
reports <- tibble(output_file, params)

library(rmarkdown)
apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "project3.Rmd", output_file = x[[1]], params = x[[2]])
      })
```
The specific links for each education level to .html files of the generated analyses:

Report for Education level 1 and 2 [ed_level_eq_2.md](ed_level_eq_2.html)

Report for Education level 3 [ed_level_eq_3.md](ed_level_eq_3.html)

Report for Education level 4 [ed_level_eq_4.md](ed_level_eq_4.html)

Report for Education level 5 [ed_level_eq_5.md](ed_level_eq_5.html)

Report for Education level 6 [ed_level_eq_6.md](ed_level_eq_6.html)
