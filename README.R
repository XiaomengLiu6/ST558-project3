# render the "project3.RMD" file as a .md file called README.md.

#a brief description of the purpose of the repo
#The repo is about ST558 Project 3. We analyzed a Diabetes Health Indicator 
#Dataset. We were doing a separate analysis (basic EDA and the fitting/selection 
#of predictive models) for each of these five Education groups using a single 
#.Rmd file. In the file, we had a parameter corresponding to which Education 
#level we were looking at and we subset the data to only use those observations 
#for that analysis. We created render() code similar to the lecture to make the 
#process of creating the five separate analysis simple to do.

#• a list of R packages used
#readr
#dplyr
#ggplot2
#corrplot
#caret
#ModelMetrics

#• the code used to create the analyses from a single .Rmd file (i.e. the render() code)

#• links to .html files of the generated analyses (which will be created by github pages! Not you!) For example,
#– Analysis for [College Graduates](college_graduate_analysis.html). Note you should only
#have a college_graduate_analysis.md file in the repo - github pages will render the .html file
#for you


# do we need to change the level names?