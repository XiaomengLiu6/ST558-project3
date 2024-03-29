    name<-c("grade K through 8","grade 9 through 11","a high school credential",
            "some college or technical school","bachelors degree or higher")
    paste0("This is the automated generated md file for the education level ",
           params$ed_level,",which means that the highest level of education is ",
           name[as.numeric(params$ed_level)-1])

    ## [1] "This is the automated generated md file for the education level 3,which means that the highest level of education is grade 9 through 11"

# Goal

We analyzed a Diabetes Health Indicator Dataset. We were doing a
separate analysis (basic EDA and the fitting/selection of predictive
models) for each of these five Education groups using a single .Rmd
file. In the file, we had a parameter corresponding to which Education
level we were looking at and we subset the data to only use those
observations for that analysis. We created render() code similar to the
lecture to make the process of creating the five separate analysis
simple to do.

# library

All the libraries used in this file are included here.

    library(readr)
    library(dplyr)
    library(ggplot2)
    library(corrplot)
    library(caret)
    library(ModelMetrics)
    library(rotationForest)

# Introduction section

The analyses presented here are based on the Diabetes Health Indicators
Dataset, made available by kaggle.com at
<https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/>.
The specific dataset upon which this analysis is based
(diabetes\_binary\_health\_indicators\_BRFSS2015.csv) is comprised of
approximately 250,000 records, each having a dichotomous 0/1 indication
of no diabetes vs. diabetes/pre-diabetes. In addition, the datafile
contains roughly 20 feature variables of various types, including
continuous (e.g. body mass index), categorical (e.g. education level),
and dummy indicators (e.g. whether the patient has high blood pressure).

The analytic approach taken here is to control for patients’ level of
education by conducting a separate analysis (i.e. exploratory data
analysis + predictive modelling) for each of five education levels: (1)
8th grade or lower; (2) grade 9 through 11; (3) high school credential;
(4) some college or technical school; and (5) bachelors degree or
higher. The outcome measure of interest for all analyses will be the
dichotomous diabetes indicator.

      #per https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf,
      #here are value meanings for the education variable:
        #1 = Never attended school or only kindergarten 
        #2 = Grades 1 through 8 (Elementary)
        #3 = Grades 9 through 11 (Some high school)
        #4 = Grade 12 or GED (High school graduate) 
        #5 = College 1 year to 3 years (Some college or technical school)
        #6 = College 4 years or more (College graduate)
        #9 = Refused

    #and here are values for other non-binary variables:
      #income
        #1 = Less than $10,000
        #2 = Less than $15,000 ($10,000 to less than $15,000)
        #3 = Less than $20,000 ($15,000 to less than $20,000)
        #4 = Less than $25,000 ($20,000 to less than $25,000)
        #5 = Less than $35,000 ($25,000 to less than $35,000)
        #6 = Less than $50,000 ($35,000 to less than $50,000)
        #7 = Less than $75,000 ($50,000 to less than $75,000)
        #8 = $75,000 or more
        #77 = Don’t know/Not sure
        #99 = Refused

      #Age
        #1 = Age 18 to 24
        #2 = Age 25 to 29
        #3 = Age 30 to 34
        #4 = Age 35 to 39
        #5 = Age 40 to 44
        #6 = Age 45 to 49
        #7 = Age 50 to 54
        #8 = Age 55 to 59
        #9 = Age 60 to 64
        #10 = Age 65 to 69
        #11 = Age 70 to 74
        #12 = Age 75 to 79
        #13 = Age 80 or older
        #14 = Don’t know/Refused/Missing

      #GenHlth
        #1 = excellent 
        #2 = very good 
        #3 = good 
        #4 = fair 
        #5 = poor 

      #Sex is coded as 0 = female, 1 = male

# Data

Before conducting analyses, we will read in the csv datafile, collapse
education levels of “Never attended school or only kindergarten” and
“Grades 1 through 8” into a single category, and convert categorical
variables into R factors.

### Read in the data

    #read in csv file
    diabetes<-read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

### Combine 1 and 2 Education levels

    #create collapsed version of education variable
    diabetes$Education <- recode(diabetes$Education, `1` = 2)
    table(diabetes$Education)

    ## 
    ##      2      3      4      5      6 
    ##   4217   9478  62750  69910 107325

### Convert variables to factors

    #create factor version of variables, where applicable
    #in order to facilitate the EDA, for now at least, we'll retain both the factor 
    #version and the numeric version of each variable (we may need to drop one 
    #version, though, before running models)
    diabetes$Diabetes_binary_f   <- as.factor(diabetes$Diabetes_binary)
    diabetes$HighBP_f    <- as.factor(diabetes$HighBP)
    diabetes$HighChol_f  <- as.factor(diabetes$HighChol)
    diabetes$CholCheck_f     <- as.factor(diabetes$CholCheck)
    #BMI -- this is a continuous var
    diabetes$Smoker_f    <- as.factor(diabetes$Smoker)
    diabetes$Stroke_f    <- as.factor(diabetes$Stroke)
    diabetes$HeartDiseaseorAttack_f  <- as.factor(diabetes$HeartDiseaseorAttack)
    diabetes$PhysActivity_f  <- as.factor(diabetes$PhysActivity)
    diabetes$Fruits_f    <- as.factor(diabetes$Fruits)
    diabetes$Veggies_f   <- as.factor(diabetes$Veggies)
    diabetes$HvyAlcoholConsump_f     <- as.factor(diabetes$HvyAlcoholConsump)
    diabetes$AnyHealthcare_f     <- as.factor(diabetes$AnyHealthcare)
    diabetes$NoDocbcCost_f   <- as.factor(diabetes$NoDocbcCost)
    diabetes$GenHlth_f <-  as.factor(diabetes$GenHlth)
    #MentHlth -- this is a continuous var
    #PhysHlth -- this is a continuous var
    diabetes$DiffWalk_f  <- as.factor(diabetes$DiffWalk)
    diabetes$Sex_f   <- as.factor(diabetes$Sex)
    diabetes$Age_f   <- as.factor(diabetes$Age)
    diabetes$Education_f     <- as.factor(diabetes$Education)
    diabetes$Income_f    <- as.factor(diabetes$Income)

## subset to specific level of education per *params* setting

    #subset to specific level of education per *params* setting
    temp <- subset(diabetes, Education==params$ed_level)

# Summarizations

Prior to modelling our data, we will first conduct an exploratory data
analysis (EDA). Our EDA will begin by confirming that we are working
with the desired set of cases, i.e. the set of cases corresponding to a
single level of the categorical “education” measure.

    #confirm that we're working with the desired set of cases
    table(temp$Education, temp$Education_f)

    ##    
    ##        2    3    4    5    6
    ##   3    0 9478    0    0    0

    ggplot(data=temp, aes(x=Education_f)) + 
      geom_dotplot(binwidth = .05, method = "histodot") + 
      labs(title = "confirm that we're working with the desired set of cases")

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-8-1.png) A
basic structure of the data set is shown below. Numerical summaries were
made for the numerical variables. This would help us to understand more
about the numerical variables.

    # basic structure of the data set
    str(temp)

    ## 'data.frame':    9478 obs. of  41 variables:
    ##  $ Diabetes_binary       : num  0 0 1 0 0 1 0 0 0 0 ...
    ##  $ HighBP                : num  1 1 1 1 1 1 0 0 0 0 ...
    ##  $ HighChol              : num  0 0 1 1 0 0 0 1 0 1 ...
    ##  $ CholCheck             : num  1 1 1 1 1 1 0 1 1 1 ...
    ##  $ BMI                   : num  27 33 24 24 22 21 24 19 23 24 ...
    ##  $ Smoker                : num  0 1 1 1 1 1 1 1 1 1 ...
    ##  $ Stroke                : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ HeartDiseaseorAttack  : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PhysActivity          : num  1 1 0 0 1 1 1 0 0 0 ...
    ##  $ Fruits                : num  1 1 0 1 1 1 0 0 0 1 ...
    ##  $ Veggies               : num  1 1 0 1 1 1 1 0 1 1 ...
    ##  $ HvyAlcoholConsump     : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AnyHealthcare         : num  1 0 1 1 1 1 1 1 0 1 ...
    ##  $ NoDocbcCost           : num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ GenHlth               : num  2 1 2 5 3 3 3 5 3 3 ...
    ##  $ MentHlth              : num  0 0 0 0 0 0 10 30 10 0 ...
    ##  $ PhysHlth              : num  0 0 0 30 0 0 0 30 6 0 ...
    ##  $ DiffWalk              : num  0 1 0 0 0 1 0 0 0 0 ...
    ##  $ Sex                   : num  0 1 0 1 0 0 0 0 1 0 ...
    ##  $ Age                   : num  11 13 12 9 7 10 6 8 1 12 ...
    ##  $ Education             : num  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ Income                : num  6 3 3 1 3 2 7 4 3 1 ...
    ##  $ Diabetes_binary_f     : Factor w/ 2 levels "0","1": 1 1 2 1 1 2 1 1 1 1 ...
    ##  $ HighBP_f              : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 1 1 1 1 ...
    ##  $ HighChol_f            : Factor w/ 2 levels "0","1": 1 1 2 2 1 1 1 2 1 2 ...
    ##  $ CholCheck_f           : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 1 2 2 2 ...
    ##  $ Smoker_f              : Factor w/ 2 levels "0","1": 1 2 2 2 2 2 2 2 2 2 ...
    ##  $ Stroke_f              : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ HeartDiseaseorAttack_f: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PhysActivity_f        : Factor w/ 2 levels "0","1": 2 2 1 1 2 2 2 1 1 1 ...
    ##  $ Fruits_f              : Factor w/ 2 levels "0","1": 2 2 1 2 2 2 1 1 1 2 ...
    ##  $ Veggies_f             : Factor w/ 2 levels "0","1": 2 2 1 2 2 2 2 1 2 2 ...
    ##  $ HvyAlcoholConsump_f   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ AnyHealthcare_f       : Factor w/ 2 levels "0","1": 2 1 2 2 2 2 2 2 1 2 ...
    ##  $ NoDocbcCost_f         : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 1 ...
    ##  $ GenHlth_f             : Factor w/ 5 levels "1","2","3","4",..: 2 1 2 5 3 3 3 5 3 3 ...
    ##  $ DiffWalk_f            : Factor w/ 2 levels "0","1": 1 2 1 1 1 2 1 1 1 1 ...
    ##  $ Sex_f                 : Factor w/ 2 levels "0","1": 1 2 1 2 1 1 1 1 2 1 ...
    ##  $ Age_f                 : Factor w/ 13 levels "1","2","3","4",..: 11 13 12 9 7 10 6 8 1 12 ...
    ##  $ Education_f           : Factor w/ 5 levels "2","3","4","5",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Income_f              : Factor w/ 8 levels "1","2","3","4",..: 6 3 3 1 3 2 7 4 3 1 ...

    # a numerical summary for BMI
    temp%>%summarize(mean=mean(BMI),median=median(BMI),std=sd(BMI))

    ##      mean median      std
    ## 1 29.6417     28 7.355138

    # a numerical summary for MentHlth
    temp%>%summarize(mean=mean(MentHlth),median=median(MentHlth),std=sd(MentHlth))

    ##       mean median      std
    ## 1 5.319266      0 9.794232

    # a numerical summary for PhysHlth
    temp%>%summarize(mean=mean(PhysHlth),median=median(PhysHlth),std=sd(PhysHlth))

    ##       mean median      std
    ## 1 7.916016      0 11.32689

After confirming correct data subsetting, for each factor variable in
the dataset, we have calculated the prevalence of diabetes at each level
of said factor in hopes of identifying any bi-variate associations with
diabetes. In addition, for each factor variable, we graphically display
(1) the relative frequency of patients within factor levels, and (2) the
proportion of patients within each level of each factor that do and do
not have diabetes.

For continuous predictor variables, we have generated a correlation
matrix (the outcome of interest is also included in this correlation
matrix). We also generate density plots and boxplots for each continuous
predictor using the dichotomous outcome measure as a “by” variable for
each of these plots.

    #function to check prevalence of diabetes at each level of each factor, 
    #and generate corresponding plots
    explore <- function(by_var)
    {
    results1 <- temp %>%
      group_by({{by_var}}) %>%
      summarize(diabetes_rate = mean(Diabetes_binary))
        #passing variable names to function using curly brackets:
    #https://stackoverflow.com/questions/63433728/how-do-i-pass-a-variable-name-to-an-argument-in-a-function
    print(results1)

    results2 <- ggplot(data=temp, aes(x={{by_var}}, fill=Diabetes_binary_f)) + 
      geom_bar(stat="count") +
      labs(title = "Presence of diabetes, by select factors")
    print(results2)
    }

    #probably need to run the above function for at least the sex, age, and income 
    #variables, but may not need to run it for this entire list
    explore(by_var = HighBP_f)

    ## # A tibble: 2 × 2
    ##   HighBP_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0                0.112
    ## 2 1                0.336

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    explore(by_var = HighChol_f)

    ## # A tibble: 2 × 2
    ##   HighChol_f diabetes_rate
    ##   <fct>              <dbl>
    ## 1 0                  0.151
    ## 2 1                  0.334

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-2.png)

    explore(by_var = CholCheck_f)

    ## # A tibble: 2 × 2
    ##   CholCheck_f diabetes_rate
    ##   <fct>               <dbl>
    ## 1 0                  0.0494
    ## 2 1                  0.250

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-3.png)

    explore(by_var = Smoker_f)

    ## # A tibble: 2 × 2
    ##   Smoker_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0                0.240
    ## 2 1                0.244

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-4.png)

    explore(by_var = Stroke_f)

    ## # A tibble: 2 × 2
    ##   Stroke_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0                0.231
    ## 2 1                0.364

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-5.png)

    explore(by_var = HeartDiseaseorAttack_f)

    ## # A tibble: 2 × 2
    ##   HeartDiseaseorAttack_f diabetes_rate
    ##   <fct>                          <dbl>
    ## 1 0                              0.216
    ## 2 1                              0.368

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-6.png)

    explore(by_var = PhysActivity_f)

    ## # A tibble: 2 × 2
    ##   PhysActivity_f diabetes_rate
    ##   <fct>                  <dbl>
    ## 1 0                      0.276
    ## 2 1                      0.216

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-7.png)

    explore(by_var = Fruits_f)

    ## # A tibble: 2 × 2
    ##   Fruits_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0                0.243
    ## 2 1                0.242

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-8.png)

    explore(by_var = Veggies_f)

    ## # A tibble: 2 × 2
    ##   Veggies_f diabetes_rate
    ##   <fct>             <dbl>
    ## 1 0                 0.258
    ## 2 1                 0.234

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-9.png)

    explore(by_var = HvyAlcoholConsump_f)

    ## # A tibble: 2 × 2
    ##   HvyAlcoholConsump_f diabetes_rate
    ##   <fct>                       <dbl>
    ## 1 0                           0.248
    ## 2 1                           0.108

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-10.png)

    explore(by_var = AnyHealthcare_f)

    ## # A tibble: 2 × 2
    ##   AnyHealthcare_f diabetes_rate
    ##   <fct>                   <dbl>
    ## 1 0                       0.127
    ## 2 1                       0.258

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-11.png)

    explore(by_var = NoDocbcCost_f)

    ## # A tibble: 2 × 2
    ##   NoDocbcCost_f diabetes_rate
    ##   <fct>                 <dbl>
    ## 1 0                     0.243
    ## 2 1                     0.240

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-12.png)

    explore(by_var = GenHlth_f)

    ## # A tibble: 5 × 2
    ##   GenHlth_f diabetes_rate
    ##   <fct>             <dbl>
    ## 1 1                0.0930
    ## 2 2                0.123 
    ## 3 3                0.203 
    ## 4 4                0.320 
    ## 5 5                0.404

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-13.png)

    explore(by_var = DiffWalk_f)

    ## # A tibble: 2 × 2
    ##   DiffWalk_f diabetes_rate
    ##   <fct>              <dbl>
    ## 1 0                  0.176
    ## 2 1                  0.355

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-14.png)

    explore(by_var = Sex_f)

    ## # A tibble: 2 × 2
    ##   Sex_f diabetes_rate
    ##   <fct>         <dbl>
    ## 1 0             0.250
    ## 2 1             0.232

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-15.png)

    explore(by_var = Age_f)

    ## # A tibble: 13 × 2
    ##    Age_f diabetes_rate
    ##    <fct>         <dbl>
    ##  1 1            0.0207
    ##  2 2            0.0521
    ##  3 3            0.0617
    ##  4 4            0.0909
    ##  5 5            0.138 
    ##  6 6            0.186 
    ##  7 7            0.214 
    ##  8 8            0.269 
    ##  9 9            0.310 
    ## 10 10           0.313 
    ## 11 11           0.324 
    ## 12 12           0.299 
    ## 13 13           0.255

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-16.png)

    explore(by_var = Income_f)

    ## # A tibble: 8 × 2
    ##   Income_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 1                0.271
    ## 2 2                0.300
    ## 3 3                0.262
    ## 4 4                0.260
    ## 5 5                0.211
    ## 6 6                0.179
    ## 7 7                0.178
    ## 8 8                0.144

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-17.png)

    #correlation matrix (outcome var x continuous vars)
    corr_vars <-
      temp %>% select(c(Diabetes_binary, BMI, MentHlth, PhysHlth))
    correlation <- cor(corr_vars, method = "spearman")
    corrplot(correlation, type = "upper", tl.pos = "lt")
    corrplot(correlation, type = "lower", method = "number", add = TRUE, 
             diag = FALSE, tl.pos = "n")

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-18.png)

    #density plots / boxplots (outcome var x continuous vars)
    #I'm guessing we could just choose one or the other
    ggplot(data=temp, aes(x=BMI, fill=Diabetes_binary_f)) + 
      geom_density(adjust = 0.5, alpha = 0.5) +
      labs(title = "BMI, by diabetes status")

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-19.png)

    ggplot(data=temp, aes(x=Diabetes_binary_f, y=BMI)) + geom_boxplot() +
      labs(title = "BMI, by diabetes status")

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-20.png)

    ggplot(data=temp, aes(x=MentHlth, fill=Diabetes_binary_f)) + 
      geom_density(adjust = 0.5, alpha = 0.5) +
      labs(title = "Number of poor mental health days, by diabetes status")

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-21.png)

    ggplot(data=temp, aes(x=Diabetes_binary_f, y=MentHlth)) + geom_boxplot() +
      labs(title = "Number of poor mental health days, by diabetes status")

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-22.png)

    ggplot(data=temp, aes(x=PhysHlth, fill=Diabetes_binary_f)) + 
      geom_density(adjust = 0.5, alpha = 0.5) +
      labs(title = "Number of poor physical health days, by diabetes status")

![](ed_level_eq_3_files/figure-markdown_strict/unnamed-chunk-10-23.png)

    bp3 <- ggplot(data=temp, aes(x=Diabetes_binary_f, y=PhysHlth)) + 
      geom_boxplot() + 
      labs(title = "Number of poor physical health days, by diabetes status")

# Modeling

We did some data cleaning and split the data into a training (70% of the
data) and test set (30% of the data). Use set.seed() to make things
reproducible. Then we will fit different models using different approach
and provide explanations.

## Data cleaning

We made a data cleaning so that the model fitting methods would work
better.

    #prior to running models, in instances where we have both a factor and 
    #a non-factor version of a given variable, we need to first drop the non-factor 
    #version of the variable.
    #we also need to drop both versions of the education variable (since it will not
    #vary given that we've subset our data to a specific education level)
    temp$Diabetes_binary     <- NULL 
    temp$HighBP  <- NULL
    temp$HighChol    <- NULL
    temp$CholCheck   <- NULL
    temp$Smoker  <- NULL
    temp$Stroke  <- NULL
    temp$HeartDiseaseorAttack    <- NULL
    temp$PhysActivity    <- NULL
    temp$Fruits  <- NULL
    temp$Veggies     <- NULL
    temp$HvyAlcoholConsump   <- NULL
    temp$AnyHealthcare   <- NULL
    temp$NoDocbcCost     <- NULL 
    temp$GenHlth <-  NULL
    temp$DiffWalk    <- NULL
    temp$Sex     <- NULL
    temp$Age     <- NULL
    temp$Education   <- NULL
    temp$Education_f     <- NULL
    temp$Income  <- NULL

    # convert the 0 and 1 to no and yes for logLoss metric to work.
    temp$Diabetes_binary_f<-ifelse(temp$Diabetes_binary_f==0,"no","yes")
    temp$Diabetes_binary_f<-as.factor(temp$Diabetes_binary_f)

## train and test set split

    # set the seed
    set.seed(433)
    # split the training and testing
    indextrain<-createDataPartition(y=temp$Diabetes_binary,p=0.7,list=FALSE)
    ed_train<-temp[indextrain,]
    ed_test<-temp[-indextrain,]

The goal is to create models for predicting the Diabetes\_binary
variable (using caret). We’ll use logLoss as our metric to evaluate
models. For all model types use logLoss with 5 fold cross-validation to
select the best model.

## what log loss is:

Log loss is a common evaluation metric for binary classification models.
It measure the performance of a model by quantifying the difference
between predicted probabilities and actual values. The more the
predicted probability diverges from the actual value, the higher is the
log-loss value.A lower log loss value means better
predictions.Mathematically, log loss is the negative average of the log
of correct predicted probabilities for each instance.

We prefer it because log loss penalizes confident and incorrect
predictors more heavily. It also provides a continuous and
differentiable meausre of the model’s performance, making it suitable of
optimization algorithms. It could be interpreted as the logarithmic
measure of the likelihood of the predicted probabilities aligning with
the true labels.

### related links

Here are two website links I found useful for explaining this term:

<https://www.analyticsvidhya.com/blog/2020/11/binary-cross-entropy-aka-log-loss-the-cost-function-used-in-logistic-regression/>

<https://towardsdatascience.com/intuition-behind-log-loss-score-4e0c9979680a>

## First method: logistic regression

### explanation of what a logistic regression is

The logistic regression is modeling average number of successes for a
given x, i.e. probability of success.Basic logistic regression models
success probability using the logistic function
$P(success|yard)=\frac{e^{\beta\_0+\beta\_1x}}{1+e^{\beta\_0+\beta\_1x}}$

### why we apply it to this kind of data

We have a response variable that is success/failure and it is perfect
for fitting a logistic regression mode.

### fit three candidate logistic regression models and choose the best model.

    ed_logistic1<-train(Diabetes_binary_f~BMI+HighChol_f+HighBP_f,data=ed_train,
                 method="glm", 
                 metric="logLoss",
                 trControl=trainControl(method = "cv",number = 5,classProbs = TRUE, 
                                        summaryFunction = mnLogLoss),
                 preProcess=c("center","scale")
    )
    ed_logistic1

    ## Generalized Linear Model 
    ## 
    ## 6636 samples
    ##    3 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (3), scaled (3) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5308, 5310, 5308, 5309, 5309 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.4951938

    ed_logistic2<-train(Diabetes_binary_f~BMI+HighChol_f+HighBP_f+MentHlth+
                          PhysActivity_f,data=ed_train,
                 method="glm", 
                 metric="logLoss",
                 trControl=trainControl(method = "cv",number = 5,classProbs = TRUE, 
                                        summaryFunction = mnLogLoss),
                 preProcess=c("center","scale")
    )
    ed_logistic2

    ## Generalized Linear Model 
    ## 
    ## 6636 samples
    ##    5 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (5), scaled (5) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5308, 5309, 5309, 5309, 5309 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.4955067

    ed_logistic3<-train(Diabetes_binary_f~.,data=ed_train,
                 method="glm", 
                 metric="logLoss",
                 trControl=trainControl(method = "cv",number = 5,classProbs = TRUE, 
                                        summaryFunction = mnLogLoss),
                 preProcess=c("center","scale")
    )
    ed_logistic3

    ## Generalized Linear Model 
    ## 
    ## 6636 samples
    ##   20 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5308, 5310, 5309, 5308, 5309 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.4719314

    # return the result
    paste0("According to the results, the lowest logLoss is the model ", c("1","2","3")[which.min(c(ed_logistic1$results[2],ed_logistic2$results[2],
                              ed_logistic3$results[2]))])

    ## [1] "According to the results, the lowest logLoss is the model 3"

## Second method: Lasso logistic

Lasso models aim to leverage the bias-variance trade-off by purposefully
introducing small amounts of bias while training the model, with the
hope of ultimately decreasing variance (and thereby improving
performance when using the model on test data). Instead of minimizing
the residual sum of squares, the lasso method attempts to minimize the
residual sum of squares *plus a so-called “shrinkage penalty”*. In
effect, the shrinkage penalty constrains regression coefficients, and in
so doing, allows the model to better generalize to test data. Here we
use cross-validation to test different values for the shrinkage penalty
in hopes of determining its optimal value, i.e. the shinkage penalty
value which eliminates the greatest amount of variance in exchange for
introduction of the smallest amount of bias.

    ed_lasso <- train(Diabetes_binary_f ~ ., data = ed_train,
      method = "glmnet",
      metric="logLoss",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "cv", number = 5, 
                               classProbs = TRUE, summaryFunction = mnLogLoss),
      tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, by = 0.1)))

    ed_lasso

    ## glmnet 
    ## 
    ## 6636 samples
    ##   20 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5309, 5308, 5309, 5309, 5309 
    ## Resampling results across tuning parameters:
    ## 
    ##   lambda  logLoss  
    ##   0.0     0.4709913
    ##   0.1     0.5480200
    ##   0.2     0.5537335
    ##   0.3     0.5537335
    ##   0.4     0.5537335
    ##   0.5     0.5537335
    ##   0.6     0.5537335
    ##   0.7     0.5537335
    ##   0.8     0.5537335
    ##   0.9     0.5537335
    ##   1.0     0.5537335
    ## 
    ## Tuning parameter 'alpha' was held constant at a value of 1
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final values used for the model were alpha = 1 and lambda = 0.

## Third method: Classification tree

Next we predict the presence of diabetes using a classification tree, in
which the predictor space is divided into various “regions”, and the
predicted value for any given observation is the most common
classification among all other observations in that region.
Classification trees are advantageous in that they are relatively easy
to understand, and that they automatically account for interaction
effects. Here we train our model using 5-fold cross-validation as well
as different tuning parameters to find the optimal number and types of
tree splits.

    #classification tree
    ed_ct <- train(Diabetes_binary_f ~ ., data = ed_train,
      method = "rpart",
      metric="logLoss",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "cv", number = 5, 
                               classProbs = TRUE, summaryFunction = mnLogLoss),
      tuneGrid = data.frame(cp = seq(from = .001, to = .1, by = .001)))

    ed_ct

    ## CART 
    ## 
    ## 6636 samples
    ##   20 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5308, 5309, 5309, 5309, 5309 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp     logLoss  
    ##   0.001  0.5597676
    ##   0.002  0.5163216
    ##   0.003  0.5114074
    ##   0.004  0.5059512
    ##   0.005  0.5055893
    ##   0.006  0.5057410
    ##   0.007  0.5059796
    ##   0.008  0.5059796
    ##   0.009  0.5154155
    ##   0.010  0.5154155
    ##   0.011  0.5154155
    ##   0.012  0.5154155
    ##   0.013  0.5154155
    ##   0.014  0.5249539
    ##   0.015  0.5249539
    ##   0.016  0.5249539
    ##   0.017  0.5339543
    ##   0.018  0.5339543
    ##   0.019  0.5537335
    ##   0.020  0.5537335
    ##   0.021  0.5537335
    ##   0.022  0.5537335
    ##   0.023  0.5537335
    ##   0.024  0.5537335
    ##   0.025  0.5537335
    ##   0.026  0.5537335
    ##   0.027  0.5537335
    ##   0.028  0.5537335
    ##   0.029  0.5537335
    ##   0.030  0.5537335
    ##   0.031  0.5537335
    ##   0.032  0.5537335
    ##   0.033  0.5537335
    ##   0.034  0.5537335
    ##   0.035  0.5537335
    ##   0.036  0.5537335
    ##   0.037  0.5537335
    ##   0.038  0.5537335
    ##   0.039  0.5537335
    ##   0.040  0.5537335
    ##   0.041  0.5537335
    ##   0.042  0.5537335
    ##   0.043  0.5537335
    ##   0.044  0.5537335
    ##   0.045  0.5537335
    ##   0.046  0.5537335
    ##   0.047  0.5537335
    ##   0.048  0.5537335
    ##   0.049  0.5537335
    ##   0.050  0.5537335
    ##   0.051  0.5537335
    ##   0.052  0.5537335
    ##   0.053  0.5537335
    ##   0.054  0.5537335
    ##   0.055  0.5537335
    ##   0.056  0.5537335
    ##   0.057  0.5537335
    ##   0.058  0.5537335
    ##   0.059  0.5537335
    ##   0.060  0.5537335
    ##   0.061  0.5537335
    ##   0.062  0.5537335
    ##   0.063  0.5537335
    ##   0.064  0.5537335
    ##   0.065  0.5537335
    ##   0.066  0.5537335
    ##   0.067  0.5537335
    ##   0.068  0.5537335
    ##   0.069  0.5537335
    ##   0.070  0.5537335
    ##   0.071  0.5537335
    ##   0.072  0.5537335
    ##   0.073  0.5537335
    ##   0.074  0.5537335
    ##   0.075  0.5537335
    ##   0.076  0.5537335
    ##   0.077  0.5537335
    ##   0.078  0.5537335
    ##   0.079  0.5537335
    ##   0.080  0.5537335
    ##   0.081  0.5537335
    ##   0.082  0.5537335
    ##   0.083  0.5537335
    ##   0.084  0.5537335
    ##   0.085  0.5537335
    ##   0.086  0.5537335
    ##   0.087  0.5537335
    ##   0.088  0.5537335
    ##   0.089  0.5537335
    ##   0.090  0.5537335
    ##   0.091  0.5537335
    ##   0.092  0.5537335
    ##   0.093  0.5537335
    ##   0.094  0.5537335
    ##   0.095  0.5537335
    ##   0.096  0.5537335
    ##   0.097  0.5537335
    ##   0.098  0.5537335
    ##   0.099  0.5537335
    ##   0.100  0.5537335
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.005.

## Fourth method: Random forest

### Explanation of the random forest:

The random forest uses the same idea as bagging. It creates multiple
trees from bootstrap samples and averages results. It uses a random
subset of predictors for each bootstrap sample fit.

### why we might use it instead of a basic classification tree:

We want to use random forest because we do not want to use all the
predictors.If a really strong predictor exists, every bootstrap tree
will probably use it for the first split and it will make the prediction
more correlated.

    ed_rf<-train(Diabetes_binary_f~.,
                 data=ed_train,
                 method="rf", 
                 metric="logLoss",
                 trControl=trainControl(method = "cv",number = 5, classProbs=TRUE, 
                                        summaryFunction=mnLogLoss),
                 preProcess=c("center","scale"),
                 tuneGrid=data.frame(mtry=c(5:7))
    )
    ed_rf

    ## Random Forest 
    ## 
    ## 6636 samples
    ##   20 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5310, 5308, 5309, 5308, 5309 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  logLoss  
    ##   5     0.4811821
    ##   6     0.4823903
    ##   7     0.4844506
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 5.

## Fifth method: rotation forest method

The rotation forest method is similar to the random forest method in
that multiple models are brought to bear – each having different numbers
and combinations of predictors – and the resulting set of predicted
values are then averaged across models (or, in the case of a categorical
outcome, the most prevalent classification across models is determined).
Unlike the random forest method, however, the rotation forest method
first extracts principal components from the various sets of predictor
variables that are considered, and enters the principal components as
predictors (as opposed to directly entering the original predictor
variables as is done when using random forest). In more general terms,
the rotation forest method is essentially a twist on the random forest
method whereby principal component regression (or, principal component
classification) is employed. As such, the rotation forest method is
particularly well-suited for dealing with any multi-collinearity that
may exist among the predictors. When training our model, we will use the
default number of variable subsets (i.e. the number of subsets that
results in 3 features per subset), and the default number of base
classifiers (i.e. 10).

    ed_rot <- train(Diabetes_binary_f ~ ., data = ed_train,
      method = "rotationForest",
      metric="logLoss",
      preProcess = c("center", "scale"),
      trControl = trainControl(method = "cv", number = 5, 
                               classProbs = TRUE, summaryFunction = mnLogLoss))

    ed_rot

    ## Rotation Forest 
    ## 
    ## 6636 samples
    ##   20 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5309, 5309, 5309, 5308, 5309 
    ## Resampling results across tuning parameters:
    ## 
    ##   K  L  logLoss  
    ##   1  3  0.5063080
    ##   1  6  0.5035840
    ##   1  9  0.5031453
    ##   2  3  0.5039212
    ##   2  6  0.5010824
    ##   2  9  0.5006250
    ##   4  3  0.5074294
    ##   4  6  0.5049547
    ##   4  9  0.5027550
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final values used for the model were K = 2 and L = 9.

## Sixth method: Bayesian Generalized Linear Model

The generalized linear model (GLM) is an approach incorporating various
probability distributions into a fitting procedure to describe
variability. A GLM with Bayesian inference is often used to avoid over
fitting. A Bayesian GLM can flexibly integrate various probability
distributions as model residuals and parameter uncertainty.

    ed_bglm<-train(Diabetes_binary_f~.,data=ed_train,
                 method="bayesglm", 
                 metric="logLoss",
                 trControl=trainControl(method = "cv",number = 5, classProbs=TRUE, 
                                        summaryFunction=mnLogLoss),
                 preProcess=c("center","scale")
    )
    ed_bglm

    ## Bayesian Generalized Linear Model 
    ## 
    ## 6636 samples
    ##   20 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5308, 5309, 5310, 5309, 5308 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.4728869

# Final Model Selection

Lastly, we generate a log loss statistic for each of the six
models/methods above by applying each one to our test data; after doing
so, we identify a “best” model as the one with lowest log loss
statistic.

    # set a function to output logLoss value from each model by the test set
    choose <- function(in_model)
    {
    pred <- predict(in_model, ed_test) #logLoss() requires a vector
    pred <- ifelse(pred=="no",0,1) # it has to be numerical

    logLoss(actual = ed_test$Diabetes_binary_f, predicted = pred)
    }

    # make the variable numerical to work in logLoss function
    ed_test$Diabetes_binary_f<-ifelse(ed_test$Diabetes_binary_f=="no",0,1)

    # Method 1
    # only output the best from method 1
    a<-c("1","2","3")[which.min(c(ed_logistic1$results[2],ed_logistic2$results[2],
                                  ed_logistic3$results[2]))]
    ifelse(a!=3,
           ifelse(a==1,
                  CM1<-choose(in_model = ed_logistic1),
                  CM1<-choose(in_model = ed_logistic2)
                  ),
           CM1<-choose(in_model = ed_logistic3)
      )

    ## [1] 8.081769

    paste0("logistic has a logLoss of ",CM1)

    ## [1] "logistic has a logLoss of 8.08176884303019"

    # Method 2
    CM2<-choose(in_model = ed_lasso)
    paste0("lasso has a logLoss of ",CM2)

    ## [1] "lasso has a logLoss of 8.08176856167994"

    # Method 3
    CM3<-choose(in_model = ed_ct)
    paste0("classification tree has a logLoss of ",CM3)

    ## [1] "classification tree has a logLoss of 7.9723841223116"

    # Method 4
    CM4<-choose(in_model = ed_rf)
    paste0("random forests has a logLoss of ",CM4)

    ## [1] "random forests has a logLoss of 8.1789853882892"

    # Method 5
    CM5<-choose(in_model = ed_rot)
    paste0("rotation forests has a logLoss of ",CM5)

    ## [1] "rotation forests has a logLoss of 8.10604836119746"

    # Method 6
    CM6<-choose(in_model = ed_bglm)
    paste0("glm has a logLoss of ",CM6)

    ## [1] "glm has a logLoss of 8.08176912438045"

## Final conclusion

The fit with the smallest logLoss value would be selected as the final
model.

    CM<-c(CM1,CM2,CM3,CM4,CM5,CM6)
    paste0("model ",c("1","2","3","4",
                    "5","6")[which.min(CM)]," is the best model")

    ## [1] "model 3 is the best model"
