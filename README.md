project3
================
Xiaomeng Liu
2023-11-05

- [library](#library)
- [Introduction section](#introduction-section)
- [Data](#data)
  - [Read in the data](#read-in-the-data)
  - [Combine 1 and 2 Education
    levels](#combine-1-and-2-education-levels)
  - [Convert variables to factors](#convert-variables-to-factors)
- [Summarizations](#summarizations)
- [Modeling](#modeling)
- [Final Model Selection](#final-model-selection)

# library

``` r
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
```

# Introduction section

``` r
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
```

# Data

### Read in the data

``` r
#read in csv file
diabetes<-read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")
```

### Combine 1 and 2 Education levels

``` r
#create collapsed version of education variable
diabetes$Education <- recode(diabetes$Education, `1` = 2)
table(diabetes$Education)
```

    ## 
    ##      2      3      4      5      6 
    ##   4217   9478  62750  69910 107325

### Convert variables to factors

``` r
#create factor version of variables, where applicable
#in order to facilitate the EDA, for now at least, we'll retain both the factor version and the numeric version of each variable (we may need to drop one version, though, before running models)
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


#separate dataset for each education level
ed_level2 <- subset(diabetes, Education==2)
ed_level3 <- subset(diabetes, Education==3)
ed_level4 <- subset(diabetes, Education==4)
ed_level5 <- subset(diabetes, Education==5)
ed_level6 <- subset(diabetes, Education==6)
```

# Summarizations

EDA done by ERich

# Modeling

``` r
# set the seed
set.seed(433)
# split the training and testing
indextrain<-createDataPartition(y=ed_level2$Diabetes_binary,p=0.7,list=FALSE)
ed_train<-ed_level2[indextrain,]
ed_test<-ed_level2[-indextrain,]
```

#### what log loss is:

The first group member should research and write up a paragraph about
what log loss is and why we may prefer it to things like accuracy.

#### First method: logistic regression

The first group member should provide a reasonably thorough explanation
of what a logistic regression is and why we apply it to this kind of
data. Then they should fit three candidate logistic regression models
and choose the best model.

``` r
ed_logistic<-train(Diabetes_binary_f~.,data=ed_level2,
             method="glm", 
             metric="logLoss",
             trControl=trainControl(method = "cv",number = 5),
             preProcess=c("center","scale")
)
```

    ## Warning in train.default(x, y, weights = w, ...): The metric "logLoss" was not in the result set. Accuracy will be used instead.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances: Education,
    ## Education_f3, Education_f4, Education_f5, Education_f6

    ## Warning: glm.fit: algorithm did not converge

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == : prediction from rank-deficient fit; attr(*, "non-estim") has
    ## doubtful cases

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances: Education,
    ## Education_f3, Education_f4, Education_f5, Education_f6

    ## Warning: glm.fit: algorithm did not converge

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == : prediction from rank-deficient fit; attr(*, "non-estim") has
    ## doubtful cases

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances: Education,
    ## Education_f3, Education_f4, Education_f5, Education_f6

    ## Warning: glm.fit: algorithm did not converge

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == : prediction from rank-deficient fit; attr(*, "non-estim") has
    ## doubtful cases

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances: Education,
    ## Education_f3, Education_f4, Education_f5, Education_f6

    ## Warning: glm.fit: algorithm did not converge

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == : prediction from rank-deficient fit; attr(*, "non-estim") has
    ## doubtful cases

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances: Education,
    ## Education_f3, Education_f4, Education_f5, Education_f6

    ## Warning: glm.fit: algorithm did not converge

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == : prediction from rank-deficient fit; attr(*, "non-estim") has
    ## doubtful cases

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances: Education,
    ## Education_f3, Education_f4, Education_f5, Education_f6

    ## Warning: glm.fit: algorithm did not converge

``` r
ed_logistic
```

    ## Generalized Linear Model 
    ## 
    ## 4217 samples
    ##   40 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (63), scaled (63) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 3374, 3373, 3374, 3373, 3374 
    ## Resampling results:
    ## 
    ##   Accuracy  Kappa
    ##   1         1

#### Second method: Lasso logistic

Erich part

#### Third method: Classification tree

Erich part

#### Fourth method: Random forest

#### Fifth method:

new method by Erich

#### Sixth method:

new method by Xiaomeng

# Final Model Selection

You should now have six best models (one for each model type above)
