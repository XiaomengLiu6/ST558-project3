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
  - [Data cleaning](#data-cleaning)
  - [train and test set](#train-and-test-set)
  - [what log loss is:](#what-log-loss-is)
  - [First method: logistic
    regression](#first-method-logistic-regression)
  - [Second method: Lasso logistic](#second-method-lasso-logistic)
  - [Third method: Classification
    tree](#third-method-classification-tree)
  - [Fourth method: Random forest](#fourth-method-random-forest)
  - [Fifth method:](#fifth-method)
  - [Sixth method: Bayesian Generalized Linear
    Model](#sixth-method-bayesian-generalized-linear-model)
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


#subset to specific level of education per *params* setting
temp <- subset(diabetes, Education==4)
```

# Summarizations

``` r
#confirm that we're working with the desired set of cases
table(temp$Education, temp$Education_f)
```

    ##    
    ##         2     3     4     5     6
    ##   4     0     0 62750     0     0

``` r
ggplot(data=temp, aes(x=Education_f)) + 
  geom_dotplot(binwidth = .05, method = "histodot") + 
  labs(title = "confirm that we're working with the desired set of cases")
```

![](README_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
#function to check prevalence of diabetes at each level of each factor, and generate corresponding plots
#(I still need to flesh out this function such that it labels the plots)
explore <- function(by_var)
{
results1 <- temp %>%
  group_by({{by_var}}) %>%
  summarize(diabetes_rate = mean(Diabetes_binary))
    #passing variable names to function using curly brackets:
    #https://stackoverflow.com/questions/63433728/how-do-i-pass-a-variable-name-to-an-argument-in-a-function
print(results1)

results2 <- ggplot(data=temp, aes(x={{by_var}}, fill=Diabetes_binary_f)) + 
  geom_bar(stat="count")
print(results2)
}

#probably need to run the above function for at least the sex, age, and income variables, but may not need to run it for this entire list
explore(by_var = HighBP_f)
```

    ## # A tibble: 2 × 2
    ##   HighBP_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0               0.0830
    ## 2 1               0.268

![](README_files/figure-gfm/unnamed-chunk-67-2.png)<!-- -->

``` r
explore(by_var = HighChol_f)
```

    ## # A tibble: 2 × 2
    ##   HighChol_f diabetes_rate
    ##   <fct>              <dbl>
    ## 1 0                  0.107
    ## 2 1                  0.257

![](README_files/figure-gfm/unnamed-chunk-67-3.png)<!-- -->

``` r
explore(by_var = CholCheck_f)
```

    ## # A tibble: 2 × 2
    ##   CholCheck_f diabetes_rate
    ##   <fct>               <dbl>
    ## 1 0                  0.0374
    ## 2 1                  0.182

![](README_files/figure-gfm/unnamed-chunk-67-4.png)<!-- -->

``` r
explore(by_var = Smoker_f)
```

    ## # A tibble: 2 × 2
    ##   Smoker_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0                0.170
    ## 2 1                0.182

![](README_files/figure-gfm/unnamed-chunk-67-5.png)<!-- -->

``` r
explore(by_var = Stroke_f)
```

    ## # A tibble: 2 × 2
    ##   Stroke_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0                0.168
    ## 2 1                0.322

![](README_files/figure-gfm/unnamed-chunk-67-6.png)<!-- -->

``` r
explore(by_var = HeartDiseaseorAttack_f)
```

    ## # A tibble: 2 × 2
    ##   HeartDiseaseorAttack_f diabetes_rate
    ##   <fct>                          <dbl>
    ## 1 0                              0.154
    ## 2 1                              0.340

![](README_files/figure-gfm/unnamed-chunk-67-7.png)<!-- -->

``` r
explore(by_var = PhysActivity_f)
```

    ## # A tibble: 2 × 2
    ##   PhysActivity_f diabetes_rate
    ##   <fct>                  <dbl>
    ## 1 0                      0.223
    ## 2 1                      0.153

![](README_files/figure-gfm/unnamed-chunk-67-8.png)<!-- -->

``` r
explore(by_var = Fruits_f)
```

    ## # A tibble: 2 × 2
    ##   Fruits_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 0                0.185
    ## 2 1                0.170

![](README_files/figure-gfm/unnamed-chunk-67-9.png)<!-- -->

``` r
explore(by_var = Veggies_f)
```

    ## # A tibble: 2 × 2
    ##   Veggies_f diabetes_rate
    ##   <fct>             <dbl>
    ## 1 0                 0.200
    ## 2 1                 0.168

![](README_files/figure-gfm/unnamed-chunk-67-10.png)<!-- -->

``` r
explore(by_var = HvyAlcoholConsump_f)
```

    ## # A tibble: 2 × 2
    ##   HvyAlcoholConsump_f diabetes_rate
    ##   <fct>                       <dbl>
    ## 1 0                          0.182 
    ## 2 1                          0.0743

![](README_files/figure-gfm/unnamed-chunk-67-11.png)<!-- -->

``` r
explore(by_var = AnyHealthcare_f)
```

    ## # A tibble: 2 × 2
    ##   AnyHealthcare_f diabetes_rate
    ##   <fct>                   <dbl>
    ## 1 0                       0.128
    ## 2 1                       0.180

![](README_files/figure-gfm/unnamed-chunk-67-12.png)<!-- -->

``` r
explore(by_var = NoDocbcCost_f)
```

    ## # A tibble: 2 × 2
    ##   NoDocbcCost_f diabetes_rate
    ##   <fct>                 <dbl>
    ## 1 0                     0.174
    ## 2 1                     0.195

![](README_files/figure-gfm/unnamed-chunk-67-13.png)<!-- -->

``` r
explore(by_var = GenHlth_f)
```

    ## # A tibble: 5 × 2
    ##   GenHlth_f diabetes_rate
    ##   <fct>             <dbl>
    ## 1 1                0.0367
    ## 2 2                0.0883
    ## 3 3                0.189 
    ## 4 4                0.316 
    ## 5 5                0.370

![](README_files/figure-gfm/unnamed-chunk-67-14.png)<!-- -->

``` r
explore(by_var = DiffWalk_f)
```

    ## # A tibble: 2 × 2
    ##   DiffWalk_f diabetes_rate
    ##   <fct>              <dbl>
    ## 1 0                  0.135
    ## 2 1                  0.316

![](README_files/figure-gfm/unnamed-chunk-67-15.png)<!-- -->

``` r
explore(by_var = Sex_f)
```

    ## # A tibble: 2 × 2
    ##   Sex_f diabetes_rate
    ##   <fct>         <dbl>
    ## 1 0             0.174
    ## 2 1             0.180

![](README_files/figure-gfm/unnamed-chunk-67-16.png)<!-- -->

``` r
explore(by_var = Age_f)
```

    ## # A tibble: 13 × 2
    ##    Age_f diabetes_rate
    ##    <fct>         <dbl>
    ##  1 1            0.0195
    ##  2 2            0.0268
    ##  3 3            0.0426
    ##  4 4            0.0712
    ##  5 5            0.0953
    ##  6 6            0.131 
    ##  7 7            0.150 
    ##  8 8            0.170 
    ##  9 9            0.210 
    ## 10 10           0.246 
    ## 11 11           0.245 
    ## 12 12           0.233 
    ## 13 13           0.195

![](README_files/figure-gfm/unnamed-chunk-67-17.png)<!-- -->

``` r
explore(by_var = Income_f)
```

    ## # A tibble: 8 × 2
    ##   Income_f diabetes_rate
    ##   <fct>            <dbl>
    ## 1 1               0.246 
    ## 2 2               0.268 
    ## 3 3               0.233 
    ## 4 4               0.214 
    ## 5 5               0.182 
    ## 6 6               0.158 
    ## 7 7               0.138 
    ## 8 8               0.0979

![](README_files/figure-gfm/unnamed-chunk-67-18.png)<!-- -->

``` r
#correlation matrix (outcome var x continuous vars)
corr_vars <-
  temp %>% select(c(Diabetes_binary, BMI, MentHlth, PhysHlth))
correlation <- cor(corr_vars, method = "spearman")
corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number", add = TRUE, diag = FALSE, tl.pos = "n")
```

![](README_files/figure-gfm/unnamed-chunk-67-19.png)<!-- -->

``` r
#density plots / boxplots (outcome var x continuous vars)
#I'm guessing we could just choose one or the other
ggplot(data=temp, aes(x=BMI, fill=Diabetes_binary_f)) + 
  geom_density(adjust = 0.5, alpha = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-67-20.png)<!-- -->

``` r
ggplot(data=temp, aes(x=Diabetes_binary_f, y=BMI)) + geom_boxplot()
```

![](README_files/figure-gfm/unnamed-chunk-67-21.png)<!-- -->

``` r
ggplot(data=temp, aes(x=MentHlth, fill=Diabetes_binary_f)) + 
  geom_density(adjust = 0.5, alpha = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-67-22.png)<!-- -->

``` r
ggplot(data=temp, aes(x=Diabetes_binary_f, y=MentHlth)) + geom_boxplot()
```

![](README_files/figure-gfm/unnamed-chunk-67-23.png)<!-- -->

``` r
ggplot(data=temp, aes(x=PhysHlth, fill=Diabetes_binary_f)) + 
  geom_density(adjust = 0.5, alpha = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-67-24.png)<!-- -->

``` r
bp3 <- ggplot(data=temp, aes(x=Diabetes_binary_f, y=PhysHlth)) + geom_boxplot()
```

# Modeling

## Data cleaning

``` r
#prior to running models, in instances where we have both a factor and a non-factor version of a given variable, we need to first drop the non-factor version of the variable
#we also need to drop both versions of the education variable (since it will not vary given that we've subset our data to a specific education level)
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

temp$Diabetes_binary_f<-ifelse(temp$Diabetes_binary_f==0,"no","yes")
temp$Diabetes_binary_f<-as.factor(temp$Diabetes_binary_f)
```

## train and test set

``` r
# set the seed
set.seed(433)
# split the training and testing
indextrain<-createDataPartition(y=temp$Diabetes_binary,p=0.7,list=FALSE)
ed_train<-temp[indextrain,]
ed_test<-temp[-indextrain,]
```

## what log loss is:

Log loss is a common evaluation metric for binary classification models.
It measure the performance of a model by quantifying the difference
between predicted probabilities and actual values. We prefer it because
log loss penalizes confident and incorrect predictors more heavily.It
also provides a continuous and differentiable meausre of the model’s
performance, making it suitable of optimization algorithms.

## First method: logistic regression

\###explanation of what a logistic regression is

The logistic regression is modeling average number of successes for a
given x, i.e. probability of success.Basic logistic regression models
success probability using the logistic function
$P(success|yard)=\frac{e^{\beta_0+\beta_1x}}{1+e^{\beta_0+\beta_1x}}$

\###why we apply it to this kind of data We have a response variable
that is success/failure and it is perfect for fitting a logistic
regression mode.

\###fit three candidate logistic regression models and choose the best
model.

``` r
ed_logistic1<-train(Diabetes_binary_f~BMI+HighChol_f+HighBP_f,data=ed_train,
             method="glm", 
             metric="logLoss",
             trControl=trainControl(method = "cv",number = 5,classProbs = TRUE, summaryFunction = mnLogLoss),
             preProcess=c("center","scale")
)
ed_logistic1
```

    ## Generalized Linear Model 
    ## 
    ## 43926 samples
    ##     3 predictor
    ##     2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (3), scaled (3) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 35140, 35142, 35141, 35140, 35141 
    ## Resampling results:
    ## 
    ##   logLoss 
    ##   0.413536

``` r
ed_logistic2<-train(Diabetes_binary_f~BMI+HighChol_f+HighBP_f+MentHlth+PhysActivity_f,data=ed_train,
             method="glm", 
             metric="logLoss",
             trControl=trainControl(method = "cv",number = 5,classProbs = TRUE, summaryFunction = mnLogLoss),
             preProcess=c("center","scale")
)
ed_logistic2
```

    ## Generalized Linear Model 
    ## 
    ## 43926 samples
    ##     5 predictor
    ##     2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (5), scaled (5) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 35141, 35141, 35141, 35141, 35140 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.4121787

``` r
ed_logistic3<-train(Diabetes_binary_f~.,data=ed_train,
             method="glm", 
             metric="logLoss",
             trControl=trainControl(method = "cv",number = 5,classProbs = TRUE, summaryFunction = mnLogLoss),
             preProcess=c("center","scale")
)
ed_logistic3
```

    ## Generalized Linear Model 
    ## 
    ## 43926 samples
    ##    20 predictor
    ##     2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 35142, 35141, 35141, 35140, 35140 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.3844808

``` r
# return the result
paste0("According to the results, the lowest logLoss is the model ", c("1","2","3")[which.min(c(ed_logistic1$results[2],ed_logistic2$results[2],ed_logistic3$results[2]))])
```

    ## [1] "According to the results, the lowest logLoss is the model 3"

## Second method: Lasso logistic

Erich part

## Third method: Classification tree

Erich part

## Fourth method: Random forest

\###Explanation of the random forest: The random forest uses the same
idea as bagging. It creates multiple trees from bootstrap samples and
averages results. It uses a random subset of predictors for each
bootstrap sample fit.

\###why we might use it instead of a basic classification tree: We want
to use random forest because we do not want to use all the predictors.If
a really strong predictor exists, every bootstrap tree will probably use
it for the first split and it will make the prediction more correlated.

``` r
ed_rf<-train(Diabetes_binary_f~.,#BMI+HighBP_f+HighChol_f
             data=ed_train,
             method="rf", 
             metric="logLoss",
             trControl=trainControl(method = "cv",number = 5, classProbs=TRUE, summaryFunction=mnLogLoss),
             preProcess=c("center","scale"),
             tuneGrid=data.frame(mtry=c(5:7))
)
ed_rf
```

    ## Random Forest 
    ## 
    ## 43926 samples
    ##    20 predictor
    ##     2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 35142, 35141, 35140, 35140, 35141 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  logLoss  
    ##   5     0.4234489
    ##   6     0.4239566
    ##   7     0.4196037
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 7.

## Fifth method:

new method by Erich

## Sixth method: Bayesian Generalized Linear Model

``` r
ed_bglm<-train(Diabetes_binary_f~.,data=ed_train,
             method="bayesglm", 
             metric="logLoss",
             trControl=trainControl(method = "cv",number = 5, classProbs=TRUE, summaryFunction=mnLogLoss),
             preProcess=c("center","scale")
)
ed_bglm
```

    ## Bayesian Generalized Linear Model 
    ## 
    ## 43926 samples
    ##    20 predictor
    ##     2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (40), scaled (40) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 35142, 35141, 35140, 35141, 35140 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.3843105

# Final Model Selection

You should now have six best models (one for each model type above)

``` r
# Method 1
a<-c("1","2","3")[which.min(c(ed_logistic1$results[2],ed_logistic2$results[2],ed_logistic3$results[2]))]
if (a==1){
  CM1<-confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_logistic1,newdata = ed_test))
}
ifelse(a==2,
  CM1<-confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_logistic2,newdata = ed_test)),
  CM1<-confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_logistic3,newdata = ed_test))
  )
```

    ## [[1]]
    ## [1] "no"

``` r
(CM1<-CM1$overall[1])
```

    ##  Accuracy 
    ## 0.8283043

``` r
# Method 2
#confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_rf,newdata = ed_test))
# Method 3
#confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_rf,newdata = ed_test))
# Method 4
CM4<-confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_rf,newdata = ed_test))
(CM<-CM4$overall[1])
```

    ##  Accuracy 
    ## 0.8263387

``` r
# Method 5
#confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_rf,newdata = ed_test))
# Method 6
CM6<-confusionMatrix(data = ed_test$Diabetes_binary_f,predict(ed_bglm,newdata = ed_test))
(CM6<-CM6$overall[1])
```

    ##  Accuracy 
    ## 0.8282512

``` r
CM<-c(CM1,CM6)
paste0(c("Model 1","Model 6")[which.max(CM)]," is the best model")
```

    ## [1] "Model 1 is the best model"
