---
title: "Harvard|Choose Your Own Project| Swedish Data Crime"
author: "Edy Susanto"
output:
  word_document:
    toc: yes
    toc_depth: '2'
  pdf_document:
    toc: yes
    toc_depth: 2
  html_document:
    toc: yes
    toc_depth: '2'
    df_print: paged
---

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(kableExtra)) install.packages("kableExtra")
library(dplyr)
library(tidyverse)
library(kableExtra)

## Part 1 Introduction

# Crime analysis and prevention is a systematic approach for identifying and analyzing patterns and trends in crime. Our system can predict regions which have high probability for crime occurrence and can visualize crime prone areas.

## Part 2 Project Goal

# This project will mainly focus on creating a Classification Machine Learning System using Swedish Data Crime.e

# Link for the datasets is https://www.kaggle.com/mguzmann/swedishcrime

## Part 3.Load Requirement Packages

#Load all packages dan all libraries into RStudio

## Part 2 Load Dataset

# urlfile <-
#   "https://raw.githubusercontent.com/siedysoes/swedishcrime/main/reported.csv"

Crimes <- read.csv(urlfile, header = TRUE) # this is our working dataset
data(Crimes)
head(Crimes)

## Part 4.Data exploration and visualization

# The dataset is a data table made of 21 (columns) and a total of  observations (67 rows).  
Crimes %>% as_tibble()

# Number of NA into the dataset:

map_int(Crimes, function(.x) sum(is.na(.x)))

plot(Crimes$crimes.person, Crimes$Year)
temp_dataset <- Crimes

scatter.smooth(Crimes)


# We can see that there are total crimes in Swedish is increase by Year

## Part 5 Pre Data Processing

# **Principal Component Analysis(PCA)**

variable_importance = var_rank_info(Crimes, "Year")
variable_importance

ggplot(variable_importance, aes(x = reorder(var, gr), y = gr, fill = var)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_bw() + 
  xlab("") + 
  ylab("Variable Importance (based on Information Gain)") + 
  guides(fill = FALSE)
set.seed(1)
pca <- prcomp(Crimes %>% select(Year), scale = TRUE, center = TRUE)

str(pca)

summary(pca)

set.seed(1) 
# set.seed(1, sample.kind="Rounding") if using R 3.5.3 or later

test_index <- createDataPartition(y = Crimes$Year,
                                  times = 1, p = 0.2, list = FALSE) 
edx <- Crimes[-test_index,]
validation <- Crimes[test_index,]

#We will split edx data into train_set and test_set.

set.seed(1)
test_index <- createDataPartition(y = edx$Year,
                                  times = 1, p = 0.2, 
                                  list = FALSE)  # test_set 20%
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

## Part 6 Building Model

###
models <- c("glm", "lda", "naive_bayes", "svmLinear",
            "gamLoess", "qda", "knn", "kknn",
            "gam", "rf", "ranger", "wsrf", "mlp")

control <- trainControl(method = "cv",   # cross validation
                        number = 10,     # 10 k-folds or number 
                                  # of resampling iterations
                        repeats = 5)

data_train <- train_set       # first value for data parameter
data_test <-  test_set        # first we´ll use train and test dataset 
true_value <- test_set$Year # true outcome from test_set 


## Part 7 Prediction 

#####
model <- train(Year ~ crimes.total,
               data = Crimes,
               method = "lm")

model

fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 5)    # repeated five times

model.cv <- train(Year ~ crimes.total,
               data = Crimes,
               method = "lm",  # now we're using the lm method
               trControl = fitControl)  

model.cv   

predictions <- predict(model.cv, Crimes)

predictions

results <- sort(predictions)
barchart(predictions)
barplot.default(predictions)
plot.default(predictions)

## Part 8 Conclusion 

#Our Model Has succesfully made with RMSE  5.38960 , which is valid for prediction in SWedish Data Crime.


