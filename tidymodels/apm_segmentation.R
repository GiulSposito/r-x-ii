# install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(tidyverse)
library(tidymodels)
library(skimr)
library(corrplot)

## EDA

# dataset
data(segmentationOriginal)

# overview
str(segmentationOriginal)
skim(segmentationOriginal)

segData <- segmentationOriginal %>% 
  # removing #ID, #Train/test, #other feature flags (status)
  select(-Cell, -Case, -contains("Status") )

# corr

## Data Preparation

# training/testing
seg_split <- initial_split(segData, prop = .8, strata = Class)
seg_train <- training(seg_split)
seg_test  <- testing(seg_split)

# transform
seg_rec <- recipe(Class~., data=seg_train) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors())  %>% 
  step_BoxCox(all_predictors()) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  step_pca(all_predictors())

seg_rec_prep <- prep(seg_rec)
  
juice(seg_rec_prep)

step_corr(all_predictors())

step_nzv()  

