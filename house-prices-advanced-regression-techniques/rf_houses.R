library(tidyverse)
library(tidymodels)

setwd("~/Documents/Kaggle/house-prices-advanced-regression-techniques/")
train <- read.csv('~/Documents/Kaggle/house-prices-advanced-regression-techniques/train.csv') %>% 
  filter(!is.na(SalePrice))
test <- read.csv('~/Documents/Kaggle/house-prices-advanced-regression-techniques/test.csv')

set.seed(42)

train_split <- train %>% 
  initial_split(prop = .8)

train_tbl <- training(train_split)
test_tbl <- testing(train_split)

folds <- vfold_cv(train_tbl)

rec_obj <- train %>% 
  recipe(SalePrice ~ .) %>%
  step_rm(Id, FireplaceQu, Fence, Alley, MiscFeature, PoolQC, Street, Utilities) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_log(all_numeric(), -all_outcomes(), offset = .1) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>% 
  step_other(all_nominal(), -all_outcomes())

rec_obj %>%
  prep(train) %>%
  juice()

rf_model <- rand_forest() %>%
  set_args(mtry  = tune(), 
           min_n = tune(), 
           trees = 1000) %>% 
  set_engine("ranger", num.threads = 2) %>% 
  set_mode("regression") 

rf_workflow <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(rf_model)

rf_tune <- rf_workflow %>%
  tune_grid(resamples = folds,
            grid      = 25,
            metrics   = metric_set(rmse, rsq))

rf_tune %>%
  show_best(metric = "rmse") %>%
  select(.metric:std_err)

rf_best <- rf_tune %>%
  select_best(metric = "rmse")

rf_workflow_fin <- rf_workflow %>%
  finalize_workflow(rf_best)

rf_workflow_fin %>%
  last_fit(train_split) %>%
  collect_metrics()

rf_model_fin <- rf_workflow_fin %>% 
  fit(train)

rf_pred <- rf_model_fin %>% 
  predict(test)

submission <- data.frame(Id = test$Id,
                         rf_pred) %>% 
  rename(SalePrice = .pred)

write_csv(submission, 'houses_submission.csv')
