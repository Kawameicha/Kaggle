library(tidyverse)
library(tidymodels)

setwd("~/Documents/GitHub/Kaggle/house-prices-advanced-regression-techniques/")
train <- read.csv('~/Documents/GitHub/Kaggle/house-prices-advanced-regression-techniques/train.csv') %>% 
  filter(!is.na(SalePrice))
test <- read.csv('~/Documents/GitHub/Kaggle/house-prices-advanced-regression-techniques/test.csv')

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
  step_other(all_nominal(), -all_outcomes()) %>% 
  # step_novel(all_predictors(), -all_numeric()) %>%
  # step_knnimpute(all_predictors())
  step_dummy(all_nominal(), -all_outcomes())

rec_obj %>%
  prep(train) %>%
  juice()

xgb_model <- boost_tree() %>%
  set_args(tree_depth = tune(),
           min_n = tune(),
           loss_reduction = tune(),
           sample_size = tune(),
           mtry = tune(),
           learn_rate = tune()) %>%
  set_engine("xgboost", num.threads = 2) %>%
  set_mode("regression")

xgb_grid <- grid_max_entropy(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_tbl),
  learn_rate(), 
  size = 60)

xgb_workflow <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(xgb_model)

xgb_tune <- xgb_workflow %>%
  tune_grid(resamples = folds,
            grid      = xgb_grid,
            metrics   = metric_set(rmse, rsq))

xgb_best <- xgb_tune %>%
  select_best(metric = "rmse")

xgb_workflow_fin <- xgb_workflow %>%
  finalize_workflow(xgb_best)

xgb_workflow_fin %>%
  last_fit(train_split) %>%
  collect_metrics()

xgb_model_fin <- xgb_workflow_fin %>%
  fit(train)

xgb_pred <- xgb_model_fin %>% 
  predict(test)

submission <- data.frame(Id = test$Id,
                         xgb_pred) %>% 
  rename(SalePrice = .pred)

write_csv(submission, 'houses_submission.csv')
