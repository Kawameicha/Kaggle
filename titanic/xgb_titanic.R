library(tidyverse)
library(tidymodels)

setwd("~/Documents/Kaggle/titanic/")
train <- read_csv('~/Documents/Kaggle/titanic/train.csv') %>% 
  mutate(Survived = as.factor(Survived))
test <- read_csv('~/Documents/Kaggle/titanic/test.csv')

set.seed(42)

train_split <- train %>% 
  initial_split(prop = .8)

train_tbl <- training(train_split)
test_tbl <- testing(train_split)

folds <- vfold_cv(train_tbl)

rec_obj <- train %>%
  recipe(Survived ~ .) %>%
  step_rm(PassengerId, Name, Ticket, Cabin) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
  step_discretize(Age, Fare) %>% 
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
  set_mode("classification")

xgb_workflow <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(xgb_model)

xgb_grid <- grid_max_entropy(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_tbl),
  learn_rate(), 
  size = 60)

xgb_tune <- xgb_workflow %>%
  tune_grid(resamples = folds,
            grid      = xgb_grid,
            metrics   = metric_set(accuracy, roc_auc))

xgb_best <- xgb_tune %>%
  select_best(metric = "accuracy")

xgb_workflow_fin <- xgb_workflow %>%
  finalize_workflow(xgb_best)

xgb_workflow_fin %>%
  last_fit(train_split) %>%
  collect_metrics()

xgb_model_fin <- xgb_workflow_fin %>%
  fit(train)

xgb_pred <- xgb_model_fin %>% 
  predict(test)

submission <- data.frame(PassengerId = test$PassengerId,
                         xgb_pred) %>% 
  rename(Survived = .pred_class)

write_csv(submission, 'titanic_submission.csv')
