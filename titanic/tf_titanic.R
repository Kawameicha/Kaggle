library(tidyverse)
library(tidymodels)

setwd("~/Documents/GitHub/Kaggle/titanic/")
train <- read_csv('~/Documents/GitHub/Kaggle/titanic/train.csv') %>% 
  mutate(Survived = as.factor(Survived))
test <- read_csv('~/Documents/GitHub/Kaggle/titanic/test.csv')

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
  step_discretize(Age, Fare)

# rec_obj <- train %>%
#   recipe(Survived ~ .) %>%
#   step_rm(PassengerId, Ticket, Cabin) %>% 
#   step_mutate_at(Name, fn = ~ gsub('(.*,)|(\\..*)', '', Name)) %>%
#   step_rename(Title = Name) %>%
#   step_other(Title) %>% 
#   step_modeimpute(all_nominal(), -all_outcomes()) %>%
#   step_meanimpute(all_numeric(), -all_outcomes()) %>%
#   step_discretize(Age, Fare)

rec_obj %>%
  prep(train) %>%
  juice()

rf_model <- rand_forest() %>%
  set_args(mtry  = tune(), 
           min_n = tune(), 
           trees = 1000) %>% 
  set_engine("ranger", num.threads = 2) %>% 
  set_mode("classification")

rf_workflow <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(rf_model)

rf_tune <- rf_workflow %>%
  tune_grid(resamples = folds,
            grid      = 25,
            metrics   = metric_set(accuracy, roc_auc))

rf_tune %>%
  show_best(metric = "accuracy")

rf_best <- rf_tune %>%
  select_best(metric = "accuracy")

rf_workflow_fin <- rf_workflow %>%
  finalize_workflow(rf_best)

rf_workflow_fin %>%
  last_fit(train_split) %>%
  collect_metrics()

rf_model_fin <- rf_workflow_fin %>% 
  fit(train)

rf_pred <- rf_model_fin %>% 
  predict(test)

submission <- data.frame(PassengerId = test$PassengerId,
                         rf_pred) %>% 
  rename(Survived = .pred_class)

write_csv(submission, 'titanic_submission.csv')
