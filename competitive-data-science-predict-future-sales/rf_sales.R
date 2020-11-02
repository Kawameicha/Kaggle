library(tidyverse)
library(tidymodels)

setwd("~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/")
train <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/sales_train.csv') %>% 
  mutate(item_cnt_day = ifelse(item_cnt_day < 0, 0, item_cnt_day),
         item_price   = ifelse(item_price < 0, 0, item_price))
items <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/items.csv')
shops <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/shops.csv')
valid <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/test.csv')

set.seed(42)

train_split <- train %>% 
  initial_split(prop = .8)

train_tbl <- training(train_split)
test_tbl <- testing(train_split)

folds <- vfold_cv(train_tbl, v = 5) # just to save time

rec_obj <- train %>%
  recipe(units ~ .) %>% 
  step_mutate(price_diff = price_1m-price_mean) %>%
  step_mutate_at(shop_name, fn = ~ gsub('(\\w)( )(.*)', '\\1', shop_name)) %>% 
  step_rename(shop_city = shop_name) %>%
  step_mutate(shop_id = as.factor(shop_id),
              item_id = as.factor(item_id),
              item_category_id = as.factor(item_category_id)) %>%
  step_other(shop_id, item_category_id, shop_city) %>%
  step_other(item_id, threshold = .005)
  # step_mutate(units = ifelse(units > 20, 20, units)) # clipping 0, 20

rec <- rec_obj %>%
  prep(train) %>%
  juice()

rf_model <- rand_forest() %>%
  set_args(mtry  = tune(),
           min_n = tune(),
           trees = 1000) %>% 
  set_engine("ranger", 
             num.threads = 2,
             importance = "impurity") %>% 
  set_mode("regression") 

rf_workflow <- workflow() %>%
  add_recipe(rec_obj) %>%
  add_model(rf_model)

rf_tune <- rf_workflow %>%
  tune_grid(resamples = folds,
            grid      = 10, # increase when submitting
            metrics   = metric_set(rmse, rsq))

rf_tune %>%
  show_best(metric = "rmse")

rf_best <- rf_tune %>%
  select_best(metric = "rmse")

rf_workflow_fin <- rf_workflow %>%
  finalize_workflow(rf_best)

rf_workflow_fin %>%
  last_fit(train_split) %>%
  collect_metrics()

# rf_workflow_fin %>%
#   last_fit(train_split) %>%
#   collect_predictions()

rf_model_fin <- rf_workflow_fin %>% 
  fit(train)

###
# Variable importance
rf_obj <- pull_workflow_fit(rf_model_fin)$fit

tibble(var = rf_obj$variable.importance,
       nam = rf_obj$variable.importance %>% names(),
) %>% 
  ggplot(aes(var, reorder(nam, var))) +
  geom_bar(stat  = 'identity', 
           alpha = .7, 
           width = .8
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()
  )
###

rf_pred <- rf_model_fin %>% 
  predict(valid)

valid <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/test.csv')

submission <- data.frame(ID = valid$ID,
                         rf_pred) %>% 
  rename(item_cnt_month = .pred)

write_csv(submission, 'sales_submission.csv')
