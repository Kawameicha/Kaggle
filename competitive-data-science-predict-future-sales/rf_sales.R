library(tidyverse)
library(tidymodels)

# load and heal data
train <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/sales_train.csv', 
                  col_types = '_?????') %>% 
  mutate(shop_id = case_when(shop_id == 57 ~ 0, 
                             shop_id == 58 ~ 1, 
                             shop_id == 11 ~ 10, 
                             shop_id == 40 ~ 39, 
                             TRUE ~ shop_id)) %>% 
  filter(between(item_cnt_day, 0, 1000) & between(item_price, 0, 100000))

valid <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/test.csv', 
                  col_types = '_??') %>% 
  mutate(shop_id = case_when(shop_id == 57 ~ 0, 
                             shop_id == 58 ~ 1, 
                             shop_id == 11 ~ 10, 
                             shop_id == 40 ~ 39, 
                             TRUE ~ shop_id))

items <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/items.csv', 
                  col_types = '_??')

shops <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/shops.csv', 
                  col_types = '??') %>% 
  mutate(shop_name = gsub('!', '', shop_name), 
         shop_name = gsub('Сергиев Посад', 'СергиевПосад', shop_name), 
         shop_city = factor(gsub('(\\w)( )(.*)', '\\1', shop_name))) %>% 
  select(-shop_name)

# merge train and valid
train_merge <- valid %>% 
  mutate(dbn_id = 1) %>% 
  left_join(data.frame(dbn_id = 1, date_block_num = seq(0, 34, by = 1)), by = 'dbn_id') %>% 
  left_join(train, by = c('shop_id', 'item_id', 'date_block_num')) %>% 
  mutate_at(vars(item_cnt_day), replace_na, replace = 0) %>% 
  select(-dbn_id) %>% 
  arrange(shop_id, item_id, date_block_num)

# last 3 months
train_sales <- train_merge %>% 
  filter(between(date_block_num, 30, 32)) %>% 
  group_by(shop_id, item_id, date_block_num) %>% 
  summarise(sales_last_1m = sum(item_cnt_day, na.rm = TRUE), 
            price_last_1m = mean(item_price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(shop_id, item_id, date_block_num) %>% 
  mutate(sales_last_2m = lag(sales_last_1m, 1), 
         price_last_2m = lag(price_last_1m, 1), 
         sales_last_3m = lag(sales_last_1m, 2), 
         price_last_3m = lag(price_last_1m, 2)) %>% 
  mutate(mean_sales_3m = (sales_last_1m + sales_last_2m + sales_last_3m)/3) %>% 
  filter(date_block_num == 32)

# last 6 months
train_6_month <- train_merge %>% 
  filter(between(date_block_num, 27, 32)) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(n_zeros_sales = sum(item_cnt_day == 0, na.rm = TRUE), 
            sales_all_min = min(item_cnt_day, na.rm = TRUE), 
            sales_all_med = median(item_cnt_day, na.rm = TRUE), 
            sales_all_max = max(item_cnt_day, na.rm = TRUE), 
            sales_all_dev = sd(item_cnt_day, na.rm = TRUE), 
            price_all_min = min(item_price, na.rm = TRUE), 
            price_all_med = median(item_price, na.rm = TRUE), 
            price_all_max = max(item_price, na.rm = TRUE), 
            price_all_dev = sd(item_price, na.rm = TRUE)) %>% 
  ungroup()

# check old/new item
train_novelty <- train_merge %>% 
  filter(item_cnt_day != 0) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(month_f_sales = min(date_block_num, na.rm = TRUE), 
            month_l_sales = max(date_block_num, na.rm = TRUE), 
            month_n_sales = month_l_sales - month_f_sales, 
            no_sales_item = ifelse(month_l_sales <= 26, 'yes', 'no'), 
            # new
            new_sold_item = ifelse(month_f_sales == 33, 'yes', 'no')) %>% 
  ungroup()

# about shops
train_shops <- train %>% 
  filter(item_cnt_day != 0) %>% 
  group_by(shop_id) %>% 
  summarise(shop_items = n_distinct(item_id), 
            shop_sales = sum(item_cnt_day, na.rm = TRUE), 
            shop_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(shops, by = c('shop_id'))

# about cities
train_where <- train_shops %>% 
  group_by(shop_city) %>% 
  summarise(city_shops = n_distinct(shop_id), 
            city_perfo = sum(shop_perfo, na.rm = TRUE)) %>% 
  ungroup()

# about items
train_items <- train %>% 
  filter(item_cnt_day != 0) %>% 
  group_by(item_id) %>% 
  summarise(item_shops = n_distinct(shop_id), 
            item_sales = sum(item_cnt_day, na.rm = TRUE), 
            item_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(items, by = c('item_id'))

# about cat_id
train_catId <- train_items %>% 
  group_by(item_category_id) %>% 
  summarise(catid_item = n_distinct(item_id), 
            catid_perf = sum(item_perfo, na.rm = TRUE)) %>% 
  ungroup()

# units to predict
train_units <- train_merge %>% 
  filter(date_block_num == 33) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(units = sum(item_cnt_day, na.rm = TRUE)) %>% 
  ungroup()

train <- valid %>% 
  left_join(train_units, by = c('shop_id', 'item_id')) %>% 
  left_join(train_sales, by = c('shop_id', 'item_id')) %>% 
  left_join(train_6_month, by = c('shop_id', 'item_id')) %>% 
  left_join(train_novelty, by = c('shop_id', 'item_id')) %>% 
  left_join(train_shops, by = c('shop_id')) %>% 
  left_join(train_items, by = c('item_id')) %>% 
  left_join(train_where, by = c('shop_city')) %>% 
  left_join(train_catId, by = c('item_category_id')) %>% 
  # replace na
  mutate_at(vars(starts_with('price'), starts_with('month')), replace_na, replace = 0) %>% 
  mutate_at(vars(no_sales_item), replace_na, replace = 'yes') %>% 
  # new
  mutate_at(vars(new_sold_item), replace_na, replace = 'no') %>% 
  # unit clipping 0, 20
  mutate(units = ifelse(units > 20, 20, units)) %>% 
  # drop new items
  drop_na()

set.seed(42)

train_split <- train %>% 
  initial_split(prop = .8)

train_tbl <- training(train_split)

folds <- vfold_cv(train_tbl, v = 3) # just to save time

rec_obj <- train %>%
  recipe(units ~ .) %>% 
  # new
  step_mutate(sales_diff_1m = sales_last_1m - mean_sales_3m) %>%
  step_other(item_category_id, shop_city, threshold = .025) %>% 
  step_rm(date_block_num)

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

###
# bayesian optimization
# params <- parameters(rf_model) %>% 
#   finalize(train)
# 
# rf_tune <- rf_workflow %>% 
#   tune_bayes(resamples = folds, 
#              param_info = params, 
#              iter = 30, 
#              metrics = metric_set(rmse, rsq), 
#              initial = 10, 
#              control = control_bayes(verbose = TRUE, 
#                                      no_improve = 10, 
#                                      seed = 123))
###

rf_tune <- rf_workflow %>% 
  tune_grid(resamples = folds, 
            grid      = 3, # increase when submitting
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

rf_pred <- rf_workflow_fin %>% 
  last_fit(train_split) %>% 
  collect_predictions()

rf_model_fin <- rf_workflow_fin %>% 
  fit(train)

###
# Variable importance
# rf_obj <- pull_workflow_fit(rf_model_fin)$fit
# 
# tibble(var = rf_obj$variable.importance, 
#        nam = rf_obj$variable.importance %>% names()) %>% 
#   ggplot(aes(var, reorder(nam, var))) + 
#   geom_bar(stat  = 'identity', alpha = .7, width = .8) + 
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_blank(), 
#         axis.text.x = element_blank())
###

rf_pred <- rf_model_fin %>% 
  predict(valid)

valid <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/test.csv')

submission <- data.frame(ID = valid$ID, 
                         rf_pred) %>% 
  rename(item_cnt_month = .pred)

write_csv(submission, 'sales_submission.csv')
