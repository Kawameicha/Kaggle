train <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/sales_train.csv') %>% 
  mutate(item_cnt_day = ifelse(item_cnt_day < 0, 0, item_cnt_day),
         item_price = ifelse(item_price < 0, 0, item_price),
         shop_id = ifelse(shop_id == 57, 0, shop_id),
         shop_id = ifelse(shop_id == 58, 1, shop_id),
         shop_id = ifelse(shop_id == 11, 10, shop_id),
         shop_id = ifelse(shop_id == 40, 39, shop_id)) %>%
  filter(item_cnt_day <= 100000 & item_price <= 1000)

items <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/items.csv')

shops <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/shops.csv') %>% 
  mutate(shop_name = gsub('!', '', shop_name),
         shop_name = gsub('(\\w)( )(.*)', '\\1', shop_name))

# last 2 months
train_month <- train %>% 
  filter(date_block_num %in% c(31, 32)) %>% 
  group_by(date_block_num, shop_id, item_id) %>% 
  summarise(sales_last_1m = sum(item_cnt_day, na.rm = TRUE), 
            price_last_1m = max(item_price, na.rm = TRUE)) %>% 
  mutate(sales_last_2m = lag(sales_last_1m, 1), 
         price_last_2m = lag(price_last_1m, 1), 
         sales_mean_12m = (sales_last_1m + sales_last_2m)/2,
         price_mean_12m = (price_last_1m + price_last_2m)/2) %>% 
  filter(date_block_num == 32) %>% 
  ungroup() %>% 
  select(-date_block_num)

# last 2 years
train_years <- train %>% 
  filter(date_block_num %in% c(9, 21)) %>% 
  group_by(date_block_num, shop_id, item_id) %>% 
  summarise(sales_last_1y = sum(item_cnt_day, na.rm = TRUE), 
            price_last_1y = max(item_price, na.rm = TRUE)) %>% 
  mutate(sales_last_2y = lag(sales_last_1y, 1), 
         price_last_2y = lag(price_last_1y, 1), 
         sales_mean_12y = (sales_last_1y + sales_last_2y)/2,
         price_mean_12y = (price_last_1y + price_last_2y)/2) %>% 
  filter(date_block_num == 21) %>% 
  ungroup() %>% 
  select(-date_block_num)

# about shops
train_shops <- train %>% 
  filter(date_block_num != 33) %>%
  group_by(shop_id) %>% 
  summarise(shop_items = n_distinct(item_id), 
            shop_sales = sum(item_cnt_day, na.rm = TRUE),
            shop_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  left_join(shops, by = c('shop_id'))#  %>% 
  # mutate(shop_name = gsub('(\\w)( )(.*)', '\\1', shop_name))

# about cities
train_city <- train_shops %>% 
  group_by(shop_name) %>% 
  summarise(shops_city = n_distinct(shop_id))

# about items
train_items <- train %>% 
  filter(date_block_num != 33) %>%
  group_by(item_id) %>% 
  summarise(item_shops = n_distinct(shop_id),
            item_sales = sum(item_cnt_day, na.rm = TRUE),
            item_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  left_join(items, by = c('item_id')) %>% 
  select(-item_name)

# units to predict
train_units <- train %>% 
  filter(date_block_num == 33) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(units = sum(item_cnt_day, na.rm = TRUE))

# create train
train <- train %>% 
  filter(date_block_num == 32) %>%
  select(shop_id, item_id) %>% 
  left_join(train_month, by = c('shop_id', 'item_id')) %>% 
  left_join(train_years, by = c('shop_id', 'item_id')) %>%
  left_join(train_units, by = c('shop_id', 'item_id')) %>% 
  left_join(train_shops, by = c('shop_id')) %>%
  left_join(train_items, by = c('item_id')) %>%
  left_join(train_city, by = c('shop_name')) %>% 
  # replace 0 and clipping 0, 20
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate(units = ifelse(units > 20, 20, units))

# add sales/item category, city, etc.
# add new item (sales this year only)

set.seed(42)

train_split <- train %>% 
  initial_split(prop = .8)

train_tbl <- training(train_split)
test_tbl <- testing(train_split)

folds <- vfold_cv(train_tbl, v = 5) # just to save time

rec_obj <- train %>%
  recipe(units ~ .) %>% 
  step_mutate(price_diff_1m = price_last_1m - price_mean_12m,
              sales_diff_1m = sales_last_1m - sales_mean_12m,
  # this is all new
              price_diff_1y = price_last_1m - price_mean_12y,
              sales_diff_1y = sales_last_1m - sales_mean_12y)  
  # step_other(item_category_id, shop_name)
