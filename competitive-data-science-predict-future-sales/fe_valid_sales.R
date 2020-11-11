valid <- read_csv('~/Documents/GitHub/Kaggle/competitive-data-science-predict-future-sales/test.csv') %>% 
  mutate(shop_id = ifelse(shop_id == 57, 0, shop_id),
         shop_id = ifelse(shop_id == 58, 1, shop_id),
         shop_id = ifelse(shop_id == 11, 10, shop_id),
         shop_id = ifelse(shop_id == 40, 39, shop_id))

# last 2 months
valid_month <- train %>% 
  filter(date_block_num %in% c(32, 33)) %>% 
  group_by(date_block_num, shop_id, item_id) %>% 
  summarise(sales_last_1m = sum(item_cnt_day, na.rm = TRUE), 
            price_last_1m = max(item_price, na.rm = TRUE)) %>% 
  mutate(sales_last_2m = lag(sales_last_1m, 1), 
         price_last_2m = lag(price_last_1m, 1), 
         sales_mean_12m = (sales_last_1m + sales_last_2m)/2,
         price_mean_12m = (price_last_1m + price_last_2m)/2) %>% 
  filter(date_block_num == 33) %>% 
  ungroup() %>% 
  select(-date_block_num)

# last 2 years
valid_years <- train %>% 
  filter(date_block_num %in% c(10, 22)) %>% 
  group_by(date_block_num, shop_id, item_id) %>% 
  summarise(sales_last_1y = sum(item_cnt_day, na.rm = TRUE), 
            price_last_1y = max(item_price, na.rm = TRUE)) %>% 
  mutate(sales_last_2y = lag(sales_last_1y, 1), 
         price_last_2y = lag(price_last_1y, 1), 
         sales_mean_12y = (sales_last_1y + sales_last_2y)/2,
         price_mean_12y = (price_last_1y + price_last_2y)/2) %>% 
  filter(date_block_num == 22) %>% 
  ungroup() %>% 
  select(-date_block_num)

# about shops
valid_shops <- train %>% 
  # filter(date_block_num != 33) %>%
  group_by(shop_id) %>% 
  summarise(shop_items = n_distinct(item_id), 
            shop_sales = sum(item_cnt_day, na.rm = TRUE),
            shop_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  left_join(shops, by = c('shop_id'))  %>% 
  mutate(shop_name = gsub('(\\w)( )(.*)', '\\1', shop_name))

# about cities
valid_city <- train_shops %>% 
  group_by(shop_name) %>% 
  summarise(shops_city = n_distinct(shop_id))

# about items
valid_items <- train %>% 
  # filter(date_block_num != 33) %>%
  group_by(item_id) %>% 
  summarise(item_shops = n_distinct(shop_id),
            item_sales = sum(item_cnt_day, na.rm = TRUE),
            item_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  left_join(items, by = c('item_id')) %>% 
  select(-item_name)

valid <- valid %>% 
  # filter(date_block_num == 32) %>%
  select(shop_id, item_id) %>% 
  left_join(valid_month, by = c('shop_id', 'item_id')) %>% 
  left_join(valid_years, by = c('shop_id', 'item_id')) %>%
  # left_join(train_units, by = c('shop_id', 'item_id')) %>%
  left_join(valid_shops, by = c('shop_id')) %>%
  left_join(valid_items, by = c('item_id')) %>%
  left_join(valid_city, by = c('shop_name')) %>% 
  # quick fix, but doesn't make sense
  mutate_if(is.numeric, replace_na, replace = 0)
