train_past3m <- train %>% 
  filter(date_block_num %in% 30:32) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(price_3m = mean(item_price, na.rm = TRUE), 
            units_3m = sum(item_cnt_day, na.rm = TRUE))

train_past9m <- train %>%
  filter(date_block_num %in% 24:32) %>%
  group_by(shop_id, item_id) %>%
  summarise(price_9m = mean(item_price, na.rm = TRUE),
            units_9m = sum(item_cnt_day, na.rm = TRUE))

train_past2y <- train %>% 
  filter(date_block_num %in% c(9, 21)) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(price_2y = mean(item_price, na.rm = TRUE), 
            units_2y = sum(item_cnt_day, na.rm = TRUE))

train_shopId <- train %>% 
  filter(date_block_num == 32) %>%
  group_by(shop_id) %>% 
  summarise(items_shop = n_distinct(item_id), 
            items_sold = sum(item_cnt_day, na.rm = TRUE),
            shop_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  left_join(shops, by = c('shop_id'))

train_itemId <- train %>% 
  filter(date_block_num == 32) %>%
  group_by(item_id) %>% 
  summarise(shop_offer = n_distinct(shop_id),
            price_mean = mean(item_price, na.rm = TRUE), 
            month_mean = sum(item_cnt_day, na.rm = TRUE)/30) %>% 
  left_join(items, by = c('item_id')) %>% 
  select(-item_name)

train_dateId <- train %>%
  filter(date_block_num != 33) %>%
  group_by(date_block_num, shop_id, item_id) %>%
  summarise(items_sold = sum(item_cnt_day, na.rm = TRUE),
            is_no_sale = ifelse(items_sold == 0, 1 ,0)) %>%
  ungroup() %>%
  group_by(shop_id, item_id) %>%
summarise(nb_no_sale = sum(is_no_sale, na.rm = TRUE))

unit_33 <- train %>% 
  filter(date_block_num == 33) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(units = sum(item_cnt_day, na.rm = TRUE))

train <- train %>% 
  filter(date_block_num == 32) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(price_1m = mean(item_price, na.rm = TRUE), 
            units_1m = sum(item_cnt_day, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(train_past3m, by = c('shop_id', 'item_id')) %>% 
  left_join(train_past9m, by = c('shop_id', 'item_id')) %>%
  left_join(train_past2y, by = c('shop_id', 'item_id')) %>%
  mutate(units_3m  = units_3m/3,
         units_9m  = units_9m/9,
         units_2y  = units_2y/2,
         price_diff_3m = price_1m-price_3m,
         price_diff_9m = price_1m-price_9m,
         price_diff_2y = price_1m-price_2y,
         units_diff_3m = units_1m-(units_3m/3),
         units_diff_9m = units_1m-(units_9m/9),
         units_diff_2y = units_1m-(units_2y/2)) %>% 
  # select(-price_2y, -price_3m, -price_9m) %>% 
  left_join(train_shopId, by = c('shop_id')) %>% 
  left_join(train_itemId, by = c('item_id')) %>%
  left_join(train_dateId, by = c('shop_id', 'item_id')) %>%
  left_join(unit_33, by = c('shop_id', 'item_id')) %>% 
  mutate(units = ifelse(is.na(units), 0, units)) %>% 
  # quick fix, but doesn't make sense
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate(units = ifelse(units > 20, 20, units)) # clipping 0, 20

###

valid_past1m <- train %>% 
  filter(date_block_num == 33) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(price_1m = mean(item_price, na.rm = TRUE), 
            units_1m = sum(item_cnt_day, na.rm = TRUE))

valid_past3m <- train %>% 
  filter(date_block_num %in% 31:33) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(price_3m = mean(item_price, na.rm = TRUE), 
            units_3m = sum(item_cnt_day, na.rm = TRUE))

valid_past9m <- train %>% 
  filter(date_block_num %in% 25:33) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(price_9m = mean(item_price, na.rm = TRUE), 
            units_9m = sum(item_cnt_day, na.rm = TRUE))

valid_past2y <- train %>% 
  filter(date_block_num %in% c(10, 22)) %>% 
  group_by(shop_id, item_id) %>% 
  summarise(price_2y = mean(item_price, na.rm = TRUE), 
            units_2y = sum(item_cnt_day, na.rm = TRUE))

valid_shopId <- train %>% 
  filter(date_block_num == 33) %>%
  group_by(shop_id) %>% 
  summarise(items_shop = n_distinct(item_id), 
            items_sold = sum(item_cnt_day, na.rm = TRUE),
            shop_perfo = sum(item_price * item_cnt_day, na.rm = TRUE)) %>% 
  left_join(shops, by = c('shop_id'))

valid_itemId <- train %>% 
  filter(date_block_num == 33) %>%
  group_by(item_id) %>% 
  summarise(shop_offer = n_distinct(shop_id),
            price_mean = mean(item_price, na.rm = TRUE), 
            month_mean = sum(item_cnt_day, na.rm = TRUE)/30) %>% 
  left_join(items, by = c('item_id')) %>% 
  select(-item_name)

valid_dateId <- train %>% 
  # filter(date_block_num != 33) %>%
  group_by(date_block_num, shop_id, item_id) %>%
  summarise(items_sold = sum(item_cnt_day, na.rm = TRUE),
            is_no_sale = ifelse(items_sold == 0, 1 ,0)) %>%
  ungroup() %>%
  group_by(shop_id, item_id) %>%
  summarise(nb_no_sale = sum(is_no_sale, na.rm = TRUE))

valid <- valid %>% 
  # group_by(shop_id, item_id) %>%
  # summarise(price_1m = mean(item_price, na.rm = TRUE),
  #           units_1m = sum(item_cnt_day, na.rm = TRUE)) %>%
  # ungroup() %>%
  left_join(valid_past1m, by = c('shop_id', 'item_id')) %>%
  left_join(valid_past3m, by = c('shop_id', 'item_id')) %>% 
  left_join(valid_past9m, by = c('shop_id', 'item_id')) %>%
  left_join(valid_past2y, by = c('shop_id', 'item_id')) %>%
  mutate(units_3m  = units_3m/3,
         units_9m  = units_9m/9,
         units_2y  = units_2y/2,
         price_diff_3m = price_1m-price_3m,
         price_diff_9m = price_1m-price_9m,
         price_diff_2y = price_1m-price_2y,
         units_diff_3m = units_1m-(units_3m/3),
         units_diff_9m = units_1m-(units_9m/9),
         units_diff_2y = units_1m-(units_2y/2)) %>% 
  # select(-price_2y, -price_3m, -price_9m) %>% 
  left_join(valid_shopId, by = c('shop_id')) %>% 
  left_join(valid_itemId, by = c('item_id')) %>%
  left_join(valid_dateId, by = c('shop_id', 'item_id')) %>%
  select(-ID) %>% 
  # quick fix, but doesn't make sense
  mutate_if(is.numeric, replace_na, replace = 0)
