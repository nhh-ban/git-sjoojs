library(dplyr)
library(readr)
ac_items <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')

# Using across
ac_items %>% group_by(category) %>% 
  summarise(across(c(sell_value, buy_value), mean, na.rm = TRUE))

# Using lambda functions
ac_items %>% group_by(category) %>% 
  mutate(across(c(sell_value, buy_value), ~ .x / max(.x, na.rm = TRUE),
                .names = "{col}_prop")) %>% select(category, ends_with("prop"))

# Changing column names
ac_items %>% group_by(category) %>% 
  summarise(across(c(sell_value, buy_value), mean, na.rm = TRUE, .names = "{col}_mean"))

# Multiple functions
ac_items %>% group_by(category) %>% 
  summarise(across(c(sell_value, buy_value), list(mean = mean, sd = sd),
                   na.rm = TRUE, .names = "{col}_{fn}"))

# Tidyselect
ac_items %>% group_by(category) %>% summarise(across(contains("value"),
                                                     mean,
                                                     na.rm=TRUE,
                                                     .names = "{col}_"))

summarizer <- function(data, numeric_cols = NULL, ...) {
  data %>%
    group_by(...) %>%
    summarise(across({{numeric_cols}}, list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      q05 = ~quantile(.x, 0.05, na.rm = TRUE),
      q95 = ~quantile(.x, 0.95, na.rm = TRUE)
    ), .names = "{col}_{fn}"))
}

summarizer(ac_items, numeric_cols = c(sell_value, buy_value), category)

