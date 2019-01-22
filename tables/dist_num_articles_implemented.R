options(stringsAsFactors = FALSE)

model_data <- readr::read_csv("data/model_data.csv", na = "<NA>")

articles <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13",
              "sum_art14") 

library(dplyr)
library(magrittr)
library(tidyr)

dat <- model_data %>% 
  select(entry, year, starts_with("sum_art")) %>%
  gather(article, n, -entry, -year) %>%
  mutate(
    article = stringr::str_replace(article, "sum_art", ""),
    article = as.integer(article),
    n       = if_else(n >= 5, "Five or more", as.character(n))
    ) %>%
  group_by(year, article, n) %>%
  summarise(count = n()) %>%
  spread(n, count) %>%
  mutate_if(is.numeric, funs(coalesce(., 0L))) %>%
  mutate(Total = `0` + `1` + `2` + `3` + `4` + `Five or more`) %>%
  ungroup %>%
  filter(year %in% c(2010, 2012, 2014, 2016)) %>%
  rename(Article = article) %>%
  select(-year)
  
readr::write_csv(dat, "tables/dist_num_articles_implemented.csv")

library(knitr)
library(kableExtra)

dat %>% kable %>%
  group_rows("2010", 1, 6) %>%
  group_rows("2012", 7, 12) %>%
  group_rows("2014", 13, 18) %>%
  group_rows("2016", 19, 24) %>%
  add_header_above(c(" " = 1, "Number of Items Implemented" = 6, " " = 1)) %>%
  column_spec(1:8, width = "5em") 

