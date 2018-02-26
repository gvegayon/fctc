library(dplyr)

model_data <- readr::read_csv("data/model_data.csv", na = "<NA>")

signature <- group_by(model_data, year_signature) %>%
  filter(year == 2010) %T>%
  assign(value=nrow(.), x="N", envir = .GlobalEnv) %>%
  summarise(
    n_signature = sprintf("%i (%.2f)", n(), n()/N),
    art05  = mean(sum_art05),
    art06  = mean(sum_art06),
    art08  = mean(sum_art08),
    art11  = mean(sum_art11),
    art13  = mean(sum_art13)
  ) %>% 
  as.data.frame %>%
  mutate(
    year =  year_signature,
    art05 = sprintf("%.2f", art05),
    art06 = sprintf("%.2f", art06),
    art08 = sprintf("%.2f", art08),
    art11 = sprintf("%.2f", art11),
    art13 = sprintf("%.2f", art13)
  ) %>% select(-year_signature)

ratification <- group_by(model_data, year_ratification) %>%
  filter(year == 2010) %T>%
  assign(value=nrow(.), x="N", envir = .GlobalEnv) %>%
  summarise(
    n_ratification = sprintf("%i (%.2f)", n(), n()/N),
    art05 = mean(sum_art05),
    art06 = mean(sum_art06),
    art08 = mean(sum_art08),
    art11 = mean(sum_art11),
    art13 = mean(sum_art13)
  ) %>% 
  as.data.frame %>%
  mutate(
    year = year_ratification,
    art05 = sprintf("%.2f", art05),
    art06 = sprintf("%.2f", art06),
    art08 = sprintf("%.2f", art08),
    art11 = sprintf("%.2f", art11),
    art13 = sprintf("%.2f", art13)
  ) %>% select(-year_ratification)


full_join(ratification, signature, by = "year", suffix = c("-ratif", "-sign")) %>%
  select(
    year,
    n_ratification,
    n_signature,
    ends_with("ratif"),
    ends_with("sign")
  ) %>%
  readr::write_csv("tables/implementaiton-per-year.csv")
# %>%
#   readr::write_csv("tables/implementation-per-year-ratification.csv")
