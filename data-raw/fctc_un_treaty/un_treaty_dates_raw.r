library(magrittr)
library(rvest)
library(xml2)
library(dplyr)
library(stringr)

dat <- read_html("data-raw/fctc_un_treaty/un_treaty_dates_raw.html") %>%
  html_table(header = TRUE) %>%
  extract2(1) %>%
  as_tibble


dat[,2] <- str_replace_all(dat[[2]], "\t+", " ")
dat[,3] <- str_replace_all(dat[[3]], "\t+", " ")

readr::write_csv(dat, "data-raw/fctc_un_treaty/un_treaty_dates_raw.csv")
