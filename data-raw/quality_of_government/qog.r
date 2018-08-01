library(dplyr)
library(magrittr)

download.file(
  "http://www.qogdata.pol.gu.se/data/qog_std_ts_jan18.dta",
  "data-raw/quality_of_government/qog_std_ts_jan18.dta"
  )
qog <- foreign::read.dta("data-raw/quality_of_government/qog_std_ts_jan18.dta")

polity <-select(qog, ccodealp, year, fh_ipolity2) %>%
  filter(year >= 2000) %>%
  rename(iso3c = ccodealp) %>%
  arrange(iso3c, year) %>% 
  as_tibble

readr::write_csv(polity, "data-raw/quality_of_government/qog.csv", na = "<NA>")
