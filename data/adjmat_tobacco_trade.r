library(netdiffuseR)
library(dplyr)
library(countrycode)

edgelist_trade <- readr::read_csv(
  "data-raw/comtrade/comtrade_tobacco.csv",
  na = "<NA>")

codes <- codelist_panel %>%
  select(year, iso3c, iso2c) %>%
  mutate(
    iso3c = tolower(iso3c)
  )

edgelist_trade <- edgelist_trade %>%
  filter(year >= 2010) %>%
  mutate(
    total = export + import
  ) %>%
  left_join(codes, by = c("year" = "year", "origin"="iso3c")) %>%
  rename(ego = iso2c) %>%
  left_join(codes, by = c("year" = "year", "dest"="iso3c")) %>%
  rename(alter = iso2c) %>%
  arrange(year, ego, alter) 

adjmat <- with(
  edgelist_trade,
  edgelist_to_adjmat(cbind(ego, alter), total, year, year)
)

saveRDS(adjmat, "data/adjmat_tobacco_trade.rds", compress = TRUE)
