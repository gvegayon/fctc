library(Amelia)
library(dplyr)
library(magrittr)

dat <- readr::read_csv("data/model_data.csv", na="<NA>") %>%
  select(-logPopulation, -none) %>%
  arrange(entry, year)


to_skip <- c(
  "country_name",
  "iso3c",
  "no_report",
  "pol_shift",
  "pol_shift_left",
  "pol_shift_right",
  "subscribed",
  "sum_art05",
  "sum_art06",
  "sum_art08",
  "sum_art11",
  "sum_art13",
  "sum_art14",
  "signature",
  "ratification",
  "who_region",
  "continent",
  "Years since Ratif.",
  "Years since Sign."
)

cl <- parallel::makeForkCluster(8L)

ans <- amelia(
  x        = as.data.frame(dat),
  idvars   = to_skip,
  ts       = "year",
  cs       = "entry",
  parallel = "multicore",
  cl       = cl
)

# tscsPlot(ans, "smoke_female", cs = "entry")


View(cbind(
  country = dat$country_name,
  year    = dat$year,
  gdp1 = ans$imputations$imp1$smoke_female,
  gdp2 = ans$imputations$imp2$smoke_female,
  gdp3 = ans$imputations$imp3$smoke_female,
  gdp4 = ans$imputations$imp5$smoke_female,
  gdp5 = ans$imputations$imp5$smoke_female,
  dat$smoke_female
), "Imputed")

library(ggplot2)

g <- ans$imputations$imp1 %>%
  rename(smoke_female_imputed = smoke_female) %>%
  left_join(subset(dat, select = c(entry, year, smoke_female))) %>%
  mutate(is_miss = is.na(smoke_female))

ggplot(g, aes(x=year, y=smoke_female_imputed, color = is_miss)) +
  geom_point() + geom_jitter()

  
