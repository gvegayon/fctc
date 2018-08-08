library(Amelia)
library(dplyr)
library(magrittr)

dat <- readr::read_csv("data/model_data_unscaled.csv", na="<NA>") %>%
  unique %>%
  arrange(entry, year)

yeardat <- model.matrix(~0+factor(year), dat)
colnames(yeardat) <- gsub(".+([0-9]{4})", "year\\1", colnames(yeardat))
dat <- cbind(dat, yeardat)

to_skip <- c(
  "country_name",
  "iso3c",
  "no_report",
  "pol_shift",
  "pol_shift_left",
  "pol_shift_right",
  "signature",
  "sum_art05",
  "sum_art06",
  "sum_art08",
  "sum_art11",
  "sum_art13",
  "sum_art14",
  "year_signature",
  "year_ratification",
  "who_region",
  "continent",
  "Years since Ratif.",
  "Years since Sign."
)

cl <- parallel::makeForkCluster(8L)

# Generating bounds
ranges <- lapply(colnames(dat), function(x) range(dat[[x]], na.rm = TRUE)) %>%
  set_names(colnames(dat)) %>%
  bind_cols

ranges <- ranges[,sapply(ranges, function(x) all(is.numeric(x)))] %>%
  as.matrix %>%
  t

ranges <- ranges[!(rownames(ranges) %in% c(to_skip, "year", "entry")),]
ranges <- cbind(
  match(rownames(ranges), colnames(dat)),
  ranges
  )

ranges <- ranges[
  c(
    "ctrl_corrup",
"gdp_percapita_ppp",
"health_exp",
"population",
"rule_of_law",
"smoke_female",
"smoke_male",
"ratification",
"bloomberg_count",
"bloomberg_amount",
"bloomberg_fctc_count",
"bloomberg_fctc_amount",
"govtown",
"tobacco_prod",
"South-East Asia",
"year2010",
"year2012",
"year2014",
"year2016"
  )
  ,
]

ans <- amelia(
  x        = as.data.frame(dat),
  idvars   = to_skip,
  ts       = "year",
  cs       = "entry",
  parallel = "multicore",
  cl       = cl,
  bounds = ranges
)

# tscsPlot(ans, "smoke_female", cs = "entry")


View(cbind(
  country = dat$country_name,
  year    = dat$year,
  smoke_im1 = ans$imputations$imp1$smoke_female,
  smoke_im2 = ans$imputations$imp2$smoke_female,
  smoke_im3 = ans$imputations$imp3$smoke_female,
  smoke_im4 = ans$imputations$imp5$smoke_female,
  smoke_im5 = ans$imputations$imp5$smoke_female,
  smoke = dat$smoke_female
), "Imputed")

library(ggplot2)

g <- ans$imputations$imp1 %>%
  rename(smoke_female_imputed = smoke_female) %>%
  left_join(subset(dat, select = c(entry, year, smoke_female))) %>%
  mutate(is_miss = is.na(smoke_female))

ggplot(g, aes(x=year, y=smoke_female_imputed, color = is_miss)) +
  geom_point() + geom_jitter()

  
