rm(list=ls())

library(dplyr)
library(magrittr)

dat       <- foreign::read.dta("data-raw/attributes_v3.dta")
worldbank <- readr::read_csv("data-raw/worldbank/worldbank.csv", na = "<NA>")

# Adding codes -----------------------------------------------------------------
dat <- worldbank %>%
  select(iso2c, iso3c) %>%
  rename(entry = iso2c, countrycode = iso3c) %>%
  unique %>%
  right_join(dat, by = "countrycode") %>%
  rename(country_name = countrynameun)
  
# Hand coding
dat[dat$country_name=="Serbia and Montenegro", "entry"] <- "RS"
dat[dat$country_name=="Democratic Republic of the Congo", "entry"] <- "CD"


# Cleaning up ------------------------------------------------------------------
dat <- subset(dat, select=c(-id, -merge_miss_imps, -year))
colnames(dat)[colnames(dat) == "year_bak"] <- "year"

party_attributes <- dat

write.csv(party_attributes, file="data/party_attributes.csv", row.names = FALSE,
          na = "<NA>")
