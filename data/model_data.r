# This file creates the final dataset that will be used to run the models
# the network data is in a separate file called data/adjmats.rda.

rm(list=ls())
options(stringsAsFactors=FALSE)
library(dplyr)

# parameter
years_reported <- c(2010, 2012) # 2014 (need to extend data)

# Reading datasets -------------------------------------------------------------
country_codes    <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
country_codes    <- subset(country_codes, select=c(-subdivision_assigned_codes))

political_shifts <- read.csv("data/political_shifts.csv", na = "<NA>")
political_shifts <- subset(political_shifts, select=c(-country_name, -execrlc))

party_attributes <- read.csv("data/party_attributes.csv", na = "<NA>")
party_attributes <- subset(party_attributes, select=c(-country_name))

bloomberg        <- read.csv("data/bloomberg.csv", na = "<NA>")
bloomberg        <- subset(bloomberg, select=c(-country_name))

treaty_dates     <- read.csv("data/treaty_dates.csv", na = "<NA>")
treaty_dates     <- subset(treaty_dates, select=c(entry, signature, ratification))

implementation   <- read.csv("data/implementation.csv", na = "<NA>")
implementation   <- subset(implementation, select=c(-country_name))

# Merging ----------------------------------------------------------------------
dat <- left_join(party_attributes, political_shifts, by=c("year", "entry"))
dat <- left_join(dat, treaty_dates, by="entry")
dat <- left_join(dat, implementation)
dat <- left_join(dat, bloomberg, by=c("year", "entry"))

# Bloomberg data should be filled with zeros instead of NAs
dat$bloomberg_amount      <- coalesce(dat$bloomberg_amount, 0)
dat$bloomberg_count       <- coalesce(dat$bloomberg_count, 0L)
dat$bloomberg_fctc_count  <- coalesce(dat$bloomberg_fctc_count, 0L)
dat$bloomberg_fctc_amount <- coalesce(dat$bloomberg_fctc_amount, 0)

# Implementation data should be filled with zeros instead of NAs
# we will assume that they did not implemented
dat$sum_art05[is.na(dat$sum_art05)] <- 0
dat$sum_art06[is.na(dat$sum_art06)] <- 0
dat$sum_art08[is.na(dat$sum_art08)] <- 0
dat$sum_art11[is.na(dat$sum_art11)] <- 0
dat$sum_art13[is.na(dat$sum_art13)] <- 0

# Pasting names
dat <- left_join(dat, country_codes, by="entry")

# Filtering --------------------------------------------------------------------
dat <- filter(dat, year %in% years_reported)
dat <- filter(dat, !is.na(ratification))

# Sorting the data
dat <- dat[with(dat, order(entry, year)),]

# View(subset(
#   dat,
#   select=c(
#     country_name, year, pol_shift, bloomberg_amount, ratification,
#     sum_art05, sum_art06, sum_art08)))
# 

# Rescaling variables ----------------------------------------------------------
dat$tobac_prod_pp       <- with(dat, tobac_prod/population)
dat$GDP_pp              <- with(dat, GDP/population)
dat$bloomberg_amount_pp <- with(dat, bloomberg_amount/population)

for (v in colnames(dat))
  if (is.double(dat[[v]]))
    dat[[v]] <- dat[[v]]/sd(dat[[v]])

# Saving the data --------------------------------------------------------------
write.csv(dat, "data/model_data.csv", row.names = FALSE, na = "<NA>")

