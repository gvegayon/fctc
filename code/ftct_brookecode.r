# Author: Brooke Bell
# Date: 8/4/17

## Data Importing and Cleaning ---------------------------------------------------

rm(list=ls())
options(stringsAsFactors=FALSE)
library(plry)
library(dplyr)
library(netdiffuseR)

# set working directory
setwd("~/Dropbox (Metzlab - VS)/USC (Graduate)/1st Year/Valente (PM 590)/fctc/data")

# import all csv files

country_codes <- read.csv("gw_country_codes.csv", na.strings = NULL)

treaty_dates <- read.csv("treaty_dates.csv", na="<NA>")
treaty_dates <- subset(treaty_dates, select=c(entry, signature, ratification))

political_shifts <- read.csv("political_shifts.csv", na="<NA>")
political_shifts <- subset(political_shifts, select=c(-country_name, -execrlc))

party_attributes <- read.csv("party_attributes.csv", na="<NA>")
party_attributes <- subset(party_attributes, who_region != "none", select=c(-country_name))

implementation <- read.csv("implementation.csv", na="<NA>")
implementation <- subset(implementation, select=c(-country_name))

bloomberg <- read.csv("bloomberg.csv", na="<NA>")
bloomberg <- subset(bloomberg, select=c(-country_name))

govtown <- read.csv("govtown.csv", na = "<NA>")

## Merging ----------------------------------------------------------------------
dat <- left_join(party_attributes, political_shifts, by=c("year", "entry"))
dat <- left_join(dat, treaty_dates, by="entry")
dat <- left_join(dat, implementation)
dat <- left_join(dat, bloomberg, by=c("year", "entry"))
dat <- left_join(dat, govtown, by = "entry")

# Bloomberg data should be filled with zeros instead of NAs
dat$bloomberg_amount      <- coalesce(dat$bloomberg_amount, 0)
dat$bloomberg_count       <- coalesce(dat$bloomberg_count, 0L)
dat$bloomberg_fctc_count  <- coalesce(dat$bloomberg_fctc_count, 0L)
dat$bloomberg_fctc_amount <- coalesce(dat$bloomberg_fctc_amount, 0)

# Implementation data should be filled with zeros instead of NAs
# we will assume that they did not implemented
dat$sum_art05[is.na(dat$sum_art05)] <- 0L
dat$sum_art06[is.na(dat$sum_art06)] <- 0L
dat$sum_art08[is.na(dat$sum_art08)] <- 0L
dat$sum_art11[is.na(dat$sum_art11)] <- 0L
dat$sum_art13[is.na(dat$sum_art13)] <- 0L

# Pasting names
dat <- left_join(dat, country_codes, by="entry")

# Creating dummies
for (cont in unique(dat$continent))
  dat[[cont]] <- ifelse(dat$continent == cont, 1L, 0L)

for (who in unique(dat$who_region))
  dat[[who]] <- ifelse(dat$who_region == who, 1L, 0L)

# Filtering --------------------------------------------------------------------

# Sorting the data
dat <- dat[with(dat, order(entry, year)),]

# look at subset of dataset
View(subset(dat,select=c(
     country_name, year, pol_shift, bloomberg_amount, ratification,
     sum_art05, sum_art06, sum_art08)))

# Updating year of ratification and signature ----------------------------------
dat$year_ratification <- dat$ratification %/% 10000L
dat$year_signature    <- dat$signature %/% 10000L

# create "ratify" indicator variable
dat$ratify <- ifelse(is.na(treaty_dates$ratification), 0, 1)

# Preparing data for netdiffuser ------------------------------------------------

# create TOA matrix
treaty_dates$ratify_year <- substr(treaty_dates$ratification, 0, 4)
ratify_year_mats <- toa_mat(as.integer(treaty_dates$ratify_year))

# [2017-08-07] George
# THIS RETURNS ERROR...
fctc_cumadopt <- ratify_year_mats$cumadopt #cumadopt matrix
dimnames(fctc_cumadopt)[[1]] <- as.character(treaty_dates$entry)

# this is where I'm confused:
# how do I transform the dataset into long format?

# [2017-08-07] George
# The dataset -dat- is already in long format. If you tabulate dat$countrycode
# You'll see that there are 12 observations per country.


# how do I create my "graph" for the exposure function? diffnet?

# [2017-08-07] The graphs have already been created. All these are the
# rda files in the -data- folder. For example, the file data/adjmat_centroid_dist.rda,
# when you load it using the function -load- you should get am R object of class
# 'dgCMatrix' called -adjmat_centroid_dist-
#
# You can load this data into netdiffuseR using the -new_diffnet- function. 
# Essentially, you have to do something like the following:
#
# your_fancy_diffnet <- new_diffnet(
#   graph = [here goes a dgCMatrix (or matrix, that's OK too)],
#   toa   = [here goes a vector with times of adoption, sorted equally as -graph-],
#   vertex.dyn.attrs = [here should go the -dat- dataset],
#   id.and.per.vars = [names of the -id- and -time- variables in the dataset (see the help)]
#)
#
# Notice that the set of countries and times should coincide across the objects
# this means that, for example, that if a country shows in -graph-, it should show
# in -vertex.dyn.attrs- and viceversa.



# Analyses ----------------------------------------------------------------------

# myprobit <- glm(ratify ~ ???, family = binomial(link = "probit"), data = )


