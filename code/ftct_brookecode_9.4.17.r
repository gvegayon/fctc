# Author: Brooke Bell
# Date: 9/4/17

## Data Importing and Cleaning ---------------------------------------------------

rm(list=ls())
options(stringsAsFactors=FALSE)
library(dplyr)
library(netdiffuseR)

# set working directory
# setwd("~/Dropbox (Metzlab - VS)/USC (Graduate)/1st Year/Valente (PM 590)/fctc")

# import all csv files
# subset so that all countries match across databases

country_codes <- read.csv("data/gw_country_codes.csv", na.strings = NULL)

treaty_dates <- read.csv("data/treaty_dates.csv", na="<NA>")
treaty_dates <- subset(treaty_dates, select=c(entry, signature, ratification))

treaty_dates <- treaty_dates[treaty_dates$entry %in% country_codes$entry,] #184 same countries
country_codes <- country_codes[country_codes$entry %in% treaty_dates$entry,] #184 same countries

political_shifts <- read.csv("data/political_shifts.csv", na="<NA>")
political_shifts <- subset(political_shifts, select=c(-country_name, -execrlc))
political_shifts <- political_shifts[political_shifts$entry %in% treaty_dates$entry,]
length(unique(political_shifts$entry)) #167 countries

party_attributes <- read.csv("data/party_attributes.csv", na="<NA>")
party_attributes <- subset(party_attributes, who_region != "none", select=c(-country_name))
party_attributes <- party_attributes[party_attributes$entry %in% treaty_dates$entry,]
length(unique(party_attributes$entry)) #183 countries

implementation <- read.csv("data/implementation.csv", na="<NA>")
implementation <- subset(implementation, select=c(-country_name))
implementation <- implementation[implementation$entry %in% treaty_dates$entry,]
length(unique(implementation$entry)) #166 countries

bloomberg <- read.csv("data/bloomberg.csv", na="<NA>")
bloomberg <- subset(bloomberg, select=c(-country_name))
bloomberg <- bloomberg[bloomberg$entry %in% treaty_dates$entry,]
length(unique(bloomberg$entry)) #77 countries

govtown <- read.csv("data/govtown.csv", na = "<NA>")
govtown <- govtown[govtown$entry %in% treaty_dates$entry,]
length(unique(govtown$entry)) #184 countries

# list of countries that are in country_codes DB but not party_attributes DB
country_codes[which(!(country_codes$entry %in% party_attributes$entry)), c("entry")] #ME-montenegro

## Merging ----------------------------------------------------------------------

dat <- data.frame()
dat <- left_join(party_attributes, political_shifts, by=c("year", "entry"))
dat <- left_join(dat, treaty_dates, by="entry")
dat <- left_join(dat, implementation)
dat <- left_join(dat, bloomberg, by=c("year", "entry"))
dat <- left_join(dat, govtown, by = "entry")

# how many countries in dat DB?
length(unique(dat$entry)) #183 (missing ME-montenegro)

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
     countrycode, country_name, year, pol_shift, bloomberg_amount, ratification,
     sum_art05, sum_art06, sum_art08)))

# Updating year of ratification and signature ----------------------------------
dat$year_ratification <- dat$ratification %/% 10000L
dat$year_signature    <- dat$signature %/% 10000L

# create "ratify" indicator variable
dat$ratify <- ifelse(is.na(dat$year_ratification), 0, 1)

# check how many countries aren't in your database??
treaty_dates[which(!(treaty_dates$entry %in% country_codes$entry)), c("entry")]

# Preparing data for netdiffuser ------------------------------------------------

## create TOA matrix (not necessary because the diffnet contains this info)
# treaty_dates$ratify_year <- substr(treaty_dates$ratification, 0, 4)
# ratify_year_mats <- toa_mat(as.integer(treaty_dates$ratify_year))
# fctc_cumadopt <- ratify_year_mats$cumadopt #cumadopt matrix
# dimnames(fctc_cumadopt)[[1]] <- as.character(treaty_dates$entry)

## George's comments
#
# your_fancy_diffnet <- new_diffnet(
#   graph = [here goes a dgCMatrix (or matrix, that's OK too)],
#   toa   = [here goes a vector with times of adoption, sorted equally as -graph-],
#   vertex.dyn.attrs = [here should go the -dat- dataset],
#   id.and.per.vars = [names of the -id- and -time- variables in the dataset (see the help)]
# )

# which countries are in treaty_dates DB, country_codes DB, and dat DB?
countrylist <-treaty_dates[which(treaty_dates$entry %in% country_codes$entry), c("entry")]
countrylist <- treaty_dates[which(treaty_dates$entry %in% dat$entry), c("entry")]
# should be 183 countries

# subset dgc matrix with countrylist vector
load("data/adjmat_centroid_dist.rda")
adjmat_centroid_dist1 <- adjmat_centroid_dist[countrylist, countrylist]


# [2017-09-06] George:
#  - Checkout the order... are these the same???
#  - The id.and.per.vars should do the work, but just make sure that after creating the
#    diffnet object, the treaty dates match the corresponding countries. Take a look at
#    the -match- function.

# subset treaty_dates DB to match countries in dat DB
treaty_dates <- treaty_dates[treaty_dates$entry %in% dat$entry,]

# [2017-09-06] George:
#  - I don't see the ratify_year variable in treaty_dates... so I get an error.
# create diffnet
my_diffnet <- new_diffnet(
  graph = adjmat_centroid_dist1,
  toa = treaty_dates$ratify_year, 
  vertex.dyn.attrs = dat,
  id.and.per.vars = c("entry", "year")
)

summary(my_diffnet)
exposure(my_diffnet)

# Analyses ----------------------------------------------------------------------

# myprobit <- glm(ratify ~ ???, family = binomial(link = "probit"), data = )
