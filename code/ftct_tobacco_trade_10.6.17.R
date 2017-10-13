# Author: Brooke Bell
# Date: 10/6/17

## DATA IMPORTING AND CLEANING -----

rm(list=ls())
options(stringsAsFactors=FALSE)
library(dplyr)
library(netdiffuseR)

# set working directory
 # setwd("~/Dropbox (Metzlab - VS)/USC (Graduate)/1st Year/Valente (FCTC)/fctc")

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

## MERGING -----

# NOTE: I'm just including all the variables for now, will subset later below

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

# Filtering -----

# Sorting the data
dat <- dat[with(dat, order(entry, year)),]

# look at subset of dataset
View(subset(dat,select=c(
     countrycode, country_name, year, pol_shift, bloomberg_amount, ratification,
     sum_art05, sum_art06, sum_art08)))

# Updating year of ratification and signature -----
dat$year_ratification <- dat$ratification %/% 10000L
dat$year_signature    <- dat$signature %/% 10000L

# create "ratify" indicator variable
dat$ratify <- ifelse(is.na(dat$year_ratification), 0, 1)

# check how many countries aren't in your database??
treaty_dates[which(!(treaty_dates$entry %in% country_codes$entry)), c("entry")]

# only use covariates included in 2010 paper model (Valente et al 2010)
model_dat <- subset(dat, select=c(year, #year
                                  entry, #country id
                                  population, #population
                                  GDP, #income
                                  democracy, #democracy
                                  tobac_prod, #tobacco production in tons
                                  perc_female_smoke, #% female smokers
                                  perc_male_smoke, #% male smokers
                                  number_ngos, #number of NGOs in FCA
                                  who_reg2 #region
                                  ))

# Preparing data for diffnet -----

treaty_dates$ratify_year <- substr(treaty_dates$ratification, 0, 4)

# which countries are in treaty_dates DB, country_codes DB, and dat DB?
countrylist <-treaty_dates[which(treaty_dates$entry %in% country_codes$entry), c("entry")]
countrylist <- treaty_dates[which(treaty_dates$entry %in% dat$entry), c("entry")]
countrylist <- sort(countrylist)
# should be 183 countries

## IMPORTING ADJACENCY MATRICES -----
load("data/adjmats_BB.rda")

# subset treaty_dates DB to match countries in dat DB
treaty_dates <- treaty_dates[treaty_dates$entry %in% dat$entry,]

# 3) TOBACCO TRADE NETWORK
str(adjmat_tobacco_trade)

#intersection of countries in both adjmat_tobacco_trade and dat 
countrylist3 <- sort(intersect(rownames(adjmat_tobacco_trade), countrylist))
adjmat_3 <- adjmat_tobacco_trade[countrylist3, countrylist3]
treaty_dates3 <- subset(treaty_dates, entry %in% countrylist3)
model_dat3 <- subset(model_dat, entry %in% countrylist3)

# create diffnet object
my_diffnet3 <- new_diffnet(
  graph = adjmat_3,
  toa = treaty_dates3$ratify_year, 
  vertex.dyn.attrs = model_dat3,
  id.and.per.vars = c("entry", "year")
)

summary(my_diffnet3)
exposure(my_diffnet3)
plot_adopters(my_diffnet3)

## LOGIT MODEL -----

# Variables to export
my_diffnet3[["CohesiveExposure"]] <- exposure(my_diffnet3)
my_diffnet3[["Adopt"]] <- my_diffnet3$cumadopt

# As data frame
my_diffnet3_df <- as.data.frame(my_diffnet3)

# Lagged exposure
my_diffnet3_df$LaggedExposure <- NULL
my_diffnet3_df$LaggedExposure[my_diffnet3_df$per > 2003] <- my_diffnet3_df$CohesiveExposure[my_diffnet3_df$per < 2014]

# Did this work?
my_diffnet3_df <- my_diffnet3_df[with(my_diffnet3_df, order(id, per)),]
tail(my_diffnet3_df)

# Running logit
summary(
  glm(Adopt ~ LaggedExposure 
      #+ factor(per) #why is this in here?
      + population 
      + GDP 
      + democracy 
      + tobac_prod 
      + perc_female_smoke 
      + perc_male_smoke 
      + number_ngos
      + who_reg2, #need to recode this...
      data=my_diffnet3_df,
      family = binomial(link="logit")
  )
)
