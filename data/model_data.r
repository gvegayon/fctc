# This file creates the final dataset that will be used to run the models
# the network data is in a separate file called data/adjmats.rda.

rm(list=ls())
options(stringsAsFactors=FALSE)
library(dplyr)
library(netdiffuseR)

# parameter
years_reported <- c(2010, 2012) # 2014 (need to extend data)

# Reading datasets -------------------------------------------------------------
country_codes    <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
country_codes    <- subset(country_codes, select=c(-subdivision_assigned_codes))

political_shifts <- read.csv("data/political_shifts.csv", na = "<NA>")
political_shifts <- subset(political_shifts, select=c(-country_name, -execrlc))

party_attributes <- read.csv("data/party_attributes.csv", na = "<NA>")
party_attributes <- subset(party_attributes, who_region != "none", select=c(-country_name))

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
dat$sum_art05[is.na(dat$sum_art05)] <- 0L
dat$sum_art06[is.na(dat$sum_art06)] <- 0L
dat$sum_art08[is.na(dat$sum_art08)] <- 0L
dat$sum_art11[is.na(dat$sum_art11)] <- 0L
dat$sum_art13[is.na(dat$sum_art13)] <- 0L

# Pasting names
dat <- left_join(dat, country_codes, by="entry")

# Creating dummies -------------------------------------------------------------
for (cont in unique(dat$continent))
  dat[[cont]] <- ifelse(dat$continent == cont, 1L, 0L)

for (who in unique(dat$who_region))
  dat[[who]] <- ifelse(dat$who_region == who, 1L, 0L)


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

# Updating year of ratification and signature ----------------------------------
dat$year_ratification <- dat$ratification %/% 10000
dat$year_signature    <- dat$signature %/% 10000

# Fixing some NAs
dat$year_signature <- with(dat, ifelse(is.na(year_signature), year_ratification,
                                       year_signature))

# Truncating to 2010
dat$`Years since Ratif.` <- with(dat, 2010 - year_ratification)
dat$`Years since Sign.`  <- with(dat, 2010 - year_signature)

dat$`Years since Ratif.`[dat$`Years since Ratif.` < 0] <- 0
dat$`Years since Sign.`[dat$`Years since Sign.` < 0]   <- 0

# Rescaling variables ----------------------------------------------------------
dat$tobac_prod_pp       <- with(dat, tobac_prod/population)
dat$GDP_pp              <- with(dat, GDP/population)
dat$bloomberg_amount_pp      <- with(dat, bloomberg_amount/population)
dat$bloomberg_fctc_amount_pp <- with(dat, bloomberg_fctc_amount/population)

for (v in colnames(dat))
  if (is.double(dat[[v]]))
    dat[[v]] <- dat[[v]]/sd(dat[[v]])

# Including interest on policy (subscribed to GL posts) ------------------------

load("data/adjmats.rda")
graph <- adjmat_gl_posts[c("2008", "2009", "2010")] # 
graph <- graph[[1]] + graph[[2]] + graph[[3]]
adjmat_gl_posts <- graph # 

posts <- dgr(adjmat_gl_posts)
posts <- data.frame(subscribed = posts, entry = rownames(posts))
posts$subscribed <- as.integer(posts$subscribed)

dat <- merge(dat, posts, by="entry", all.x=TRUE, all.y=FALSE)
dat$subscribed[is.na(dat$subscribed)] <- 0L

# Saving the data --------------------------------------------------------------
write.csv(dat, "data/model_data.csv", row.names = FALSE, na = "<NA>")

