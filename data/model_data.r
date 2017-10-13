# This file creates the final dataset that will be used to run the models
# the network data is in a separate file called data/adjmats.rda.

rm(list=ls())
options(stringsAsFactors=FALSE)
library(dplyr)
library(readxl)
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
# implementation   <- subset(implementation, select=c(-country_name))

govtown          <- read.csv("data/govtown.csv", na = "<NA>")

# Imputation for implementation ------------------------------------------------

# Merging ----------------------------------------------------------------------
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
# > subset(dat, year == 2012 & !is.na(ratification))$no_report %>% table(useNA="always")
# .
# FALSE  TRUE  <NA> 
#   143    22    11 

# Carry forward imputation
carry_forward_and_zero_back <- function(idv, v, dat) {
  
  for (.entry in unique(dat[[idv]])) {
    
    # Which needs to be solved
    idx <- which(dat[[idv]] == .entry)
    
    # Forward
    for (t in 2:length(idx))
      dat[idx, v][t] <- ifelse(
        is.na(dat[idx, v][t]), dat[idx, v][t - 1L],
        dat[idx, v][t])
    
    # Backwards (only zeros)
    for (t in (length(idx) - 1L):1)
      dat[idx, v][t] <- ifelse(
        is.na(dat[idx, v][t]) & dat[idx, v][t + 1L] == 0, dat[idx, v][t + 1L],
        dat[idx, v][t])
  }
  
  dat
}

# Sorting
dat <- dat[with(dat, order(entry, year)),]
dat$no_report[is.na(dat$no_report)] <- 1L
articles <- sprintf("sum_art%02i", c(5, 6, 8, 11, 13))


# Imputing
dat2 <- dat
for (art in articles)
  dat2 <- carry_forward_and_zero_back("entry", art, dat2)

dat2$no_report <- apply(
  dat2[,grepl("^sum_art[0-9]+",colnames(dat2))], 
  1,
  function(x) all(is.na(x))
)
dat2$no_report <- as.integer(dat2$no_report)

for (y in c(2010, 2012))
  print(table(
    `n/a (Original)` = dat$no_report[dat$year == y],
    Imputed  = dat2$no_report[dat2$year == y]
    )
  )

# View(cbind(dat2[,c("entry", "year", "sum_art11")], dat[,c("entry", "year", "sum_art11")]),
#      "Imputation")

# Comparing before and after
for (art in articles)
print(table(
  Original = dat[[art]][dat$year %in% c(2012)],
  Imputed  = dat2[[art]][dat2$year %in% c(2012)],
  useNA="always"
  ))

# We'll just used the imputed data instead...
# Egypt did not reported on 2012 but did on 2010
# So carry forward only works on it!
# See http://apps.who.int/fctc/implementation/database/article/article-13/indicators/5346/reports
# Comprehensive ban on all tobacco advertising, promotion and sponsorship
# Party	2014	2012	2010
# Egypt	| Yes |	Answer/report not provided |	Yes
dat <- dat2

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
dat <- filter(dat, !is.na(ratification)) # Only those which ratified

# Sorting the data
dat <- dat[with(dat, order(entry, year)),]

# View(subset(
#   dat,
#   select=c(
#     country_name, year, pol_shift, bloomberg_amount, ratification,
#     sum_art05, sum_art06, sum_art08)))
# 

# Updating year of ratification and signature ----------------------------------
dat$year_ratification <- dat$ratification %/% 10000L
dat$year_signature    <- dat$signature %/% 10000L

# Fixing some NAs
dat$year_signature <- with(dat, ifelse(is.na(year_signature), year_ratification,
                                       year_signature))

# With respect to 2010 (trucate it to 2010)
# 
# Prior to 2010, implementation cannot affect ratification/signature since it
# was an event that happened before. But, the implementation level of 2010 can
# certainly affect signature and ratification as it is a posterior event. Therefore
# we truncate these variables at 0.
dat$`Years since Ratif.` <- with(dat, 2010L - year_ratification)
dat$`Years since Sign.`  <- with(dat, 2010L - year_signature)

dat$`Years since Ratif.`[dat$`Years since Ratif.` < 0] <- 0L
dat$`Years since Sign.`[dat$`Years since Sign.` < 0]   <- 0L

# If NA, it means that these didn't showed in the lists of reports, but may
# have ratified.
dat$no_report[is.na(dat$no_report)] <- 1L

write.csv(dat, "data/model_data_unscaled.csv", row.names = FALSE, na = "<NA>")

# Rescaling variables ----------------------------------------------------------
dat$tobac_prod_pp            <- with(dat, tobac_prod/population)
dat$bloomberg_amount_pp      <- with(dat, bloomberg_amount/population)
dat$bloomberg_fctc_amount_pp <- with(dat, bloomberg_fctc_amount/population)
dat$logPopulation            <- log(dat$population)

for (v in colnames(dat))
  if (is.double(dat[[v]])) 
    dat[[v]] <- dat[[v]]/sd(dat[[v]])

  #   {
  #   cat(sprintf("%30s: Yes\n", v))
  # } else
  #   cat(sprintf("%30s:     No\n", v))
  #   # dat[[v]] <- dat[[v]]/sd(dat[[v]])

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

