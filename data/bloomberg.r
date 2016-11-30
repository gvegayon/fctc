#
# This script processes the bloomberg data to generate a table with investments
# overall per country/year, and specific investments targeted to the FCTC
#
rm(list=ls())

library(dplyr)

dat <- read.csv("data-raw/bloomberg/bloomberg.csv", stringsAsFactors = FALSE)
dat <- subset(dat, select=c(country_name, start_date, end_date, fctc, amount))

# Adding codes -----------------------------------------------------------------
country_codes <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
country_codes <- subset(country_codes, select=c(-subdivision_assigned_codes))

dat <- merge(dat, country_codes, by="country_name", all.x=TRUE, all.y = FALSE)

# Hand coding
dat[dat$country_name=="Vietnam", "entry"] <- "VN"
dat[dat$country_name=="Bolivia", "entry"] <- "BO"
dat[dat$country_name=="Bosnia", "entry"] <- "BA"
dat[dat$country_name=="Laos", "entry"] <- "LA"
dat[dat$country_name=="Moldova", "entry"] <- "MD"

dat[dat$country_name=="Democratic Republic of the Congo", "entry"] <- "CD"
# dat[dat$country_name=="Kosovo", "entry"] <- Not relevant for the study


# Special case: Pacific Island Community ---------------------------------------
# The idea is that, instead of dividing the budget accross its members, we
# can think of it as a single org that fosters cooperation (as described
# in the website http://www.spc.int/php/)

# Members of the Pacific Island Community: Downloaded from 
# http://www.spc.int/en/about-spc/members.html on 2016-11-29 15:44:57 PST
members <- "American Samoa, Cook Islands, Fiji, French Polynesia, Guam, Kiribati, Marshall Islands, Nauru, New Caledonia, Niue, Northern Mariana Islands, Palau, Papua New Guinea, Pitcairn, Samoa, Solomon Islands, Tokelau, Tonga, Tuvalu, Vanuatu, Australia, France, New Zealand"
members <- stringr::str_trim(strsplit(members,",")[[1]], "both")
members <- c(members, "Wallis and Futuna", "Micronesia, Federated States of", "United States")

record <- dat[dat$country_name == "Pacific Islands",]

record <- data.frame(
  country_name = members,
  start_date   = record$start_date,
  end_date     = record$end_date,
  fctc         = record$fctc,
  amount       = record$amount,
  entry        = country_codes$entry[match(members, country_codes$country_name)],
  stringsAsFactors = FALSE
)

dat <- subset(dat, country_name != "Pacific Islands")
dat <- rbind(dat, record); rm(record)

# Now, we round year and add to year-country level -----------------------------
dat$year <- dat$start_date %/% 100

# Overall
aux <- dplyr::group_by(dat, country_name, entry, year) %>%
  dplyr::summarise(
    bloomberg_count = n(),
    bloomberg_amount   = sum(amount, na.rm=TRUE))

# FCTC only
dat <- filter(dat, fctc==1) %>%
  group_by(country_name, entry, year) %>%
  summarise(
    bloomberg_fctc_count = n(),
    bloomberg_fctc_amount   = sum(amount, na.rm=TRUE))

dat <- left_join(aux,dat, by=c("country_name", "entry", "year")); rm(aux)

# Filling the NAs
dat$bloomberg_fctc_count <- dplyr::coalesce(dat$bloomberg_fctc_count, 0L)
dat$bloomberg_fctc_amount <- dplyr::coalesce(dat$bloomberg_fctc_amount, 0)

# Saving the file --------------------------------------------------------------
bloomberg <- dat
write.csv(bloomberg, file="data/bloomberg.csv", na="<NA>", row.names=FALSE)