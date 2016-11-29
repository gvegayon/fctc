# This script generates the pol_switch variable which is marked with a 1 if
# the party observed a political switch relatively from the year of ratification

rm(list=ls())

library(foreign)
library(stringr)

# Variables of interest:
# * EXECRLC: Right (1); Left (3); Center (2); No information (0); No executive (NA)
#            shows the orientation of the party
# Terms on the website such as “liberal”, “progressive”, “authoritarian” or “xenophobic” were dealt
# with in the following way: For “liberal” we went with the European definition (right), since the
# website is based in Europe. We classified “progressive”, “authoritarian”, “xenophobic” as “0”
# (none of the above) unless we had additional information that allowed us to position the party on
# the left - right - spectrum (see 2).

dat <- foreign::read.dta("data-raw/database_of_political_institutions/DPI2015/DPI2015_stata11.dta")
dat <- subset(dat, select=c(countryname, year, execrlc))

colnames(dat)[1] <- "country_name"
dat$country_name <- stringr::str_trim(dat$country_name, "both")

# Adding codes -----------------------------------------------------------------
country_codes <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
country_codes <- subset(country_codes, select=c(-subdivision_assigned_codes))

dat <- merge(dat, country_codes, by="country_name", all.x=TRUE, all.y = FALSE)

# Manual coding
dat[dat$country_name=="Yugoslavia", "entry"] <- "MK"
# dat[dat$country_name=="Yemen (PDR)", "entry"] <- Not relevant for our study
# dat[dat$country_name=="Yemen (AR)", "entry"] <- Not relevant for our study
dat[dat$country_name=="Vietnam", "entry"] <- "VN"
dat[dat$country_name=="Venezuela", "entry"] <- "VE"
dat[dat$country_name=="USA", "entry"] <- "US"
dat[dat$country_name=="UK", "entry"] <- "GB"
# dat[dat$country_name=="Turk Cyprus", "entry"] <- NO ISO CODE
dat[dat$country_name=="Trinidad-Tobago", "entry"] <- "TT"
dat[dat$country_name=="Tanzania", "entry"] <- "TZ"
dat[dat$country_name=="Taiwan", "entry"] <- "TW"
dat[dat$country_name=="Syria", "entry"] <- "SY"
dat[dat$country_name=="St. Lucia", "entry"] <- "LC"

# dat[dat$country_name=="Soviet Union", "entry"] <- Not relevant for our study

dat[dat$country_name=="Solomon Is.", "entry"] <- "SB"
dat[dat$country_name=="S. Africa", "entry"] <- "ZA"
dat[dat$country_name=="Russia", "entry"] <- "RU"
dat[dat$country_name=="ROK", "entry"] <- "KR"
dat[dat$country_name=="PRK", "entry"] <- "KP"
dat[dat$country_name=="PRC", "entry"] <- "CN"

dat[dat$country_name=="P. N. Guinea", "entry"] <- "PG"
dat[dat$country_name=="Moldova", "entry"] <- "MD"
dat[dat$country_name=="Macedonia", "entry"] <- "MK"
dat[dat$country_name=="Laos", "entry"] <- "LA"
dat[dat$country_name=="Iran", "entry"] <- "IR"

# dat[dat$country_name=="GDR", "entry"] <- Not relevant for our study

dat[dat$country_name=="FRG/Germany", "entry"] <- "DE"
dat[dat$country_name=="Eq. Guinea", "entry"] <- "GQ"
dat[dat$country_name=="Dom. Rep.", "entry"] <- "DO"
dat[dat$country_name=="Czech Rep.", "entry"] <- "CZ"
dat[dat$country_name=="C. Verde Is.", "entry"] <- "CV"
dat[dat$country_name=="Cote d'Ivoire", "entry"] <- "CI"

dat[dat$country_name=="Congo (DRC)", "entry"] <- "CD"
dat[dat$country_name=="Comoro Is.", "entry"] <- "KM"
dat[dat$country_name=="Cent. Af. Rep.", "entry"] <- "CF"

dat[dat$country_name=="Brunei", "entry"] <- "BN"
dat[dat$country_name=="Bosnia-Herz", "entry"] <- "BA"
dat[dat$country_name=="Bolivia", "entry"] <- "BO"

# Dropping no coded, sorting and replacing -999 with NAs -----------------------
dat <- subset(dat, !is.na(entry))
dat <- dat[with(dat, order(entry, year)),]
dat$execrlc[dat$execrlc==-999] <- NA

# Merging with treaty dates ----------------------------------------------------
treaty_dates <- load("data/treaty_dates.rda", verbose = TRUE)
treaty_dates$year_ratification <- treaty_dates$ratification %/% 10000
treaty_dates$year_signature    <- treaty_dates$signature %/% 10000

treaty_dates <- subset(
  treaty_dates, select = c(entry, year_ratification, year_signature))

dat <- merge(dat, treaty_dates, by=c("entry"), all.x=TRUE, all.y = FALSE)

# Creating the political shift variable ----------------------------------------
# Recall that 1, 2, 3 means left, center, and right. A zero has to do with
# dictatorship and other types of goverments, so it can only be used in
# -pol_shift- and not in left or right shift.
dat <- by(dat, dat$country_name, function(d) {
  
  # Constants
  n    <- nrow(d)
  execrlc_ratification <- d$execrlc[d$year == d$year_ratification[1]]

  # Shift to the left
  d$pol_shift_left <- as.integer(d$execrlc < execrlc_ratification)
  d$pol_shift_left[is.na(d$execrlc) || d$execrlc == 0] <- NA
  
  # Shift to the right
  d$pol_shift_right <- as.integer(d$execrlc > execrlc_ratification)
  d$pol_shift_right[is.na(d$execrlc) || d$execrlc == 0] <- NA
  
  # Shift
  d$pol_shift <- as.integer(d$execrlc != execrlc_ratification)
  d$pol_shift[is.na(d$execrlc)] <- NA
  
  d
})

dat <- do.call(rbind, dat)

# Preparing and saving ---------------------------------------------------------
rownames(dat) <- NULL
dat <- dat[with(dat, order(entry, year)),]

political_shifts <- dat
save(political_shifts, file="data/political_shifts.rda")
