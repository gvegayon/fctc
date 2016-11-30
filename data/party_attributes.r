rm(list=ls())

dat <- foreign::read.dta("data-raw/attributes_v3.dta")

# Adding codes -----------------------------------------------------------------
country_codes <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
country_codes <- subset(country_codes, select=c(-subdivision_assigned_codes))

colnames(dat)[3] <- "country_name"

dat <- merge(dat, country_codes, by="country_name", all.x=TRUE, all.y = FALSE)

# Hand coding
dat[dat$country_name=="Venezuela (Bolivarian Republic of)", "entry"] <- "VE"
dat[dat$country_name=="United States of America", "entry"] <- "US"
dat[dat$country_name=="United Republic of Tanzania", "entry"] <- "TZ"

dat[dat$country_name=="United Kingdom of Great Britain and Northern Ireland", "entry"] <- "GB"
dat[dat$country_name=="The former Yugoslav Republic of Macedonia", "entry"] <- "MK"
dat[dat$country_name=="Serbia and Montenegro", "entry"] <- "RS"
dat[dat$country_name=="Republic of Moldova", "entry"] <- "MD"
dat[dat$country_name=="Republic of Korea", "entry"] <- "KR"
dat[dat$country_name=="Micronesia (Federated States of)", "entry"] <- "FM"
dat[dat$country_name=="Iran (Islamic Republic of)", "entry"] <- "IR"
dat[dat$country_name=="Guinea Bissau", "entry"] <- "GW"
dat[dat$country_name=="Democratic Republic of the Congo", "entry"] <- "CD"
dat[dat$country_name=="Democratic People's Republic of Korea", "entry"] <- "KP"
dat[dat$country_name=="Czech Republic", "entry"] <- "CZ"

dat[which(stringr::str_detect(dat$country_name,"voire")), "entry"] <- "CI"

dat[dat$country_name=="Cape Verde", "entry"] <- "CV"
dat[dat$country_name=="Bolivia (Plurinational State of)", "entry"] <- "BO"

# dat[dat$country_name=="United States of America", "entry"] <- "US"

# Cleaning up ------------------------------------------------------------------
dat <- subset(dat, select=c(-id, -merge_miss_imps, -year))
colnames(dat)[colnames(dat) == "year_bak"] <- "year"

party_attributes <- dat

write.csv(party_attributes, file="data/party_attributes.csv", row.names = FALSE,
          na = "<NA>")
