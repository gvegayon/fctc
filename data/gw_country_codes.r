options(stringsAsFactors = FALSE)
rm(list=ls())

# Merging with cshapes country codes
dat <- cshapes::cshp(as.Date("2012-12-31"))
country_codes <- unique(data.frame(
  gwcode       = dat$GWCODE,
  entry        = dat$ISO1AL2,
  country_name = dat$CNTRY_NAME)
  ); rm(dat)

# Checking duplicates
test <- country_codes[is.na(country_codes$entry), "gwcode"]
test <- test[which(!(test %in% country_codes[!is.na(country_codes$entry), "gwcode"]))]

# https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
test # 347 Kosovo XK IT DOESNT SHOW IN THE TREATY DATES... SO ITS OK

# table(dat$entry)[which(table(dat$entry) > 1)]
country_codes <- country_codes[complete.cases(country_codes),]

write.csv(country_codes, "data/gw_country_codes.csv", na="<NA>", row.names = FALSE)

