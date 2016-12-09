options(stringsAsFactors = FALSE)
rm(list=ls())

# Reading the dataset
dat <- read.csv("data-raw/gw_country_codes/gw_country_codes.csv")
dat <- unique(subset(dat, select=c(code, country_name)))

# Pasting the country codes ----------------------------------------------------
country_codes <- read.csv("data-raw/country_codes/country_codes.csv",
                          na.strings = "<NA>")
country_codes <- subset(country_codes, select=c(-subdivision_assigned_codes))

# Merging
dat <- merge(dat, country_codes, by="country_name", all.x=TRUE, all.y=FALSE)

# Hand coding
dat$entry[dat$country_name == "Zimbabwe (Rhodesia)"] <- "ZW"

# dat$entry[dat$country_name == "Zanzibar"] <- "ZW"
# dat$entry[dat$country_name == "Yugoslavia"] <- "MK"
dat$entry[dat$country_name == "Macedonia (Former Yugoslav Republic of)"] <- "MK"
dat$entry[dat$country_name == "Yemen, People's Republic of"] <- "YE"
dat$entry[dat$country_name == "Vietnam, Democratic Republic of"] <- "VN"
dat$entry[dat$country_name == "Venezuela"] <- "VE"
dat$entry[dat$country_name == "United States of America"] <- "US"

dat$entry[dat$country_name == "Turkey (Ottoman Empire)"] <- "TR"
dat$entry[dat$country_name == "Tanzania/Tanganyika"] <- "TZ"
dat$entry[dat$country_name == "Taiwan"] <- "TW"
dat$entry[dat$country_name == "Syria"] <- "SY"
dat$entry[dat$country_name == "Surinam"] <- "SR"

dat$entry[dat$country_name == "Sri Lanka (Ceylon)"] <- "LK"
dat$entry[dat$country_name == "Russia (Soviet Union)"] <- "RU"
dat$entry[dat$country_name == "Myanmar (Burma)"] <- "MM"
dat$entry[dat$country_name == "Moldova"] <- "MD"
# dat$entry[dat$country_name == "Madagascar (Malagasy)"] <- "MG"

dat$entry[dat$country_name == "Laos"] <- "LA"
dat$entry[dat$country_name == "Kyrgyz Republic"] <- "KG"
dat$entry[dat$country_name == "Korea, Republic of"] <- "KR"
dat$entry[dat$country_name == "Korea, People's Republic of"] <- "KP"
dat$entry[dat$country_name == "Italy/Sardinia"] <- "IT"

dat$entry[dat$country_name == "Iran (Persia)"] <- "IR"
dat$entry[dat$country_name == "German Federal Republic"] <- "DE"
dat$entry[dat$country_name == "East Timor"] <- "TL"
dat$entry[dat$country_name == "Czech Republic"] <- "CZ"
dat$entry[dat$country_name == "Cote D?Ivoire"] <- "CI"

dat$entry[dat$country_name == "Congo, Democratic Republic of (Zaire)"] <- "CD"
dat$entry[dat$country_name == "Cape Verde"] <- "CV"
dat$entry[dat$country_name == "Cambodia (Kampuchea)"] <- "KH"
dat$entry[dat$country_name == "Burkina Faso (Upper Volta)"] <- "BF"
dat$entry[dat$country_name == "Brunei"] <- "BN"

dat$entry[dat$country_name == "Bolivia"] <- "BO"
dat$entry[dat$country_name == "Belarus (Byelorussia)"] <- "BY"

# Removing empty and saving
dat <- subset(dat, !is.na(entry))

# Checking duplicates
table(dat$entry)[which(table(dat$entry) > 1)]

write.csv(dat, "data/gw_country_codes.csv", na="<NA>", row.names = FALSE)
