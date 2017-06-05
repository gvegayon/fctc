rm(list = ls())

library(readxl)

country_codes    <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
country_codes    <- subset(country_codes, select=c(-subdivision_assigned_codes))

govtown          <- read_xls("data-raw/govtown/govtown.xls")

govtown <- left_join(govtown, country_codes, by = c("countrynameun" = "country_name"))

govtown$entry[govtown$countrynameun == "Bolivia (Plurinational State of)"] <- "BO"
govtown$entry[govtown$countrynameun == "Cape Verde"] <- "CV"
govtown$entry[govtown$countrynameun == "Czech Republic"] <- "CZ"
govtown$entry[govtown$countrynameun == "C�te D'Ivoire"] <- "CI"
govtown$entry[govtown$countrynameun == "Democratic People's Republic of Korea"] <- "KP"
govtown$entry[govtown$countrynameun == "Democratic Republic of the Congo"] <- "CD"
govtown$entry[govtown$countrynameun == "Guinea Bissau"] <- "GW"
govtown$entry[govtown$countrynameun == "Iran (Islamic Republic of)"] <-  "IR"
govtown$entry[govtown$countrynameun == "Micronesia (Federated States of)"] <- "FM"
govtown$entry[govtown$countrynameun == "Republic of Korea"] <- "KR"
govtown$entry[govtown$countrynameun == "Republic of Moldova"] <- "MD"

# Montenegro has a tobacco company
govtown$entry[govtown$countrynameun == "Serbia and Montenegro"] <- "ME"
govtown <- rbind(
  govtown,
  data.frame(
    id = 1e3,
    countrynameun = "Serbia",
    govtown = govtown$govtown[which(govtown$entry == "ME")],
    entry = "RS"
  ))

govtown$entry[govtown$countrynameun == "The former Yugoslav Republic of Macedonia"] <- "MK"

# Investment on pension funds (see raw data)
govtown$entry[govtown$countrynameun == "United Kingdom of Great Britain and Northern Ireland"] <- "GB"

govtown$entry[govtown$countrynameun == "United Republic of Tanzania"] <- "TZ"
govtown$entry[govtown$countrynameun == "United States of America"] <- "US"
govtown$entry[govtown$countrynameun == "Venezuela (Bolivarian Republic of)"] <- "VE"

# Keeping the variable
govtown <- as.data.frame(subset(govtown, select = c(entry, govtown)))
govtown$govtown <- as.integer(govtown$govtown)

# Saving the data --------------------------------------------------------------
write.csv(govtown, "data/govtown.csv", row.names = FALSE, na = "<NA>")
