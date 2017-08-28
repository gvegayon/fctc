# This files creates the adjacency matrix of the network based on
# land borders
rm(list=ls())

library(netdiffuseR)

options(stringsAsFactors = FALSE)

# Reading data
dat <- read.csv("data-raw/country_borders/country_borders.csv", na="<NA>")
country_codes <- read.csv("data/gw_country_codes.csv", na="<NA>")
colnames(country_codes)[3] <- "ego"

dat <- merge(dat, country_codes, by="ego", all.x=TRUE, all.y=FALSE)

# Hand coding
dat$entry[dat$ego == "Bolivia"] <- "BO"
dat$entry[dat$ego == "Côte d'Ivoire"] <- "CI"
dat$entry[dat$ego == "Czech Republic"] <- "CZ"
dat$entry[dat$ego == "Democratic Republic of the Congo"] <- "CD"

dat$entry[dat$ego == "East Timor"] <- "TL"
dat$entry[dat$ego == "French Guiana (France)"] <- "GF"
dat$entry[dat$ego == "Hong Kong (People's Republic of China)"] <- "HK"

dat$entry[dat$ego == "Iran"] <- "IR"
dat$entry[dat$ego == "Laos"] <- "LA"
dat$entry[dat$ego == "Macau"] <- "MO"
dat$entry[dat$ego == "Macedonia"] <- "MK"

dat$entry[dat$ego == "Moldova"] <- "MD"
dat$entry[dat$ego == "North Korea"] <- "KP"
dat$entry[dat$ego == "South Korea"] <- "KR"
dat$entry[dat$ego == "Palestine"] <- "PS"
dat$entry[dat$ego == "People's Republic of China"] <- "CN"
dat$entry[dat$ego == "Republic of the Congo"] <- "CG"
dat$entry[dat$ego == "Russia"] <- "RU"
dat$entry[dat$ego == "Saint Martin"] <- "MF"
dat$entry[dat$ego == "Syria"] <- "SY"

dat$entry[dat$ego == "Tanzania"] <- "TZ"
dat$entry[dat$ego == "The Gambia"] <- "GM"
dat$entry[dat$ego == "Venezuela"] <- "VE"
dat$entry[dat$ego == "Vietnam"] <- "VN"
dat$entry[dat$ego == "Vatican City"] <- "VA"
dat$entry[dat$ego == "Vietnam"] <- "VN"

# View(dat[is.na(dat$entry),],"dat")

# All the rest that has NA in entry is droped
dat <- subset(dat, !is.na(entry))

# Replacing alter
dat$alter <- with(dat, entry[match(alter, ego)])
dat$ego <- dat$entry
dat <- subset(dat, select=c(-entry))

# Droping self (there are a few of those), and those with value == 0
dat <- subset(dat, ego != alter)
dat <- subset(dat, value > 0)

# Creating adjmat and saving it
adjmat_border <- edgelist_to_adjmat(
  edgelist   = dat[,c("ego","alter")],
  undirected = TRUE,
  w          = dat$value
)

save(adjmat_border, file="data/adjmat_border.rda")
