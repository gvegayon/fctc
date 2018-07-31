# By only using those indicators that have 'yes', 'no', 'no answ...', we
# can create dichotomous versions of the indicators, and hence, counts
# of how many indicators they implemented.
rm(list=ls())
options(stringsAsFactors = FALSE)

# Parameters
codes       <- read.csv("data-raw/fctc_implementation_db/key.csv")
codes$art   <- gsub("-","", codes$art)
codes$art   <- gsub("a","1", codes$art)
codes$art   <- gsub("b","2", codes$art)
codes$art   <- gsub("c","3", codes$art)
codes$art   <- gsub("d","4", codes$art)
codes$art   <- as.integer(codes$art)

articles    <- sort(unique(codes$art))
art_to_keep <- c(5, 6, 8, 11, 13, 14)
na_replace  <- NA # If 'no response', what to replace with
process     <- function(x) {
  if (all(is.na(x))) return(NA)
  sum(x, na.rm=TRUE)
}

# Reading keys
country_codes <- read.csv("data-raw/country_codes/country_codes.csv")

lvls  <- list()
for (article in articles) {
  ss <- subset(codes, art == article)
  
  # Analyzing each indicator
  ans <- list()
  for (ind in ss$id) {
    dat  <- read.csv(sprintf("data-raw/fctc_implementation_db/%s.csv", ind))
    test <- which(grepl("^X[0-9]+$", colnames(dat)))
    lvls[[as.character(ind)]] <- sapply(dat[,test], function(x) length(unique(x)))
    
    # If this is an indicator var, then keep it
    # Some variables are actually descriptions, so we only keep those that have
    # 3 levels... Yes/No/No answer
    if (max(lvls[[as.character(ind)]]) > 3) next
    
    # Reshaping
    dat <- reshape(dat, direction = "long", varying = colnames(dat)[test],
            idvar = "Party", v.names = "X",
            times = gsub("X","",  colnames(dat)[test]))
    
    # Dichotomizing data
    dat$X <- ifelse(dat$X == "Yes", 1L, ifelse(dat$X == "No", 0L, na_replace))
    
    colnames(dat)[colnames(dat) == "X"] <- sprintf("ind%s", ind)
    
    ans[[as.character(ind)]] <- dat
  }

  # In the case that no article is 'useful'
  if (!length(unlist(ans))) {
    message("Article ", article," has no indicator (Yes/No) of implementations. Skipping.")
    next
  }
    
  
  # More processing (stacking)
  newdat <- ans[[1]]
  for (d in ans[-1]) {
    newdat <- merge(newdat, d, by=c("Party", "time"))
  }
  newdat <- data.frame(
    Party = newdat$Party,
    year  = newdat$time,
    count = apply(subset(newdat, select=c(-Party, -time)), 1, process)
  )
    
  colnames(newdat)[3] <- sprintf("sum_art%05d", article)
  
  # Saving the object
  assign(sprintf("art%05d", article), newdat)
  
  message("Article ", article, " processed.")
}

# Putting all together
arts <- ls(pattern = "^art[0-9]+$")
fctc_implementation_sums <- get(arts[1])
for (obj in arts[-1]) {
  fctc_implementation_sums <- merge(
    fctc_implementation_sums, get(obj), by=c("Party", "year"))
}

# Renaming
colnames(fctc_implementation_sums)[1] <- c("country_name")

# Checking countries for which we have network data
country_codes <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
country_codes <- subset(country_codes, select= c(-subdivision_assigned_codes))
dat <- merge(fctc_implementation_sums,
             country_codes, by="country_name", all.x=TRUE, all.y=FALSE)

# Not all match, so need some hand coding
# dat[dat$country_name=="St. Kitts and Nevis",]$entry <- "KN"
dat[dat$country_name=="Côte d'Ivoire",]$entry <- "CI"
dat[dat$country_name=="Democratic People's Republic of Korea",]$entry <- "KP"
dat[dat$country_name=="Republic of Korea",]$entry <- "KR"
dat[dat$country_name=="Democratic Republic of the Congo",]$entry <- "CD"
dat[dat$country_name=="European Union",]$entry <- "EU" # Not a country
dat[dat$country_name=="Republic of Moldova",]$entry <- "MD"
dat[dat$country_name=="The former Yugoslav Republic of Macedonia",]$entry <- "MK"
dat[dat$country_name=="United Republic of Tanzania",]$entry <- "TZ"

dat[dat$country_name=="Bahrain (Kingdom of)",]$entry <- "BH"
dat[dat$country_name=="Bolivia",]$entry <- "BO"
dat[dat$country_name=="Iran (Islamic Republic of)",]$entry <- "IR"
dat[dat$country_name=="Libyan Arab Jamahiriya",]$entry <- "LY"
dat[dat$country_name=="Micronesia (Federated States of)",]$entry <- "FM"
dat[dat$country_name=="Venezuela",]$entry <- "VE"
dat[dat$country_name=="Czech Republic",]$entry <- "CZ"

for (v in which(grepl("sum_art",colnames(dat))))
  dat[,v] <- as.integer(dat[,v])

# Checking which countries did not report anything
dat$no_report <- apply(
  dat[,grepl("^sum_art[0-9]+",colnames(dat))], 
  1,
  function(x) all(is.na(x))
)
dat$no_report <- as.integer(dat$no_report)

# Sorting
implementation <- dat[with(dat, order(country_name, year)), ]
implementation <- subset(implementation, select = c(
  "entry", "year", "no_report", sprintf("sum_art%05i", art_to_keep)
))

colnames(implementation) <-
  c("entry", "year", "no_report", sprintf("sum_art%02d", art_to_keep))

write.csv(implementation, file = "data/implementation.csv", row.names = FALSE, na="<NA>")


# Some stats -------------------------------------------------------------------
party_attributes <- read.csv("data/party_attributes.csv", na = "<NA>")
party_attributes <- subset(party_attributes, who_region != "none", select=c(-country_name))

party_attributes <- subset(party_attributes, select = c(entry, year))

treaty_dates     <- read.csv("data/treaty_dates.csv", na = "<NA>")
treaty_dates     <- subset(treaty_dates, select=c(entry, country_name, signature, ratification))

party_attributes <- dplyr::left_join(party_attributes, treaty_dates)
party_attributes <- subset(party_attributes, year == 2012 & !is.na(ratification))

dat$year <- as.integer(dat$year)
dat      <- subset(dat, year==2012, select = -country_name)
dat2     <- dplyr::left_join(party_attributes, dat, by=c("entry"))

sums <- colnames(dat2)[grepl("sum_art",colnames(dat2))]
sums <- apply(dat2[,c("country_name", sums)], 1, function(x) c(x[1], all(is.na(x[-1])))) %>%
  t %>% as.data.frame

sums

table(sums, useNA="always") %>%
  addmargins %>% print(digits=2)

# Listing the countries
unname(sums[sums$V2 == TRUE,1,drop=FALSE]) %>% `row.names<-`(NULL)

# 1 Angola
# 2 Belize
# 3 Cameroon
# 4 Cabo Verde
# 5 Czechia
# 6 Democratic Republic of the Congo
# 7 Dominica
# 8 El Salvador
# 9 Equatorial Guinea
# 10 Ethiopia
# 11 Grenada
# 12 Guinea
# 13 Guinea-Bissau
# 14 Jamaica
# 15 Kenya
# 16 Kiribati
# 17 Liberia
# 18 Maldives
# 19 Myanmar
# 20 Nauru
# 21 Nicaragua
# 22 Nigeria
# 23 Papua New Guinea
# 24 Romania
# 25 Samoa
# 26 Syrian Arab Republic
# 27 Tajikistan
# 28 The former Yugoslav Republic of Macedonia
# 29 Timor-Leste
# 30 Turkmenistan
# 31 Uzbekistan
# 32 Zambia
# 33 Zimbabwe