# By only using those indicators that have 'yes', 'no', 'no answ...', we
# can create dichotomous versions of the indicators, and hence, counts
# of how many indicators they implemented.
rm(list=ls())
options(stringsAsFactors = FALSE)

# Parameters
articles   <- c(5, 6, 8, 11, 13)
na_replace <- 0L # If 'no response', what to replace with
process    <- function(x) sum(x)

# Reading keys
codes <- read.csv("data-raw/fctc_implementation_db/key.csv")
country_codes <- read.csv("data-raw/country_codes/country_codes.csv")

lvls  <- list()
for (article in articles) {
  ss <- subset(codes, art == article)
  
  # Analyzing each indicator
  ans <- list()
  for (ind in ss$id) {
    dat  <- read.csv(sprintf("data-raw/fctc_implementation_db/%s.csv", ind))
    test <- which(grepl("^X[0-9]+$", colnames(dat)))
    lvls[[as.character(ind)]] <- sapply(dat[,test], nlevels)
    
    # If this is an indicator var, then keep it
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

  # More processing (stacking)
  newdat <- ans[[1]]
  for (d in ans[-1]) {
    newdat <- merge(newdat, d, by=c("Party", "time"))
  }
  newdat <- data.frame(
    Party = newdat$Party,
    year  = newdat$time,
    count = rowSums(subset(newdat, select=c(-Party, -time)))
  )
  
  colnames(newdat)[3] <- sprintf("sum_art%02d", article)
  
  # Saving the object
  assign(sprintf("art%02d", article), newdat)
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

fctc_implementation_sums <- dat

curdate     <- Sys.time()
description <- "
This dataset contains the counts of items to which a country marked 'yes' in the
survey. It was generated using the data at -data-raw/fctc_implementation_db- and
the script -process_implementation_csv_from_web.r-"


save(list = c("fctc_implementation_sums", "curdate", "description"), file = 
       "data/fctc_implementation_sums.rda")