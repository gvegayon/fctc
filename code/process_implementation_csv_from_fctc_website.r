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

# Checking countries for which we have network data
countries <- foreign::read.dta("data-raw/attributes_v3.dta")$countrynameun
countries <- unique(countries)

imple     <- foreign::read.dta("data-raw/imp_04142015.dta")
countries <- unique(imple$countrynameun)

countries[which(!(countries %in% fctc_implementation_sums$Party))]

curdate     <- Sys.time()
description <- "
This dataset contains the counts of items to which a country marked 'yes' in the
survey. It was generated using the data at -data-raw/fctc_implementation_db- and
the script -process_implementation_csv_from_web.r-"


save(list = c("fctc_implementation_sums", "curdate", "description"), file = 
       "data/fctc_implementation_sums.rda")
