# By only using those indicators that have 'yes', 'no', 'no answ...', we
# can create dichotomous versions of the indicators, and hence, counts
# of how many indicators they implemented.
rm(list=ls())
options(stringsAsFactors = FALSE)

# Parameters
articles   <- c(5, 6, 8, 11, 13)
na_replace <- 0L # If 'no response', what to replace with

# Reading keys
codes <- read.csv("data-raw/fctc_implementation_db/key.csv")
codes <- codes[,-1]

# Creating space
codes$`Avg2010` <- NA
codes$`Avg2012` <- NA
codes$`Avg2014` <- NA

codes$`Sd2010` <- NA
codes$`Sd2012` <- NA
codes$`Sd2014` <- NA

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
    if (max(lvls[[as.character(ind)]]) > 3) next
    
    # Reshaping
    dat <- reshape(dat, direction = "long", varying = colnames(dat)[test],
            idvar = "Party", v.names = "X",
            times = gsub("X","",  colnames(dat)[test]))
    
    # Dichotomizing data
    dat$X <- ifelse(dat$X == "Yes", 1L, ifelse(dat$X == "No", 0L, na_replace))
    
    # Adding up and filling the data
    means  <- tapply(dat$X, dat$time, mean)
    sds    <- tapply(dat$X, dat$time, sd)
    
    codes$Avg2010[which(codes$id == ind)] <- means["2010"]
    codes$Avg2012[which(codes$id == ind)] <- means["2012"]
    codes$Avg2014[which(codes$id == ind)] <- means["2014"]
    
    codes$Sd2010[which(codes$id == ind)] <- sds["2010"]
    codes$Sd2012[which(codes$id == ind)] <- sds["2012"]
    codes$Sd2014[which(codes$id == ind)] <- sds["2014"]
    
    
  }
}

write.csv(codes, file = "data/implementation_per_item.csv",
          row.names = FALSE, na="<NA>")
