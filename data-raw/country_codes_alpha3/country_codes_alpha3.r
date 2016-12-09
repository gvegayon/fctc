# ------------------------------------------------------------------------------
#
# This script gets data from Wikipedia (country codes)
# last modified: 2016-12-08
# author: George G Vega Yon
#
# ------------------------------------------------------------------------------

# Cleaning the space and loading the required packages
rm(list=ls())

library(rvest)
library(xml2)

# Fetching country codes from WIKIPEDIA
site <- xml2::read_html("https://en.wikipedia.org/wiki/ISO_3166-1")
dat  <- rvest::html_table(site, fill=TRUE)

# We only care about the first table
dat <- dat[[1]]
colnames(dat) <- c("country_name", "entry", "alpha3","numeric_code", "iso_subdiv_codes")
dat[dat$country_name == "Namibia",]$entry <- "NA"

for (i in colnames(dat))
  dat[[i]] <- stringr::str_trim(dat[[i]], "both")

# Saving the results
write.csv(dat, file="data-raw/country_codes_alpha3/country_codes_alpha3.csv",
          row.names = FALSE, na="<NA>")
cat("This data was downloaded from https://en.wikipedia.org/wiki/ISO_3166-1",
    sprintf("on %s",as.character(Sys.time())), 
    "using the script -country_codes_alpha3.r-", sep="\n",
    file="data-raw/country_codes_alpha3/readme.md")
