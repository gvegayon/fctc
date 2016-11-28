# ------------------------------------------------------------------------------
#
# This script gets data from Wikipedia (country codes)
# last modified: 2016-11-28
# author: George G Vega Yon
#
# ------------------------------------------------------------------------------

# Cleaning the space and loading the required packages
rm(list=ls())

library(rvest)
library(xml2)

# Fetching country codes from WIKIPEDIA
site <- xml2::read_html("https://en.wikipedia.org/wiki/ISO_3166-2")
dat  <- rvest::html_table(site, fill=TRUE)

# We only care about the first table
country_codes <- dat[[1]]
colnames(country_codes) <- c("entry", "country_name", "subdivision_assigned_codes")
country_codes[country_codes$country_name == "Namibia",]$entry <- "NA"
country_codes$country_name <- stringr::str_replace(country_codes$country_name,
                                                   "\\[.+", "")

# Saving the results
write.csv(country_codes, file="data-raw/country_codes/country_codes.csv",
          row.names = FALSE)
cat("This data was downloaded from https://en.wikipedia.org/wiki/ISO_3166-2",
    sprintf("on %s",as.character(Sys.time())), 
    "using the script -country_codes.r-", sep="\n",
    file="data-raw/country_codes/readme.md")
