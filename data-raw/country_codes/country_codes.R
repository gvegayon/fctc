rm(list=ls())

library(rvest)
library(xml2)

site <- xml2::read_html("https://en.wikipedia.org/wiki/ISO_3166-2")
dat  <- rvest::html_table(site, fill=TRUE)

country_codes <- dat[[1]]
colnames(country_codes) <- c("entry", "country_name", "subdivision_assigned_codes")

write.csv()
