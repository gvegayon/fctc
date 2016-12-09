rm(list=ls())
library(cshapes)

country_distance <- distmatrix(as.Date("2012-12-31"), tolerance=0.1, useGW = TRUE)

save(country_distance, file="country_distance.rda")
