rm(list=ls())
library(cshapes)

country_min_dist <- distmatrix(as.Date("2012-12-31"), type="mindist",tolerance=0.1, useGW = TRUE)

save(country_min_dist, file="data-raw/country_distance/country_min_dist.rda")
