rm(list=ls())
library(cshapes)

country_centroid_dist <- distmatrix(as.Date("2012-12-31"), type="centdist",tolerance=0.1, useGW = TRUE)

save(country_centroid_dist, file="data-raw/country_distance/country_centroid_dist.rda")
