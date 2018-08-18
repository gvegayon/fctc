
options(stringsAsFactors = FALSE)
library(Matrix)
rm(list=ls())

load("data-raw/country_distance/country_centroid_dist.rda")

# Loading names
country_names <- read.csv("data/gw_country_codes.csv", na="<NA>")

# Subseting only those that we have
ids <- which(rownames(country_centroid_dist) %in% country_names$gwcode)
adjmat_centroid_dist <- country_centroid_dist[ids,ids]

# Renaming dims
newnames <- dimnames(adjmat_centroid_dist)
newnames <- lapply(newnames, function(x) with(country_names,entry[match(x, gwcode)]))
dimnames(adjmat_centroid_dist) <- newnames

# Coercing to dgCMatrix
adjmat_centroid_dist <- 1/(adjmat_centroid_dist)
diag(adjmat_centroid_dist) <- 0
adjmat_centroid_dist <- methods::as(adjmat_centroid_dist, "dgCMatrix")

# saving
saveRDS(adjmat_centroid_dist, file="data/adjmat_centroid_dist.rds",
        compress = TRUE)
