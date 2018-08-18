
options(stringsAsFactors = FALSE)
library(Matrix)
rm(list=ls())

load("data-raw/country_distance/country_min_dist.rda")

# Loading names
country_names <- read.csv("data/gw_country_codes.csv", na="<NA>")

# Subseting only those that we have
ids <- which(rownames(country_min_dist) %in% country_names$gwcode)
adjmat_mindist <- country_min_dist[ids,ids]

# Renaming dims
newnames <- dimnames(adjmat_mindist)
newnames <- lapply(newnames, function(x) with(country_names,entry[match(x, gwcode)]))
dimnames(adjmat_mindist) <- newnames

# Coercing to dgCMatrix
adjmat_mindist <- 1/(round(adjmat_mindist))
diag(adjmat_mindist) <- 0
adjmat_mindist[is.infinite(adjmat_mindist)] <- NA
adjmat_mindist[is.na(adjmat_mindist)] <- max(adjmat_mindist, na.rm = TRUE)
adjmat_mindist <- methods::as(adjmat_mindist, "dgCMatrix")

summary(adjmat_mindist@x)

# saving
saveRDS(adjmat_mindist, file="data/adjmat_mindist.rds")
