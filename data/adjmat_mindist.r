
options(stringsAsFactors = FALSE)
rm(list=ls())

load("data-raw/country_distance/country_distance.rda")

# Loading names
country_names <- read.csv("data/gw_country_codes.csv", na="<NA>")

# Subseting only those that we have
ids <- which(rownames(adjmat_mindist) %in% country_names$code)
adjmat_mindist <- adjmat_mindist[ids,ids]

# Renaming dims
newnames <- dimnames(adjmat_mindist)
newnames <- lapply(newnames, function(x) with(country_names,entry[match(x, code)]))
dimnames(adjmat_mindist) <- newnames

# Coercing to dgCMatrix
adjmat_mindist <- methods::as(adjmat_mindist, "dgCMatrix")

# saving
save(adjmat_mindist, file="data/adjmat_mindist.rda")