
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
adjmat_mindist <- 1/(round(adjmat_mindist))
diag(adjmat_mindist) <- 0
adjmat_mindist[is.infinite(adjmat_mindist)] <- NA
adjmat_mindist[is.na(adjmat_mindist)] <- max(adjmat_mindist, na.rm = TRUE)
adjmat_mindist <- methods::as(adjmat_mindist, "dgCMatrix")

summary(adjmat_mindist@x)

# saving
save(adjmat_mindist, file="data/adjmat_mindist.rda")
