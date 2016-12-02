rm(list=ls())

options(stringsAsFactors = FALSE)

library(xml2)
library(rvest)
library(stringr)

# library(cshapes)
# 
# cshp.data <- cshp()

# Fetching the data from WIKIPEDIA ---------------------------------------------
site <- "https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_land_borders"
dat <- xml2::read_html(site)

dat <- rvest::html_table(dat)[[1]]
colnames(dat) <- c("country_name", "length", "n_land_borders", "n_land_neighbors",
                   "border_neighbors")

# Cleaning a little bit --------------------------------------------------------
neighbors <- stringr::str_replace(dat$border_neighbors, "Includes[:]\\s*\\n\\s*", "")
neighbors <- stringr::str_replace_all(neighbors,
                                      "(\\[[0-9]+\\])|(\\([0-9]+\\))", "")

neighbors <- stringr::str_trim(neighbors, "both")

# Extracting groups
neighbors <- stringr::str_extract_all(
  neighbors, "([a-zA-Z' ]+)(\\s*[:]\\s*)?([0-9,.]+\\s+km)?")

neighbors <- lapply(neighbors, stringr::str_trim, side="both")
neighbors <- lapply(neighbors, function(x) {
  x[nchar(x)>0]
  })

# Fixing "empty"
neighbors <- lapply(neighbors, function(x) {
  ifelse(grepl("^[a-zA-Z' ]+[:]?$", x), paste0(x, ": 0 km"), x)
  })

neighbors <- lapply(neighbors, function(x) {
  x <- stringr::str_split(x, "[:]\\s+")
  x <- do.call(rbind, lapply(x, c))
})


# Merging with original row
neighbors <- Map(function(a,b) {
  if (length(a)) cbind(b, a)
  else NULL
}, a = neighbors, b=dat$country_name)

# Coercing table and cleaning --------------------------------------------------
neighbors <- do.call(rbind, neighbors)
colnames(neighbors) <- c("ego", "alter", "value")
neighbors <- as.data.frame(neighbors)

# Removing remaining footnote references
for (i in 1:ncol(neighbors)) {
  neighbors[,i] <- stringr::str_replace_all(neighbors[,i], "\\[[0-9]+\\]", "")
  neighbors[,i] <- stringr::str_trim(neighbors[,i], "both")
}

# Converting value to numeric
neighbors$value <- stringr::str_replace_all(
  neighbors$value, "[km,]+", "")

neighbors$value <- as.numeric(
  stringr::str_trim(neighbors$value, "both")
  )

# All alter must be in ego
valid <- unique(neighbors$ego)
valid <- subset(neighbors, alter %in% valid)

# Saving data
write.csv(valid, file="data-raw/country_borders/country_borders.csv",
          na="<NA>", row.names = FALSE)

cat("This file was generated on ", as.character(Sys.time()), " with data\n",
    "downloaded from:\n", site, file = "data-raw/country_borders/readme.md\n",
    "The field -value- indicates length of border shared in KM (a zero means NA).",
    sep="")