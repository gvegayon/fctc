# This file creates the file -cshape_data.rda- from the cshapes R package
rm(list=ls())

library(cshapes)

cshape_data <- cshp(as.Date("2012-12-31"), useGW = TRUE)

save(cshape_data, file="data-raw/cshape_data/cshape_data.rda")

cat(
  "This data was retrieved from the `cshapes` R package version",
  as.character(packageVersion("cshapes")),
  "and generated on", as.character(Sys.time()),
  file="data-raw/cshape_data/readme.md", sep="\n"
)
