library(oec)
library(magrittr)
library(dplyr)

# Data from sitc
2401 # Unmanufactured tobacco; tobacco refuse.
2403 # Other manufactured tobacco and tobacco substitutes
8478 # Machinery for preparing or making up tobacco

get_all <- function(years, code) {
  
  countries <- combn(country_codes$country_code, 2, simplify = FALSE)
  
  on.exit(parallel::stopCluster(cl))
  cl <- parallel::makeForkCluster(4L)
  
  uri <- if (length(years) > 1)
    sprintf("http://atlas.media.mit.edu/sitc/export/%d.%d/all/all/%d/", years[1], tail(years,1), code)
  else
    sprintf("http://atlas.media.mit.edu/sitc/export/%d.%d/all/all/%d/", years, code)
  
  z <- httr::GET(uri)
  z <- httr::content(z)[[1]]
  
  lapply(z, as.data.frame) %>%
    bind_rows %>%
    as_tibble
  
  
}

ans <- get_all(2010:2015, 2401)
