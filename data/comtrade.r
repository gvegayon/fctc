library(dplyr)
library(magrittr)

load("data-raw/cshape_data/cshape_data.rda")

codes <- tibble(
  iso2 = as.character(cshape_data$ISO1AL2),
  iso3 = tolower(cshape_data$ISO1AL3)
)

# Comtrade all -----------------------------------------------------------------

comtrade <- readr::read_csv("data-raw/comtrade/comtrade.csv", na = "<NA>") %>%
  filter(year >= 2008)

# Merging country codes
ans <- comtrade %>%
  left_join(codes, by = c("origin" = "iso3")) %>%
  rename(code_origin = iso2) %>%
  left_join(codes, by = c("dest" = "iso3")) %>%
  rename(code_dest = iso2)  %>%
  filter_all(all_vars(!is.na(.)))

# Creating social networks
countries <- sort(with(ans, unique(c(code_origin, code_dest))))
n         <- length(countries)
years     <- sort(unique(ans$year))

G <- lapply(years, function(y) {
  
  net <- matrix(0, ncol = n, nrow=n, dimnames = list(countries, countries))  
  g   <- filter(ans, year == y)
  
  net[with(g, cbind(code_origin, code_dest))] <- g$export
  net[with(g, cbind(code_dest, code_origin))] <- g$import
  
  net
  
})

names(G) <- years
  
saveRDS(G, "data/comtrade.csv")

# Comtrade tobacco -------------------------------------------------------------

comtrade <- readr::read_csv("data-raw/comtrade/comtrade_tobacco.csv", na = "<NA>") %>%
  filter(year >= 2008)

# Merging country codes
ans <- comtrade %>%
  left_join(codes, by = c("origin" = "iso3")) %>%
  rename(code_origin = iso2) %>%
  left_join(codes, by = c("dest" = "iso3")) %>%
  rename(code_dest = iso2)  %>%
  filter_all(all_vars(!is.na(.)))

# Creating social networks
countries <- sort(with(ans, unique(c(code_origin, code_dest))))
n         <- length(countries)
years     <- sort(unique(ans$year))

G <- lapply(years, function(y) {
  
  net <- matrix(0, ncol = n, nrow=n, dimnames = list(countries, countries))  
  g   <- filter(ans, year == y)
  
  net[with(g, cbind(code_origin, code_dest))] <- g$export
  net[with(g, cbind(code_dest, code_origin))] <- g$import
  
  net
  
})

names(G) <- years

saveRDS(G, "data/comtrade_tobacco.rds")
