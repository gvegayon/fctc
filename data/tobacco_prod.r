
library(dplyr)
library(magrittr)

tobacco_prod <- readr::read_csv("data-raw/faostat/UNdata_Export_20180803_210305378.csv")

codes <- readr::read_csv("data/gw_country_codes.csv") %>%
  select(entry, country_name)


tobacco_prod %<>%
  left_join(codes, by = c("Country or Area" = "country_name")) 

tobacco_prod %<>%
  rename(
    country_name = "Country or Area",
    year         = "Year",
    footnote     = "Value Footnotes",
    value        = "Value"
  ) %>% 
  filter(!is.na(country_name)) %>%
  as.data.frame

# Which didn't matched
tobacco_prod %>%
  select(country_name, entry) %>%
  unique %>%
  filter(is.na(entry)) %>%
  as.data.frame() %$% {
    cat(
      sprintf(
        "tobacco_prod[tobacco_prod$country_name == \"%s\",\"entry\"] <- ''",
        country_name
        ),
      sep = "\n")
  }
  


tobacco_prod[tobacco_prod$country_name == "Bolivia (Plurinational State of)","entry"] <- "BO"
tobacco_prod[tobacco_prod$country_name == "Côte d'Ivoire","entry"] <- 'CI'
tobacco_prod[tobacco_prod$country_name == "Democratic People's Republic of Korea","entry"] <- 'KP'
tobacco_prod[tobacco_prod$country_name == "Democratic Republic of the Congo","entry"] <- 'CD'
tobacco_prod[tobacco_prod$country_name == "Ethiopia PDR","entry"] <- 'ET'
tobacco_prod[tobacco_prod$country_name == "Iran (Islamic Republic of)","entry"] <- 'IR'
tobacco_prod[tobacco_prod$country_name == "Lao People's Democratic Republic","entry"] <- 'LA'
tobacco_prod[tobacco_prod$country_name == "Republic of Korea","entry"] <- 'KR'
tobacco_prod[tobacco_prod$country_name == "Republic of Moldova","entry"] <- 'MD'

tobacco_prod[tobacco_prod$country_name == "Russian Federation","entry"] <- 'RU'
tobacco_prod[tobacco_prod$country_name == "Saint Vincent and the Grenadines","entry"] <- 'VC'

tobacco_prod[tobacco_prod$country_name == "Solomon Islands","entry"] <- 'SB'

tobacco_prod[tobacco_prod$country_name == "Syrian Arab Republic","entry"] <- 'SY'
tobacco_prod[tobacco_prod$country_name == "The former Yugoslav Republic of Macedonia","entry"] <- 'MK'

tobacco_prod[tobacco_prod$country_name == "Timor-Leste","entry"] <- 'TL'
tobacco_prod[tobacco_prod$country_name == "United Republic of Tanzania","entry"] <- 'TZ'

tobacco_prod[tobacco_prod$country_name == "United States of America","entry"] <- 'US'

tobacco_prod[tobacco_prod$country_name == "Venezuela (Bolivarian Republic of)","entry"] <- 'VE'
tobacco_prod[tobacco_prod$country_name == "Viet Nam","entry"] <- 'VN'


tobacco_prod <- filter(
  tobacco_prod, !is.na(entry), Element == "Production") %>%
  select(entry, year, value) %>%
  rename(tobacco_prod = value)

readr::write_csv(tobacco_prod, "data/tobacco_prod.csv", na = "<NA>")

