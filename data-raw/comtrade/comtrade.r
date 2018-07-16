
# https://atlas.media.mit.edu/en/resources/data/
# wget https://atlas.media.mit.edu/static/db/raw/year_origin_destination_hs96_4.tsv.bz2

library(readr)
library(magrittr)
library(dplyr)

# Reading the data in
comtrade <- readr::read_tsv(
  "data-raw/comtrade/year_origin_destination_hs96_4.tsv",
  col_types = cols(
    col_integer(),
    col_character(),
    col_character(),
    col_integer(),
    col_double(),
    col_double()
  ),
  na = "NULL"
)

# Getting total
comtrade_total <- comtrade %>%
  group_by(year, origin, dest) %>%
  summarize(
    export = sum(export_val, na.rm = TRUE),
    import = sum(import_val, na.rm = TRUE)
  )

comtrade %<>%
  filter(hs96 >= 2400, hs96 < 2500) %>%
  group_by(year, origin, dest) %>%
  summarize(
    export = sum(export_val, na.rm = TRUE),
    import = sum(import_val, na.rm = TRUE)
  )

readr::write_csv(comtrade, "data-raw/comtrade/comtrade_tobacco.csv", na = "<NA>")
readr::write_csv(comtrade_total, "data-raw/comtrade/comtrade.csv", na = "<NA>")


