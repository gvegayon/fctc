
rm(list=ls())

# Downloading if not there already
if (!file.exists("data-raw/gw_country_codes/iisystem.dat.txt"))
  download.file(
    "http://privatewww.essex.ac.uk/~ksg/data/iisystem.dat",
    "data-raw/gw_country_codes/iisystem.dat.txt")


dat <- read.table("data-raw/gw_country_codes/iisystem.dat.txt",
                  col.names = c("code", "alpha3", "country_name", "date1", "date2"),
                  header = FALSE, sep="\t")

write.csv(dat, "data-raw/gw_country_codes/gw_country_codes.csv", row.names = FALSE)

