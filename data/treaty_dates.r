# This file processes the raw data presented in the UN Treaty Collection
# website. 
rm(list=ls())

dat <- read.csv("data-raw/fctc_un_treaty/un_treaty_dates_raw.csv")

# Codes of end dates
# Acceptance(A), Approval(AA), Formal confirmation(c), Accession(a), Succession(d)
colnames(dat) <- c("country_name", "signature", "ratification")

# Trimming
for (i in colnames(dat))
  dat[[i]] <- trimws(dat[[i]], "both")

dat[["acceptance"]]     <- ifelse(grepl("\\s+A$", dat$ratification), TRUE, NA)
dat[["approval"]]       <- ifelse(grepl("AA$", dat$ratification), TRUE, NA)
dat[["formal_confirm"]] <- ifelse(grepl("\\s+c$", dat$ratification), TRUE, NA)
dat[["accession"]]      <- ifelse(grepl("\\s+a$", dat$ratification), TRUE, NA)
dat[["succession"]]     <- ifelse(grepl("\\s+d$", dat$ratification), TRUE, NA)

# Processing dates
for (i in c("ratification", "signature")) {
  dat[[i]] <- gsub("\\s*( A| AA| c| a| d)$", "", dat[[i]])
  dates <- strsplit(dat[[i]], "\\s+")
  dates <- lapply(dates, stringr::str_trim, side="both")
  dates <- unlist(sapply(dates, function(x) {
    
    if (length(x)==1) return(NA)
    
    as.integer(x[3])*10000 + 
      as.integer(which(month.abb == x[2]))*100 +
      as.integer(x[1])
  }))
  
  dat[[i]] <- dates
}

# Processing names and adding country codes
dat$country_name <- stringr::str_replace_all(
  dat$country_name, "[0-9]+\\s*$", "")

dat$country_name <- stringr::str_trim(dat$country_name, "both")

# Tweak
dat$country_name <- stringr::str_replace(dat$country_name, " \\(",", ")
dat$country_name <- stringr::str_replace(dat$country_name, "\\)","")

country_codes <- read.csv("data-raw/country_codes/country_codes.csv", na.strings = NULL)
dat <- merge(dat, country_codes, by="country_name", all.x=TRUE, all.y=FALSE)

# Not all match, so need some hand coding
dat[dat$country_name=="St. Kitts and Nevis",]$entry <- "KN"
dat[dat$country_name=="Côte d'Ivoire",]$entry <- "CI"
dat[dat$country_name=="Democratic People's Republic of Korea",]$entry <- "KP"
dat[dat$country_name=="Republic of Korea",]$entry <- "KR"
dat[dat$country_name=="Democratic Republic of the Congo",]$entry <- "CD"
# dat[dat$country_name=="European Union",]$entry <- "EU" 
dat[dat$country_name=="Republic of Moldova",]$entry <- "MD"
dat[dat$country_name=="St. Lucia",]$entry <- "LC"
dat[dat$country_name=="St. Vincent and the Grenadines",]$entry <- "VC"
dat[dat$country_name=="The former Yugoslav Republic of Macedonia",]$entry <- "MK"
dat[dat$country_name=="United Kingdom of Great Britain and Northern Ireland",]$entry <- "GB"
dat[dat$country_name=="United Republic of Tanzania",]$entry <- "TZ"
dat[dat$country_name=="United States of America",]$entry <- "US"

# Saving
dat <- dat[order(dat$country_name),]
treaty_dates <- dat
write.csv(treaty_dates, file = "data/treaty_dates.csv", row.names = FALSE,
          na = "<NA>")


