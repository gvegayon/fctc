#' ---
#' title: Bloomberg Initiative To Reduce Tobacco Use
#' author:
#' date: June 26, 2016
#' ---

#+ echo=FALSE, warning=FALSE, message=FALSE

rm(list=ls())

library(httr)
library(xml2)
library(stringr)

# "http://tobaccocontrolgrants.org/Pages/40/What-we-fund?date_from=&date_to=&viewall=View+All"
# doc <- readLines("fctc/data-raw/20160526_Bloomberg.org What we fund - Tobacco Control Grants.html")
# doc <- read_html("http://tobaccocontrolgrants.org/Pages/40/What-we-fund?date_from=&date_to=&viewall=View+All")
doc <- read_html("data-raw/bloomberg/20160526_Bloomberg.org What we fund - Tobacco Control Grants.html")

# Pin down the HTML part of it
grants  <- xml_find_all(doc, '//div[contains(@class, "grants")]')

# Coercing into a data.frame
dat <- xml2::xml_find_all(grants, "p")
dat <- xml2::xml_text(dat)
dat <- matrix(dat, byrow = TRUE, ncol=2,
               dimnames = list(NULL, c("description", "details")))
dat <- as.data.frame(dat, stringsAsFactors = FALSE)

# Easy headers
dat$program_name <- xml2::xml_text(xml2::xml_find_all(grants, "h3"))
dat$institution  <- xml2::xml_text(xml2::xml_find_all(grants, "h4"))

# Parsing metadata
meta <- c("Country", "Website", "Focus", "Approach", "Amount", "Start Date", "End Date")
for (m in meta) {
  expr <- sprintf("(?<=%s[:]\\s{0,2}).+", m)
  dat[[m]] <- stringr::str_extract(dat$details, expr)
  
  # Cleaning
  expr <- paste0("(",paste(meta, collapse="|"),")[:].+")
  dat[[m]] <- trimws(gsub(expr, "", dat[[m]]), "both")
  dat[[m]] <- stringr::str_replace_all(dat[[m]], "\\s{2,}", " ")
}

# Changing names and dropping metadata column
colnames(dat)[5:11] <-
  c("country_name", "websute", "focus", "approach", "amount", "start_date",
    "end_date")

dat <- subset(dat, select=c(-details))

# Converting dates and money!
dat$amount <- gsub("\\$", "", dat$amount)
dat$amount <- gsub("[,]", "", dat$amount)
dat$amount <- gsub("\\.", ",", dat$amount)
dat$amount <- as.numeric(dat$amount)

dat$start_date <- with(dat, {
  sapply(
    str_split(start_date, "\\s+")
    , function(x) as.integer(x[2])*100 + match(x[1], month.abb)
    )
})

dat$open_ended <- as.integer(nchar(dat$end_date) == 0)

dat$end_date <- with(dat, {
  sapply(
    str_split(end_date, "\\s+")
    , function(x) as.integer(x[2])*100 + match(x[1], month.abb)
  )
})

# Included for FTCT?
expr <- "(FCTC|fctc|Framework Convention on Tobacco)"
dat$fctc <- with(dat,
                     grepl(expr, program_name) |
                       grepl(expr, institution) |
                       grepl(expr, description) |
                       grepl(expr, focus)
                     )
dat$fctc <- as.integer(dat$fctc)

# Checking everything went fine
stopifnot(!length(which(is.na(dat$end_date) & !dat$open_ended)))

# Saving the data
write.csv(dat, file="data-raw/bloomberg/bloomberg.csv", row.names = FALSE)


# Now some tabulations ---------------------------------------------------------

# Tabulating: How much per country
library(dplyr)
spend_per_country <- group_by(dataset, country) %>%
  summarize(Total=sum(amount)/1e3, N = n())
spend_per_country <- as.data.frame(spend_per_country)
spend_per_country <- spend_per_country[order(-spend_per_country$Total),,drop=FALSE]

spend_per_fctc <- group_by(dataset, FCTC) %>%
  summarize(Total=sum(amount)/1e3, N = n())
spend_per_fctc <- as.data.frame(spend_per_fctc)
spend_per_fctc <- spend_per_fctc[order(-spend_per_fctc$Total),,drop=FALSE]

#' Total spent: USD `r formatC(sum(dataset$amount)/1e6, big.mark=",", digits=2, format="f")` Millions

#' \footnotesize

#+ echo=FALSE
pander::pander(spend_per_country,
               caption="Total amount spent per country (thousands): Extracted from
               http://tobaccocontrolgrants.org/",
               big.mark=",", digits=2)

pander::pander(spend_per_fctc,
               caption="Total amount spent on FCTC (thousands): Extracted from
               http://tobaccocontrolgrants.org/",
               big.mark=",", digits=2)

#' \normalsize

