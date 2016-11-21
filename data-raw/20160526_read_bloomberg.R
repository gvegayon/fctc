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
doc <- read_html("../data-raw/20160526_Bloomberg.org What we fund - Tobacco Control Grants.html")

# Pin down the HTML part of it
grants <- xml_find_all(doc, '//div[contains(@class, "grants")]')

# Extracting the text
grants <- lapply(as_list(grants), xml_text)
grants <- lapply(grants, function(x) str_split(x, "\\n|\\s{3,}")[[1]])
grants <- do.call(c, grants)

# Cleaning the text
grants <- str_trim(grants, "both")
grants <- grants[which(nchar(grants) > 0)]

# Processing: Some of these don't have website. the Attribute "End Date:" Marks everything
indices <- which(grepl("^End Date[:]", grants))
indices <- cbind(
  start = c(1, indices[-length(indices)]+1), 
  end   = indices)

# Creating dataset

# Function to catch REGEXP
catchregex <- function(x, pattern) {
  y <- which(grepl(pattern, x))
  ifelse(length(y), x[y], NA)
}

# Coercing into a data.frame
dataset <- apply(indices, 1, function(x) {
  d <- grants[x[1]:x[2]]
  data.frame(name=d[1], org=d[2], description=d[3],
    country = catchregex(d, "^Country[:]"),
    focus   = catchregex(d, "^Focus[:]"),
    amount  = catchregex(d, "^Amount[:]"),
    start   = catchregex(d, "^Start Date[:]"),
    end     = catchregex(d, "^End Date[:]"),
    website = catchregex(d, "^Website[:]")
    )
})

dataset <- do.call(rbind, dataset)

# Cleaning
for (a in c("focus", "amount", "country", "start", "end", "website")) {
  dataset[[a]] <- str_replace(dataset[[a]], "^[a-zA-Z ]+[:]\\s?", "")
  dataset[[a]] <- str_trim(dataset[[a]], "both")
}

# Converting dates and money!
dataset$amount <- gsub("\\$", "", dataset$amount)
dataset$amount <- gsub("[,]", "", dataset$amount)
dataset$amount <- gsub("\\.", ",", dataset$amount)
dataset$amount <- as.numeric(dataset$amount)

dataset$start <- with(dataset, {
  sapply(
    str_split(start, "\\s+")
    , function(x) as.integer(x[2])*100 + match(x[1], month.abb)
    )
})

dataset$open_ended <- nchar(dataset$end) == 0

dataset$end <- with(dataset, {
  sapply(
    str_split(end, "\\s+")
    , function(x) as.integer(x[2])*100 + match(x[1], month.abb)
  )
})

# Included for FTCT?
dataset$FCTC <- with(dataset, grepl("FCTC", name) | grepl("FCTC", org) |
                     grepl("FCTC", description) | grepl("FCTC", focus))

# Checking everything went fine
stopifnot(!length(which(is.na(dataset$end) & !dataset$open_ended)))

# Saving the data
save(dataset, grants, indices, file="../data/bloomberg.rda")
write.csv(dataset, file="../data/bloomberg.csv")

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

