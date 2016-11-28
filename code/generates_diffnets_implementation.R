# Data preprocessing
#
# Datasets included (/data-raw):
# - implementation: Panel dataset with implementation measurements based on articles
#   of the FCTC.
# - edgelist: Collection of different networks (some dynamic).
# - countries: Panel dataset with country covariates.
# - implementation_key: Varnames in implementation

# 0. Cleaning working space and loading packages -------------------------------
rm(list = ls())
library(foreign)
library(readxl)
library(netdiffuseR)
library(stringr)

# 1. Reading data --------------------------------------------------------------
implementation     <- read.dta("data-raw/imp_04142015.dta")
implementation_key <- read_excel('data-raw/Key for implementation data.xlsx', 2,
                                 col_names = FALSE) # articles 22 & 26 are mixed (why?)
edgelist           <- read.dta("data-raw/allnets.dta")
countries          <- read.dta("data-raw/attributes_v3.dta")
classifications    <- read.table("data-raw/network_classification.csv", sep=".",
                              stringsAsFactors = FALSE, allowEscapes = TRUE)

# 2. Processing article numbers ------------------------------------------------
# Replacing colnames (which are in the form of var1, var2, ...) to article
# numbers.
implementation_key$vname <- sprintf('var%03d', 1:nrow(implementation_key))
implementation_key$newname <- sprintf(
  'art%02d',
  as.integer(
    stringr::str_extract(implementation_key$X0, '(?<=Article[ ]?)[0-9]+')
    )
  )

# Generating a subid (e.g. article 8 has 7 points, so goes from 1 to 7)
index <- unique(implementation_key$newname)

implementation_key$subid <- unlist(
  with(implementation_key, tapply(
    newname,
    factor(newname, ordered = FALSE),
    function(x) seq_len(length(x))))[index]
  )

implementation_key$newname <- with(implementation_key,
                                   sprintf('%s_%02d', newname,subid, sep='_'))

# 3. Processing network data ---------------------------------------------------
# The data.frame -classifications- has the names of the networks.

colnames(classifications) <- c("id", "description", "static")
classifications$static <- classifications$static == "static"
classifications$description <- trimws(classifications$description, "both")

# Classifying the edgelist
edgelist$net <- factor(
  classifications$description[edgelist$relation],
  levels=classifications$description)

# 4. Fixing names --------------------------------------------------------------
# Using the data from -implementation_key-, here we replace the names in the
# data.frame -implementation- 
cn <- colnames(implementation)
index <- which(grepl('^var[0-9]+$', cn))
cn <- cn[index]
cn <- strsplit(cn, 'r')

cn <- data.frame(id=1:length(cn), cn=sapply(cn, function(x) {
  sprintf('var%03d',as.integer(x[2]))
}))

cn <- merge(cn, implementation_key, by.x='cn', by.y='vname', all.x=TRUE, all.y=FALSE)
cn <- cn[order(cn$id),]
cn <- subset(cn, select=c(cn, newname))

colnames(implementation)[index] <- cn$newname

# ------------------------------------------------------------------------------
#
# Up to here we haven't created any new data, just change names and labels.
#
# ------------------------------------------------------------------------------

# Adoption (or partial adoption) -----------------------------------------------
# Generates average adoption of articles
index <- grepl("^var",colnames(implementation))
implementation$count <- rowSums(implementation[,index], na.rm = TRUE)

# Computing means 8, 11, 13, 5, 6
for (a in c(5, 6, 8, 11, 13)) {
  
  # Which variables are part of the article
  vn <- which(
    grepl(sprintf('^art%02d', a),colnames(implementation))
    )
  
  # Observe that we are computing means using NAs
  newvn <- sprintf('mean_art%02d', a)
  
  implementation[[newvn]] <-
    rowMeans(implementation[,vn,drop=FALSE], na.rm = TRUE)
  
  implementation[[newvn]][is.nan(implementation[[newvn]])] <-
    NA
  
  # Sums
  newvn <- sprintf('sum_art%02d', a)
  
  implementation[[newvn]] <-
    rowSums(implementation[,vn,drop=FALSE], na.rm = TRUE)
  
  implementation[[newvn]][is.nan(implementation[[newvn]])] <-
    NA
}

# Shaping data (articles)
imp <- implementation
index <- c('year', 'matrix_id', 
           colnames(imp)[which(grepl('(mean|sum)_art[0-9]+', colnames(imp)))]
           )
countries <- merge(
  x=countries, by.x=c('year_bak', 'id'),
  y=subset(imp, select=index), by.y=c('year', 'matrix_id'),
  all.x=TRUE, all.y=FALSE)

# Changing range of TOA
for (i in c("toa_year_fctc", "toa_year_corrupt", "toa_year_pollute", "toa_year_picc"))
  countries[[i]] <- countries[[i]] + 2001

# Creating diffnetobjects
i         <- 1
relations <- c(1, 2, 3, 12, 13, 14)
diffnets  <- vector("list", length(relations))
for (r in relations) {
  E <- subset(edgelist, relation == r)
  
  # Year starts from 2002
  E$year <- E$year + 2001
  
  # Checkiong if there is data or not
  if (!nrow(E)) {
    next
  }
  
  # NEED TO EXTEND YEARS SO THAT WE CAN USE THE DATA IN COUNTRIES.
  # RIGHT NOW WE ARE ONLY USING A SUBSET OF THE YEARS.
  t1 <- if (r %in%  c(1,12:14)) NULL else E$year
  tran <- range(E$year)
  
  diffnets[[i]] <- edgelist_to_diffnet(
    edgelist = E[,c("id","nom")],
    t0       = E$year,
    t1       = t1,
    # Be sure the data are the same as the spanned time in the network!
    dat      = subset(countries, year_bak %in% tran[1]:tran[2]),
    idvar = "id", toavar = "toa_year_fctc", timevar="year_bak",
    warn.coercion = FALSE,
    fill.missing = "both" # fill missing in either network or adoption
  )
  
  i <- i+1
}

# Generating diffnets names
nicename <- function(x) {
  x <- tolower(x)
  x <- stringr::str_replace_all(x, '[:punct:]', '')
  stringr::str_replace_all(x, '[:space:]', '_')
}
for (i in seq_len(length(diffnets))) {
  nm <- paste0('dn_',nicename(classifications$description[relations[i]]))
  assign(nm, diffnets[[i]], envir = .GlobalEnv)
}

# Keeping diffnets
save(list=ls(pattern = '^dn_'), file = 'data/fctc_diffnets.rda')
