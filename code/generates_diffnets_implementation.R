# rm(list = ls())

library(foreign)
library(readxl)
library(netdiffuseR)

path <- '~/Dropbox/usc/research/fctc/implementation'

implementation <- read.dta(paste0(path, "/imp_04142015.dta"))

# Processing article numbers ---------------------------------------------------
# articles 22 & 26 are mixed (why?)
implementation_key <- read_excel(paste0(path, '/Key for implementation data.xlsx'), 2,
                                 col_names = FALSE)
implementation_key$vname <- sprintf(
  'var%03d',
  1:nrow(implementation_key))
implementation_key$newname <- sprintf(
  'art%02d',
  as.integer(stringr::str_extract(implementation_key$X0, '(?<=Article[ ]?)[0-9]+'))
  )

index <- unique(implementation_key$newname)

implementation_key$subid <- unlist(
  with(implementation_key, tapply(
    newname,
    factor(newname, ordered = FALSE),
    function(x) seq_len(length(x))))[index]
  )

implementation_key$newname <- with(implementation_key,
                                   sprintf('%s_%02d', newname,subid, sep='_'))

implementation_key <- subset(implementation_key, !is.na(X0))

# Reading network data ---------------------------------------------------------
edgelist       <- read.dta(paste0(path, "/../allnets.dta"))
countries      <- read.dta(paste0(path, "/../attributes_v3.dta"))

# Classifying
classifications <- read.table(paste0(path, "/../network_classification.csv"), sep=".",
                              stringsAsFactors = FALSE, allowEscapes = TRUE)
colnames(classifications) <- c("id", "description", "static")
classifications$static <- classifications$static == "static"
classifications$description <- trimws(classifications$description, "both")

# Classifying the edgelist
edgelist$net <- factor(
  classifications$description[edgelist$relation],
  levels=classifications$description)

# Fixing names -----------------------------------------------------------------
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

# Adoption (or partial adoption)
index <- grepl("^var",colnames(implementation))
implementation$count <- rowSums(implementation[,index], na.rm = TRUE)


# Computing means 8, 11, 13, 5, 6
for (a in c(8, 11, 13, 5, 6)) {
  vn <- which(grepl(
    sprintf('^art%02d', a),
    colnames(implementation)))
  # stop()
  newvn <- sprintf('mean_art%02d', a)
  implementation[[newvn]] <-
    rowMeans(implementation[,vn,drop=FALSE], na.rm = TRUE)
  
  implementation[[newvn]][is.nan(implementation[[newvn]])] <-
    NA
}

# Exposure
toa_implementation <- subset(implementation, select=c(year, countrynameun, meanall))
toa_implementation <- reshape(
  toa_implementation,
  direction = "wide", timevar = "year",
  idvar="countrynameun")

toa_implementation <- merge(
  unique(subset(countries, select=c(id, countrynameun))),
  toa_implementation,
  by = "countrynameun",
  all.x=TRUE, all.y=FALSE
)

toa_implementation[is.na(toa_implementation)] <- 0

# Sorting
toa_implementation <- toa_implementation[order(
  as.character(toa_implementation$id)
),]

# Shaping data (articles)
imp <- implementation
index <- c('year', 'matrix_id', colnames(imp)[which(grepl('mean_art[0-9]+', colnames(imp)))])
countries <- merge(
  x=countries, by.x=c('year_bak', 'id'),
  y=subset(imp, select=index), by.y=c('year', 'matrix_id'),
  all.x=TRUE, all.y=FALSE)


# Creating diffnetobjects
i        <- 1
relations <- c(1, 2, 3, 12, 13, 14)
diffnets <- vector("list", length(relations))
for (r in relations) {
  E <- subset(edgelist, relation == r)
  
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
    dat      = subset(countries, year %in% tran[1]:tran[2]),
    idvar = "id", toavar = "toa_year_fctc", timevar="year",
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
save(list=ls(pattern = '^dn_'), file = paste0(path, '/../data/diffnets.rda'))
