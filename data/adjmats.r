# This script creates the adjacency matrices that will be used in the paper

# Cleaning working space and loading packages ----------------------------------
rm(list = ls())
options(stringsAsFactors=FALSE)

networks_to_use <- c(
  # Trading networks
  "General Trade", "Tobacco Trade", "Bilateral Investment Treaties",
  # Co-suscription
  "FCTC INB co-participation (two-mode)","FCTC COP co-participation (two-mode)",
  # Smuggling
  "Distance (static)",
  "Referrals",
  "GL Posts",
  "Interest Group co-membership (two-mode)"
)

library(foreign)
library(netdiffuseR)
library(stringr)

# 1. Reading data --------------------------------------------------------------
edgelist <- foreign::read.dta("data-raw/allnets.dta")
id_entry <- read.csv("data/party_attributes.csv", na="<NA>")
id_entry <- unique(subset(id_entry, select=c(ego_number, entry)))

classifications  <- read.table("data-raw/network_classification.csv", sep=".",
                               stringsAsFactors = FALSE, allowEscapes = TRUE)

# Changing strings to characters
colnames(edgelist)[1:2] <- c("ego", "alter")
edgelist$ego   <- with(id_entry, entry[match(edgelist$ego, ego_number)])
edgelist$alter <- with(id_entry, entry[match(edgelist$alter, ego_number)])

colnames(classifications) <- c("id", "description", "static")
classifications$static <- classifications$static == "static"
classifications$description <- trimws(classifications$description, "both")

# Changing year  
edgelist$year <- edgelist$year + 2001
edgelist$year <- ifelse(classifications$static[edgelist$relation], NA, edgelist$year)
edgelist <- unique(edgelist)

# Classifying the edgelist
edgelist$relation <- classifications$description[edgelist$relation]

# If value is NA, then we replace it with a one
edgelist$value[is.na(edgelist$value)] <- 1

# Sorting data
edgelist <- edgelist[with(edgelist, order(year, relation)),]

# Creating adjacency matrices --------------------------------------------------

# Creating diffnetobjects
networks        <- vector("list", length(networks_to_use))
names(networks) <- networks_to_use

for (net in networks_to_use) {
  E <- subset(edgelist, relation == net)
  
  # Checkiong if there is data or not
  if (!nrow(E)) {
    next
  }
  
  # Checking if it is dynamic
  is_dyn <- !classifications$static[match(net, classifications$description)]
  
  # NEED TO EXTEND YEARS SO THAT WE CAN USE THE DATA IN COUNTRIES.
  # RIGHT NOW WE ARE ONLY USING A SUBSET OF THE YEARS.
  t0 <- if (is_dyn) E$year else NULL
  t1 <- if (is_dyn) E$year else NULL

  networks[[net]] <- edgelist_to_adjmat(
    edgelist   = E[,c("ego","alter")],
    t0         = t0,
    t1         = t1,
    undirected = FALSE,
    w          = E$value
  )
}

rm(t0, t1, net, E, is_dyn, classifications, edgelist, id_entry)

# Generating diffnets names
nicename <- function(x) {
  x <- tolower(x)
  x <- stringr::str_replace_all(x, '[:punct:]', '')
  stringr::str_replace_all(x, '[:space:]', '_')
}

for (net in names(networks)) {
  nm <- paste0('adjmat_',nicename(net))
  assign(nm, networks[[net]], envir = .GlobalEnv)
}
rm(nm, net, nicename, networks_to_use, networks)

# Keeping diffnets
save.image(file = 'data/adjmats.rda')
