# This script gets the data from the FCTC website
# This file is ment to be ran from the top folder of the project.
# doing webscrapping

rm(list=ls())

# Packages used for this
library(httr)
library(xml2)
library(rvest)
library(stringr)

# Parameters 
articles <- c(5, 6, 8, 11, 13)

# Generic country url
url_art_expr <- "http://apps.who.int/fctc/implementation/database/article/article-%d/reports"
url_ind_expr <- "http://apps.who.int/fctc/implementation/database/sites/implementation/scripts/src/tabulardata.php?indicator=%s"

# Container (metadata)
INDICATORS <- structure(
  list(id=NULL, title=NULL, art=NULL, description=NULL, url=NULL),
  class="data.frame"
)

for (art in articles) {
  # Step 0: Generating the uri and checking if it exists or not
  url_art <- sprintf(url_art_expr, art)
  ans     <- httr::GET(url_art)
  
  # Step 1: Get the list of links ------------------------------------------------
  ans <- httr::content(ans)
  ans <- xml2::xml_find_all(ans, '//*[@id="indicators"]')
  ans <- xml2::xml_attr(ans, "src")
  
  # Now we have the site of the list
  ans <- httr::GET(ans)
  ans <- httr::content(ans)
  
  sites <- xml2::xml_find_all(ans, '/html/body/ul/li')
  sites <- xml2::xml_find_all(sites, "a")
  urls  <- xml2::xml_attr(sites, "href")
  
  # If there's no data
  if (!length(urls)) {
    message("No data found at url:", url_art)
    next
  }
  
  descs <- xml2::xml_text(sites)
  
  # Step 2: Download each site ---------------------------------------------------
  for (i in seq_along(urls)) {
    # Getting the name of the indivator
    ind  <- stringr::str_extract(urls[i], "(?<=indicators/)[0-9]+")
    fn   <- sprintf("data-raw/fctc_implementation_db/%s.csv", ind)
    
    # if (file.exists(fn) & !overwrite)
    
    ind_url <- sprintf(url_ind_expr, ind)
    site <- xml2::read_html(ind_url)
    
    # Parsing webpage
    titl <- xml2::xml_find_all(site, '//*[@id="indicator-container"]/h2')
    titl <- xml2::xml_text(titl)
    
    cn  <- xml2::xml_find_all(site, '//*[@id="table-answers"]/thead')
    cn  <- xml2::xml_text(xml2::xml_contents(cn))
    cn  <- cn[nchar(cn) != 0]
    dat <- rvest::html_table(site)[[1]]
    colnames(dat) <- cn
    
    # Adding name to list
    INDICATORS <- rbind(
      INDICATORS,
      cbind(id=ind, title=titl, art=art, description=descs[i],
            url = ind_url)
    )
    
    # Saving dataset
    write.csv(dat, fn)
    write.csv(INDICATORS, "data-raw/fctc_implementation_db/key.csv")
    message("Indicator ",ind, " complete (", titl, ").")
  }
}

cat(sprintf("This dataset has been generated on %s",Sys.time()),
    "Using http://apps.who.int/fctc/implementation/database ",
    "Each file has the form -[indicator id].csv-.",
    "Details on what does each indicator has can be found at the -key.csv- file",
    file = "data-raw/fctc_implementation_db/readme.md", sep="\n")

