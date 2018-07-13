# This script gets the data from the FCTC website
# This file is ment to be ran from the top folder of the project.
# doing webscrapping

rm(list=ls())

# Packages used for this
library(httr)
library(xml2)
library(rvest)
library(stringr)
library(magrittr)

# Parameters 
articles  <- c("22-26a", "22-26b", "22-26c", "22-26d", 5, 6, 8, 11, 13, 2:20) 
overwrite <- FALSE

# Generic country url
url_art_expr <- "http://untobaccocontrol.org/impldb/article-%s"
fn_key <- "data-raw/fctc_implementation_2014-and-on/key.csv"

# Container (metadata)
INDICATORS <- structure(
  list(id=NULL, title=NULL, art=NULL, description=NULL, url=NULL),
  class="data.frame"
)

for (art in articles) {
  # Step 0: Generating the uri and checking if it exists or not
  url_art <- switch(art,
         `22-26a` = "http://untobaccocontrol.org/impldb/assistance-provided",
         `22-26b` = "http://untobaccocontrol.org/impldb/assistance-received",
         `22-26c` = "http://untobaccocontrol.org/impldb/other-questions-related-assistance",
         `22-26d` = "http://untobaccocontrol.org/impldb/priorities-and-comments",
         sprintf(url_art_expr, art)
  )
  
  # url_art <- sprintf(url_art_expr, art)
  ans     <- httr::GET(url_art)
  
  # Step 1: Get the list of links ------------------------------------------------
  ans <- httr::content(ans)
  sites <- xml2::xml_find_all(ans, '//*[@id="tabs-1"]/ul/li') %>%
    xml2::xml_find_all("a")
  
  descs <- xml2::xml_text(sites)
  urls  <- xml2::xml_attr(sites, "href")
  
  # If there's no data
  if (!length(urls)) {
    message("No data found at url:", url_art)
    next
  } 
  
  # Step 2: Download each site ---------------------------------------------------
  for (i in seq_along(urls)) {
    # Getting the name of the indivator
    ind  <- stringr::str_extract(urls[i], "(?<=wpdtvar[=]).+$")
    fn   <- sprintf("data-raw/fctc_implementation_2014-and-on/%s.csv", ind)
    
    if (file.exists(fn) & !overwrite) {
      message("File ", fn, " already exists, skipping.")
      next
    }
    
    # ind_url <- sprintf(url_ind_expr, ind)
    site <- xml2::read_html(urls[i])
    titl <- xml2::xml_find_first(site, "//*[@class=\"title\"]") %>%
      xml2::xml_text()

    # Getting the table
    dat <- rvest::html_table(site)[[1]]
    
    # Adding name to list
    INDICATORS <- rbind(
        INDICATORS,
      cbind(id=ind, title=titl, art=art, description=descs[i],
            url = urls[i])
    )
    
    # Saving dataset
    if (file.exists(fn_key)) 
      INDICATORS <- unique(rbind(read.csv(fn_key), INDICATORS))
        
    write.csv(dat, fn)
    write.csv(INDICATORS, fn_key, row.names = FALSE)
    message("Indicator ",ind, " complete (", titl, ").")
  
  }
}

cat(sprintf("This dataset has been generated on %s",Sys.time()),
    "Using http://untobaccocontrol.org/impldb/ ",
    "Each file has the form -[indicator id].csv-.",
    "Details on what does each indicator has can be found at the -key.csv- file.",
    file = "data-raw/fctc_implementation_2014-and-on/README.md", sep="\n")

