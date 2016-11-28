rm(list=ls())

# Generic country url
url       <- "http://apps.who.int/fctc/implementation/database/parties/%s"
countries <- foreign::read.dta("data-raw/attributes_v3.dta")
cnames    <- iconv(unique(countries$countrynameun), to = "ASCII//translit")

library(httr)

downloadAns <- vector("list", length(cnames))
names(downloadAns) <- cnames

for (i in cnames) {
  # Getting the country profile
  ans <- GET(sprintf(url, i))
  ans <- content(ans, "text")
  
  # Get pdf files links
  urls   <- stringr::str_extract_all(ans, "http://apps.who.int/fctc/implementation/database/sites/implementation/files/documents/reports/[a-zA-Z0-9_]+\\.pdf")[[1]]
  fnames <- stringr::str_extract_all(urls, "[a-zA-Z0-9_-]+\\.pdf")
  fnames <- unlist(fnames, recursive = TRUE)
  
  # Download them
  downloadAns[[i]] <- Map(function(a,b) {
    fn <- sprintf("data-raw/fctc_reports/%s_%s", i, b)
    if (file.exists(fn)) {
      message("File ", b, " already downloaded.")
      return(NA)
    } else {
      tryCatch({
        download.file(a, fn)
      }, error = function(e) e)
    }
    
  }, a = urls, b=fnames)
  
  # Announcing
  message("Country ", i, " done ", which(cnames==i), "/", length(cnames))
}

save.image("data/fetch_reports_from_fctc_website.rda")