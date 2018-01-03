# TAKE A LOOK AT
# https://cran.r-project.org/web/packages/dggridR/vignettes/dggridR.html

rm(list=ls())
options(stringsAsFactors = FALSE)
library(cshapes)
library(maps)

# Reading data
model_data       <- read.csv("data/model_data.csv", na="<NA>")
party_attributes <- read.csv("data/party_attributes.csv", na="<NA>")
for (cont in unique(party_attributes$continent))
  party_attributes[[cont]] <- ifelse(party_attributes$continent == cont, 1L, 0L)

party_attributes <- subset(party_attributes, year==2012)
party_attributes <- party_attributes[,c("entry", unique(party_attributes$continent))]

model_data       <- merge(model_data, party_attributes, by="entry", all=TRUE,
                          suffixes=c("",".y"))
load("data-raw/cshape_data/cshape_data.rda")

draw_implementation_map <- function(
  dat,
  iso2var,
  countvar,
  world,
  main = "",
  sub=NULL,
  title.args = list(),
  add.key    = TRUE,
  itemran
  ) {
  
  # Marking those countries for which we have data
  ids   <- dat[[iso2var]]
  world <- world[world$ISO1AL2 %in% ids,]
  dat   <- dat[match(world$ISO1AL2, ids),]
  ids   <- ids[match(world$ISO1AL2, ids)]
  
  cdat <- dat[[countvar]]
  # cdat <- cdat[match(world$ISO1AL2, ids)]
  have_data <- !is.na(cdat)
  
  cdat <- (cdat - itemran[1])/
    (itemran[2] - itemran[1])
  
  colors <- colorRamp(blues9)(cdat)
  colors[is.na(colors)] <- 255
  colors <-rgb(colors, maxColorValue = 255)
  
  if (add.key) {
    oldpar <- par(no.readonly = TRUE)
    par(mar = par()$mar*c(0,0,0,3))
  }
  # plot(world[have_data,], col=colors, border="gray", lwd=.5)
  plot(world[!have_data,], col="gray", density=30, add=TRUE, border="gray",
       lwd=.5)
  
  # Adding labels
  text(
    getSpPPolygonsLabptSlots(world[have_data,]),
    labels = world[have_data,]$ISO1AL2,
    cex    = .5
      )
  
  # Adding color key
  if (add.key) {
    par(oldpar)
    dat_seq <- itemran[1]:itemran[2]
    netdiffuseR::drawColorKey(
      x             = c(-1, itemran), 
      tick.marks    = c(-1, dat_seq),
      labels        = c("Treaty not ratified",dat_seq),
      color.palette = c("gray",colorRampPalette(blues9)(length(dat_seq))),
      pos           = 2,
      add.box       = TRUE, 
      main          = "Number\nof Items",
      border        = NA,
      density       = c(30, rep(-1,length(dat_seq)))
    )
    
  }
  
  
  do.call(title, c(list(main=main, sub=sub),title.args))
}

for (art in c(5, 6, 8, 11, 13)) {
  
  # Varname
  vn <- sprintf("sum_art%02i", art)
  
  # For 2012
  fn <- sprintf("fig/implementation_art%02i_map2012.pdf", art)

  # Maximum number of items implemented
  iran <- range(model_data[[vn]], na.rm = TRUE)
  
  graphics.off()
  oldpar <- par(no.readonly = TRUE)
  pdf(file = fn, width = 12, height = 8.5)
  draw_implementation_map(
    subset(model_data, is.na(year) | year==2012),
    "entry", vn, cshape_data,
    main="", # Number of items implemented of Art. 11 by 2012.
    sub="", # Source: Downloaded from http://apps.who.int/fctc/implementation/database
    itemran = iran
  )
  par(oldpar)
  dev.off()
  
  # For 2010
  fn <- sprintf("fig/implementation_art%02i_map2010.pdf", art)

  graphics.off()
  oldpar <- par(no.readonly = TRUE)
  pdf(file = fn, width = 12, height = 8.5)
  draw_implementation_map(
    subset(model_data, is.na(year) | year==2010),
    "entry", vn, cshape_data,
    main="", # Number of items implemented of Art. 11 by 2010.
    sub="",  # Source: Downloaded from http://apps.who.int/fctc/implementation/database
    itemran = iran
  )
  par(oldpar)
  dev.off()
}

