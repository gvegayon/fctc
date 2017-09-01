rm(list = ls())

library(igraph)
library(netdiffuseR)

options(stringsAsFactors = FALSE)

load("data/adjmats.rda")
model_data <- read.csv("data/model_data.csv", na = "<NA>")

# Filtering the right countries
ids <- sort(unique(model_data$entry))

W <- adjmat_interest_group_comembership_twomode$`2010`

# Filling the missing entities
test <- ids[which(!(ids %in% colnames(W)))]
if (length(test)) {
  W <- rbind(
    W,
    matrix(0, nrow=length(test), ncol=ncol(W),
           dimnames = list(test, colnames(W)))
  )
  
  W <- cbind(
    W,
    matrix(0, ncol=length(test), nrow=nrow(W),
           dimnames = list(rownames(W), test))
  )
  
}
W <- W[ids,ids]


net <- graph_from_adjacency_matrix(W)

# coordinates <- layout_with_sugiyama(net)$layout
put_together <- function(x, net, criter = 6) {
  
  # 1. Dropping isolates
  d <- degree(net, mode = "total")
  xnew <- x[d > criter, ,drop=FALSE]
  
  # 2. Computing ranges and finding center
  xran <- range(xnew[,1])
  yran <- range(xnew[,2])
  
  center <- cbind(mean(xran), mean(yran))
  radii  <- sqrt(sum((center - c(xran[1], yran[1]))^2))
  
  # 3. Pulling all the rest together
  alpha <- apply(x[d<=criter, ,drop=FALSE], 1, function(y) center - y)
  alpha <- atan(alpha[2,]/alpha[1,])
  x[d<=criter, 1] <- center[1]
  x[d<=criter, 2] <- center[2]
  radii <- radii*runif(sum(d<=criter), 1, 1+radii*.025)
  x[d<=criter,] <- x[d<=criter,] + cbind(radii*cos(alpha), radii*sin(alpha))
  
  x
  
}

# Function to assign a color according to 
setcolor <- function(varname, yearnum, maxrange = NULL) {
  ids  <- model_data[model_data$year == yearnum,]
  cols <- ids[[varname]]
  ids  <- ids[["entry"]]
  
  if (!length(maxrange))
    maxrange <- max(cols)
  
  # Truncating
  cols <- ifelse(cols > maxrange, maxrange, cols)
  
  cols <- cols/maxrange
  
  cols <- grDevices::colorRamp(mixing_cols)(cols)
  structure(rgb(cols[,1], cols[,2], cols[,3], maxColorValue = 255), 
            names = ids)
}

# Computing coordinates
set.seed(1231)
coordinates <- layout_with_fr(net)
coordinates <- put_together(coordinates, net)

maximpl     <- 10
mixing_cols <- blues9 # c(blues9[4], "tomato")

graphics.off()
for (yearnum in c(2010, 2012)) {
  
  # Making space
  # pdf(sprintf("fig/interest_group_comembership%i.pdf", yearnum),
  #     width = 10, height = 12)
  png(sprintf("fig/interest_group_comembership%i.png", yearnum),
      width = 700,
      height = 900,
      family = "Arial")
  
  oldpar <- par(no.readonly = TRUE)
  par(mfrow = c(3, 2), mar = rep(0.25, 4), oma = c(0, 0, 0, 0))
  
  for (art in c(5, 6, 8, 11, 13)) {
    
    # Creating varname
    varname <- sprintf("sum_art%02i", art)
    
    # Creating space
    plot.new()
    plot.window(range(coordinates[,1]), range(coordinates[,2]))
    
    # Specifying color
    vcols <- setcolor(varname, yearnum, maximpl)
    V(net)[names(vcols)]$color <- vcols
    
    # Vertex and label size
    vsize <- rescale_vertex_igraph(degree(net), minmax.relative.size = c(.025, .08))
    lsize <- ((vsize - min(vsize))/(max(vsize) - min(vsize)) + .5)/1.5
    
    
    # Calling igraph
    plot(net, layout = coordinates,
         vertex.size = vsize,
         add = TRUE,
         rescale = FALSE,
         edge.arrow.size = .25,
         vertex.label.cex = lsize,
         vertex.label.color = "white",
         vertex.frame.color = "black",
         edge.color = adjustcolor("gray", .5),
         edge.width = .5,
         edge.curved = TRUE,
         vertex.label = NA
         
    )
    
    box(lwd = 2, col="gray")
    
    legend(
      "topleft",
      legend = c(
        sprintf("Art. %i", art),
        sprintf(
        "Avg. Impl.: %.2f",
        mean(model_data[[varname]][model_data$year == yearnum])
        )
        ),
      bty = "n",
      cex = 1.5, text.font = c(2, 1)
    )
    
  }
  
  plot.new()
  plot.window(c(-1,1), c(-1,1))
  
  labran <- pretty_within(c(0, maximpl))
  labran[length(labran)] <- paste("\n",maximpl, "or more")
  
  
  par(cex=1.2)
  drawColorKey(
    x = c(0,maximpl), 
    labels = labran,
    color.palette = grDevices::colorRampPalette(mixing_cols)(maximpl + 1),
    nlevels = maximpl + 1,
    key.pos = c(.5, .7, .1, .9),
    main = "Number of Items\nImplemented\nper Article\n", lwd = 0
  )
  
  par(oldpar)
  dev.off()
  message(yearnum, " done.")
}

