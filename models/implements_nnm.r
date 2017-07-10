rm(list=ls())
options(stringsAsFactors = FALSE)

# Packages
library(netdiffuseR)
library(parallel)
library(Matrix)
library(MatchIt)

# Loading data
load("data/adjmats.rda")
load("data/adjmat_border.rda")
load("data/adjmat_mindist.rda")
load("data/adjmat_centroid_dist.rda")
model_data <- read.csv("data/model_data.csv", na="<NA>", check.names = FALSE)

# Sorting the data appropiately
model_data    <- model_data[with(model_data, order(year, entry)),]
model_data$years_since_ratif <- model_data$`Years since Ratif.`
model_data$ratified <- with(
  model_data, ifelse(years_since_ratif > 5, 2, ifelse(years_since_ratif == 5, 1, 0))
)

model_data$subscribed <- ifelse(model_data$subscribed > 0, 1, 0)

common_covars <- c("GDP", "subscribed", "ratified")
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")
nboot         <- 1000L
ncores        <- 18L
networks      <- c("adjmat_centroid_dist", "adjmat_gl_posts", "adjmat_referrals",
                   "adjmat_fctc_cop_coparticipation_twomode",
                   "adjmat_fctc_inb_coparticipation_twomode")

dummy_net     <- "adjmat_centroid_dist"

# Preprocessing networks gl_posts and referrals
g0 <- adjmat_gl_posts[as.character(2008:2010)] # 
adjmat_gl_posts <- g0[[1]]
for (g in g0[-1])
  adjmat_gl_posts <- adjmat_gl_posts + g

image(adjmat_gl_posts)
nlinks(adjmat_gl_posts)/(nnodes(adjmat_gl_posts)*(nnodes(adjmat_gl_posts)-1))

g0 <- adjmat_referrals # 
adjmat_referrals <- g0[[1]]
for (g in g0[-1])
  adjmat_referrals <- adjmat_referrals + g

image(adjmat_referrals)
nlinks(adjmat_referrals)/(nnodes(adjmat_referrals)*(nnodes(adjmat_referrals)-1))

# If i referred j, then i has an influence over j, hence we transpose to compute
# exposures.
adjmat_referrals <- t(adjmat_referrals)

# Filtering data: The network must be accomodated to the observed data
# removing(adding) the extra(missing) nodes in the graph.
prepare_graph <- function(graph, dat, as_bool) {
  # Getting ids
  ids <- sort(unique(dat$entry))
  
  # Filling the missing entities
  test <- ids[which(!(ids %in% colnames(graph)))]
  if (length(test)) {
    graph <- rbind(
      graph,
      matrix(0, nrow=length(test), ncol=ncol(graph),
             dimnames = list(test, colnames(graph)))
    )
    
    graph <- cbind(
      graph,
      matrix(0, ncol=length(test), nrow=nrow(graph),
             dimnames = list(rownames(graph), test))
    )
    
  }
  
  # Subsetting to the relevant set of individuals and time period
  graph <- graph[ids,ids]
  
  # Making dichotomous
  if (as_bool) graph@x <- rep(1, length(graph@x))
  
  # We return de data in two timepoints
  return(lapply(c(2010,2012), function(x) graph))
}

#' Generating standard errors using bootstrapping
#' @param tt      Treatment threshold
#' @param expolag Number of lags for exposure
#' @param cl      A cluster object
#' @param nreps   Number of reps
booted_nnm <- function(tt, expolag, cl, nreps, art, expo_pcent) {
  # clusterExport(cl, c("tt", "expolag"))
  bootnet(graph, function(g, idx,...) {
    # Subsetting the data
    dat <- rbind(model_data2010[idx,], model_data2012[idx,])

    out <- tryCatch({
      netmatch(
        # Actual data
        dat   = dat,
        graph = g,
        # Variable names
        timevar    = "year",
        depvar     = art,
        covariates = common_covars,
        # Preprocessing parameters
        treat_thr  = tt,
        expo_pcent = expo_pcent,
        expo_lag   = expolag,
        # Matching parameters
        distance       = "mahalanobis",
        method         = "cem" #,
        # baseline.group = "1"
        # replace  = FALSE
      )
    }, error=function(e) e)
    
    if (inherits(out, "error")) {
      return(c(
        fATT = NA,
        n1   = NA,
        n0   = NA
      ))
    } else {
      return(c(
        fATT = out$fATT,
        n1   = out$match_obj$nn["Matched","Treated"],
        n0   = out$match_obj$nn["Matched","Control"]
      ))
    }
    
  }, R = nreps, cl=cl, parallel="multicore", ncpus=length(cl), tt=tt, expolag=expolag)
}

# Since is panel data, need to split by year and merge it inside
# the boot function
model_data     <- model_data[with(model_data, order(year, entry)),]
model_data2010 <- subset(model_data, year==2010)
model_data2012 <- subset(model_data, year==2012)

# Calibration ------------------------------------------------------------------

# Options for each network
thresholds <- list(
  c(.35, .40, .45),
  c(.4, .5, .6)+.1,
  c(.4, .5, .6),
  c(.70, .75, .80),
  c(25, 30, 35)
)

# True if the data must be processed as boolean (0-1 graph)
boolnets   <- list(FALSE, FALSE, FALSE, FALSE, TRUE)

names(thresholds) <- c(networks)
names(boolnets)   <- c(networks)

# Computes balance
get_balance_stats <- function(x) {
  x <- summary(x$match_obj)
  x$sum.all$Type     <- "Pre-Match"
  x$sum.matched$Type <- "Post-Match"
  
  return(with(x,rbind(sum.all, sum.matched)))
}

# Computes balance L1()
get_imbalance <- function(x) {
  dat0 <- as.data.frame(with(x$match_obj, cbind(w=weights, X, treat)))
  dat1 <- dat0[dat0[,1] >0,,drop=FALSE]
  dat0$w <- 1
  ans  <- list(dat0, dat1)
  
  
  ans  <- lapply(ans, function(y) imbalance(y$treat, y, c("w", "treat"), weights=y$w))
  names(ans) <- c("Pre-Match", "Post-Match")
  ans
}

# Looping through networks
IMBALANCE <- vector("list", length(networks))
BALANCES  <- vector("list", length(networks))
SAMPSIZES <- matrix(ncol=2, nrow=length(networks), 
                    dimnames = list(networks, c("Control", "Treated")))
names(BALANCES) <- networks
names(IMBALANCE) <- networks
for (net in networks) {
  
  # Step 0: get the options
  threshold_levels <- thresholds[[net]]
  as_bool          <- boolnets[[net]]
  
  # Step 1: set and adjust the graph
  graph <- get(net, .GlobalEnv)
  graph <- prepare_graph(graph, model_data, as_bool = as_bool)
  
  ans<-netmatch(
    # Actual data
    dat   = model_data,
    graph = graph, #graph, #rewire_graph(graph, p = n_rewires(graph), algorithm = "swap"),
    # Variable names
    timevar    = "year",
    depvar     = "sum_art05",
    covariates = common_covars,
    # Preprocessing parameters
    treat_thr  = thresholds[[net]][2],
    expo_pcent = !boolnets[[net]],
    expo_lag   = 1,
    # Matching parameters
    method   = "cem", # Coarsened Exact Matching
    distance = "mahalanobis",
    grouping = list(subscribed=list(0,1))
  )
  
  # Computing stats
  IMBALANCE[[net]] <- get_imbalance(ans)
  BALANCES[[net]]  <- get_balance_stats(ans)
  SAMPSIZES[net,]  <- ans$match_obj$nn["Matched",,drop=TRUE]
}

# Overall imbalance
ans <- t(sapply(IMBALANCE, function(x) c(x$`Pre-Match`$L1$L1, x$`Post-Match`$L1$L1)))
colnames(ans) <- c("Pre-Imbalance", "Post-Imbalance")
ans <- cbind(ans, SAMPSIZES)
ans


# Main part of the code --------------------------------------------------------

# Loop through netwrorks/articles/threshold levels to generate distributions
# of the matching estimators
for (net in networks) {
  # Step 0: get the options
  threshold_levels <- thresholds[[net]]
  as_bool          <- boolnets[[net]]
  
  # Step 1: set and adjust the graph
  graph <- get(net, .GlobalEnv)
  graph <- prepare_graph(graph, model_data, as_bool = as_bool)
  
  # Step 3: Setting up parallel
  cl <- makeCluster(ncores)
  clusterExport(cl, c("model_data2010", "model_data2012", "common_covars",
                      "nboot"))
  clusterEvalQ(cl, library(netdiffuseR))
  
  # Step 4: Computing the standard errors for each level
  for (art in articles) {
    
    ans <- NULL
    for (i in threshold_levels) {
      suppressMessages({
        ans[[as.character(i)]] <- tryCatch({
          booted_nnm(i, 1L, cl, nreps = nboot, art=art, expo_pcent = !as_bool)
        }, error=function(e) e)
      })
      message("Threshold level: ", i, " article:", art," done.")
    }
    
    # Extracting the important data
    dat  <- lapply(lapply(ans, "[[", "boot"), "[[", "t")
    
    # Number of treated and control
    numb <- lapply(dat, "[", i=1:nboot,j=-1)
    numb <- sapply(numb, colMeans, na.rm=TRUE)
    
    # Putting all together
    dat  <- lapply(dat, "[", i=1:nboot,j=1)
    dat  <- do.call(cbind, dat)
    attr(dat, "nn") <- numb
    
    assign(sprintf("bs_%s_%s",net, art), dat, envir = .GlobalEnv)
    
  }
  
  
  stopCluster(cl)
}

# Structural test --------------------------------------------------------------

# Step 0: get the options
threshold_levels <- thresholds[[dummy_net]]
as_bool          <- boolnets[[dummy_net]]

# Step 1: set and adjust the graph
graph <- get(dummy_net, .GlobalEnv)
graph <- prepare_graph(graph, model_data, as_bool = as_bool)

# Setting up parallel
cl <- makeCluster(ncores)
clusterExport(cl, c("model_data", "common_covars", "graph"))
clusterEvalQ(cl, library(netdiffuseR))


# Running the test
for (art in articles) {
  
  ans <- NULL
  for (thrlvl in threshold_levels) {
    clusterExport(cl, c("thrlvl", "art", "as_bool"))
    ans[[as.character(thrlvl)]] <- parLapply(cl, 1:nboot, function(i) {
      out <- tryCatch({
        netmatch(
          # Actual data
          dat   = model_data,
          graph = permute_graph(graph),
          # Variable names
          timevar    = "year",
          depvar     = art,
          covariates = common_covars,
          # Preprocessing parameters
          treat_thr  = thrlvl,
          expo_pcent = !as_bool,
          expo_lag   = 1L,
          # Matching parameters
          distance       = "mahalanobis",
          method         = "cem" #,
          # baseline.group = "1"
          # replace  = FALSE
        )
      }, error=function(e) e)
      
      if (inherits(out, "error")) {
        return(c(
          fATT = NA,
          n1   = NA,
          n0   = NA
        ))
      } else {
        return(c(
          fATT = out$fATT,
          n1   = out$match_obj$nn["Matched","Treated"],
          n0   = out$match_obj$nn["Matched","Control"]
        ))
      }
    })
    
    message("Threshold level: ", thrlvl, " article:", art," done.")
  }
  
  # Extracting the important data
  dat  <- lapply(ans, do.call, what=rbind)
  
  # Number of treated and control
  numb <- lapply(dat, "[", i=1:nboot,j=-1)
  numb <- sapply(numb, colMeans, na.rm=TRUE)
  
  # Putting all together
  dat  <- lapply(dat, "[", i=1:nboot,j=1)
  dat  <- do.call(cbind, dat)
  attr(dat, "nn") <- numb
  
  assign(sprintf("bs_dummy_%s", art), dat, envir = .GlobalEnv)
  
}

stopCluster(cl)

# Saving all the relevant data -------------------------------------------------
save(
  list = c(
    ls(pattern = "^bs_.+"), "articles", "common_covars", "boolnets",
    "dummy_net", "nboot", "ncores", "network_is_undirected", "threshold_levels",
    "thresholds"
  ),
  file = "models/implements_nnm.rda"
)

# Plotting ---------------------------------------------------------------------

# This function creates a fancy plot for each group
grouped_boxplot <- function(net, ylim=NULL) {
  
  # Getting the data
  ANS <- ls(pattern = paste0("bs_", net,".+"), envir = .GlobalEnv)
  
  oldpar <- par(no.readonly = TRUE)
  par(mfrow=c(3,2), oma=c(5,5,1,1))
  
  # Parameters
  s <- .25
  S <- .5
  labcex <- 1.5
  labpos <- "topright"
  if (!length(ylim)) ylim <- c(-6,8)

  i <- 1
  for (ans in ANS) {
    x   <- get(ans, .GlobalEnv)
    lab <- gsub(".+_art0?", "Art. ", ans)
    
    # Adjusting margin
    if (i %% 2) par(mai=oldpar$mai*c(s,S,s,s))
    else        par(mai=oldpar$mai*c(s,s,s,S))
    
    # Fancy column names
    boxnames <- sprintf(
      "(%d samples)",  
      apply(x, 2, function(y) length(which(complete.cases(y))))
      )
    
    # Information about the matches
    nn <- attr(x, "nn")
    nn <- apply(nn, 2, function(z) {
      z <- as.integer(round(z))
      sprintf("%d/%d", z[1], z[2])
    })
    
    boxplot(x, ylim=ylim)
    mtext(boxnames, side = 1, outer = FALSE, at=1:3, cex=.6, line=-1)
    mtext(nn, side = 3, outer = FALSE, at=1:3, cex=.6, line=-1)
    legend(labpos, legend = lab, bty = "n", cex=labcex)
    abline(h=0, lty=2)
    
    # Incrementing
    i <- i+1
  }
  
  # Adding legends
  par(mfrow=c(1,1))
  mtext(side = 1, text = "Exposure Threshold", outer = TRUE, line=2)
  mtext(side = 2, text = "Feasible Average Treatment Effect", outer = TRUE, line=2)
  par(oldpar)
  
  return(ANS)
}

# Step 5: Reporting
graphics.off()
for (net in c("dummy", networks)) {
  
  # Getting the limits for plotting
  nets <- ls(pattern = paste0("bs_", net,".+"), envir = .GlobalEnv)
  nets <- range(sapply(nets, function(x) {
    range(get(x, .GlobalEnv), na.rm = TRUE)
    }))
  
  pdf(sprintf("fig/matching_bloxplot_%s.pdf", net), width = 7, height = 10)
  grouped_boxplot(net, ylim = nets)
  dev.off()
}

# Summary table (pvalues) ------------------------------------------------------

# Computes the probability of X = 0
pvalfun <- function(x) {
  x <- x[!is.na(x)]
  ans <- sum(0 < x)/length(x)
  ifelse(ans > 0.5, 1-ans, ans)*2
}

PVALS <- matrix(ncol=5, nrow=0)
PVALS <- as.data.frame(PVALS)
for (net in c("dummy", networks)) {
  
  # Getting the limits for plotting
  nets <- ls(pattern = paste0("bs_", net,".+"), envir = .GlobalEnv)
  
  for (n in nets) {
    art <- as.integer(gsub(".+_art(?=[0-9])", "", n, perl = TRUE))
    PVALS <- rbind(
      PVALS,
      data.frame(
        net, art, 
        t(unname(apply(get(n, envir = .GlobalEnv), 2, pvalfun))))
    )
  }
  
}

# Adding column names
colnames(PVALS) <- c("Network", "Art.", "Low Thr.", "Mid.  Thr.", "High  Thr.")

cat(as.character(
  knitr::kable(PVALS, format="latex", caption = "p-values of NetMatching",
               booktabs=TRUE)),
    file = "fig/matching_summary.tex")

# Creates plot per article -----------------------------------------------------

# Graph parameters
graphics.off()
pdf(sprintf("fig/matching_summary.pdf", net), width = 7, height = 10)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(3,2), oma=c(5,5,1,1))

s <- .25
S <- .5
labcex <- 1.5
labpos <- "topright"

i <- 1
netcolors <- c("red","blue","darkred","purple", "darkgreen", "darkred")

for (art in unique(PVALS$`Art.`)) {
  
  # Getting the article
  pvals <- subset(PVALS, `Art.` == art)
  
  # Plotting
  
  # Adjusting margin
  if (i %% 2) par(mai=oldpar$mai*c(s,S,s,s))
  else        par(mai=oldpar$mai*c(s,s,s,S))
  
  plot.new()
  plot.window(xlim = c(1-.25,3.25), ylim = c(0,1))
  
  # Adding rectangle
  rect(-1,-1,4,2, col="gray")
  
  # 5% Level
  abline(h=.05, lwd=2, col=adjustcolor("black", alpha.f =.9))
  
  # Adding lines
  for (l in 1:nrow(pvals)) {
    lines(y=pvals[l,-c(1,2),drop=FALSE], x=1:3, 
          col = adjustcolor(netcolors[l], alpha.f =.9),
          lty = l, lwd=2)
  }
  
  # Adding names
  box()
  axis(1, at=1:3, labels=c("Low","Mid","High"))
  axis(2, at=seq(0,1,by=.2), las=1)
  legend(labpos, legend = paste("Art.", art), bty = "n", cex=labcex)
  
  i <- i + 1
  
  
}

# Adding legend

# Adjusting margin
if (i %% 2) {
  par(mai=oldpar$mai*c(s,S,s,s))
} else {
  par(mai=oldpar$mai*c(s,s,s,S))
}

plot.new()
plot.window(c(0,1),c(0,1))
legend(
  x      = "center",
  legend = gsub("adjmat_","", pvals$Network),
  col    = adjustcolor(netcolors, alpha.f =.9),
  lty    = 1:nrow(pvals),
  lwd    = 2,
  bty    = "n",
  title  = "Legend" 
  
)

# Adding legends
par(mfrow=c(1,1))
mtext(side = 1, text = "Exposure Threshold Level", outer = TRUE, line=2)
mtext(side = 2, text = "p-values (Probability of Effect equal to 0)", outer = TRUE, line=2)
par(oldpar)
dev.off()

