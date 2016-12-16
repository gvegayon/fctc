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
model_data <- read.csv("data/model_data.csv", na="<NA>")

# Sorting the data appropiately
model_data    <- model_data[with(model_data, order(year, entry)),]

common_covars <- c("democracy", "GDP_pp", "tobac_prod_pp", "labor", "bloomberg_fctc_count",
                   "subscribed")
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")
nboot         <- 2000L
ncores        <- 20L
networks      <- c("adjmat_centroid_dist",
                   "adjmat_tobacco_trade",
                   "adjmat_gl_posts" # , "adjmat_referrals"
                   )

dummy_net     <- "adjmat_centroid_dist"

# Preprocessing networks gl_posts and referrals
graph <- adjmat_gl_posts[c("2009", "2010")] # 
graph <- graph[[1]] + graph[[2]]
adjmat_gl_posts <- graph # 

# graph <- adjmat_referrals[c("2008", "2009","2010", "2011")] # 
# graph <- graph[[1]] + graph[[2]] + graph[[3]] + graph[[4]]  
# adjmat_referrals <- graph %*% graph # 


# Options for each network
thresholds <- list(
  c(.35, .40, .45),
  c(.30, .40, .50),
  c(1L, 2L, 3L) #,  c(.30, .40, .50)
)

# True if the data must be processed as boolean (0-1 graph)
boolnets   <- list(FALSE, FALSE, TRUE) #, TRUE)

names(thresholds) <- c(networks)
names(boolnets)   <- c(networks)

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


# Step 1: set and adjust the graph
testnet <- "adjmat_gl_posts"
graph <- get(testnet) # 
# graph[] <- ifelse(graph[] > 2.5e-4, 1, 0)
graph <- prepare_graph(graph, model_data, as_bool = boolnets[[testnet]])

ans<-netmatch(
  # Actual data
  dat   = model_data,
  graph = graph, #graph, #rewire_graph(graph, p = n_rewires(graph), algorithm = "swap"),
  # Variable names
  timevar    = "year",
  depvar     = "sum_art05",
  covariates = common_covars,
  # Preprocessing parameters
  treat_thr  = thresholds[[testnet]][3],
  expo_pcent = !boolnets[[testnet]],
  expo_lag   = 1,
  # Matching parameters
  method   = "cem", # Coarsened Exact Matching
  distance = "mahalanobis"
);ans[-3] # ;with(ans$dat[[2]], cor(Y, treat))


# Step 2: Calibrate (define the right parameters)
# change: expo_pcent, expo_lag, method, distance
g <- resample_graph(graph);{
  d0 <- rbind(
    model_data2010[attr(g, "sample_indices"),],
    model_data2012[attr(g, "sample_indices"),]
  )
};ans<-netmatch(
  # Actual data
  dat   = d0,
  graph = g,
  # Variable names
  timevar    = "year",
  depvar     = "sum_art05",
  covariates = common_covars,
  # Preprocessing parameters
  treat_thr  = .35,
  expo_pcent = TRUE,
  expo_lag   = 1,
  # Matching parameters
  method   = "cem", # Coarsened Exact Matching
  distance = "mahalanobis"
);ans



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
  # stop()
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
    # clusterExport(cl, c("thrlvl", "art", "as_bool"))
    # suppressMessages({
    #   ans[[as.character(thrlvl)]] <- struct_test(graph, function(g) {
    #     
    #     out <- tryCatch({
    #       netmatch(
    #         # Actual data
    #         dat   = model_data,
    #         graph = g,
    #         # Variable names
    #         timevar    = "year",
    #         depvar     = art,
    #         covariates = common_covars,
    #         # Preprocessing parameters
    #         treat_thr  = thrlvl,
    #         expo_pcent = !as_bool,
    #         expo_lag   = 1L,
    #         # Matching parameters
    #         distance       = "mahalanobis",
    #         method         = "cem" #,
    #         # baseline.group = "1"
    #         # replace  = FALSE
    #       )
    #     }, error=function(e) e)
    #     
    #     if (inherits(out, "error")) {
    #       return(c(
    #         fATT = NA,
    #         n1   = NA,
    #         n0   = NA
    #       ))
    #     } else {
    #       return(c(
    #         fATT = out$fATT,
    #         n1   = out$match_obj$nn["Matched","Treated"],
    #         n0   = out$match_obj$nn["Matched","Control"]
    #       ))
    #     }
    #     },
    #     R = nboot, cl=cl, parallel="multicore", ncpus=length(cl))
    # })
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


# Structural test --------------------------------------------------------------
# stop()
# # Setting up parallel
# library(parallel)
# cl <- makeCluster(7)
# clusterExport(cl, c("model_data", "common_covars"))
# clusterEvalQ(cl, library(netdiffuseR))
# 
# # Running the test
# ans <- struct_test(graph, function(g) {
#   netmatch(model_data, g, "year", "sum_art05", common_covars,
#            treat_thr = .4, expo_pcent = TRUE, expo_lag = 1L, method="cem")$fATT
# }, R = 1000, cl=cl, parallel="multicore", ncpus=length(cl))
# 
# stopCluster(cl)
