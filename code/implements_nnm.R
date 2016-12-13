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

common_covars <- c("democracy", "GDP_pp", "tobac_prod_pp",
                   "perc_female_smoke", "perc_male_smoke")
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")
nboot         <- 1000L

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
booted_nnm <- function(tt, expolag, cl, nreps, art) {
  # clusterExport(cl, c("tt", "expolag"))
  bootnet(graph, function(g, idx,...) {
    # Subsetting the data
    dat <- rbind(model_data2010[idx,], model_data2012[idx,])
    out <- netmatch(
      # Actual data
      dat   = dat,
      graph = g,
      # Variable names
      timevar    = "year",
      depvar     = art,
      covariates = common_covars,
      # Preprocessing parameters
      treat_thr  = tt,
      expo_pcent = TRUE,
      expo_lag   = expolag,
      # Matching parameters
      # distance = "mahalanobis",
      method   = "cem" #,
      # replace  = FALSE
    )
    
    c(
      fATT = out$fATT,
      n1   = out$match_obj$nn["Matched","Treated"],
      n0   = out$match_obj$nn["Matched","Control"]
    )
  }, R = nreps, cl=cl, parallel="multicore", ncpus=length(cl), tt=tt, expolag=expolag)
}

# Since is panel data, need to split by year and merge it inside
# the boot function
model_data <- model_data[with(model_data, order(year, entry)),]
model_data2010 <- subset(model_data, year==2010)
model_data2012 <- subset(model_data, year==2012)

# FCTC centroid network -------------------------------------------------------------

# Step 1: set and adjust the graph
graph <- adjmat_centroid_dist # 
graph <- prepare_graph(graph, model_data, as_bool = FALSE)

# Step 2: Calibrate (define the right parameters)
# change: expo_pcent, expo_lag, method, distance
ans <- netmatch(
  # Actual data
  dat   = model_data,
  graph = graph,
  # Variable names
  timevar    = "year",
  depvar     = "sum_art05",
  covariates = common_covars,
  # Preprocessing parameters
  treat_thr  = .35,
  expo_pcent = TRUE,
  expo_lag   = 1,
  # Matching parameters
  method   = "cem" # Coarsened Exact Matching
)

ans
threshold_levels <- c(.35, .4,.45)

# Step 3: Setting up parallel
cl <- makeCluster(2)
clusterExport(cl, c("model_data2010", "model_data2012", "common_covars", "nboot"))
clusterEvalQ(cl, library(netdiffuseR))

# Step 4: Computing the standard errors for each level
for (a in articles) {
  
  ans <- NULL
  for (i in threshold_levels) {
    suppressMessages({
      ans[[as.character(i)]] <- tryCatch({
        booted_nnm(i, 1L, cl, nreps = nboot, art=a)
      }, error=function(e) e)
    })
    message("Threshold level: ", i, " article:", a," done.")
  }
  
  # Coercing data
  dat <- lapply(lapply(ans, "[[", "boot"), "[[", "t")
  dat <- lapply(dat, "[", i=1:nboot,j=1)
  dat <- do.call(cbind, dat)
  
  assign(paste0("bs_centroid_",a), dat, envir = .GlobalEnv)
}

stopCluster(cl)

# Step 5: Reporting
graphics.off()
pdf("fig/matching_bloxplot_centroid.pdf", width = 7, height = 10)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(3,2), oma=c(5,5,1,1))

s <- .25
S <- .5
labcex <- 1.5
labpos <- "topright"
ylim <- c(-6,8)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_centroid_sum_art05, ylim=ylim)
legend(labpos, legend = "Art. 5", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_centroid_sum_art06, ylim=ylim)
legend(labpos, legend = "Art. 6", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_centroid_sum_art08, ylim=ylim)
legend(labpos, legend = "Art. 8", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_centroid_sum_art11, ylim=ylim)
legend(labpos, legend = "Art. 11", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_centroid_sum_art13, ylim=ylim)
legend(labpos, legend = "Art. 13", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mfrow=c(1,1))
mtext(side = 1, text = "Exposure", outer = TRUE, line=2)
mtext(side = 2, text = "Feasible Average Treatment Effect", outer = TRUE, line=2)
par(oldpar)
dev.off()

# FCTC border network -------------------------------------------------------------

# Step 1: set and adjust the graph
graph <- adjmat_border # 
graph <- prepare_graph(graph, model_data, as_bool = FALSE)

# Step 2: Calibrate (define the right parameters)
# change: expo_pcent, expo_lag, method, distance
ans <- netmatch(
  # Actual data
  dat   = model_data,
  graph = graph,
  # Variable names
  timevar    = "year",
  depvar     = "sum_art05",
  covariates = common_covars,
  # Preprocessing parameters
  treat_thr  = .6,
  expo_pcent = TRUE,
  expo_lag   = 1,
  # Matching parameters
  method   = "cem" # Coarsened Exact Matching
)

ans
threshold_levels <- c(.4,.5,.6)

# Step 3: Setting up parallel
cl <- makeCluster(2)
clusterExport(cl, c("model_data2010", "model_data2012", "common_covars", "nboot"))
clusterEvalQ(cl, library(netdiffuseR))

# Step 4: Computing the standard errors for each level
for (a in articles) {
  
  ans <- NULL
  for (i in threshold_levels) {
    suppressMessages({
      ans[[as.character(i)]] <- tryCatch({
        booted_nnm(i, 1L, cl, nreps = nboot, art=a)
      }, error=function(e) e)
    })
    message("Threshold level: ", i, " article:", a," done.")
  }
  
  # Coercing data
  dat <- lapply(lapply(ans, "[[", "boot"), "[[", "t")
  dat <- lapply(dat, "[", i=1:nboot,j=1)
  dat <- do.call(cbind, dat)
  
  assign(paste0("bs_border_",a), dat, envir = .GlobalEnv)
}

stopCluster(cl)

# Step 5: Reporting
graphics.off()
pdf("fig/matching_bloxplot_border.pdf", width = 7, height = 10)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(3,2), oma=c(5,5,1,1))

s <- .25
S <- .5
labcex <- 1.5
labpos <- "topright"
ylim <- c(-6,8)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_border_sum_art05, ylim=ylim)
legend(labpos, legend = "Art. 5", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_border_sum_art06, ylim=ylim)
legend(labpos, legend = "Art. 6", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_border_sum_art08, ylim=ylim)
legend(labpos, legend = "Art. 8", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_border_sum_art11, ylim=ylim)
legend(labpos, legend = "Art. 11", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_border_sum_art13, ylim=ylim)
legend(labpos, legend = "Art. 13", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mfrow=c(1,1))
mtext(side = 1, text = "Exposure", outer = TRUE, line=2)
mtext(side = 2, text = "Feasible Average Treatment Effect", outer = TRUE, line=2)
par(oldpar)
dev.off()

# FCTC cop network -------------------------------------------------------------

# Step 1: set and adjust the graph
graph <- adjmat_fctc_cop_coparticipation_twomode # 
graph <- prepare_graph(graph, model_data, as_bool = FALSE)

# Step 2: Calibrate (define the right parameters)
# change: expo_pcent, expo_lag, method, distance
ans <- netmatch(
  # Actual data
  dat   = model_data,
  graph = graph,
  # Variable names
  timevar    = "year",
  depvar     = "sum_art05",
  covariates = common_covars,
  # Preprocessing parameters
  treat_thr  = .8,
  expo_pcent = TRUE,
  expo_lag   = 1,
  # Matching parameters
  method   = "cem" # Coarsened Exact Matching
)

ans
threshold_levels <- c(.6,.7,.8)

# Step 3: Setting up parallel
cl <- makeCluster(2)
clusterExport(cl, c("model_data2010", "model_data2012", "common_covars", "nboot"))
clusterEvalQ(cl, library(netdiffuseR))

# Step 4: Computing the standard errors for each level
for (a in articles) {
  
  ans <- NULL
  for (i in threshold_levels) {
    suppressMessages({
      ans[[as.character(i)]] <- tryCatch({
        booted_nnm(i, 1L, cl, nreps = nboot, art=a)
      }, error=function(e) e)
    })
    message("Threshold level: ", i, " article:", a," done.")
  }
  
  # Coercing data
  dat <- lapply(lapply(ans, "[[", "boot"), "[[", "t")
  dat <- lapply(dat, "[", i=1:nboot,j=1)
  dat <- do.call(cbind, dat)
  
  assign(paste0("bs_cop_",a), dat, envir = .GlobalEnv)
}

stopCluster(cl)

# Step 5: Reporting
graphics.off()
pdf("fig/matching_bloxplot_fctc_cop.pdf", width = 7, height = 10)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(3,2), oma=c(5,5,1,1))

s <- .25
S <- .5
labcex <- 1.5
labpos <- "topright"
ylim <- c(-6,8)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_cop_sum_art05, ylim=ylim)
legend(labpos, legend = "Art. 5", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_cop_sum_art06, ylim=ylim)
legend(labpos, legend = "Art. 6", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_cop_sum_art08, ylim=ylim)
legend(labpos, legend = "Art. 8", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_cop_sum_art11, ylim=ylim)
legend(labpos, legend = "Art. 11", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_cop_sum_art13, ylim=ylim)
legend(labpos, legend = "Art. 13", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mfrow=c(1,1))
mtext(side = 1, text = "Exposure", outer = TRUE, line=2)
mtext(side = 2, text = "Feasible Average Treatment Effect", outer = TRUE, line=2)
par(oldpar)
dev.off()

# FCTC inb network -------------------------------------------------------------

# Step 1: set and adjust the graph
graph <- adjmat_fctc_inb_coparticipation_twomode # 
graph <- prepare_graph(graph, model_data, as_bool = FALSE)

# Step 2: Calibrate (define the right parameters)
# change: expo_pcent, expo_lag, method, distance
ans <- netmatch(
  # Actual data
  dat   = model_data,
  graph = graph,
  # Variable names
  timevar    = "year",
  depvar     = "sum_art06",
  covariates = common_covars,
  # Preprocessing parameters
  treat_thr  = .6,
  expo_pcent = TRUE,
  expo_lag   = 1,
  # Matching parameters
  method   = "cem" # Coarsened Exact Matching
)

ans
threshold_levels <- c(.6,.7,.8)

# Step 3: Setting up parallel
cl <- makeCluster(2)
clusterExport(cl, c("model_data2010", "model_data2012", "common_covars", "nboot"))
clusterEvalQ(cl, library(netdiffuseR))

# Step 4: Computing the standard errors for each level
for (a in articles) {
  
  ans <- NULL
  for (i in threshold_levels) {
    suppressMessages({
      ans[[as.character(i)]] <- tryCatch({
        booted_nnm(i, 1L, cl, nreps = nboot, art=a)
      }, error=function(e) e)
    })
    message("Threshold level: ", i, " article:", a," done.")
  }
  
  # Coercing data
  dat <- lapply(lapply(ans, "[[", "boot"), "[[", "t")
  dat <- lapply(dat, "[", i=1:nboot,j=1)
  dat <- do.call(cbind, dat)
  
  assign(paste0("bs_inb_",a), dat, envir = .GlobalEnv)
}

stopCluster(cl)

# Step 5: Reporting
graphics.off()
pdf("fig/matching_bloxplot_inb.pdf", width = 7, height = 10)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(3,2), oma=c(5,5,1,1))

s <- .25
S <- .5
labcex <- 1.5
labpos <- "topright"
ylim <- c(-6,8)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_inb_sum_art05, ylim=ylim)
legend(labpos, legend = "Art. 5", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_inb_sum_art06, ylim=ylim)
legend(labpos, legend = "Art. 6", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_inb_sum_art08, ylim=ylim)
legend(labpos, legend = "Art. 8", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,s,s,S))
boxplot(bs_inb_sum_art11, ylim=ylim)
legend(labpos, legend = "Art. 11", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mai=oldpar$mai*c(s,S,s,s))
boxplot(bs_inb_sum_art13, ylim=ylim)
legend(labpos, legend = "Art. 13", bty = "n", cex=labcex)
abline(h=0, lty=2)

par(mfrow=c(1,1))
mtext(side = 1, text = "Exposure", outer = TRUE, line=2)
mtext(side = 2, text = "Feasible Average Treatment Effect", outer = TRUE, line=2)
par(oldpar)
dev.off()



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
