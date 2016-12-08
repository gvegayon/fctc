rm(list=ls())
options(stringsAsFactors = FALSE)

# Packages
library(netdiffuseR)
library(Matrix)
library(MatchIt)

# Loading data
load("data/adjmats.rda")
load("data/adjmat_border.rda")
model_data <- read.csv("data/model_data.csv", na="<NA>")


#' @param dat A dataframe with dynamic attributes
#' @param graph a List with sparse matrices
#' @param timevar Character scalar. Name of the \code{timevar}
#' @param depvar Character scalar. Name of the dependent variable
netmatch_prepare <- function (
  dat, graph, timevar, depvar, covariates,
  treat_thr = rep(1L, length(graph)),
  adopt_thr = rep(1L, length(graph)),
  expo_pcent = FALSE,
  expo_lag = 0L
  ) {
  
  # Subset
  Y    <- as.matrix(dat[, depvar, drop=FALSE])
  X    <- as.matrix(dat[, covariates, drop=FALSE])
  Time <- as.matrix(dat[, timevar, drop=FALSE])
  
  # Checking thresholds
  if (length(treat_thr)==1)
    treat_thr <- rep(treat_thr, length(graph))
  if (length(adopt_thr)==1)
    adopt_thr <- rep(adopt_thr, length(graph))
  
  # Obtaining constants
  pers  <- sort(unique(Time))
  npers <- length(pers)
  n     <- length(Y)/npers

  if (expo_lag >= npers)
    stop("Not enought time points for such lag.")
  
  # Generating exposures and treatment
  expo  <- matrix(NA, ncol=npers, nrow=n)
  adopt <- matrix(NA, ncol=npers, nrow=n)
  treat <- matrix(NA, ncol=npers, nrow=n)
  ans   <- vector("list", npers)
  for (i in seq_along(pers)) {
    
    # Subset data
    per <- pers[i]
    y   <- Y[Time == per,, drop=FALSE]
    x   <- X[Time == per,, drop=FALSE]
    
    # Computing exposure and seting treatment
    adopt[,i] <- as.integer(y >= adopt_thr[i])
    
    # Skiping lags
    if (i <= expo_lag) next
    
    # Computing exposure (and normalizing if required)
    expo[,i]  <- as.vector(graph[[i-expo_lag]] %*% adopt[,i-expo_lag,drop=FALSE])
    if (expo_pcent) 
      expo[,i] <- expo[,i]/(Matrix::rowSums(graph[[i-expo_lag]]) + 1e-20)
    
    # Assigning regime  
    treat[,i] <- as.integer(expo[,i] >= treat_thr[i])
    
    # Creating dataset
    ans[[i]] <- data.frame(Y=y, X=x, treat=treat[,i],
                           adopt = adopt[,i-expo_lag],
                           expo  = expo[,i], check.names = FALSE)
    colnames(ans[[i]]) <-
      c("Y", covariates, "treat", "adopt", "expo")
    
  }
  
  # Model
  match_model <- as.formula(paste("treat ~", paste0(covariates, collapse=" + ")))
  
  # Returning outcome
  return(list(dat=ans, match_model=match_model))
}


model_data <- model_data[with(model_data, order(year, entry)),]
graph      <- adjmat_fctc_cop_coparticipation_twomode # adjmat_border # , adjmat_tobacco_trade

# Filtering data: The network must be accomodated to the observed data
ids <- sort(unique(model_data$entry))

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
graph <- graph[ids,ids]

# Matching the data
graph <- lapply(c(2010,2012), function(x) graph)

netmatch <- function(dat, graph, timevar, depvar, covars,
                     treat_thr, expo_pcent, expo_lag) {
  
  # Preparing the data
  ans   <- netmatch_prepare(dat, graph, timevar, depvar, covars,
                            treat_thr, expo_pcent, expo_lag)
  
  # print(str(ans[[1]]))
  # Processing the data so we can use it with -matchit-
  dat <- ans[[1]][[2]]
  rownames(dat) <- 1:nrow(dat)
  
  # Matching (with replacement)
  match_obj   <- matchit(ans$match_model, data=dat,
                         distance="mahalanobis", replace=TRUE)
  match_index <- match_obj$match.matrix
  
  # Checking balance
  summary(match_obj)
  
  # Computing feasible Average Treatment Effect on the Treated
  fATT            <- dat[,"Y"]
  matched_treated <- match(rownames(match_index), rownames(dat))
  matched_control <- match(match_index, rownames(dat))
  fATT            <- fATT[matched_treated] - fATT[matched_control]
  
  return(fATT)
  
}

ans <- netmatch(model_data, graph, "year", "sum_art05", 
         c("democracy", "GDP_pp", "bloomberg_fctc_count",
           "tobac_prod_pp", "perc_female_smoke", "perc_male_smoke"),
         treat_thr = .5, expo_pcent = TRUE, expo_lag   = 1L
         )

model_data <- model_data[with(model_data, order(year, entry)),]
model_data2010 <- subset(model_data, year==2010)
model_data2012 <- subset(model_data, year==2012)

dimnames(graph[[1]]) <- list(1:nnodes(graph), 1:nnodes(graph))
dimnames(graph[[2]]) <- list(1:nnodes(graph), 1:nnodes(graph))
ans <- bootnet(graph, function(g, ...) {
  ids <- as.integer(nodes(g))
  # print(summary(ids))
  dat <- rbind(model_data2010[ids,], model_data2012[ids,])
  out <- netmatch(dat, g, "year", "sum_art05", 
                  c("democracy", "GDP_pp", "bloomberg_fctc_count",
                    "tobac_prod_pp", "perc_female_smoke", "perc_male_smoke"),
                  treat_thr = .4, expo_pcent = TRUE, expo_lag   = 1L
  )
  
  mean(out)
}, R = 1000)

g2 <- resample_graph(graph[[1]], self = FALSE)

library(netdiffuseR)


# Checking effect
# summary(fATT)
