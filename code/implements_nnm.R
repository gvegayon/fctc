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

# Sorting the data appropiately
model_data <- model_data[with(model_data, order(year, entry)),]

common_covars <- c("Asia", "Europe", "Africa", "America", "democracy", "GDP_pp", "tobac_prod_pp",
                   "perc_female_smoke", "perc_male_smoke")
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")

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


# Since is panel data, need to split by year and merge it inside
# the boot function
model_data <- model_data[with(model_data, order(year, entry)),]
model_data2010 <- subset(model_data, year==2010)
model_data2012 <- subset(model_data, year==2012)

# FCTC cop network -------------------------------------------------------------

# Step 1: set and adjust the graph
graph <- adjmat_fctc_cop_coparticipation_twomode
graph <- prepare_graph(graph, model_data, as_bool = TRUE)

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
  treat_thr  = 0.6,
  expo_pcent = TRUE,
  expo_lag   = 1,
  # Matching parameters
  method   = "cem" # Coarsened Exact Matching
)

ans

# stop()
# Setting up parallel
library(parallel)
cl <- makeCluster(7)
clusterExport(cl, c("model_data2010", "model_data2012"))
clusterEvalQ(cl, library(netdiffuseR))

# Generating standard errors using bootstrapping
booted_nnm <- function(tt, expolag, cl, nreps) {
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
      depvar     = "sum_art05",
      covariates = c("democracy", "GDP_pp", "bloomberg_fctc_count",
                     "tobac_prod_pp", "perc_female_smoke", "perc_male_smoke"),
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
      fATT = mean(out$fATT, na.rm=TRUE) ,
      n1   = out$match_obj$nn["Matched","Treated"],
      n0   = out$match_obj$nn["Matched","Control"]
    )
  }, R = nreps, cl=cl, parallel="multicore", ncpus=length(cl), tt=tt, expolag=expolag)
}

threshold_levels <- c(.4,.5,.6)
expolag          <- c(0L, 1L)
ans <- NULL

for (i in threshold_levels)
  for (j in expolag) {
    suppressMessages({
      ans[[as.character(i)]][[as.character(j)]] <- tryCatch({
        booted_nnm(i, j, cl, 100)
      }, error=function(e) e)
    })
    message("Threshold level: ", i, ", and expolag:", j," done.")
  }
    

stopCluster(cl)


# Structural test --------------------------------------------------------------
stop()
# Setting up parallel
library(parallel)
cl <- makeCluster(7)
clusterExport(cl, "model_data")
clusterEvalQ(cl, library(netdiffuseR))

# Running the test
ans <- struct_test(graph, function(g) {
  out <- netmatch(model_data, g, "year", "sum_art05", 
                  c("democracy", "GDP_pp", "bloomberg_fctc_count",
                    "tobac_prod_pp", "perc_female_smoke", "perc_male_smoke"),
                  treat_thr = .4, expo_pcent = TRUE, expo_lag = 1L,
                  distance=""
  )
  
  mean(out)
}, R = 4000, cl=cl, parallel="multicore", ncpus=length(cl))

stopCluster(cl)
