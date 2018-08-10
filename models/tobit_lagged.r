# DESCRIPTION:
# This file runs linear models on then proportion of implementation using the
# co-subscription networks.

# Preparing space
options(stringsAsFactors = FALSE)
rm(list=ls())

library(netdiffuseR)
library(texreg)
library(Matrix)
library(AER)
library(magrittr)
library(dplyr)

model_data <- readr::read_csv("data/multiple-imputation1.csv")

# Year fixed effects: 2010 as reference
year0_1           <- model.matrix(~0+factor(year), model_data)
colnames(year0_1) <- gsub(".+([0-9]{4})$", "Year \\1", colnames(year0_1))
model_data        <- cbind(model_data, year0_1[,-1]) 

load("data/adjmats.rda")
load("data/adjmat_border.rda")
load("data/adjmat_mindist.rda")
load("data/adjmat_centroid_dist.rda")

# Function to create formulas
makeformula <- function(x) {
  paste(paste(x, collapse=" + "))
}

common_covars <- c(
  # "`Eastern Mediterranean`",
  "Americas",
  "African",
  "European",
  "`Western Pacific`",
  "`South-East Asia`",
  # "democracy",
  "ctrl_corrup",
  "rule_of_law",
  "gdp_percapita_ppp",
  "`Years since Ratif.`",
  "tobac_prod_pp",
  "smoke_female",
  "smoke_male",
  "labor",
  "womens_rights",
  "population",
  "govtown"
  )

which(!(gsub("`", "", common_covars) %in% colnames(model_data)))
                   
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13", "sum_art14")

# List of networks (with pretty names) that will be used
networks      <- c(
  "adjmat_centroid_dist", "Centroid Distance",
  "adjmat_mindist", "Minimal distance",
  "adjmat_general_trade", "General Trade",
  "adjmat_tobacco_trade", "Tobacco Trade",
  "adjmat_referrals", "GLOBALink Referrals",
  "adjmat_interest_group_comembership_twomode", "GLOBALink co-subscription"#,"Interest Group co-membership",
  # "adjmat_fctc_cop_coparticipation_twomode", "FCTC COP co-participation",
  # "adjmat_fctc_inb_coparticipation_twomode", "FCTC INB co-participation",
  # "adjmat_border", "Country Borders",
  # "adjmat_gl_posts", "GlobalLink Posts"
  )

# Listing models
# Models are lists that hold the following values:
# - vars: List of the variables that will be used as predictors.
# - filter (optional): A function that will be applied prior to run the model.
# - about (optional): A brief description
models <- list(
  Baseline              = list(
    vars = c("rho", common_covars),
    about  = "This is the baseline specification."),
  Imp2010               = list(
    vars = c("rho", "y_lagged", common_covars),
    about  = "This specification includes the number of items reported on 2010."),
  Imp2010_report        = list(
    vars   = c("rho", "y_lagged", common_covars),
    filter = function(x) {
      subset(x, no_report == 0L)
    },
    about  = "This specification includes the number of items reported on 2010. Also, it only includes members that provided a reported on 2012."),
  Imp2010_dummy_report  = list(
    vars   = c("rho", "y_lagged", "no_report", common_covars),
    about  = "This specification includes a dummy equal to 1 when the member did not provided a report on 2012."),
  PolShift              = list(
    vars   = c("rho", "pol_shift", common_covars),
    about  = "This specification includes a variable capturing political shifts."),
  Bloomberg_amount      = list(
    vars   = c("rho", "bloomberg_amount_pp", common_covars),
    about  = "This specification includes 'percapita amount of Bloomberg funds'."),
  Bloomberg_count       = list(
    vars   = c("rho", "bloomberg_count", common_covars),
    about  = "This specification includes 'Number of Bloomberg project'."),
  Bloomberg_amount_fctc = list(
    vars   = c("rho", "bloomberg_fctc_amount_pp", common_covars),
    about  = "This specification includes 'percapita amount of FCTC Bloomberg funds'."),
  Bloomberg_count_fctc  = list(
    vars   = c("rho", "bloomberg_fctc_count", common_covars),
    about  = "This specification includes 'Number of FCTC Bloomberg project'.")
)


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

adjmat_interest_group_comembership_twomode <-
  adjmat_interest_group_comembership_twomode$`2010`

image(adjmat_referrals)
nlinks(adjmat_referrals)/(nnodes(adjmat_referrals)*(nnodes(adjmat_referrals)-1))

# If i referred j, then i has an influence over j, hence we transpose to compute
# exposures.
adjmat_referrals <- t(adjmat_referrals)
networks      <- matrix(networks, byrow = TRUE, ncol=2)
asterisks     <- c(.05, .01, .001)

# We will only work with static networs, this is, we will exclude the
# bilateral investment network
rm(adjmat_bilateral_investment_treaties)

# The general model is
# Y = rho W Y + X B + eps, where 
# Y: number of articles implemented
# X: party attributes

# Creating the lagged exposure -------------------------------------------------
for (art in articles) {
  ids1 <- sort(which(model_data$year == 2012))
  ids0 <- sort(which(model_data$year == 2010))
  
  model_data[[sprintf("%s_lagged", art)]] <- NA
  model_data[ids1,sprintf("%s_lagged", art)] <- model_data[ids0,][[art]]
  
  # We assume that, if missing the lagged value is zero
  model_data[ids1,sprintf("%s_lagged", art)][
    is.na(model_data[ids1,sprintf("%s_lagged", art)])
  ] <- 0
}

# Dropping and sorting again (just in case)
model_data <- subset(model_data, year == 2012)
model_data <- model_data[order(model_data$entry),]

# Distance network -------------------------------------------------------------
SIGNIFICANCE_MODEL1 <- NULL
for (Wnum in 1:nrow(networks)) {
  Wname <- networks[Wnum, 1]
  W <- get(Wname) #adjmat_tobacco_trade # adjmat_general_trade #adjmat_distance_static
  
  # Filtering data: The network must be accomodated to the observed data
  ids <- sort(unique(model_data$entry))
  
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
  

  # Row normalization
  W <- W/rowSums(W)
  W@x[is.nan(W@x)] <- 0
  W <- methods::as(W, "dgCMatrix")
  
  # This table will store the values (and add asterisks at the end) on the values
  # of rho and the sifnificance level.
  rho_asterisk_table <- NULL
  rho_per_article    <- vector("numeric", length(articles))
  names(rho_per_article) <- articles
  
  # Looping through the models
  for (m in seq_along(models)) {
    
    for (art in articles) {
      
      # Name of the lagged variable
      art_lagged <- sprintf("%s_lagged", art)
      
      # Generate the exposure data and lagged outcome
      model_data[["rho"]]      <- as.matrix(W %*% model_data[[art_lagged]])
      model_data[["y_lagged"]] <- model_data[[art_lagged]]

      # Write down the model
      mod <- as.formula(paste0(art, " ~ ", makeformula(models[[m]]$vars)))
      
      # Filtering the data (if needed)
      if (length(models[[m]]$filter))
        tmpdata <- models[[m]]$filter(model_data)
      else tmpdata <- model_data
      
      # Run the model
      ans <- tryCatch(AER::tobit(mod, data=tmpdata), error=function(e) e)
    
      # Did it run?  
      if (inherits(ans, "error")) {
        message("!!! Error in network ", Wname, " article ", art, " model.")
        next
      }
      
      # Creating the object
      assign(paste("tobit_lagged",art, m, sep="_"), ans, envir = .GlobalEnv)
      message("Network ", Wname, " article ", art, " model ", m,  " done.")
    }
    
  }
  
  # Saving results -----------------------------------------------------------
  save(Wname, list = ls(pattern = "tobit_lagged_sum.+[0-9]$"), file = sprintf("models/tobit_lagged_%s.rda", Wname))
  
  message("Network ", Wname, " done.")
}

# Saving the models specifications
save(models, networks, articles,
     file = sprintf("models/tobit_lagged_specifications.rda"))

