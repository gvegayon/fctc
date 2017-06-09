# DESCRIPTION:
# This file runs linear models on then proportion of implementation using the
# co-subscription networks.

# Preparing space
options(stringsAsFactors = FALSE)
rm(list=ls())

library(netdiffuseR)
library(texreg)
library(Matrix)
library(spdep)
library(spatialprobit)

model_data <- read.csv("data/model_data.csv", na="<NA>", check.names = FALSE)
model_data <- model_data[with(model_data, order(year, entry)),]
model_data$`year2012` <- as.integer(model_data$year == 2012)
load("data/adjmats.rda")
load("data/adjmat_border.rda")
load("data/adjmat_mindist.rda")
load("data/adjmat_centroid_dist.rda")

# Function to create formulas
makeformula <- function(y, x) {
  as.formula(paste(y, paste(x, collapse=" + "), sep=" ~ "))
}

common_covars <- c("`Eastern Mediterranean`", "European", "African", "`Western Pacific`", "`South-East Asia`", #"Asia", "Europe", "Africa", "America",
                   "democracy", "GDP", "`Years since Sign.`", "`Years since Ratif.`",
                   "tobac_prod_pp", "perc_female_smoke", "perc_male_smoke",
                   "year2012", "labor", "womens_rights", "population", "govtown")
                   # "subscribed", "govtown")
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")

# List of networks (with pretty names) that will be used
networks      <- c(
  "adjmat_general_trade", "General Trade",
  "adjmat_tobacco_trade", "Tobacco Trade",
  "adjmat_mindist", "Minimal distance",
  "adjmat_centroid_dist", "Centroid Distance",
  "adjmat_border", "Country Borders",
  "adjmat_gl_posts", "GlobalLink Posts",
  "adjmat_referrals", "GlobalLink Referrals",
  "adjmat_fctc_cop_coparticipation_twomode", "FCTC COP co-participation",
  "adjmat_fctc_inb_coparticipation_twomode", "FCTC INB co-participation",
  "adjmat_interest_group_comembership_twomode", " Interest Group co-membership"
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
  adjmat_interest_group_comembership_twomode$`2011`

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
  W <- netdiffuseR::diag_expand(list(W,W), valued=TRUE) # This generates a square mat with struct zeros

  # Row normalization
  W <- W/rowSums(W)
  W@x[is.nan(W@x)] <- 0
  W <- methods::as(W, "dgCMatrix")
  
  # This table will store the values (and add asterisks at the end) on the values
  # of rho and the sifnificance level.
  rho_asterisk_table <- NULL
  rho_per_article    <- vector("numeric", length(articles))
  names(rho_per_article) <- articles
  
  # Model 1: Only characteristics
  
  for (art in articles) {
    # Creating and estimating model 
    # stop()
    mod <- makeformula(art, common_covars)

    ans <- sartobit(mod, W = W, data=model_data,
                    ndraw = 5e3, burn.in = 1e3)
    
    # Creating the object
    assign(paste("sar_tobit",art,1,sep="_"), ans, envir = .GlobalEnv)
    message("Network ", Wname, " article ", art, " model ", 1,  " done.")
  }
  
  
  
  # Model 2: We add political shifts
  ids <- complete.cases(model_data$pol_shift)
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "pol_shift"))
    
    ans <- sartobit(mod, W = W[ids,ids], data=model_data[ids,],
                    ndraw = 5e3, burn.in = 1e3)
    
    # Creating the object
    assign(paste("sar_tobit",art,2,sep="_"), ans, envir = .GlobalEnv)
    message("Network ", Wname, " article ", art, " model ", 2,  " done.")
  }
  
  # Model 3: Bloomberg amount
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_amount"))
    ans <- sartobit(mod, W = W, data=model_data,
                    ndraw = 5e3, burn.in = 1e3)
    
    # Creating the object
    assign(paste("sar_tobit",art,3,sep="_"), ans, envir = .GlobalEnv)
    message("Network ", Wname, " article ", art, " model ", 3,  " done.")
  }
  
  # Model 4: Bloomberg count
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_count"))
    ans <- sartobit(mod, W = W, data=model_data,
                    ndraw = 5e3, burn.in = 1e3)
    
    # Creating the object
    assign(paste("sar_tobit",art,4,sep="_"), ans, envir = .GlobalEnv)
    message("Network ", Wname, " article ", art, " model ", 4,  " done.")
  }
  
  # Model 5: Bloomberg amount
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_fctc_amount"))
    ans <- sartobit(mod, W = W, data=model_data,
                     ndraw = 5e3, burn.in = 1e3)
    
    # Creating the object
    assign(paste("sar_tobit",art,5,sep="_"), ans, envir = .GlobalEnv)
    message("Network ", Wname, " article ", art, " model ", 5,  " done.")
  }
  
  # Model 6: Bloomberg count fctc
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_fctc_count"))
    ans <- sartobit(mod, W = W, data=model_data,
                    ndraw = 5e3, burn.in = 1e3)
    
    # Creating the object
    assign(paste("sar_tobit",art,6,sep="_"), ans, envir = .GlobalEnv)
    message("Network ", Wname, " article ", art, " model ", 6,  " done.")
  }
  
  
  # Saving results -----------------------------------------------------------
  save(Wname, list = ls(pattern = "sar_tobit_sum.+[0-9]$"), file = sprintf("code/sar_tobit_%s.rda", Wname))
  
  message("Network ", Wname, " done.")
}

stop()
# Neat table with all Rho ------------------------------------------------------

rownames(SIGNIFICANCE_MODEL1) <- networks[,2]

dat <- SIGNIFICANCE_MODEL1
dat[] <- paste(
  sprintf("%0.2f", dat),
  ifelse(dat[] <= asterisks[3], "***",
         ifelse(dat[] <= asterisks[2], "**",
                ifelse(dat <= asterisks[1], "*", " ")))
)

# Column/row names and alignment
colnames(dat) <- gsub(".+art0?", "Art. ",colnames(dat))

dat <- xtable::xtable(dat)

xtable::align(dat) <- rep("l", ncol(dat) + 1)
xtable::caption(dat) <- paste0(
  "Significance level of $\\rho$ per network/article in terms of p-values of the SAR models.", 
  "*** $p < ", asterisks[3],"$, ",
  "** $p < ", asterisks[2],"$, and ",
  "* $p < ", asterisks[1],"$."
)

print(dat, file="fig/sar_rho_summary.tex", booktabs=TRUE)
