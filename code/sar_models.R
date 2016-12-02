# DESCRIPTION:
# This file runs linear models on then proportion of implementation using the
# co-subscription networks.

# Preparing space
options(stringsAsFactors = FALSE)
rm(list=ls())

library(texreg)
library(Matrix)
library(spdep)

model_data <- read.csv("data/model_data.csv", na="<NA>")
model_data <- model_data[with(model_data, order(year, entry)),]
load("data/adjmats.rda")
load("data/adjmat_border.rda")

# Function to create formulas
makeformula <- function(y, x) {
  as.formula(paste(y, paste(x, collapse=" + "), sep=" ~ "))
}

common_covars <- c("factor(continent)", "democracy", "GDP_pp*tobac_prod_pp",
                   "perc_female_smoke", "perc_male_smoke")
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")

# We will only work with static networs, this is, we will exclude the
# bilateral investment network
rm(adjmat_bilateral_investment_treaties)

# The general model is
# Y = rho W Y + X B + eps, where 
# Y: number of articles implemented
# X: party attributes

# Distance network -------------------------------------------------------------
for (Wname in c("adjmat_tobacco_trade", "adjmat_general_trade", "adjmat_distance_static", "adjmat_border")) {
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
  W <- netdiffuseR::diag_expand(list(W,W)) # This generates a square mat with struct zeros
  
  # Model 1: Only characteristics
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, common_covars)
    ans <- lagsarlm(mod, data = model_data, listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,1,sep="_"), ans, envir = .GlobalEnv)
  }
  
  
  # Model 2: We add political shifts
  ids <- complete.cases(model_data$pol_shift)
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "pol_shift"))
    ans <- lagsarlm(mod, data = model_data[ids,],
                    listw = mat2listw(W[ids,ids], style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,2,sep="_"), ans, envir = .GlobalEnv)
  }
  
  # Model 3: Bloomberg amount
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_amount"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,3,sep="_"), ans, envir = .GlobalEnv)
  }
  
  # Model 4: Bloomberg count
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_count"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,4,sep="_"), ans, envir = .GlobalEnv)
  }
  
  # Model 5: Bloomberg amount
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_fctc_amount"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,5,sep="_"), ans, envir = .GlobalEnv)
  }
  
  # Model 6: Bloomberg count fctc
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_fctc_count"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,6,sep="_"), ans, envir = .GlobalEnv)
  }
  
  # Tabulating results -----------------------------------------------------------
  
  Wname_fancy <- gsub("adjmat_", "", Wname)
  Wname_fancy <- stringr::str_replace_all(Wname_fancy, "_", " ")
  
  sar_art05 <- lapply(ls(pattern = "sum_art05"), get)
  texreg(sar_art05, file = sprintf("fig/sar_art05_%s.tex", Wname), 
         caption = sprintf("SAR on Number of Items of Art. 5 (%s)", Wname_fancy),
         booktabs=TRUE, use.packages = FALSE,
         float.pos = "!h", omit.coef = "factor")
  
  sar_art06 <- lapply(ls(pattern = "sum_art06"), get)
  texreg(sar_art06, file = sprintf("fig/sar_art06_%s.tex", Wname),   
         caption = sprintf("SAR on Number of Items of Art. 6 (%s)",Wname_fancy),
         booktabs=TRUE, use.packages = FALSE,
         float.pos = "!h", omit.coef = "factor")
  
  sar_art08 <- lapply(ls(pattern = "sum_art08"), get)
  texreg(sar_art08, file = sprintf("fig/sar_art08_%s.tex", Wname),  
         caption = sprintf("SAR on Number of Items of Art. 8 (%s)", Wname_fancy),
         booktabs=TRUE, use.packages = FALSE,
         float.pos = "!h", omit.coef = "factor")
  
  sar_art11 <- lapply(ls(pattern = "sum_art11"), get)
  texreg(sar_art11, file = sprintf("fig/sar_art11_%s.tex", Wname), 
         caption = sprintf("SAR on Number of Items of Art. 11 (%s)", Wname_fancy),
         booktabs=TRUE, use.packages = FALSE,
         float.pos = "!h", omit.coef = "factor")
  
  sar_art13 <- lapply(ls(pattern = "sum_art13"), get)
  texreg(sar_art13, file = sprintf("fig/sar_art13_%s.tex", Wname),
         caption = sprintf("SAR on Number of Items of Art. 13 (%s)", Wname_fancy),
         booktabs=TRUE, use.packages = FALSE,
         float.pos = "!h", omit.coef = "factor")
  
  message("Network ", Wname, " done.")
}

