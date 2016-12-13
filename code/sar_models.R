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
model_data$`year2012` <- as.integer(model_data$year == 2012)
load("data/adjmats.rda")
load("data/adjmat_border.rda")
load("data/adjmat_mindist.rda")
load("data/adjmat_centroid_dist.rda")

# Function to create formulas
makeformula <- function(y, x) {
  as.formula(paste(y, paste(x, collapse=" + "), sep=" ~ "))
}

common_covars <- c("Asia", "Europe", "Africa", "America", "democracy", "GDP_pp",
                   "tobac_prod_pp", "perc_female_smoke", "perc_male_smoke",
                   "year2012")
articles      <- c("sum_art05", "sum_art06", "sum_art08", "sum_art11", "sum_art13")

# List of networks (with pretty names) that will be used
networks      <- c(
  "adjmat_general_trade", "General Trade",
  "adjmat_tobacco_trade", "Tobacco Trade",
  "adjmat_mindist", "Minimal distance",
  "adjmat_centroid_dist", "Centroid Distance",
  "adjmat_border", "Country Borders"
  )

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

  # This table will store the values (and add asterisks at the end) on the values
  # of rho and the sifnificance level.
  rho_asterisk_table <- NULL
  rho_per_article    <- vector("numeric", length(articles))
  names(rho_per_article) <- articles
  
  # Model 1: Only characteristics
  
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, common_covars)
    ans <- lagsarlm(mod, data = model_data, listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,1,sep="_"), ans, envir = .GlobalEnv)
    rho_per_article[art] <- summary(ans)$LR1$p.value[1]
  }
  rho_asterisk_table <- cbind(rho_asterisk_table, rho_per_article)
  
  
  
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
    rho_per_article[art] <- summary(ans)$LR1$p.value[1]
  }
  rho_asterisk_table <- cbind(rho_asterisk_table, rho_per_article)
  
  # Model 3: Bloomberg amount
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_amount"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,3,sep="_"), ans, envir = .GlobalEnv)
    rho_per_article[art] <- summary(ans)$LR1$p.value[1]
  }
  rho_asterisk_table <- cbind(rho_asterisk_table, rho_per_article)
  
  # Model 4: Bloomberg count
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_count"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,4,sep="_"), ans, envir = .GlobalEnv)
    rho_per_article[art] <- summary(ans)$LR1$p.value[1]
  }
  rho_asterisk_table <- cbind(rho_asterisk_table, rho_per_article)
  
  # Model 5: Bloomberg amount
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_fctc_amount"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,5,sep="_"), ans, envir = .GlobalEnv)
    rho_per_article[art] <- summary(ans)$LR1$p.value[1]
  }
  rho_asterisk_table <- cbind(rho_asterisk_table, rho_per_article)
  
  # Model 6: Bloomberg count fctc
  for (art in articles) {
    # Creating and estimating model 
    mod <- makeformula(art, c(common_covars, "bloomberg_fctc_count"))
    ans <- lagsarlm(mod, data = model_data,
                    listw = mat2listw(W, style="W"), 
                    zero.policy = TRUE)
    
    # Creating the object
    assign(paste("sar",art,6,sep="_"), ans, envir = .GlobalEnv)
    rho_per_article[art] <- summary(ans)$LR1$p.value[1]
  }
  rho_asterisk_table <- cbind(rho_asterisk_table, rho_per_article)
  
  # Tabulating results -----------------------------------------------------------
  
  Wname_fancy <- networks[Wnum,2]
  
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
  
  # Fancy, altogether table: rho + significance, bloomberg count
  # Should be Article
  tmp <- rho_asterisk_table
  tmp[] <- ifelse(tmp[] <= asterisks[3], "***",
                  ifelse(tmp[] <= asterisks[2], "**",
                         ifelse(tmp <= asterisks[1], "*", " ")))
  
  # Column/row names and alignment
  rownames(tmp) <- gsub(".+art0?", "Art. ",rownames(tmp))
  colnames(tmp) <- paste("Model", 1:ncol(tmp))
  tmp <- xtable::xtable(tmp)
  
  xtable::align(tmp) <- rep("c", ncol(tmp) + 1)
  xtable::caption(tmp) <- paste0(
    "Significance level of $\\rho$ per article/model (",
    Wname_fancy," network). ", 
    "*** $p < ", asterisks[3],"$, ",
    "** $p < ", asterisks[3],"$, and ",
    "* $p < ", asterisks[3],"$."
  )
  
  print(tmp, file=sprintf("fig/sar_%s_summary.tex", Wname), booktabs=TRUE,
        hline.after=-1:nrow(tmp))
  # fetch_values <- function(x) {
  #   data.frame(
  #   `\\rho`                 = coef(x)["rho"],
  #   `\\rho pval`            = summary(x)$LR1$p.value[1],
  #   `Bloomberg FCTC count`  = coef(x)["bloomberg_fctc_count"],
  #   `Bloomberg pval`        = summary(x)$Coef["bloomberg_fctc_count","Pr(>|z|)"],
  #   check.names = FALSE
  #   )
  # }
  # 

  # stop()
  message("Network ", Wname, " done.")
}

