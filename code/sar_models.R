# DESCRIPTION:
# This file runs linear models on then proportion of implementation using the
# co-subscription networks.

# Preparing space
options(stringsAsFactors = FALSE)
rm(list=ls())

library(Matrix)
library(spdep)

model_data <- read.csv("data/model_data.csv", na="<NA>")
model_data <- model_data[with(model_data, order(year, entry)),]
load("data/adjmats.rda")

# We will only work with static networs, this is, we will exclude the
# bilateral investment network
rm(adjmat_bilateral_investment_treaties)

# The general model is
# Y = rho W Y + X B + eps, where 
# Y: number of articles implemented
# X: party attributes

# Distance network -------------------------------------------------------------
W <- adjmat_general_trade #adjmat_distance_static

# Filtering data: The network must be accomodated to the observed data
ids <- sort(unique(model_data$entry))
W <- W[ids,ids]
W <- netdiffuseR::diag_expand(list(W,W)) # This generates a square mat with struct zeros

# Model 1: 
sar_art05_1 <- lagsarlm(sum_art05 ~ 
                          factor(continent) +
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke,
                        data = model_data,
                        listw = mat2listw(W, style="W"), 
                        zero.policy = TRUE, quiet=FALSE)

sar_art08_1 <- lagsarlm(sum_art08 ~ 
                          factor(continent) +
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke,
                        data = model_data,
                        listw = mat2listw(W, style="W"), 
                        zero.policy = TRUE, quiet=FALSE)

# Model 2: We add political shifts
ids <- complete.cases(model_data$pol_shift)

sar_art05_2 <- lagsarlm(sum_art05 ~ 
                          factor(continent) +
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke + 
                          pol_shift,
                        data = model_data[ids,],
                        listw = mat2listw(W[ids,ids], style = "W"), 
                        zero.policy = TRUE)

sar_art08_2 <- lagsarlm(sum_art08 ~ 
                          factor(continent) +
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke + 
                          pol_shift,
                        data = model_data[ids,],
                        listw = mat2listw(W[ids,ids], style = "W"), 
                        zero.policy = TRUE)

# Model 3: We add bloomberg data (amount)
sar_art05_3 <- lagsarlm(sum_art05 ~ 
                          factor(continent) +
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke + 
                          I(bloomberg_amount/population),
                        data = model_data,
                        listw = mat2listw(W, style = "W"), 
                        zero.policy = TRUE)

sar_art08_3 <- lagsarlm(sum_art08 ~ 
                          factor(continent) +
                          factor(year) + 
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke + 
                          I(bloomberg_amount/population),
                        data = model_data,
                        listw = mat2listw(W, style="W"), 
                        zero.policy = TRUE)

# Model 4: We add bloomberg data (count)
sar_art05_4 <- lagsarlm(sum_art05 ~ 
                          factor(continent) +
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke + 
                          bloomberg_count,
                        data = model_data,
                        listw = mat2listw(W, style="W"), 
                        zero.policy = TRUE)

sar_art08_4 <- lagsarlm(sum_art08 ~ 
                          factor(continent) +
                          factor(year) + 
                          I(tobac_prod/population) + 
                          democracy + 
                          I(GDP/population) + 
                          perc_female_smoke + perc_male_smoke + 
                          bloomberg_count,
                        data = model_data,
                        listw = mat2listw(W, style="W"), 
                        zero.policy = TRUE)



# The model to run is to see if there's any relation (linear) on whether
# a implemented or not. To see this we generate the matrix

# The general specification will include:
#   - year,
#   - region
#   - GDP
#   - Tobacco prod
#   - population
#   - Change in government

dat <- diffnet.attrs(dn_general_trade, as.df = TRUE)

# Checking complete
dat <- subset(dat, select=c(per, toa, continent, tobac_prod, GDP, labor,
                            democracy, mean_art08, mean_art11, mean_art13,
                            population,
                            mean_art05, mean_art06))

# Leaving in zero
for (a in c('mean_art08', 'mean_art11', 'mean_art13', 'mean_art05', 'mean_art06'))
  dat[[a]][is.na(dat[[a]])] <- 0

# index <- complete.cases(dat)
# table(index)

# library(sphet)
library(spdep)
library(spatialprobit)
library(Matrix)
W <-  netdiffuseR:::diag_expand.diffnet(dn_general_trade)
W <- W/(rowSums(W) + 1e-150)
Wmat <- W
W <- mat2listw(W)

model_art08 <- lagsarlm(mean_art08~factor(toa)+
                     factor(continent)+GDP+I(tobac_prod/population)+democracy+
                     I(GDP/population),
                   dat=dat, listw = W, 
         zero.policy=TRUE)

model_art08_sartobit <- sartobit(mean_art08~factor(toa)+
                                   factor(continent)+GDP+I(tobac_prod/population)+democracy+
                                   I(GDP/population),
                                 W=Wmat, dat=dat, showProgress=TRUE)

model_art11 <- lagsarlm(mean_art11~factor(toa)+
                          factor(continent)+GDP+I(tobac_prod/population)+democracy+
                          I(GDP/population),
                        dat=dat, listw = W, 
                        zero.policy=TRUE)

model_art13 <- lagsarlm(mean_art13~factor(toa)+
                          factor(continent)+GDP+I(tobac_prod/population)+democracy+
                          I(GDP/population),
                        dat=dat, listw = W, 
                        zero.policy=TRUE)

model_art13_sartobit <- sartobit(mean_art13~factor(toa)+
                                   factor(continent)+GDP+I(tobac_prod/population)+democracy+
                                   I(GDP/population),
                                 W=Wmat, dat=dat, showProgress=TRUE)

model_art05 <- lagsarlm(mean_art05~factor(toa)+
                          factor(continent)+GDP+I(tobac_prod/population)+democracy+
                          I(GDP/population),
                        dat=dat, listw = W, 
                        zero.policy=TRUE)

model_art05_sartobit <- sartobit(mean_art05~factor(toa)+
                                   factor(continent)+GDP+I(tobac_prod/population)+democracy+
                                   I(GDP/population),
                                 W=Wmat, dat=dat, showProgress=TRUE)

model_art06 <- lagsarlm(mean_art06~factor(toa)+
                          factor(continent)+GDP+I(tobac_prod/population)+democracy+
                          I(GDP/population),
                        dat=dat, listw = W,
                        zero.policy=TRUE)

model_art06_sartobit <- sartobit(mean_art06~factor(toa)+
                                   factor(continent)+GDP+I(tobac_prod/population)+democracy+
                                   I(GDP/population),
                                 W=Wmat, dat=dat, showProgress=TRUE)

lapply(lapply(ls(pattern='model_'), get), summary)

# Function to fetch important info
fetch_important <- function(x, coefs=NULL) {
  s <- summary(x)
  s <- if (inherits(x, 'sarlm')) {
    if (!length(coefs)) coefs <- names(coef(x))
    rbind(s[['Coef']][,-3,drop=FALSE],
               cbind(
                 Estimate=s$rho,
                 `Std. Error`=s$rho.se,
                 `Pr(>|z|)`=s$LR1$p.value))
  } else {
    if (!length(coefs)) coefs <- rownames(s)
    as.matrix(s[,c(1,2,5)])
  }
  s[coefs,,drop=FALSE]
  
}


fetch_important(model_art06)

# Creating lots of tables
mod_names <- ls(pattern='model_')
objs <- lapply(mod_names, get)
objs <- lapply(objs, fetch_important)
names(objs) <- mod_names

# Writing the CSV file
fn <- 'fctc/data/20160714_sar_art_5-6-8-11-13.csv'
if (file.exists(fn)) file.remove(fn)
write.table('SAR models: rho is the autocorrelation coef.\nUses dn_general_trade network',
            fn)
for (i in names(objs)) {
  write.table(i, fn, append = TRUE, sep=',')
  write.table(objs[[i]], fn, append = TRUE, sep=',')
}
