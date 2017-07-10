# DESCRIPTION:
# This file runs linear models on then proportion of implementation using the
# co-subscription networks.

# Preparing space
rm(list=ls())
library(netdiffuseR)

load('fctc/data/diffnets.rda')

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
