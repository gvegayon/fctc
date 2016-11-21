# DESCRIPTION:
# This file runs linear models on then proportion of implementation using the
# co-subscription networks.

# Preparing space
rm(list=ls())
library(netdiffuseR)

load('fctc/data/diffnets.rda')

nrw <- rep(0,10)
nrw[9] <- nlinks(dn_general_trade)[[9]]*20
structural_test_art08 <- struct_test(dn_general_trade, function(x) {
  # Observing in graph 9
  g <- x$graph[[9]]
  v <- matrix(x[['mean_art08']][[9]], ncol=1)
  colnames(v) <- 'm08'
  v[is.na(v)] <- 0
  
  # Average distance
  v <- egonet_attrs(g, v, self.attrs = TRUE)
  mean(sapply(v, function(x) {
    if (nrow(x)==1) return(NA)
    mean(abs(x[1,'m08'] - x[-1,'m08']))
  }), na.rm=TRUE)
}, R = 1000, rewire.args=list(p=nrw, copy.first=FALSE, algorithm='swap'),
ncpus=4, parallel='multicore')

# > structural_test_art08
# Structure dependence test
# # Simulations     : 1,000
# # nodes           : 191
# # of time periods : 10
# --------------------------------------------------------------------------------
#   H0: t - t0 = 0 (no structure dependency)
#      t0 (observed) = 0.5045108
#      t (simulated) = 0.5283805
#      p-value = 0.00000 # No random distance (so there's evidence of contagion)

save(structural_test_art08, file='fctc/data/20160626_struct_test_art_5-6-8-11-13.rda')
