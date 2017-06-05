rm(list=ls())

library(netdiffuseR)
library(parallel)

set.seed(1231)
n <- 200
Y1 <- sample(c(0,1), n, TRUE)
X0 <- rep(c(1,1,20,20),n/4)
G <- rgraph_ba(t=n-1, self=FALSE, m=3, eta = X0)
G <- list(G,G)
Y2 <- Y1
Y2[Y2==0] <- sample(c(0,1), sum(Y2==0), TRUE)
X1 <- runif(n*2)
dat <- data.frame(Y=c(Y1, Y2), X0, X1, year=c(rep(1,n), rep(2,n)))
dat1 <- subset(dat, year==1)
dat2 <- subset(dat, year==2)


cl <- makeCluster(18)
clusterExport(cl, c("dat", "dat1", "dat2"))
clusterEvalQ(cl, library(netdiffuseR))

ans1 <- bootnet(G, function(g, idx, ...) {
  d <- rbind(dat1[idx,], dat2[idx,])
  netmatch(d, g, "year", "Y", "X0", "X1", treat_thr = 2, method="cem")$fATT
}, R=1e3, cl=cl, ncpus=length(cl), parallel="multicore")

ans2 <- struct_test(G, function(g) {
  netmatch(dat, g, "year", "Y", "X", treat_thr = 3, method="cem")$fATT
}, R=1e3, cl=cl, ncpus=length(cl), parallel="multicore")

stopCluster(cl)
