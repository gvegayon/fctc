rm(list=ls())
library(parallel)
library(Matrix)
library(sphet) # spreg
library(splm) # spgm
library(plm)  # pdata.frame

# Function to generate a vector y in a panel fashion
gen_sar_panel <- function(n, P, rho, lambda, W) {
  y    <- cbind(rnorm(n))
  WI   <- solve(diag(n) - rho*W)
  ones <- cbind(rep(.2, n))

  # Response variable
  for (p in 1:P) {
    y <- rbind(y, WI %*% (
      # Spatio-temporal lag
      lambda * W %*% tail(y, n) + cbind(rnorm(n)) + ones
      ))
  }
  
  # Generating the lagged version, including the orthogonal projection
  Y <- matrix(ncol=5, nrow=n*(P - 1)) 
  for (p in 1:(P - 1)) {
    
    # Orthogonal projection
    yt  <- y[(1:n) + n*p,,drop=FALSE]
    yt1 <- y[(1:n) + n*(p-1),,drop=FALSE]
    My  <- diag(n) - W %*% yt %*% solve(t(W %*% yt) %*% W %*% yt) %*% t(W %*% yt)
    
    # Putting all together
    Y[(1:n) + (p - 1)*n, 1] <- yt
    Y[(1:n) + (p - 1)*n, 2] <- yt1
    Y[(1:n) + (p - 1)*n, 3] <- My %*% yt1
    Y[(1:n) + (p - 1)*n, 4] <- 1:n
    Y[(1:n) + (p - 1)*n, 5] <- rep(p, n)
  }
  
  colnames(Y) <- c("y", "ylag", "ylag_adj", "id", "period")
  
  
  return(as.data.frame(Y))
}

# Function to generate a spatial weights matrix (network)
sim_net <- function(n, P) {
  # Random graph
  W  <- as.matrix(netdiffuseR::rgraph_ws(n, 8, .4))
  W/(1e-20 + rowSums(W))

}

# Function to simulate the data and run the estimations
experiment <- function(n, P, rho, lambda) {
  # Simulating the data --------------------------------------------------------
  W   <- sim_net(n, P)
  dat <- gen_sar_panel(n = n, P = P, rho = rho, lambda = lambda, W = W)
  
  WW <- as.matrix(kronecker(Diagonal(P - 1L), W))
  
  # Running the models ---------------------------------------------------------
  
  # Omitted lag
  ans_omitted <- summary(
    lagsarlm(y ~ 1, data = dat, listw = mat2listw(WW))
  )[c("Coef", "rho", "LR1")]
  
  # Lag
  ans_lag     <- summary(
    lagsarlm(y ~ 1 + I(WW %*% ylag), data = dat, listw = mat2listw(WW))
  )[c("Coef", "rho", "LR1")]
  
  # Adjusted lag
  ans_lagadj     <- summary(
    lagsarlm(y ~ 1 + I(WW %*% ylag_adj), data = dat, listw = mat2listw(WW))
  )[c("Coef", "rho", "LR1")]
  
  # Creating output
  list(
    Omitted   = ans_omitted,
    Lagged    = ans_lag,
    LaggedAdj = ans_lagadj,
    dat       = dat,
    W         = W
  )
}


# Simulations
n    <- 200
P    <- 4
nsim <- 1000

set.seed(1231)

parameters <- cbind(
  rho    = runif(nsim, 0, .4),
  lambda = runif(nsim, 0, .4)
)

cl <- makeForkCluster(8)

# Running the simulations in parallel
ans <- parApply(cl, parameters, 1, function(x) experiment(n, P, x["rho"], x["lambda"]))
stopCluster(cl)

save(ans, experiment, parameters, n, P, nsim, sim_net, gen_sar_panel, file = "simulations.rda")

Omitted   <- lapply(ans, "[[", "Omitted")
Lagged    <- lapply(ans, "[[", "Lagged")
LaggedAdj <- lapply(ans, "[[", "LaggedAdj")

# Function to generate power surfaces
power_plot <- function(
  mat, parameter, mfrow,
  xlab = "",
  ylab = "",
  pch = 20, 
  col = adjustcolor(blues9[5], alpha.f = .8),
  main = "",
  legend.cex = 1.5,
  legend.bty = "n",
  ...) {
  
  labs <- colnames(mat)
  
  oldpar <- par(no.readonly = TRUE)
  par(mfrow = mfrow, mar = rep(0, 4), oma = c(rep(5, 3), 1), las = 2)
  for (i in 1:length(labs)) {
    
    # Plotting
    plot(x = parameter, y = mat[,i], col=col, pch = pch,
         ylab = "", xlab = "", main = "",
         xaxt = ifelse((i - 1) %/% mfrow[1], "s", "n"), 
         yaxt = ifelse(i %% mfrow[2], "s", "n"), 
         ...)
    
    # Adding a legend
    legend("topleft", box.col = "transparent", legend=labs[i], bg="white",
           cex = legend.cex, bty=legend.bty)
  }
  
  par(oldpar)
  title(main = main, xlab = xlab, ylab = ylab)
  
}

# Bias -------------------------------------------------------------------------

# Rho
ans_rho <- cbind(
  Ommited    = sapply(Omitted, "[[", "rho") - parameters[,"rho"],
  TimeLag    = sapply(Lagged, "[[", "rho") - parameters[,"rho"],
  TimeLagAdj = sapply(LaggedAdj, "[[", "rho") - parameters[,"rho"]
)

boxplot(ans_rho)
abline(h = 0, col = "gray", lwd = 4, lty = 2)

# Lambda
ans_lambda <- cbind(
  TimeLag = sapply(Lagged, function(x) x$Coef["I(WW %*% ylag)", "Estimate"]) - parameters[,"lambda"],
  TimeLagAdj = sapply(LaggedAdj, function(x) x$Coef["I(WW %*% ylag_adj)", "Estimate"]) - parameters[,"lambda"]
)

boxplot(ans_lambda)
abline(h = 0, col = "gray", lwd = 4, lty = 2)


# Power analysis ---------------------------------------------------------------



# Extracting the p-values
power_rho <- cbind(
  A = 1 - sapply(Omitted, function(x) unname(x$LR1$p.value[1])),
  B = 1 - sapply(Lagged, function(x) unname(x$LR1$p.value[1])),
  C = 1 - sapply(LaggedAdj, function(x) unname(x$LR1$p.value[1]))
)

# Plotting
power_plot(power_rho, parameters[,"rho"], mfrow=c(2,2), xlab="Rho",
           ylab = "Bias", legend.bty = "n", legend.cex = 2,
           main = "Bias size given"
           )

# Extracting the p-values
power_lambda <- cbind(
  TimeLag = 1 - sapply(Lagged, function(x) x$Coef["I(WW %*% ylag)","Pr(>|z|)"]),
  TimeLagAdj = 1 - sapply(LaggedAdj, function(x) x$Coef["I(WW %*% ylag_adj)","Pr(>|z|)"])
)

# Plotting
power_plot(power_lambda, parameters[,"lambda"], mfrow=c(1,2), xlab="lambda",
           pch=8, col=adjustcolor("blue",.4)
           )
