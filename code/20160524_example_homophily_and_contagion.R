#' ---
#' title: "Example of confounding homophily with diffusion"
#' author: "George G. Vega Yon"
#' date: "May 24, 2016"
#' fontsize: 10pt
#' ---
#' 
rm(list=ls())

library(Matrix) # To handle sparse matrices
library(spdep)  # To run SAR model
library(spatialprobit) # To run SAR probit

# Parameters for the simulation
set.seed(121)
n <- 5e2

#' # Data generating process
#' 
#' - The latent variable follows $L\sim N(0, 2)$
#' - The graph is homophily-based as $\Pr{(i\to j)} = 1 - \|L_i - L_j\|$
#' - The behavior is based only on $L$ (no diffusion): $\Pr{(y=1)} = \Phi(L)$

# Latent variable (could be 'interest on implementing policy')
L <- rnorm(n, sd = 2)

# Generating random graph
# i and j are connected with probability proportional to the euclidean
# distance, this is
# Pr( i -> j) ~ 1 - |L[i] - L[j]|
A <- matrix(0, ncol=n, nrow=n)
for (i in 1:n)
  for (j in 1:n)
    if ((i != j) && (1 - abs(L[i] - L[j])) > runif(1))
      A[i,j] <- 1

# Computing density and taking a look
sum(A)/(n*(n-1))
A[1:10,1:10]
    
# library(igraph)
# ig <- graph_from_adjacency_matrix(A)
# plot(ig, vertex.size=1, layout=layout_with_fr)

# Now generating behavior using the same variable.
# Pr(y=1) = Pr(L > 0) = Phi(L)
y <- as.integer(pnorm(L, sd=2) > runif(n))

#' \clearpage 
#' 
#' # Logit model
# Now we run a logit, ----------------------------------------------------------

# the weighting matrix W is in fact the exposure
# level to the adoption y.
W <- A/(rowSums(A) + 1e-15)
summary(glm(y ~ I(W %*% y), family=binomial()))

# Controling for Latent variable (which we can't)
summary(glm(y ~ I(W %*% y) + L, family=binomial()))

#' \clearpage 
#' 
#' # SAR-probit

# Now we run a SAR-probit ------------------------------------------------------

# Dropping 'missing'
i <- which(rowSums(W)>0)
y <- y[i]
L <- L[i]

W <- methods::as(W[i,][,i], "dgCMatrix")

if (n <= 5e2) {
  sar_probit <- sarprobit(y~1, W = W, showProgress=FALSE)
  summary(sar_probit)
  
  # Controling for Latent variable (which we can't)
  sar_probitL <- sarprobit(y~L, W = W, showProgress=FALSE)
  summary(sar_probitL)
}

#' \clearpage
#' 
#' # SAR

# Running a SAR model ----------------------------------------------------------

summary(lagsarlm(y~1, listw = mat2listw(W), zero.policy = TRUE))
# Controling for Latent variable (which we can't)
summary(lagsarlm(y~L, listw = mat2listw(W), zero.policy = TRUE))

