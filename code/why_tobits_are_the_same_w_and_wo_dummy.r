# Are these equal

library(AER)

rm(list = ls())

load("models/tobit_lagged_adjmat_centroid_dist.rda")
load("models/tobit_lagged_specifications.rda")

# Model 
tosee <- which(names(models) %in% c("Imp2010_report", "Imp2010_dummy_report"))

Imp2010_report       <- get(paste0("tobit_lagged_sum_art11_", tosee[1]))
Imp2010_dummy_report <- get(paste0("tobit_lagged_sum_art11_", tosee[2]))

summary(Imp2010_report)
summary(Imp2010_dummy_report)

# Simulation -------------------------------------------------------------------

# Seeds and parameters
set.seed(1123)
N <- 1e3
K <- 4

# Data Generating Proccess
a     <- 5
b     <- cbind(runif(K, -1,1))
X     <- matrix(rnorm(N*K), ncol = K)
y        <- a + X %*% b + rnorm(N)
y[y < 0] <- 0

# The zero dummy
zero_dummy <- cbind(runif(N)>.9) # 10% of non-reporting
y[zero_dummy] <- 0

# Running the specifications
summary(tobit(y~ X, subset = !zero_dummy))
summary(tobit(y~ X + zero_dummy))

# Another simulation
set.seed(1123)

N <- 1e3
K <- 4

a     <- 5
b     <- cbind(runif(K, -1,1))
X     <- matrix(rnorm(N*K), ncol = K)
zero_dummy <- cbind(runif(N)>.9) # 10% of non-reporting

y        <- a + X %*% b + rnorm(N) > 0
y[zero_dummy] <- 0


summary(glm(y~ X + zero_dummy, family = binomial()))
summary(glm(y~ X, subset = !zero_dummy, family = binomial()))