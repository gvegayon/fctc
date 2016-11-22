mykern <- function(dat, kern, bw=.1) {
  function(x) {
  denom <- sum(kern(abs(x - dat$x)/bw))
  
  return(
    sum(dat$y*kern(abs(x - dat$x)/bw))/denom
  )}
}

set.seed(123)
X <- rnorm(100)
w <- .5
bw <- .2
Y <- as.integer(X*w + rnorm(100)*(1-w) > 0)
dat <- data.frame(x=X,y=Y)

test <- seq(-2,2, length.out = 100)

mk <- mykern(dat, dnorm, bw)

oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,3))
plot(x=test,y=sapply(test, mk), type="l",
     ylim = c(0,1), xlab="X", ylab="Expected Y",
     main=sprintf("Kernel regression with\nw=%5.2f and bw=%5.2f", w, bw))
points(x=X,y=sapply(X, mk), pch=22,
       col=c("blue","red")[Y+1])

plot(x=jitter(Y), y=sapply(X, mk),
     col=c("blue","red")[Y+1], xlab="Observed Y (+ noise)",
     ylab="Expected Y")


plot(ksmooth(X,Y, bandwidth = bw),
     col=c("blue","red")[Y+1], xlab="Observed Y (+ noise)",
     ylab="Expected Y")


par(oldpar)
