# Experimenting with the Jeffery's prior as described in
# PoolScreen2
# A software package for pool-screening analysis
# Charles R. Katholi, Ph.D. Department of Biostatistics School of Public Health University of Alabama at Birmingham ckatholi@uab.edu
# April 30, 2015


JP <- function(n,p){
  N <- length(p)
  out <- vector("numeric",N)
  for(i in 1:N){
    q <- 1-p[i]
    out[i] <- sqrt(sum((n^2 * q^(n-2))/(1 - q^n)))
  }
  out
}

JPLO <- function(n,x){
  N <- length(x)
  out <- vector("numeric",N)
  for(i in 1:N){
    y <- (1 + exp(x[i]))
    out[i] <- sqrt(sum((n^2 * y^2)/(y^n - 1)))
  }
  out
}


JPCLL <- function(n,x){
  N <- length(x)
  out <- vector("numeric",N)
  for(i in 1:N){
    y <- exp(-exp(x[i]))
    out[i] <- sqrt(sum((n^2 * y^(n-2))/(1-y^n)))
  }
  out
}


plot(function(p){JP(1:10,p)},from = 0 , to =1, n = 1e3+1)
plot(function(x){JPLO(1,x)},from = -20 , to = 20, n = 1e3+1)
plot(function(x){JPCLL(1,x)},from = -10 , to = 2.5, n = 1e3+1)

r <- 0.8
plot(function(x){dbeta(x, 0.1*r,0.4*r)},from = 0 , to =1, n = 1e3+1)



plot(function(p){JP(1:10,p) - 5*dbeta(p, 0.05,0.4) },from = 0 , to =1, n = 1e3+1)

## Experimenting with bayesian updating optimal pool

  plot(function(x){dbeta(x,5,190)},from = 0, to =1,n= 1000)



