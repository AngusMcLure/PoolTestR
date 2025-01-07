# Try to reproduce bug with large pool sizes

#Read in Brian Johnson's data
library(devtools)
library(tidyverse)
load_all()

d <- read_csv("C:/Users/u4859599/Downloads/PlasmoBook.csv")

PoolPrev(d,ResultPCR, NumInPool, bayesian = FALSE)

#Come up with a minimal dataset that triggers the same bug

min_d <- data.frame(Result = c(0,1), NumInPool = c(1000,1000))

PoolPrev(min_d,Result, NumInPool, bayesian = FALSE)
PoolPrev(min_d,Result, NumInPool, bayesian = TRUE)

1-0.5^(1/1000)


exp(log1p(0.5) * 300)
