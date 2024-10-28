
mu <- 0
sigma <- 100

range <- 5

# invlink <- PoolPoweR:::cloglog_inv
# link <- PoolPoweR:::cloglog

invlink <- plogis
link <- qlogis

plot(\(x)Vectorize(meanlinknormal, 'mu')(mu + x,sigma,invlink),from = -mu - sigma * range, to = -mu + sigma * range)

k <- sigma
scaling1 <- link(meanlinknormal(-k, sigma, invlink))/-k
scaling2 <- link(meanlinknormal(k, sigma, invlink))/k

plot(\(x)invlink((mu + x)*scaling1), from = -mu-sigma * range, to = -mu + sigma* range, add = T)
#plot(\(x)invlink((mu + x)*scaling2), from = -mu-sigma * range, to = -mu + sigma* range, add = T)



f <- function(x){
  xs <- -sigma - mu
  xe <-  sigma - mu
  
  fs <- invlink((xs + mu)*scaling1)
  fe <- invlink((xe + mu)*scaling2)
  
  out <- x
  out[x < xs] <- invlink((mu + x[x < xs])*scaling1)
  out[x > xe] <- invlink((mu + x[x > xe])*scaling2)
  out[x >= xs & x <= xe] <- fs + (fe - fs)/(xe - xs) * (x[x >= xs & x <= xe] - xs)
  out
} 

plot(f, from = -mu-sigma * range, to = -mu + sigma* range, add = T)

