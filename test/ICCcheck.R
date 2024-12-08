n <- 5000

temp <- tibble(mu = runif(n,-4,-3)) %>%
  rowwise() %>%
  mutate(sigma = list(runif(2)))


# start <- Sys.time()
# cl <- parallel::makeCluster(2)
# parallel::clusterMap(cl, ICC,
#                      mu = as.list(temp$mu),
#                      sigma = temp$sigma,
#                      invlink = list(plogis))
# parallel::stopCluster(cl)
# end <- Sys.time()
# elapsed <- end - start
# elapsed
# elapsed /n * 4000/ 60

map(c('approx', 'nested', 'triple'), ~{
  start <- Sys.time()
  temp %>% as_tibble %>% rowwise %>% mutate(ICC = t(ICC(mu, sigma, 'logit', method = .x)))
  end <- Sys.time()
  end - start
}
)

ICCs <- temp %>% as_tibble %>% rowwise %>% mutate(ICC = t(ICC(mu, sigma, 'logit', method = 'approx')))




n <- 1e4
sigma1 <- 1
sigma2 <- 1

.mu <- -5 

invlink <- plogis

tibble(mu = .mu, sigma = rnorm(n * 1) * sigma1) %>%
  rowwise() %>%
  mutate(villageprev = meanlinknormal(mu + sigma, sigma2, invlink)) %>%
  ungroup() %>%
  summarise(obsmean = mean(villageprev),
            obsvar = var(villageprev)) %>%
  mutate(obsICC = obsvar/(obsmean * (1- obsmean)),
         expmean = meanlinknormal(.mu, sqrt(sigma1^2 + sigma2^2), invlink),
         expICC  = ICC(.mu, c(sigma1, sigma2), method = 'approx', link = 'logit')[1]
         )

## Check the approximate method for ICC

d_approx <- tibble(mu = seq(0,-20, by =-0.1),sigma = t(c(2,2/5))) %>%
  rowwise() %>%
  mutate(nested = list(ICC(mu, sigma, 'logit', method = 'nested')),
         approx = list(ICC(mu, sigma, 'logit', method = 'approx'))) %>%
  pivot_longer(c(nested, approx),names_to = 'method', values_to = 'ICC') %>%
  unnest_longer(ICC, indices_to = 'level')
d_approx
d_approx %>% ggplot(aes(x = mu, y = ICC, color = method)) +
  geom_line() +
  facet_grid(~level)
            