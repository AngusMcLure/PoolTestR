# Spatial test case

#Generate spatial data

library(geoR)
library(tidyverse)
library(PoolTestR)
#devtools::install_github("paul-buerkner/brms")
library(brms)
devtools::load_all('C:/Users/u4859599/Documents/GitHub/PoolPoweR/')


set.seed(123)

# Define parameters
n <- 2000  # Number of points
minlonlat <- -1
maxlonlat <-  1
points <- data.frame(lat = runif(n, min = minlonlat, max = maxlonlat),
                     lon = runif(n, min = minlonlat, max = maxlonlat),
                     LID = 1:n)
  
lengthscale <- 0.25
sigma <- 1
nugget <- 0.001
intercept <- -4

#An appropriate prior on the lengthscales that strongly penalises the
#lengthscale from getting smaller than typical distance between points or some
#other scale which is considered to be too small

#lim_l <- sqrt(2) * (maxlonlat - minlonlat)/n * 5;
lim_l <- 0.02
p_l <- 1e-5
alpha <- 1
beta <- - lim_l * log(p_l); beta
plot(\(x){0.056607^1.494197/gamma(1.494197) * x^(-1.494197 -1) * exp(-0.056607/x)},
     n = 1000,  to = lim_l * 50) #brms default prior
plot(\(x){1.47198^5.391402/gamma(5.391402) * x^(-5.391402 -1) * exp(-1.47198/x)},
     n = 1000,  to = lim_l * 50) #brms default prior
plot(\(x){beta^alpha/gamma(alpha) * x^(-alpha-1) * exp(-beta/x)},
     n = 1000,add = T, col= 'red', to = lim_l *50) #suggested prior


#An appropriate prior on the sigma that strongly penalises the variance from
#being too big

lim_sigma <- 5
p_sigma <- 1e-3
lambda <- -log(p_sigma)/lim_sigma
plot(\(x)dexp(x,lambda),n = 1000, to = lim_sigma * 1, col = 'red') #suggested prior
plot(\(x)dstudent_t(x, 3,0,2.5),n = 1000, to = lim_sigma * 1, add =T) #brms default prior



#An appropriate prior on the intercept (assumes intercept only model or with
#covariates normalised to be 0 at their mean values or something to keep it
#interpretible)


plot(\(x)dstudent_t(x, 3,0,2.5),n = 1001, from = -10, to = 10) #brms default prior
plot(\(x)dstudent_t(x, 3,-5,1.5),n = 1000, from = -10, to = 10, add = T, col = 'red') #suggested prior

test <- data.frame(Intercept = rstudent_t(1e4, 3,-5,1.5),
                   sigma = rexp(1e4,lambda)) %>%
  rowwise() %>%
  mutate(prev = PoolTestR:::meanlinknormal(Intercept, sigma, plogis))
test$prev %>% density(from = 0, to = 1) %>% plot
plot(quantile(test$prev, seq(0,1,length = 101)), seq(0,1,length = 101))
  





points <- points %>% mutate(
  gp_sample = grf(n = n, as.matrix(points[,c('lat', 'lon')]),
                  cov.model = "exponential",
                  cov.pars = c(sigma^2, lengthscale),
                  nugget = nugget)$data,
  prevalence = plogis(gp_sample + intercept)) #'true' prevalence
mean(points$prevalence)

# Plot the result
points %>%
ggplot(aes(x = lon, y = lat, color = log(prevalence))) +
  geom_point() +
  scale_color_distiller(palette = 4)


#### Conduct simulated MX survey
catch_dist <- nb_catch(300,400) #catch per site (random)
pool_rule <- pool_target_number(20) #pooling rule


data <- points %>%
  group_by(lon, lat) %>%
  group_modify(~{ #generate the catches and assign to pools
    catch <- distr::r(catch_dist)(1)
    as.data.frame(pool_rule(catch))
  }) %>%
  group_by(lon, lat) %>%
  group_modify(~{
    data.frame(pool_size = rep(.x$pool_size, .x$pool_number))
  }) %>%
  ungroup() %>%
  full_join(points,by = join_by(lon, lat)) %>%
  mutate(result = as.integer(runif(nrow(.)) > (1 - prevalence)^pool_size)) %>%
  arrange(LID)

data

default_prior(result ~ 1 + gp(lon, lat, cov = 'exponential'),
              data = data, family = bernoulli())
lambda
beta
alpha
bprior <- c(prior(student_t(3,-5,1.5),class = 'Intercept'),
            prior(inv_gamma(1, 0.2302585), class = 'lscale', coef = 'gplonlat'),
            prior(exponential(1.381551), class = 'sdgp', coef = 'gplonlat'))


mod20 <- PoolRegBayes(result ~ 1 + gp(lon, lat, cov = 'exponential'),
                      data = data |> filter(LID <= 20),
                      poolSize = pool_size,
                      prior = bprior,
                      cores = 4, verbose = TRUE,
                      warmup = 1e4, iter = 2e4,
                      control = list(adapt_delta = 0.95))
mod20

mod100 <- PoolRegBayes(result ~ 1 + gp(lon, lat, cov = 'exponential'),
                       data = data |> filter(LID <= 100), poolSize = pool_size,
                       prior = bprior,
                       cores = 4, verbose = TRUE,
                       control = list(adapt_delta = 0.95))

mod100


mod300 <- PoolRegBayes(result ~ 1 + gp(lon, lat, cov = 'exponential'),
                       data = data |> filter(LID <= 300),poolSize = pool_size,
                       prior = bprior,
                       cores = 4, verbose = TRUE,
                       control = list(adapt_delta = 0.95))

mod300


mod300approx10 <- PoolRegBayes(result ~ 1 + gp(lon, lat, cov = 'exponential', k=10),
                               data = data |> filter(LID <= 300),poolSize = pool_size,
                               prior = bprior,
                               cores = 4, verbose = TRUE,
                               control = list(adapt_delta = 0.95))
mod300approx10


mod300approx20 <- PoolRegBayes(result ~ 1 + gp(lon, lat, cov = 'exponential', k=20),
                               data = data |> filter(LID <= 300),poolSize = pool_size,
                               prior = bprior,
                               cores = 4, verbose = TRUE,
                               control = list(adapt_delta = 0.95))
mod300approx20

mod1000approx30 <- PoolRegBayes(result ~ 1 + gp(lon, lat, cov = 'exponential', k= 30),
                               data = data |> filter(LID <= 1000), poolSize = pool_size,
                               prior = bprior,
                               cores = 4, verbose = TRUE,
                               control = list(adapt_delta = 0.95))
mod1000approx30

mod1000nogp <- HierPoolPrev(data |> filter(LID <= 1000), result, pool_size, 'LID',
                            cores = 4, verbose = TRUE,
                            control = list(adapt_delta = 0.95))

#Fit statistics and visualisations
prev <- getPrevalence(mod300approx10)

moddata <- mod1000approx30$data %>% select(lon, lat) %>% unique()

temp <- rstan::extract(mod1000approx30$fit) %>%
  as_tibble %>%
  rowwise() %>%
  mutate(prevalence = plogis(t(as.vector(Intercept)  + as.vector(zgp_1))))
s <- temp$prevalence %>% apply(2,quantile,prob = c(0.5, 0.025, 0.975)) %>% t() %>%
  bind_cols(moddata) %>%
  inner_join(data %>% select(lon,lat, LID, prevalence) %>% unique) %>%
  rename(CrIHigh = `97.5%`, CrILow = `2.5%`, Estimate = `50%`) %>%
  mutate(covered = CrIHigh > prevalence & CrILow < prevalence)


moddata <- mod1000approx30 %>%
  fitted(newdata =  moddata %>%
           mutate(pool_size = 1)) %>%
  bind_cols(moddata) %>%
  inner_join(data %>% select(lon,lat, LID, prevalence) %>% unique) %>%
  rename(CrIHigh = `Q97.5`, CrILow = `Q2.5`) %>%
  mutate(covered = CrIHigh > prevalence & CrILow < prevalence)


moddata
# s <- prev$PopulationEffects %>%
#   left_join(points) %>%
#   mutate(covered = CrIHigh > prevalence & CrILow < prevalence)

moddata %>% select(covered, lon, lat, Estimate, prevalence) %>%
  unique() %>%
  summarise(coverage = mean(covered),
            rmse = sqrt(mean((Estimate - prevalence)^2)),
            meanest = mean(Estimate),
            meantrue = mean(prevalence))

moddata %>%
  mutate(error = abs(Estimate - prevalence)) %>%
  pivot_longer(c(Estimate, prevalence, error), names_to = 'measure',values_to = 'value') %>%
  subset(measure %in% c('Estimate', 'prevalence')) %>%
  #subset(measure == 'error') %>%
  ggplot(aes(x = lon, y = lat,
             color = log10(value)
             )) +
  geom_point() +
  facet_grid(~measure) +
  scale_color_distiller(palette = 4)

moddata %>% ggplot(aes(x = lon, y = lat, color = covered)) + geom_point()













### OLD CODE

library(mvtnorm)

# Covariance function (exponential kernel)
cov_func <- function(x1, x2, lengthscale, sigma) {
  sigma^2 * exp(- sqrt(sum((x1 - x2)^2)) / lengthscale)
}

# Create covariance matrix
Sigma <- points %>% t() %>% apply(2, list) %>% map(\(x)x[[1]]) %>%
  outer(., .,
        Vectorize(function(x1, x2) cov_func(x1, x2, lengthscale, sigma)))

# Simulate from GP
points$gp_sample <- rmvnorm(1, mean = rep(0, n), sigma = Sigma)  %>% as.vector()

