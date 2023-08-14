#Start with Simple Example Data and Pretend that the 3 different pool sizes are
#actually different species or something. Try to estimate correlation of
#prevalence at site level between different species. Could do something like this
#with a proper dataset, but this is the proof of concept and model template

data <- PoolTestR::SimpleExampleData %>%
  mutate(NumInPool  = as.factor(NumInPool) %>%
           recode(`1` = 'A',
                  `5` = 'B',
                  `10` = 'C')) %>%
  rename(Species = NumInPool)

data

modbayes<- rstanarm::stan_glmer(Result ~ 0 + Species + Year + (0+Species|Site),
                                data,
                                binomial, cores = 4)


#calculate regression coefficients and R2 for site level prevalence from one species against the others.
#Choose which one is predicted by setting k
sigma <- rstanarm::VarCorr(modbayes)$Site
k <- 3
coef <- sigma[k,setdiff(1:3, k)] %*% solve(sigma[setdiff(1:3, k),setdiff(1:3, k)])
R2 <- coef %*% sigma[k,setdiff(1:3, k)] / sigma[k,k]
intercept <- modbayes$coefficients[k] - sum(modbayes$coefficients[setdiff(1:3, k)] * coef)
coef
intercept
R2


# Would be better to do the above for each MCMC draw -- this is much easier with
# brms outputs than with stanreg so should work better for PoolTestR also. You
# should just have to use VarrCorr(., summary = FALSE) and loop over array


#visualise re to see if they are approximately multivariate normal
re <-
  as.data.frame(modbayes) %>%
  select(starts_with('b')) %>%
  mutate(.,Draw = as.numeric(rownames(.))) %>%
  pivot_longer(-Draw,
               names_prefix = "b\\[", names_sep = ' ',
               names_to = c('Species', 'Site')) %>%
  pivot_wider(names_from = Species, values_from = value)

re  %>% subset(Draw == 1) %>% ggplot(aes(x = SpeciesA, y = SpeciesB)) +
  geom_point()




### Figure illustrating how to interpret 'regression coefficients' extracted
### from covariance matrix. When the site level predictor prevalence and outcome
### prevalence are near their median values (mean/median values on the logit
### scale) the relationship between prevalence is approximately linear. The
### regression coefficients are the slope of the line in this neighbourhood. As
### long as coefficients are positive, outcome prevalence goes to 0 or 1 as
### predictors go to 0 or 1, but this is reversed if the slope coefficient is
### negative. If slope coefficient is 0, there's no relationship between
### predictor and outcome.

prev_human <- 0.05
prev_mos <- 0.05
slope <- c(2,1,0.5,0,-0.5,-1,-2)
for(s in slope){
  plot(function(x){plogis(qlogis(prev_human) + (qlogis(x) - qlogis(prev_mos)) * s)}, to = 0.1, add = s<2, n= 1000, ylim = c(0,0.1))
}


### Same but for actual example above just looking at single regression of A vs B

sigma <- rstanarm::VarCorr(modbayes)$Site[1:2,1:2]
l <- nrow(sigma)
k <- 2
coef <- sigma[k,setdiff(1:l, k)] %*% solve(sigma[setdiff(1:l, k),setdiff(1:l, k)])
R2 <- coef %*% sigma[k,setdiff(1:l, k)] / sigma[k,k]
intercept <- modbayes$coefficients[k] - sum(modbayes$coefficients[setdiff(1:l, k)] * coef)
coef
intercept
R2

plot(function(x){plogis(modbayes$coefficients["SpeciesB"] + (qlogis(x) - modbayes$coefficients["SpeciesA"]) * coef[1,1])}, to = 1, n= 1000)

plot(function(x){plogis(modbayes$coefficients["SpeciesB"] + (qlogis(x) - modbayes$coefficients["SpeciesA"]) * -2)}, to = 1, n= 1000, add = T)


plogis(modbayes$coefficients[1:3])




##### Trying with PoolTestR with better fake dataset
library(tidyverse)
library(PoolTestR)

Sigma <- c(1,0.5,0.5,
           0.5,1,0.5,
           0.5,0.5,1) %>% matrix(ncol = 3)



BasePrevalence <- c(A = 0.01, B= 0.02, C = 0.03)
base_logodds <- qlogis(BasePrevalence)
FakeData <- expand.grid(Species = LETTERS[1:3],
                        Site = 1:450) %>%
  group_by(Site) %>%
  mutate(site_logodds = base_logodds + as.vector(mvtnorm::rmvnorm(1,sigma = Sigma)),
         site_prevalence = plogis(site_logodds)) %>%
  merge(data.frame(PoolNum = 1:3),.) %>%
  mutate(.,PoolSize = sample(1:25, nrow(.),T),
         Result = as.numeric(runif(nrow(.)) < 1 - (1-site_prevalence)^PoolSize))


FakeData




freq_mod <-       PoolReg(Result ~ 0 + Species + (0 + Species|Site),
                          FakeData, PoolSize)

freq_prev <- getPrevalence(freq_mod)

bayes_mod <- PoolRegBayes(Result ~ 0 + Species + (0 + Species|Site),
                          FakeData, PoolSize, cores = 4)
bayes_prev <- getPrevalence(bayes_mod)

bayes_prev$Site %>% select(Species, Site, Estimate) %>%
  pivot_wider(names_from = Species, values_from = Estimate) %>%
  ggplot(aes(x = C, y = A)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0,0.1), ylim = c(0,0.1))

bayes_Sigma <- brms::VarCorr(bayes_mod,summary = FALSE)$Site$cov
bayes_fixef <- brms::fixef(bayes_mod,summary = FALSE)

ndraw <- dim(bayes_Sigma)[1]
coef <- array(NA, dim = c(ndraw,2))
R2 <- intercept <- numeric(ndraw)

outcome <- 'SpeciesA'
predictors <- setdiff(dimnames(bayes_Sigma)[[2]], outcome)

for(n in 1:ndraw){
  sigma <- bayes_Sigma[n,,]
  coef[n,] <- sigma[outcome,predictors] %*% solve(sigma[predictors,predictors])
  R2[n] <- coef[n,] %*% sigma[outcome,predictors] / sigma[outcome,outcome]
  intercept[n] <- bayes_fixef[n,outcome] - sum(bayes_fixef[n,predictors] * coef[n,])
}
quantile(intercept, probs = c(0.025, 0.5,0.975))
t(apply(coef,2,quantile,probs = c(0.025, 0.5,0.975)))
