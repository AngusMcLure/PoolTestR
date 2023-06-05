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

re  %>% subset(Draw == 1) %>% ggplot(aes(x = NumInPool1, y = NumInPool10)) +
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
prev_mos <- 0.01
slope <- c(1,0.5,0.25,2,4,0,-0.25,-1)
for(s in slope){
  plot(function(x){plogis(qlogis(prev_human) + (qlogis(x) - qlogis(prev_mos)) * s)}, to = 0.1, add = T, n= 1000)
}


