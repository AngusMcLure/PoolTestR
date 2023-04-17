library(PoolTestR)
library(tidyverse)
library(brms)
library(brmsmargins)
mod <- PoolRegBayes(Result ~ Year + (1+Year|Village) + (1|Site),
                    subset(ExampleData, Region == "C"),
                    NumInPool,cores = 4)

prev <- getPrevalence(mod)

#Check that CrI at site level are good
TruePrev %>% subset(Region == "C") %>%
  select(Year, Region, Village, Site, PrevalenceSite) %>%
  unique %>% merge(prev$Site) %>%
  mutate(Within = PrevalenceSite <= CrIHigh & PrevalenceSite >= CrILow) %>%
  group_by(Region,Year, Village) %>%
  summarise(Coverage = mean(Within)) %>% View

#Check that CrI at village level are good
TruePrev %>% subset(Region == "C") %>%
  select(Year, Region, Village, PrevalenceVillage) %>%
  unique %>% merge(prev$Village) %>%
  mutate(Within = PrevalenceVillage <= CrIHigh & PrevalenceVillage >= CrILow) %>%
  group_by(Region,Year) %>%
  summarise(Coverage = mean(Within)) %>% View

#Check that CrI at the year/region level are good
TruePrev %>% subset(Region == "C") %>%
  select(Year, Region, PrevalenceRegion) %>%
  unique %>% merge(prev$PopulationEffects) %>%
  mutate(Within = PrevalenceRegion <= CrIHigh & PrevalenceRegion >= CrILow) %>%
  View

#Frequentist glmer version

modglmer <- PoolReg(Result ~ Region + Year + (1+Year|Village) + (1|Site),
                    ExampleData,
                    NumInPool)
modglmer

glmerprev <- getPrevalence(modglmer)
glmerprev

#Check that Estimates at site level are good
TruePrev %>%
  select(Year, Region, Village, Site, PrevalenceSite) %>%
  unique %>% merge(glmerprev$Site) %>%
  mutate(error = Estimate - PrevalenceSite) %>%
  ggplot(aes(x = error)) + geom_histogram(bins = 100,alpha = 0.7)

#Check that Estimates at village level are good
TruePrev %>%
  select(Year, Region, Village, PrevalenceVillage) %>%
  unique %>% merge(glmerprev$Village) %>%
  mutate(error = Estimate - PrevalenceVillage) %>%
  ggplot(aes(x = error)) + geom_histogram(bins = 100) + facet_grid(Year~.)

#Check that Estimates at the year/region level are good
TruePrev %>%
  select(Year, Region, PrevalenceRegion) %>%
  unique %>% merge(glmerprev$PopulationEffects) %>%
  mutate(error = Estimate - PrevalenceRegion) %>%
  ggplot(aes(x = error)) + geom_histogram(bins = 100)


glmerfutureprev <- getPrevalence(modglmer, newdata = data.frame(Region = 'C',Year = 0:10, Village= "C-1"), re.form = ~(1+Year|Village))
glmerfutureprev


glmmod <- PoolReg(Result ~ Region + Year ,
                  ExampleData,
                  NumInPool)
glmprev <- getPrevalence(glmmod)
glmprev

#Check that CrI at the year/region level are BAD (they should have poor coverage)
TruePrev %>%
  select(Year, Region, PrevalenceRegion) %>%
  unique %>% merge(glmprev$PopulationEffects) %>%
  mutate(Within = PrevalenceRegion <= CIHigh & PrevalenceRegion >= CILow) %>%
  View

#Check that Estimates at the year/region level are OK (should have be similar to glmer version)
TruePrev %>%
  select(Year, Region, PrevalenceRegion) %>%
  unique %>% merge(glmprev$PopulationEffects) %>%
  mutate(error = Estimate - PrevalenceRegion) %>%
  ggplot(aes(x = error)) + geom_histogram(bins = 100)

#
#
#
# f <- modglmer@call$formula
# re.terms <- f %>% lme4::findbars()
# newdata <- modglmer@frame %>% select(Year) %>% unique
# sd <- vector('numeric', nrow(newdata))
# for(re in re.terms){
#   mm <- model.matrix(reformulate(as.character(re[2])),newdata)
#   Sigma <- VarCorr(weirdmod)[[as.character(re[3])]]
#   sd <- sd + sapply(1:nrow(mm), function(n){d <- mm[n,,drop = FALSE]; d %*% Sigma %*% t(d)})
# }
# sd
# newdata
#
# Sigma <- matrix(0,3,3)
# nms <- vector('character',3)
# Sigma[1,1] <- VarCorr(modglmer)$Site
# nms[1] <- dimnames(VarCorr(modglmer)$Site)[[1]]
# Sigma[2:3,2:3] <- VarCorr(modglmer)$Village
# nms[2:3] <- dimnames(VarCorr(modglmer)$Village)[[1]]
# dimnames(Sigma) <- list(nms, nms)
# Sigma
# newdata <- modglmer@frame %>% select(Year,Region) %>% unique
# Zsimp <- modglmer@frame %>% select(Year,Region) %>% unique %>% select(Year) %>% as.matrix()
# nintercept <- sum(dimnames(Sigma)[[1]] == "(Intercept)")
# Zsimp <- cbind(Zsimp,matrix(1,nrow(Zsimp),nintercept))
# colnames(Zsimp)[2:3] <- "(Intercept)"
# Zsimp <- Zsimp[,colnames(Sigma)]
#
# out <- newdata %>% mutate(eta = predict(modglmer,
#                              newdata = out,
#                              re.form = NA),
#                sd = sqrt(sapply(1:nrow(Zsimp), function(n){d <- Zsimp[n,,drop = FALSE]; d %*% Sigma %*% t(d)})),
#                prev = Vectorize(meanlogitnorm)(eta, sd),
#                badprev = plogis(eta))
#
# meanlogitnorm <- function(mu, sigma){
#   logitnorm::momentsLogitnorm(mu,sigma)[1]
# }
#
#
# modsimpleglmer <- PoolReg(Result ~ Year + (1|Village) + (1|Site),
#                    subset(ExampleData, Region == "C"),
#                    NumInPool)
#
# badmod <- brm(Result ~ Year + (1+Year|Village) + (1|Site),
#               subset(ExampleData, Region == "C"),family = bernoulli(),cores = 4,
#               save_pars = save_pars(all = TRUE))
#
# mod$family <- brms:::bernoulli(link = 'logit')
# modsimple$family <- brms:::bernoulli(link = 'logit')
#
#
# brmsmargins::prediction(modsimple,
#                         data = modsimple$data %>% select(Year) %>% unique %>% mutate(NumInPool =1),
#                         summarize = FALSE,
#                         raw = TRUE,
#                         posterior = TRUE,
#                         effects = 'integrateoutRE',
#                         k = 1000)$Posterior %>%
#   as.data.frame() %>%
#   map_df(~quantile(.x,probs = c(0.5,0.025,0.975)))
#
# getPrevalence(mod)$PopulationEffects
#
# mod$fit


