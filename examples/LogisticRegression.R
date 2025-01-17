# Perform logistic-type regression modelling for a synthetic dataset consisting
# of pools (sizes 1, 5, or 10) taken from 4 different regions and 3 different
# years. Within each region specimens are collected at 4 different villages,
# and within each village specimens are collected at 8 different sites.


### Models in a frequentist framework
#ignoring hierarchical sampling frame within each region
Mod <- PoolReg(Result ~ Region + Year,
               data = SimpleExampleData,
               poolSize = NumInPool)
summary(Mod)

#accounting hierarchical sampling frame within each region
HierMod <- PoolReg(Result ~ Region + Year + (1|Village) + (1|Site),
                   data = SimpleExampleData,
                   poolSize = NumInPool)
summary(HierMod)


### Models in a Bayesian framework with default (non-informative) priors
#ignoring hierarchical sampling frame within each region
\donttest{
  BayesMod <- PoolRegBayes(Result ~ Region + Year,
                           data = SimpleExampleData,
                           poolSize = NumInPool)
  summary(BayesMod)

  #we could also account for hierarchical sampling frame within each region but
  #note that this is more complex and slower)

  # BayesHierMod <- PoolRegBayes(Result ~ Region + Year + (1|Village) + (1|Site),
  #                              data = SimpleExampleData,
  #                              poolSize = NumInPool)
}

### Calculate adjusted estimates of prevalence
# We use the same function for all four models, but the outputs are slightly different

#For models without hierarchical sampling structure there is an estimate of
#prevalence for every combination of population (fixed) effects: e.g. Region and
#Year
getPrevalence(Mod) #Frequentist model
\donttest{
  getPrevalence(BayesMod) #Bayesian model
}

#For models without hierarchical sampling structure, there is a prevalence
#estimate for each combination of region and year and then at each level of the
#hierarchical sampling frame (i.e. for each village in each region and each site
#in each village)
getPrevalence(HierMod)

# You can also use getPrevalence to predict prevalence for other values of the
# covariates (e.g. predict prevalence in year 4 based on linear trend on the
# logit scale)

#Making a data frame containing data make predictions on
DataFuture <- unique(data.frame(Region = SimpleExampleData$Region,
                                Village = SimpleExampleData$Village,
                                Site = SimpleExampleData$Site,
                                Year = 4))

getPrevalence(Mod, newdata = DataFuture)
getPrevalence(HierMod, newdata = DataFuture)
