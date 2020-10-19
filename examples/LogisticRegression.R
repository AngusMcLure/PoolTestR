# Perform logistic-type regression modelling for a synthetic dataset consisting
# of pools (sizes 1, 5, or 10) taken from 4 different regions and 3 different
# years. Within each region specimens are collected at 4 different villages,
# and within each village specimens are collected at 8 different sites.


### Models in a frequentist framework
#ignoring hierarchical sampling frame within each region
Mod <- PoolReg(Result ~ Region + Year,
               data = ExamplePoolData,
               poolSize = NumInPool)
summary(Mod)

#accounting hierarchical sampling frame within each region
HierMod <- PoolReg(Result ~ Region + Year + (1|Village/Site),
                   data = ExamplePoolData,
                   poolSize = NumInPool)
summary(HierMod)
#Extract fitted prevalence for each combination of region and year and then at
#each level of the heirarchical sampling frame (i.e. for each village in each
#region and  each site in each village)
getPrevalence(HierMod)


### Models in a Bayesian framework with default (non-informative) priors
#ignoring hierarchical sampling frame within each region
\dontrun{
BayesMod <- PoolRegBayes(Result ~ Region + Year,
                         data = ExamplePoolData,
                         poolSize = NumInPool)
summary(BayesMod)
getPrevalence(BayesMod) #Extract fitted prevalence for each combination of region and year

#accounting hierarchical sampling frame within each region
BayesHierMod <- PoolRegBayes(Result ~ Region + Year + (1|Village/Site),
                             data = ExamplePoolData,
                             poolSize = NumInPool)
summary(BayesHierMod)
getPrevalence(BayesHierMod)
}

### Calculate adjusted estimates of prevalence
# We use the same function for all four models, but the outputs are slightly different

# Extract fitted prevalence for each combination of region and year
getPrevalence(Mod)
\dontrun{
getPrevalence(BayesMod)
}

#Extract fitted prevalence for each combination of region and year and then at
#each level of the heirarchical sampling frame (i.e. for each village in each
#region and  each site in each village)
getPrevalence(HierMod)
\dontrun{
getPrevalence(BayesHierMod)
}

# You can also use getPrevalence to predict at prevalence for other values of
# the covariates (e.g. predict prevalence in year 4)

#Making a data frame containing data make predict on
ExamplePoolDataFuture <- unique(data.frame(Region = ExamplePoolData$Region,
                                           Village = ExamplePoolData$Village,
                                           Site = ExamplePoolData$Site,
                                           Year = 4))

getPrevalence(Mod, data = ExamplePoolDataFuture)
getPrevalence(HierMod, data = ExamplePoolDataFuture)
\dontrun{
getPrevalence(BayesMod, data = ExamplePoolDataFuture)
getPrevalence(BayesHierMod, data = ExamplePoolDataFuture)
}
