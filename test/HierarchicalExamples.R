#Hierarchical nested example where you should really be using a glmm

pkgbuild::compile_dll(force = T)
pkgbuild::compile_dll()
roxygen2::roxygenize()

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

RegionPrevs <- c(A = 0.5, B = 2, C = 4)/100 #Prevalence in each region
SpeciesCatchSizeExp <- c(X = 0.5, Y = 0, Z = -0.5) # Are mosquito catch sizes for a species correlated with prevalence
NumRegions <- length(RegionPrevs)
NumVillages <- 10 #Villages per Region
NumSites <- 15 #Sites per village
MeanCatch <- 1000; #Mean and dispersion of mosquito catch sizes (neg binomial distributed)
DispersionCatch <- 5
MaxPoolSize <- 25
VillageSD <- 0.25
SiteSD <- 0.1
N <- 10^4

#Plot catch size distribution
plot(function(x){dnbinom(x,mu = MeanCatch, size = DispersionCatch)},from = 0, to =2000)

Data <- data.frame()
TruePrev <- data.frame()
for(Sp in names(SpeciesCatchSizeExp)){
  for(R in names(RegionPrevs)){
    #Hypothetical true prevalence of region that we would measure by averaging the prevalence across a large number of sites and villages weighting for mosquito abundance
    temp <- rnorm(N,mean = 0, sd = sqrt(SiteSD^2 + VillageSD^2)) #Generate lots of deviates for hypothetical sites
    RegionTruePrevWeightedMeanLogit <- weighted.mean(plogis(qlogis(RegionPrevs[[R]]) + temp), exp(temp)^SpeciesCatchSizeExp[Sp])
    RegionTruePrevLogitWeightedMean <- plogis(weighted.mean(qlogis(RegionPrevs[[R]]) + temp, exp(temp)^SpeciesCatchSizeExp[Sp]))
    RegionTruePrevMeanLogit <- mean(plogis(qlogis(RegionPrevs[[R]]) + temp))
    RegionTruePrevLogitMean <- RegionPrevs[[R]] # equal to plogis(mean(qlogis(RegionPrevs[[R]]) + temp))


    for(V in 1:NumVillages){
      #Difference of village prevalence from the region prevalence on log-odds scale
      VillageDeviate <- rnorm(1,mean = 0, sd = VillageSD)
      #Hypothetical true prevalence of village that we would measure by averaging the prevalence across a large number of sites weighting for mosquito abundance
      temp <- rnorm(N,mean = VillageDeviate, sd = SiteSD) #Generate lots of deviates for hypothetical sites
      VillageTruePrevWeightedMeanLogit <- weighted.mean(plogis(qlogis(RegionPrevs[[R]]) + temp), exp(temp)^SpeciesCatchSizeExp[Sp])
      VillageTruePrevLogitWeightedMean <- plogis(weighted.mean(qlogis(RegionPrevs[[R]]) + temp, exp(temp)^SpeciesCatchSizeExp[Sp]))
      VillageTruePrevMeanLogit <- mean(plogis(qlogis(RegionPrevs[[R]]) + temp))
      VillageTruePrevLogitMean <- plogis(qlogis(RegionPrevs[[R]]) + VillageDeviate)
      for(S in 1:NumSites){
        #Difference of site prevalence from the village prevalence on log-odds scale
        SiteDeviate <- rnorm(1,mean = 0, sd = SiteSD)
        SiteTruePrev <- plogis(qlogis(RegionPrevs[[R]]) + VillageDeviate + SiteDeviate)
        TruePrev <- rbind(TruePrev,
                          data.frame(Species = Sp,
                                     Region = R,
                                     Village = paste(R,V,sep = "-"),
                                     Site = paste(R,V,S,sep = "-"),
                                     PrevalenceRegionWeightedMeanLogit = RegionTruePrevWeightedMeanLogit,
                                     PrevalenceRegionLogitWeightedMean = RegionTruePrevLogitWeightedMean,
                                     PrevalenceRegionMeanLogit = RegionTruePrevMeanLogit,
                                     PrevalenceRegionLogitMean = RegionTruePrevLogitMean,
                                     PrevalenceVillageWeightedMeanLogit = VillageTruePrevWeightedMeanLogit,
                                     PrevalenceVillageLogitWeightedMean = VillageTruePrevLogitWeightedMean,
                                     PrevalenceVillageMeanLogit = VillageTruePrevMeanLogit,
                                     PrevalenceVillageLogitMean = VillageTruePrevLogitMean,
                                     PrevalenceSite = SiteTruePrev))

        #Generate catch sizes from zero-truncated negative binomial distribution. 're-roll' sizes <= 0 to guarantee at least one mossie
        CatchSizeMult <- exp(VillageDeviate + SiteDeviate)
        Catch <- 0
        while(Catch<=0){
          Catch <- rnbinom(1,mu = MeanCatch * CatchSizeMult^SpeciesCatchSizeExp[Sp],DispersionCatch)
        }
        NumBigPools <- Catch %/% MaxPoolSize
        if(NumBigPools){
          Data <- rbind(Data,
                        data.frame(Species = Sp,
                                   Region = R,
                                   Village = paste(R,V,sep = "-"),
                                   Site = paste(R,V,S,sep = "-"),
                                   PoolSize = rep(MaxPoolSize,NumBigPools),
                                   PrevalenceSite = SiteTruePrev,
                                   PrevalenceVillage = VillageTruePrevLogitMean,
                                   PrevalenceRegion = RegionPrevs[[R]]))
        }
        SizeSmallPool <- Catch %% MaxPoolSize
        if(SizeSmallPool){
          Data <- rbind(Data,
                        data.frame(Species = Sp,
                                   Region = R,
                                   Village = paste(R,V,sep = "-"),
                                   Site = paste(R,V,S,sep = "-"),
                                   PoolSize = SizeSmallPool,
                                   PrevalenceSite = SiteTruePrev,
                                   PrevalenceVillage = VillageTruePrevLogitMean,
                                   PrevalenceRegion = RegionPrevs[[R]]))
        }
      }
    }
  }
}
Data$Result <- with(Data,as.numeric(runif(nrow(Data)) < (1-(1-PrevalenceSite)^PoolSize)))
TruePrev %>% select(Species, Region, PrevalenceRegionWeightedMeanLogit,PrevalenceRegionLogitWeightedMean,PrevalenceRegionMeanLogit,PrevalenceRegionLogitMean) %>% unique

# DataAlt <- expand.grid(Region = names(RegionPrevs),
#                        Village = 1:NumVillages,
#                        Site = 1:NumSites) %>%
#   mutate(Village = paste(Region, Village, sep = "-")) %>%
#   mutate(Site = paste(Village, Site, sep = "-")) %>%
#   mutate(SiteDeviate = )
#   mutate(PrevalenceRegion = RegionPrevs[Region]) %>%
#   mutate(PrevalenceVillage = plogis(qlogis(PrevalenceRegion) + rnorm(nrow(.),0, VillageSD))) %>%
#   mutate(PrevalenceSite = plogis(qlogis(PrevalenceVillage) + rnorm(nrow(.),0, SiteSD))) %>%
#   mutate(Catch = rnbinom(1,mu = MeanCatch * PrevalenceRegion))

# DataAlt


#number of of pools per site
Data %>% group_by(Species, Region,Village,Site) %>%
  summarise(Pools = n()) %>%
  summarise(MedPools = median(Pools),
            MinPools = min(Pools),
            MaxPools = max(Pools)) %>% View

Data %>% group_by(Species, Region,Village,Site) %>%
  summarise(Pools = n(),
            PosPools = sum(Result)) %>%
  summarise(MedPools = median(Pools),
            MinPools = min(Pools),
            MaxPools = max(Pools),
            MedPosPools = median(PosPools),
            MinPosPools = min(PosPools),
            MaxPosPools = max(PosPools)) %>% View

Data <- Data %>%
  group_by(Species,Region,Village,Site) %>%
  summarise(Abundance = sum(PoolSize)) %>%
  merge(Data,.) %>%
  mutate(LogAb = log(Abundance))
Data



library(tidyr)
library(ggplot2)


Data %>% select(-PoolSize, -Result) %>% unique() %>% #ONly interested in the numbers caught at each site so drop individual pools
  ggplot() +
  geom_violin(aes(x = Village, y = Abundance)) +
  scale_y_log10() +
  facet_grid(~Region,scales = 'free')

Data %>% select(-PoolSize, -Result) %>% unique() %>% #ONly interested in the numbers caught at each site so drop individual pools
  ggplot() +
  geom_histogram(aes(x = Abundance),bins = 100)
TruePrev %>%
  separate(Site,c(NA,"VillageNum","SiteNum")) %>%
  ggplot() +
  geom_point(aes(x = SiteNum, y = PrevalenceSite, color = Region)) +
  facet_grid( ~ VillageNum) +
  scale_y_log10()

TruePrev %>%
  separate(Site,c(NA,"VillageNum","SiteNum")) %>%
  ggplot() +
  geom_point(aes(x = VillageNum, y = PrevalenceSite, color = Region)) +
  scale_y_log10()


#Calculating the mean prevalence in the whole population and each Region,
#without adjusting for the psuedoreplicaiton introduced by the heiracrchical sampling frame
UnadjustedMean <-  PoolLogitReg(data = Data,
                                formula = Result ~ 1,
                                PoolSize)
plogis(coef(UnadjustedMean))
plogis(confint(UnadjustedMean))

UnadjustedMean.Region <- PoolLogitReg(data = Data,
                                      formula = Result ~ Region,
                                      PoolSize)
plogis(coef(UnadjustedMean.Region) + c(0,rep(coef(UnadjustedMean.Region)['(Intercept)'],2)))
confint(UnadjustedMean.Region)

#Adjusting now for sampling frame
AdjustedMean <- PoolLogitRegMixed(Data,
                                  Result ~ Species + Region + (1|Village/Site), #Adding site sometimes causes convergence issues
                                  PoolSize)
summary(AdjustedMean)
plogis(coef(AdjustedMean)$Region$`(Intercept)`)
confint(AdjustedMean) ## THIS SHOULD WORK BUT DOESN'T

library(lme4)
#This should be the same as above, but for some reason confint doesn't work on the above
memPool <- glmer(data = Data,
                 formula = Result ~ Species + Region + (1|Village/Site),
                 family = binomial(PoolLink(Data$PoolSize)))
summary(memPool)
confint(memPool)


Data %>%
  select(Species,Region) %>%
  mutate(PoolSize = 1) %>%
  unique %>% cbind(.,Prev = predict(memPool,
                                    type = 'link',
                                    re.form = ~0,
                                    newdata = .) %>% plogis)




## Can I write a general case for the above which calculates the prevalence at every level?

Formula <- attr(memPool,'call')$formula


GroupTermNames <- (as.character(Formula)[[3]] %>%
                     str_match_all("\\|\\s*(.*?)\\s*\\)"))[[1]][,2] %>%
  strsplit(split = "\\/") %>%
  unlist

GroupTerms <- findbars(Formula)
NGroupTerms <- length(GroupTerms)
PredData <- memPool %>%
  attr('frame') %>%
  select_at(-c(1)) %>%
  mutate(PoolSize = 1) %>%
  unique
pred <- PredData
for(n in 1:NGroupTerms){
  GroupEffectForm <- reformulate(paste0("(",as.character(GroupTerms[(NGroupTerms-n+1):NGroupTerms]),")"))

  Prev = predict(memPool,
                 type = 'link',
                 re.form = GroupEffectForm,
                 newdata = PredData) %>% plogis
  pred <- cbind(pred, Prev =  Prev)
  colnames(pred)[ncol(pred)] <- paste(GroupTermNames[n],"Prev")
}
ColNames <- colnames(pred)
ColNames[NTerms+ 1 +(1:NTerms)] <- paste0(ColNames[1:NTerms],"Prev")
colnames(pred) <- ColNames
pred <- pred %>% select(-PoolSize)
pred


##Bayesian version with brms
pkgbuild::compile_dll()
roxygen2::roxygenize()

Moda <- PoolRegBayes(Data, Result ~ Species + Region + (1|Village/Site), PoolSize)
ModaBC <- PoolRegBayes(Data %>% subset(Region != "A"), Result ~ Species + Region + (1|Village/Site), PoolSize)
ModaY <- PoolRegBayes(Data %>% subset(Species == "Y"), Result ~ Region + (1|Village/Site), PoolSize)
Modb <- PoolRegBayes(Data, Result ~ Species + (1|Region/Village/Site), PoolSize)
Modc <- PoolRegBayes(Data, Result ~ Species + Village + (1|Site), PoolSize,iter = 5000,warmup = 1000)
Modd <- PoolRegBayes(Data, Result ~ Species + Site, PoolSize)
Mode <- PoolRegBayes(Data, Result ~ Species + Region + (1|Village), PoolSize) # ignore the use of different sites

Modf <- PoolRegBayes(Data %>% subset(Region == "A" & Species == "X"), Result ~  1 + (1|Village/Site), PoolSize)

Modg <- Data %>% group_by(Region, Species) %>%
  group_map(function(x,...){PoolRegBayes(x,Result ~  1 + (1|Village/Site), PoolSize)})


pa <- getPrevalence(Moda)
paBC <- getPrevalence(ModaBC)
paY <- getPrevalence(ModaY)

pb <- getPrevalence(Modb)
pc <- getPrevalence(Modc)
pd <- getPrevalence(Modd)
pe <- getPrevalence(Mode)

pf <- getPrevalence(Modf); pf$PopulationEffects <- pf$PopulationEffects %>% mutate(Region = "A", Species = "X")

pg <- lapply(Modg,getPrevalence)


Keys <- Data %>% group_by(Region, Species) %>% group_keys
out <- list()
out$PopulationEffects <- data.frame()
for(n in 1:nrow(Keys)){
  out$PopulationEffects <- bind_rows(out$PopulationEffects, cbind(pg[[n]]$PopulationEffects,Keys[n,]))
}

pg$PopulationEffects <- sapply(pg, `[[`, c('PopulationEffects')) %>% t() %>% as.data.frame %>%
  cbind(Data %>% group_by(Region, Species) %>% group_keys,.) %>% mutate(Esto)

PredData <- Data %>%
  group_by(Species, Region) %>%
  summarise(LogAb = mean(LogAb)) %>%
  as.data.frame
pa.alt <- fitted(Moda,
                 scale = 'response',
                 re_formula = NA,
                 newdata =  PredData %>%
                   mutate(PoolSize = 1)) %>%
  as.data.frame %>%
  select(-Est.Error)
colnames(pa.alt) <- c("Estimate", "CrILow", "CrIHigh")
pa.alt <- cbind(PredData,pa.alt)




### Plots and comparison with estimates unadjusted for heirarchical sampling frame

PrevRegion <- PoolPrev(Data,Result,PoolSize,Species,Region)
PrevVillage <- PoolPrev(Data,Result,PoolSize,Species,Region,Village)
PrevSite.Village1 <- PoolPrev(subset(Data,Village %in% c("A-1","B-1","C-1")),Result,PoolSize,Species,Region,Village,Site)




PlotDataRegion <- PrevRegion %>%
  unique %>%
  select(Species, Region, PrevMLE, CILow, CIHigh) %>%
  mutate(Method = 'BasicMLE') %>%
  rename(Prev = PrevMLE) %>%
  bind_rows(PrevRegion %>%
              unique %>%
              select(Species, Region, PrevBayes, CrILow, CrIHigh) %>%
              mutate(Method = 'BasicBayes') %>%
              rename(Prev = PrevBayes,
                     CILow= CrILow,
                     CIHigh = CrIHigh)) %>%
  bind_rows(TruePrev %>%
              select(Region,Species,
                     PrevalenceRegionWeightedMeanLogit,
                     PrevalenceRegionLogitWeightedMean,
                     PrevalenceRegionMeanLogit,
                     PrevalenceRegionLogitMean
                     ) %>%
              unique %>%
              pivot_longer(cols = -c(Species,Region),
                           names_prefix = "PrevalenceRegion",
                           names_to = "Method",
                           values_to = "Prev")) %>%
  bind_rows(paY$PopulationEffects %>%
              mutate(Species = "Y") %>%
              mutate(Method = 'Heirarchical Bayes \n Region/Village/Site') %>%
              rename(Prev = Estimate,
                     CILow = CrILow,
                     CIHigh = CrIHigh) %>%
              select(Species, Region, Prev, CILow, CIHigh, Method))

PlotDataVillage <- PrevVillage %>%
  unique %>%
  select(Species, Region, Village, PrevMLE, CILow, CIHigh) %>%
  mutate(Method = 'BasicMLE') %>%
  rename(Prev = PrevMLE) %>%
  bind_rows(PrevVillage %>%
              unique %>%
              select(Species, Region, Village, PrevBayes, CrILow, CrIHigh) %>%
              mutate(Method = 'BasicBayes') %>%
              rename(Prev = PrevBayes,
                     CILow= CrILow,
                     CIHigh = CrIHigh)) %>%
  bind_rows(TruePrev %>%
              select(Species, Region,Village,PrevalenceVillageLogitMean) %>%
              unique %>%
              mutate(Method = "True") %>%
              rename(Prev = PrevalenceVillageLogitMean)) %>%
  bind_rows(paY$Village %>%
              mutate(Species = "Y") %>%
              mutate(Method = 'Heirarchical Bayes \n Village/Site') %>%
              rename(Prev = Estimate,
                     CILow= CrILow,
                     CIHigh = CrIHigh)) %>%
  # bind_rows(pc$PopulationEffects %>%
  #             mutate(Method = 'Heirarchical Bayes \n Site') %>%
  #             mutate(Region = substr(Village,1,1)) %>%
  #             rename(Prev = Estimate,
  #                    CILow= CrILow,
  #                    CIHigh = CrIHigh)) %>%
  mutate(VillageNum = (strsplit(as.character(Village),"-") %>% as.data.frame() %>% t)[,2] %>% as.integer) %>%
  subset(VillageNum %in% 1:5)

PlotDataSite <- PrevSite.Village1 %>%
  unique %>%
  select(Species, Region, Village, Site, PrevMLE, CILow, CIHigh) %>%
  subset(Village %in% c("A-1","B-1","C-1")) %>%
  mutate(Method = 'BasicMLE') %>%
  rename(Prev = PrevMLE) %>%
  bind_rows(PrevSite.Village1 %>%
              unique %>%
              select(Species, Region, Village, Site, PrevBayes, CrILow, CrIHigh) %>%
              subset(Village %in% c("A-1","B-1","C-1")) %>%
              mutate(Method = 'BasicBayes') %>%
              rename(Prev = PrevBayes,
                     CILow= CrILow,
                     CIHigh = CrIHigh)) %>%
  bind_rows(TruePrev %>%
              select(Species, Region,Village,Site,PrevalenceSite) %>%
              subset(Village %in% c("A-1","B-1","C-1")) %>%
              unique %>%
              mutate(Method = "True") %>%
              rename(Prev = PrevalenceSite)) %>%
  bind_rows(p$Site %>%
              subset(Village %in% c("A-1","B-1","C-1")) %>%
              mutate(Method = 'HeirarchicalBayes') %>%
              rename(Prev = Estimate,
                     CILow= CrILow,
                     CIHigh = CrIHigh)) %>%
  mutate(SiteNum = (strsplit(as.character(Site),"-") %>% as.data.frame() %>% t)[,3] %>% as.integer) %>%
  subset(SiteNum %in% 1:5)


#Plots by region
PlotsRegion <- PlotDataRegion %>%
  ggplot() +
  geom_pointrange(aes(x = Method,
                      color = Region,
                      y = Prev,
                      ymin = CILow,
                      ymax = CIHigh,
                      #shape = Method,
                      linetype = Species),
                  position = position_dodge(width = 0.8)) +
  facet_grid(Species~Region,scales = "free_x") +
  scale_y_continuous(labels=scales::percent) +
  #scale_y_log10(labels=scales::percent) +
  scale_x_discrete()+
  theme(axis.text.x = element_text(angle = 90)) +
  #scale_y_log10() +
  ylab('Prevalence')
PlotsRegion
ggplotly(PlotsRegion)

#Plots by village
PlotsVillage <- PlotDataVillage  %>%
  ggplot() +
  geom_pointrange(aes(x = VillageNum,
                      color = Region,
                      y = Prev,
                      ymin = CILow,
                      ymax = CIHigh,
                      shape = Method,
                      linetype = Species),
                  position = position_dodge(width = 0.8)) +
  facet_grid(Species~Region) +
  #scale_y_log10(labels=scales::percent) +
  #scale_y_continuous(labels=scales::percent) +
  scale_y_log10(labels=scales::percent, limits = 10^c(-3.3,-0.7)) +
  #scale_x_discrete() +
  ylab('Prevalence') +
  xlab('Village')
PlotsVillage
ggplotly(PlotsVillage)

#Plots by site

PlotsSite <-  PlotDataSite %>%
  ggplot() +
  geom_pointrange(aes(x = SiteNum,
                      color = Region,
                      y = Prev,
                      ymin = CILow,
                      ymax = CIHigh,
                      shape = Method),
                  position = position_dodge(0.8)) +
  facet_grid(~Region) +
  scale_y_log10(labels=scales::percent, limits = 10^c(-4,0)) +
  #scale_y_continuous(labels=scales::percent,limits = c(0, .30)) +
  scale_x_continuous() +
  ylab('Prevalence') +
  xlab('Site')
PlotsSite


# All as one figure
multiplot(PlotsRegion + theme(legend.position = "none"),
          PlotsVillage + theme(legend.position = "none"),
          PlotsSite  + theme(legend.position = "none"))

multiplot(PlotsRegion + theme(legend.position = "none"),
          PlotsVillage + theme(legend.position = "none"),
          PlotsSite,
          cols = 3)

PlotDataCombined <- PlotDataRegion %>% mutate(PlotX = Region,
                                              Level = "Region") %>%
  bind_rows(PlotDataVillage %>% mutate(PlotX = factor(VillageNum),
                                       Level = "Village")) %>%
  bind_rows(PlotDataSite %>% mutate(PlotX = factor(SiteNum),
                                    Level = "Site"))

PlotCombined <- ggplot(PlotDataCombined) +
  geom_pointrange(aes(x = PlotX,
                      color = Region,
                      y = Prev,
                      ymin = CILow,
                      ymax = CIHigh,
                      shape = Method)) +
  facet_grid(Level~Region,scales = "free")
PlotCombined


##
DataWide <- Data

LineList2Count <- function(d,...){
  out <- data.frame(PoolResults = I(with(d,table(Result,PoolSize))))
  out
}

Data %>% group_by(Region, Village, Site) %>% group_modify(LineList2Count)






## Weights to correct true prevalences when abundance correlates with prevalence

N <- 10^6
basePrev <- RegionPrevs[[1]]
siteDeviates <- rnorm(N, mean = 0, sd = sqrt(0.5))
sitePrevalences <- plogis(qlogis(basePrev) + siteDeviates)
siteAbundanceWeights <- exp(siteDeviates)
WeightedPrev <- weighted.mean(sitePrevalences,siteAbundanceWeights)
WeightedPrev
unweightedPrev <- mean(sitePrevalences)
unweightedPrev



## With custom code

pkgbuild::compile_dll(force = T)
pkgbuild::compile_dll()
roxygen2::roxygenize()

myModel <- HierPoolPrev(Data, Result, PoolSize,
                        hierarchy = c("Village","Site"),
                        Species, Region,
                        cores = 4, verbose=T)
myModel
myModel$Z %>% View

sfit <- stan("inst/stan/HierBayesianPoolScreen.stan",
             data = myModel,
             cores = 4)




