library(dplyr)
set.seed(as.numeric(as.Date("2020/10/22")))
RegionPrevs <- c(A = 0.5, B = 2, C = 4)/100 #Prevalence in each region
NumRegions <- length(RegionPrevs)
NumVillages <- 10 #Villages per Region
NumSites <- 10 #Sites per village
MeanCatch <- 200 #Mean and dispersion of mosquito catch sizes (neg binomial distributed)
DispersionCatch <- 5
MaxPoolSize <- 25
# Variability between villages and between sites in a given village.
# We could probably increase these standard deviations to make the difference between the hierarchical and non-hierarchical models more stark
VillageSD <- 0.5
SiteSD <- 0.5
VillageSDTrend <- 0.2
SiteSDTrend <- 0
Years <- 0:2
GrowthRate <- log(0.8)



ExampleData <- data.frame()
for(R in names(RegionPrevs)){
  for(V in 1:NumVillages){
    #Difference of village prevalence from the region prevalence on log-odds scale
    VillageDeviate <- rnorm(1, mean = 0, sd = VillageSD)
    VillageDeviateTrend <- rnorm(1, mean = 0, sd = VillageSDTrend)
    for(S in 1:NumSites){
      #Difference of site prevalence from the village prevalence on log-odds scale
      SiteDeviate <- rnorm(1, mean = 0, sd = SiteSD)
      SiteDeviateTrend <- rnorm(1, mean = 0, sd = SiteSDTrend)

      for(Y in Years){
        VillageTruePrev <- plogis(qlogis(RegionPrevs[[R]]) + VillageDeviate + Y * (GrowthRate + VillageDeviateTrend))
        SiteTruePrev <- plogis(qlogis(RegionPrevs[[R]]) + VillageDeviate + SiteDeviate + Y * (GrowthRate + VillageDeviateTrend + SiteDeviateTrend))
        #Generate catch sizes from zero-truncated negative binomial distribution. 're-roll' sizes == 0 to guarantee at least one mossie
        Catch <- 0
        while(Catch<=0){
          Catch <- rnbinom(1, mu = MeanCatch, DispersionCatch)
        }
        #Split pools into as many of size MaxPoolSize as possible and the remainder in another small pool
        NumBigPools <- Catch %/% MaxPoolSize
        if(NumBigPools){
          ExampleData <- rbind(ExampleData,
                               data.frame(Year = Y,
                                                 Region = R,
                                                 Village = paste(R,V,sep = "-"),
                                                 Site = paste(R,V,S,sep = "-"),
                                                 NumInPool = rep(MaxPoolSize,NumBigPools),
                                                 PrevalenceSite = SiteTruePrev,
                                                 PrevalenceVillage = VillageTruePrev,
                                                 PrevalenceRegion = plogis(qlogis(RegionPrevs[[R]]) + GrowthRate * Y)))
        }
        SizeSmallPool <- Catch %% MaxPoolSize
        if(SizeSmallPool){
          ExampleData <- rbind(ExampleData,
                               data.frame(Year = Y,
                                          Region = R,
                                          Village = paste(R,V,sep = "-"),
                                          Site = paste(R,V,S,sep = "-"),
                                          NumInPool = SizeSmallPool,
                                          PrevalenceSite = SiteTruePrev,
                                          PrevalenceVillage = VillageTruePrev,
                                          PrevalenceRegion = plogis(qlogis(RegionPrevs[[R]]) + GrowthRate * Y)))
        }
      }
    }
  }
}
ExampleData$Result <- with(ExampleData,as.numeric(runif(nrow(ExampleData)) < (1-(1-PrevalenceSite)^NumInPool)))

TruePrev <- ExampleData %>%
  select(Year, Site, Village, Region,
         PrevalenceSite,PrevalenceVillage,PrevalenceRegion) %>%
  unique
rownames(TruePrev) <- NULL
usethis::use_data(TruePrev, overwrite = TRUE)

ExampleData <- ExampleData %>% select(-c(PrevalenceSite,PrevalenceVillage,PrevalenceRegion))
usethis::use_data(ExampleData, overwrite = TRUE)


####
#
# PrevsYearRegion <- PoolPrev(ExampleData, Result, NumInPool,Region,Year)
# PrevsYearRegionVillage <- PoolPrev(ExampleData, Result, NumInPool,Region,Year,Village)
#
# ModFreq <- PoolReg(Result ~ Region + Year,
#                    ExampleData, NumInPool)
# ModFreqVillage <- PoolReg(Result ~ Region + Year + Village,
#                    ExampleData, NumInPool)
# ModBayesHier2 <- PoolRegBayes(Result ~ Region + Year + (1 + Year|Village) + (1|Site),
#                               ExampleData, NumInPool)
#
# HierPrevsYearRegion <- HierPoolPrev(ExampleData, Result, NumInPool, c("Village", "Site"), Region, Year)
# HierPrevsYearRegionVillage <- HierPoolPrev(ExampleData, Result, NumInPool, c("Site"), Region, Year, Village)
#
# RegionPO <- rbind(TruePrev %>%
#                     select(Region, Year, Estimate = PrevalenceRegion) %>%
#                     mutate(CILow = NA, CIHigh = NA, Method = 'True') %>%
#                     unique(),
#                   PrevsYearRegion %>%
#                     select(Region, Year, Estimate = PrevMLE, CILow, CIHigh) %>%
#                     mutate(Method = 'PoolPrev (Frequentist)'),
#                   PrevsYearRegion %>%
#                     select(Region, Year, Estimate = PrevBayes, CILow = CrILow, CIHigh = CrIHigh) %>%
#                     mutate(Method = 'PoolPrev (Bayesian)'),
#                   getPrevalence(ModFreq)$PopulationEffects %>%
#                     mutate(Method = "PoolRegBayes"),
#                   getPrevalence(ModBayesHier2)$PopulationEffects %>%
#                     rename(CILow = CrILow, CIHigh = CrIHigh) %>%
#                     mutate(Method = "PoolRegBayes (Adjusted for hierarchy)"),
#                   HierPrevsYearRegion %>%
#                     select(Region, Year, Estimate = PrevBayes, CILow = CrILow, CIHigh = CrIHigh) %>%
#                     mutate(Method = "HierPoolPrev")) %>%
#   mutate(Region = factor(paste0("Region ", Region), levels = paste0("Region ",c("C","B","A"))),
#          Method = factor(Method, levels = c('PoolPrev (Frequentist)',
#                                             'PoolPrev (Bayesian)',
#                                             "HierPoolPrev",
#                                             "PoolRegBayes",
#                                             "PoolRegBayes (Adjusted for hierarchy)",
#                                             "True"))) %>%
#   unique() %>%
#   ggplot(aes(x = Year, y = Estimate,
#              ymin = CILow, ymax = CIHigh, shape = Method)) +
#   geom_pointrange(position = position_dodge(width = 0.3)) +
#   facet_grid(Region~.,scales = 'free_y') +
#   scale_y_log10() +
#   scale_x_continuous(breaks = 0:2) +
#   ylab("Prevalence") +
#   theme(text = element_text(size = 15),legend.position = "bottom") +
#   guides(shape = guide_legend(override.aes = list(linetype = 0)))
#
# VillagePO <- rbind(TruePrev %>%
#                      select(Region, Year, Village, Estimate = PrevalenceVillage) %>%
#                      mutate(CILow = NA, CIHigh = NA, Method = 'True'),
#                    PrevsYearRegionVillage %>%
#                      select(Region, Year, Village, Estimate = PrevMLE, CILow, CIHigh) %>%
#                      mutate(Method = 'PoolPrev (frequentist)'),
#                    PrevsYearRegionVillage %>%
#                      select(Region, Year, Village, Estimate = PrevBayes, CILow = CrILow, CIHigh = CrIHigh) %>%
#                      mutate(Method = 'PoolPrev (Bayesian)'),
#                    getPrevalence(ModFreqVillage)$PopulationEffects %>%
#                      mutate(Method = "PoolRegBayes",),
#                    getPrevalence(ModBayesHier2)$Village %>%
#                      rename(CILow = CrILow, CIHigh = CrIHigh) %>%
#                      mutate(Method = "PoolRegBayes (adjusted for hierarchy)"),
#                    HierPrevsYearRegionVillage %>%
#                      select(Region, Year, Village, Estimate = PrevBayes, CILow = CrILow, CIHigh = CrIHigh) %>%
#                      mutate(Method = "HierPoolPrev")) %>%
#   mutate(Region = factor(Region, levels = c("C","B","A")),
#          Method = factor(Method, levels = c('PoolPrev (frequentist)',
#                                             'PoolPrev (Bayesian)',
#                                             "HierPoolPrev",
#                                             "PoolRegBayes",
#                                             "PoolRegBayes (adjusted for hierarchy)",
#                                             "True"))) %>%
#   unique() %>%
#   mutate(VillageNum = stringr::str_split_fixed(Village, pattern = "-", n= 2)[,2]) %>%
#   subset(VillageNum %in% c(3,6,9)) %>%
#   mutate(Village = paste0("Village ",VillageNum),
#          Region = factor(paste0("Region ", Region), levels = paste0("Region ",c("C","B","A")))) %>%
#   ggplot(aes(x = Year, y = Estimate,
#              ymin = CILow, ymax = CIHigh, shape = Method)) +
#   geom_pointrange(position = position_dodge(width = 0.6)) +
#   facet_grid(Region~Village,scales = 'free_y') +
#   scale_y_log10() +
#   scale_x_continuous(breaks = 0:2) +
#   ylab("Prevalence") +
#   theme(text = element_text(size = 15),legend.position = "bottom") +
#   guides(shape = guide_legend(override.aes = list(linetype = 0)))
#
# RegionPO
#
# VillagePO

