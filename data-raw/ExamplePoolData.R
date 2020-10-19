library(dplyr)
set.seed(as.integer(as.Date("2020/10/15")))

ExamplePoolData <- expand.grid(NumInPool = c(1,5,10),
                               Site = as.character(1:8),
                               Village = as.character(1:4),
                               Year = 0:2,
                               Region = c("A","B","C","D")) %>%
  dplyr::mutate(Village = paste(Region, Village, sep = '-')) %>%
  dplyr::mutate(Site = paste(Village, Site, sep = '-'))


BaseOddsRegion <- c(A = 0.16, B = 0.04, C = 0.01, D = 0.16)
BaseOddsVillage <- rlnorm(length(unique(ExamplePoolData$Village)),sdlog = 0.2)
names(BaseOddsVillage) <- unique(ExamplePoolData$Village)
BaseOddsSite <- rlnorm(length(unique(ExamplePoolData$Site)),sdlog = 0.2)
names(BaseOddsSite) <- unique(ExamplePoolData$Site)

OddsRatioYear <- 0.8

#'True' odds/prevalence in each location
ExamplePoolData$TrueOdds <- with(ExamplePoolData,
                                 BaseOddsRegion[Region] *
                                   BaseOddsVillage[Village] *
                                   BaseOddsSite[Site] *
                                   OddsRatioYear^(Year-min(Year)))
ExamplePoolData$TruePrev <- with(ExamplePoolData, TrueOdds/(1+TrueOdds))

#Simulate test results on pools
ExamplePoolData$Result <- with(ExamplePoolData,
                               as.numeric(runif(nrow(ExamplePoolData)) < 1-(1-TruePrev)^NumInPool))
ExamplePoolData <- ExamplePoolData %>% dplyr::select(-TrueOdds,-TruePrev)

usethis::use_data(ExamplePoolData, overwrite = TRUE)


Prev <- PoolPrev(ExamplePoolData, Result, NumInPool, Year, Region)
#HierPrev <- HierPoolPrev(ExamplePoolData, Result, NumInPool, c('Village', 'Site'), Year, Region)
Reg <- PoolReg(Result ~ Region + Year + (1|Village/Site), ExamplePoolData, NumInPool)
#RegBayes <- PoolRegBayes(Result ~ Region + Year + (1|Village/Site), ExamplePoolData, NumInPool)
