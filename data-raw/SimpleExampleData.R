library(dplyr)
set.seed(as.integer(as.Date("2020/10/15")))

SimpleExampleData <- expand.grid(NumInPool = c(1,5,10),
                               Site = as.character(1:8),
                               Village = as.character(1:4),
                               Year = 0:2,
                               Region = c("A","B","C","D")) %>%
  dplyr::mutate(Village = paste(Region, Village, sep = '-')) %>%
  dplyr::mutate(Site = paste(Village, Site, sep = '-'))


BaseOddsRegion <- c(A = 0.16, B = 0.04, C = 0.01, D = 0.16)
BaseOddsVillage <- rlnorm(length(unique(SimpleExampleData$Village)),sdlog = 0.2)
names(BaseOddsVillage) <- unique(SimpleExampleData$Village)
BaseOddsSite <- rlnorm(length(unique(SimpleExampleData$Site)),sdlog = 0.2)
names(BaseOddsSite) <- unique(SimpleExampleData$Site)

OddsRatioYear <- 0.8

#'True' odds/prevalence in each location
SimpleExampleData$TrueOdds <- with(SimpleExampleData,
                                 BaseOddsRegion[Region] *
                                   BaseOddsVillage[Village] *
                                   BaseOddsSite[Site] *
                                   OddsRatioYear^(Year-min(Year)))
SimpleExampleData$TruePrev <- with(SimpleExampleData, TrueOdds/(1+TrueOdds))

#Simulate test results on pools
SimpleExampleData$Result <- with(SimpleExampleData,
                               as.numeric(runif(nrow(SimpleExampleData)) < 1-(1-TruePrev)^NumInPool))
SimpleExampleData <- SimpleExampleData %>% dplyr::select(-TrueOdds,-TruePrev)

usethis::use_data(SimpleExampleData, overwrite = TRUE)
