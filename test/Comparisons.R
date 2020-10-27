library(dplyr)
NumPools <- 1000
#Odds that individual sample is positive in each location in the first year
BaseOdds <- c(A = 0.16, B = 0.04, C = 0.01, D = 0.16)
OddsRatioYear <- 0.8
#Randomly distribute pools between the 4 locations and 5 years,
#and chose random pool sizes between 10 and 25
Data <- data.frame(Place = sample(c("A","B","C","D"),NumPools, replace = T),
                   Year = sample(c(0:4), NumPools, replace = T),
                   NumInPool = sample(10:25, NumPools, replace = T))
#'True' odds/prevalence in each location
Data$TrueOdds <- with(Data,BaseOdds[Place] * OddsRatioYear^(Year-min(Year)))
Data$TruePrev <- with(Data, TrueOdds/(1+TrueOdds))
#Simulate test results on pools
Data$Result <- with(Data,as.numeric(runif(NumPools) < 1-(1-TruePrev)^NumInPool))


DataCount <- with(Data,table(Result, NumInPool)) %>% as.matrix
DataCount[2,]

binGroup2::propCI(DataCount[1,], as.integer(colnames(DataCount)),colSums(DataCount), ci.method = "exact")
