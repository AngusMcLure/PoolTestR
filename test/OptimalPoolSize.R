#Optimal pool size
library(tidyverse)
library(plotly)
#Assuming prefect sensitivity and specificity

information <- function(poolSize, prevalence){
  info <- poolSize^2 * (1 - prevalence)^(poolSize-2)/(1- (1- prevalence)^poolSize)
  info
}

InfoPerUnitCost <- function(poolSize, prevalence, sampleCost, testCost){
  IPUC <- poolSize^2 * (1 - prevalence)^(poolSize-2)/
    ((1 - (1-prevalence)^poolSize) * (sampleCost * poolSize + testCost))
  IPUC
}

InfoPerUnitCostMaster <- function(poolSize, poolNum, prevalence, sampleCost, testCost){
  ExpTests <- 1 + poolNum * (1-(1-prevalence)^(poolSize * poolNum))  #Expected number of tests
  IPUC <-  poolNum * poolSize^2 * (1 - prevalence)^(poolSize-2)/
    ((1 - (1-prevalence)^poolSize) * (sampleCost * poolSize * poolNum + ExpTests * testCost))
  IPUC
}


OptPoolSize <- function(prevalence, sampleCost, testCost){
  realOptim <- optimize(InfoPerUnitCost,
                        c(0,10/prevalence),
                        prevalence = prevalence,
                        sampleCost = sampleCost,
                        testCost = testCost,
                        maximum = TRUE)
  low <- floor(realOptim$maximum)
  high <- ceiling(realOptim$maximum)
  if(InfoPerUnitCost(low,prevalence, sampleCost, testCost) >
     InfoPerUnitCost(high,prevalence, sampleCost, testCost)){
    return(low)
  }else{
    return(high)
  }
}

OptPoolSize(0.01, 1, 1)

CostRatios <- c(0,0.001,0.01,0.1,0.5,1,2,5,10)
Prevalence <- c(0.001,0.005,1:10/100)

OptimalTable <- matrix(nrow = length(CostRatios), ncol = length(Prevalence))
for(n1 in 1:length(CostRatios)){
  for(n2 in 1:length(Prevalence)){
    OptimalTable[n1,n2] <- OptPoolSize(Prevalence[n2], CostRatios[n1],1)
  }
}
dimnames(OptimalTable) <- list(`Cost of Sampling One Mosquito: Cost of Testing One Pool` = paste0(CostRatios,":1"),
                               Prevalence = paste0(Prevalence * 100, "%"))
OptimalTable

write.csv(OptimalTable, file = "~/Desktop/test.csv")


TargetPoolSize <- function(units, targetSize, useRemainder = TRUE){
  numBigPools <- units %/% targetSize
  poolSizes <- rep(targetSize, numBigPools)
  remainder <- units %% targetSize
  if(remainder & useRemainder){
    poolSizes <- c(poolSizes, remainder)
  }
  poolSizes
}

FixedNumberPools <- function(units, number){
  minSizePools <- units %/% number
  numBigPools <- units %% number
  poolSizes <- c(rep(minSizePools + 1,numBigPools),
                 rep(minSizePools,number - numBigPools))
  poolSizes
}


StratInformation <- function(strategy, units, prevalence,...){
  poolSizes <- strategy(units, ...)
  info <- sum(information(poolSizes,prevalence))
  info
}

StratInformationPerUnitCost <- function(strategy, units, prevalence,
                                        sampleCost, testCost,
                                        masterpool = FALSE, ...){

  poolSizes <- strategy(units, ...)
  info <- sum(information(poolSizes, prevalence))
  N <- sum(poolSizes) #Total mosquito
  M <- length(poolSizes) # Total number of pools (not counting master)
  if(masterpool){
    ExpTests <- 1 + (1 - (1 - prevalence) ^ N) * M #Expected number of tests
    cost <- sum(poolSizes) * sampleCost + ExpTests *  testCost
  }else{
    cost <- sum(poolSizes) * sampleCost + M * testCost
  }
  info/cost
}


PlotStratInformationPerUnitCost <- function(units, prevalence,
                                        sampleCost, testCost,targetSize){
  IPUC <- vector(mode = "numeric", length = length(targetSize))
  n <- 0
  for(t in targetSize){
     n <- n + 1
    IPUC[n] <- StratInformationPerUnitCost(EqualPools,units, prevalence,
                                           sampleCost, testCost, targetSize = t)
  }
  plot(targetSize, IPUC)
}
StratInformationPerUnitCost(EqualPools, 100, 0.01, sampleCost = 0, testCost = 1,targetSize = 10)

PlotStratInformationPerUnitCost(1000, 0.01, 0, 1, 1:400)


plot(function(x){information(x, prevalence = 0.01)}, from = 1, to = 1000,n=1000)

plot(function(x){InfoPerUnitCost(x, prevalence = 0.01, sampleCost = 1, testCost = 1)}, from = 1, to = 1000,n=1000)

plot(function(x){StratInformationPerUnitCost(FixedNumberPools, x, 0.01, 0, 1, number = 3)}, from = 1, to = 5000,n=5000)

StratInformationPerUnitCost(FixedNumberPools, 2000, 0.01, 0, 1, number = 3)




#Thinking about master pools
PoolNum <- (1:100)
PoolSize <- (1:100)
Prevalence = 0.005
CostRatio = .1

InfTable <- matrix(nrow = length(PoolNum), ncol = length(PoolSize))
for(n1 in 1:length(PoolNum)){
  N <- PoolNum[n1]
  for(n2 in 1:length(PoolSize)){
    S <- PoolSize[n2]
    InfTable[n1,n2] <- InfoPerUnitCostMaster(S,N,Prevalence,sampleCost = CostRatio,
                                             testCost = 1)
  }
}

dimnames(InfTable) <- list(`Pool Number`= PoolNum,
                               `Pool Size` = PoolSize)

InfTable == max(InfTable)

(InfTable %>% as.data.frame() %>%
  mutate(PoolNum = PoolNum) %>%
  pivot_longer(-PoolNum, names_to = "PoolSize") %>%
  mutate(PoolSize = as.numeric(PoolSize)) %>%
  ggplot(aes(x = PoolNum, y = PoolSize, z = value, fill = value)) +
  geom_raster() +
  geom_contour(bins = 100) +
  ggtitle(paste0("Prevalence = ", Prevalence * 100, "%", "\n",
                "Cost Ratio = ", CostRatio))) %>%
  ggplotly()



OptPoolSizeNum <- function(prevalence, sampleCost, testCost, init = NULL){
  fn <- function(x){
    min(-InfoPerUnitCostMaster(x[1],x[2], prevalence, sampleCost, testCost),
        InfoPerUnitCost(x[1], prevalence, sampleCost, testCost))
  }
  if(is.null(init)){
    init = c(2/prevalence,3);
  }
  realOptim <- optim(par = init,
                     fn =fn,
                     lower = c(1,1),
                     upper = c(10/prevalence,20),
                     method = "L-BFGS-B")
  RealS <- realOptim$par[1]
  RealN <- realOptim$par[2]

  lowS <- max(floor(RealS),1); highS <- ceiling(RealS)
  lowN <- max(floor(RealN),1); highN <- ceiling(RealN)

  optS <- lowS; optN <- lowN;

  if(fn(c(lowS, highN)) < fn(c(optS, optN))){
    optS <- lowS; optN <- highN
  }
  if(fn(c(highS, lowN)) < fn(c(optS, optN))){
    optS <- highS; optN <- lowN
  }
  if(fn(c(highS, highN)) < fn(c(optS, optN))){
    optS <- highS; optN <- highN
  }

  ExpTests <- 1 + optN * (1-(1-prevalence)^(optS * optN))  #Expected number of tests
  if(ExpTests > optN){
    optN = 1
  }

  c(optS = optS, optN = optN)
}


CostRatios <- rev(c(0,0.001,0.01,0.1,0.5,1,2,5,10))
Prevalence <- c(0.001,0.005,1:10/100)

OptimalArray <- array(dim = c(length(CostRatios), length(Prevalence), 2 ))
InfoRatio <- array(dim = c(length(CostRatios), length(Prevalence)))

OptimalTable <- matrix(nrow = length(CostRatios), ncol = length(Prevalence))

for(n1 in 1:length(CostRatios)){
  for(n2 in 1:length(Prevalence)){
    if(n1>1){
      init = OptimalArray[n1-1,n2,]}
    else{init = NULL}
    OptimalTable[n1,n2] <- OptPoolSize(Prevalence[n2], CostRatios[n1],1)
    OptimalArray[n1,n2,] <- OptPoolSizeNum(Prevalence[n2], CostRatios[n1],1, init = init)
    InfoRatio[n1,n2] <- InfoPerUnitCostMaster(OptimalArray[n1,n2,1],
                                              OptimalArray[n1,n2,2],
                                              Prevalence[n2],
                                              CostRatios[n1],1)/
      InfoPerUnitCost(OptimalArray[n1,n2,1],Prevalence[n2], CostRatios[n1],1)
  }
}
dimnames(OptimalTable) <- list(`Cost of Sampling One Mosquito: Cost of Testing One Pool` = paste0(CostRatios,":1"),
                               Prevalence = paste0(Prevalence * 100, "%"))

dimnames(OptimalArray) <- list(`Cost of Sampling One Mosquito: Cost of Testing One Pool` = paste0(CostRatios,":1"),
                               Prevalence = paste0(Prevalence * 100, "%"),
                               c("Size", "Number"))
OptimalArray

InfoRatio


OptimalTable

