### Create a simulated dataset with 4 locations across 5 years, where prevalence is declining
NumPools <- 1000
#Odds that individual sample is positive in each location in the first year
BaseOdds <- c(A = 0.16, B = 0.04, C = 0.01, D = 0.16)
OddsRatioYear <- 0.8
#Randomly distribute pools between the 4 locations and 5 years,
#and chose random pool sizes between 10 and 25
Data <- data.frame(Place = sample(c("A","B","C","D"),NumPools, replace = TRUE),
                   Year = sample(c(0:4), NumPools, replace = TRUE),
                   NumInPool = sample(10:25, NumPools, replace = TRUE))
#'True' odds/prevalence in each location
Data$TrueOdds <- with(Data,BaseOdds[Place] * OddsRatioYear^(Year-min(Year)))
Data$TruePrev <- with(Data, TrueOdds/(1+TrueOdds))
#Simulate test results on pools
Data$Result <- with(Data,as.numeric(runif(NumPools) < 1-(1-TruePrev)^NumInPool))

### Fit modified logistic regression model

#The below two lines produce identical results and support all the same methods
Reg <- PoolReg(Result ~ Place + Year,
               data = Data,
               NumInPool)
Reg.glm <- glm(Result ~ Place + Year,
               data = Data,
               family = binomial(PoolLink(Data$NumInPool)))
#View summary of model
summary(Reg)

#Estimate and confidence intervals for the base odds and odds ratios
#These should be approx 0.16 for the intercept (i.e. Place A, Year 0),
#0.25, 0.0625, and 1.0 for places B-D and 0.8 for Year
exp(cbind(Estimate = coefficients(Reg), confint(Reg)))

#Compare the predicted prevalence at each location and time
Comparison <-  unique(dplyr::select(Data, Place, Year, TruePrev))
Comparison <- Comparison[with(Comparison,order(Place, Year)),]

Comparison$PredictPrev <- plogis(predict(Reg, newdata = Comparison))
Comparison

#You can also use the fitted model to predict the odds/prevalence at other times:

#Generate a set of times and places to predict on - note that we don't need to
#provide pool sizes for the new data as we are predicting the prevalence in
#individual specimens
DataPredict <- expand.grid(Place = c("A","B","C","D"),
                           Year = seq(2,3,by = 0.2))
#Predicted odds at the new times
DataPredict$PredictOdds <- predict(Reg, newdata = DataPredict)
#Predicted prevalence at the new times
DataPredict$PredictPrev <- plogis(predict(Reg, newdata = DataPredict))
DataPredict

#Note that predicting the response on new data using predict(type = "response")
#(i.e. the predicting the probability of observing a positive test) does not
#work as expected on new data, as there is currently no way of getting predict
#to account for the new pool sizes. However it is easy to calculate the response
#from the prevalence of individuals:

#Generate new random pool sizes
DataPredict$NumInPool <- sample(5:10,nrow(DataPredict),replace = TRUE)
#Predicted probability of test being positive based on predicted prevalence and the size of the pool
DataPredict$PredictTestProb <- 1 - (1 - DataPredict$PredictPrev) ^ DataPredict$NumInPool
DataPredict

