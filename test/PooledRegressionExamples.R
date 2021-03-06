pkgbuild::compile_dll()
roxygen2::roxygenize()

### Create a simulated dataset with 4 locations across 5 years, where prevalence is declining
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

### Fit modified logistic regression model

#The below two lines produce identical results and support all the same methods
Reg <- PoolLogitReg(Data,
                    Result ~ Place + Year,
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
Comparison <- Data %>% select(Place, Year, TruePrev) %>% unique()
Comparison <- Comparison[with(Comparison,order(Place, Year)),]
Comparison$PredictPrev <- plogis(predict(Reg, newdata = Comparison))
Comparison

#You can also use the fitted model to predict the prevalence at other times
DataPredict <- expand.grid(Place = c("A","B","C","D"),Year = seq(2,3,by = 0.2)) #Times and places to predict
DataPredict$PredictPrev <- plogis(predict(Reg, newdata = DataPredict)) #Predicted prevalence
DataPredict

# Note that predicting the response using predict(type = "response")
# (i.e. the predicting the probability of observing a positive test)
# does not work as expected on new data. Use the following instead:

#Generate new random pool sizes
DataPredict$NumInPool <- sample(5:10,nrow(DataPredict),replace = T)
#Predicted probability of test being positive based on predicted prevalence and the size of the pool
DataPredict$PredictTestProb <- with(DataPredict, 1 - (1-PredictPrev)^NumInPool)
DataPredict
library(ggplot2)
PrevsYearPlace <- PoolPrev(Data, Result,NumInPool,Place,Year)
PO <- PrevsYearPlace %>%
  mutate(BaseOdds = BaseOdds[Place],
         TrueOdds = BaseOdds * OddsRatioYear^(Year-min(Year)),
         TruePrev = TrueOdds/(1+TrueOdds),
         FreqPred = plogis(predict(Reg,newdata = PrevsYearPlace)),
         FreqPredCILow = with(predict(Reg,
                                      newdata = PrevsYearPlace,
                                      se.fit =T),
                              fit - se.fit * 1.96) %>% plogis,
         FreqPredCIHigh = with(predict(Reg,
                                       newdata = PrevsYearPlace,
                                       se.fit =T),
                               fit + se.fit * 1.96) %>% plogis
  ) %>%
  ggplot() +
  geom_pointrange(aes(x = Year,
                      color = Place,
                      y = PrevMLE,
                      ymin = CILow,
                      ymax = CIHigh)) +
  geom_line(aes(x = Year,
                y= FreqPred,
                color = Place)) +
  geom_ribbon(aes(x = Year,
                  ymin = FreqPredCILow,
                  ymax = FreqPredCIHigh,
                  fill = Place),
              alpha = 0.3) +
  geom_point(aes(x = Year, color = Place, y = TruePrev),
             shape = 4,
             size = 3) +
  #scale_y_log10() +
  ylab('Prevalence')
PO
