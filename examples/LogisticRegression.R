#Simulated data with the same trend but different baselines in every place
N <- 1000
DataTrend <- data.frame(Place = sample(c("A","B","C","D"),N, replace = T),
                        Year = sample(c(0:2), N, replace = T),
                        NumInPool = sample(1:100, N, replace = T)
)
BaseOdds <- c(A = 0.1, B = 0.05, C = 0.002, D = 0.1)
GrowthRate = -0.5 #This is equivalent to an odds ratio for year of exp(-0.5) = 0.6065
DataTrend$TrueOdds <- with(DataTrend,BaseOdds[Place] * exp(GrowthRate*(Year-min(Year))))
DataTrend$TruePrev <- with(DataTrend, TrueOdds/(1+TrueOdds))
DataTrend$Result <- with(DataTrend,as.numeric(runif(N) < 1-(1-TruePrev)^NumInPool))

#The below two lines produce identical results and support all the same methods
Reg <- PoolLogitReg(DataTrend,
                    Result ~ Place + Year,
                    NumInPool)
Reg.glm <- glm(Result ~ Place + Year,
               data = DataTrend,
               family = binomial(PoolLink(DataTrend$NumInPool)))
#View summary of model
summary(Reg)

#Estimate and confidence intervals for the base odds and odds ratios
#These should be approx 0.1 for the intercept (i.e. Place A, Year 0),
#0.5, 0.02, and 1.0 for places B-D and 0.6 for Year
exp(cbind(Estimate = summary(Reg)$coefficients[,'Estimate'], confint(Reg)))

#Compare the predicted prevalence at each location and time
Comp <- DataTrend %>% select(Place, Year, TruePrev) %>% unique()
Comp <- Comp[with(Comp,order(Place, Year)),]
Comp$eta <- predict(Reg, newdata = Comp)
Comp$PredictPrev <- plogis(predict(Reg, newdata = Comp))
Comp

#You can also use the fitted model predict the prevalence at other times
DataPredict <- expand.grid(Place = c("A","B","C","D"),Year = seq(2,3,by = 0.2)) #Times and places to predict
DataPredict$PredictPrev <- plogis(predict(Reg, newdata = DataPredict)) #Predicted prevalence
DataPredict

# Note that predicting the response using predict(type = "response")
# (i.e. the probability of observing a positive test) does not work
# as expected on new data. Use the following instead:

#Generating random pool sizes
DataPredict$NumInPool <- sample(5:10,nrow(DataPredict),replace = T)
#Predicted probability of test being positive based on predicted prevalence and the size of the pool
DataPredict$PredictTestProb <- with(DataPredict, 1 - (1-PredictPrev)^NumInPool)
DataPredict

