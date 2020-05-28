#Fake data with no trend
pkgbuild::compile_dll(force =T)
pkgbuild::compile_dll()
roxygen2::roxygenize()
devtools::check()

N <- 200

Data <- data.frame(Place = sample(c("A","B","C","D"),N, replace = T),
                   Year = sample(c(2000,2001,2002),N, replace = T),
                   NumInPool = sample(10:30,N, replace = T),
                   Result = sample(0:1,N,replace = T)
                   )

#Simulated data with the same trend but different baselines in every place

DataTrend <- data.frame(Place = sample(c("A","B","C","D"),N, replace = T),
                        Year = sample(c(0:2), N, replace = T),
                        NumInPool = sample(10:25, N, replace = T)
                        )
BaseOdds <- c(A = 0.1, B = 0.05, C = 0.002, D = 0.1)
GrowthRate = -0.5 #This is equivalent to an odds ratio for year of exp(-0.5) = 0.6065
DataTrend$TrueOdds <- with(DataTrend,BaseOdds[Place] * exp(GrowthRate*(Year-min(Year))))
DataTrend$TruePrev <- with(DataTrend, TrueOdds/(1+TrueOdds))
DataTrend$Result <- with(DataTrend,as.numeric(runif(N) < 1-(1-TruePrev)^NumInPool))

PrevsYearPlaceTrend <- PoolPrev(DataTrend,Result,NumInPool,Place,Year,prior.absent = 0.05)
PrevsYearPlaceTrend

#This has issues - the model matrix regularisation doesn't work that well here.
#This makes it slow to run, but should still have correct result.
#A way round this would be to do a change of varaibles to make 2000 year 0.
#RegFull <- PooledLogitRegression(DataTrend,"NumInPool",Result ~ Place*Year,verbose = T)
#RegFull
#exp(rstan::summary(RegFull$fit)$summary)

#Bayesian
BRegYearPlaceTrend <- BayesPoolLogitReg(DataTrend,"NumInPool",Result ~ Year + Place,verbose = T)
BRegYearPlaceTrend
exp(rstan::summary(BRegYearPlaceTrend$fit)$summary)
#Frequentist
FRegYearPlaceTrend <- PoolLogitReg(DataTrend,Result ~ Year + Place,NumInPool)
FRegYearPlaceTrend
summary(FRegYearPlaceTrend)
exp(cbind(Estimate = coefficients(FRegYearPlaceTrend), confint(FRegYearPlaceTrend)))

#Compare with standard logistic regression (i.e. assuming all pool sizes are 1)
FLogit <- glm(Result ~ Year + Place,data = DataTrend,family = 'binomial')
summary(FLogit)
exp(cbind(Estimate = coefficients(FLogit), confint(FLogit)))

BLogit <- rstanarm::stan_glm(formula = Result ~ Year + Place,family = binomial(), data = DataTrend)
summary(BLogit)
