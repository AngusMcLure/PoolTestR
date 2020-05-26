#Fake data with no trend
pkgbuild::compile_dll(force =T)
roxygen2::roxygenize()
devtools::check()

N <- 100

Data <- data.frame(Place = sample(c("A","B","C","D"),N, replace = T),
                   Year = sample(c(2000,2001,2002),N, replace = T),
                   NumInPool = sample(10:30,N, replace = T),
                   Result = sample(0:1,N,replace = T)
                   )

#Simulated data with the same trend but different baselines in every place

DataTrend <- data.frame(Place = sample(c("A","B","C","D"),N, replace = T),
                        Year = sample(c(0:2), N, replace = T),
                        NumInPool = sample(20:25, N, replace = T)
                        )
BaseOdds <- c(A = 0.1, B = 0.05, C = 0.002, D = 0.1)
GrowthRate = -0.5 #This is equivalent to an odds ratio for year of exp(-0.5) = 0.6065
DataTrend$TrueOdds <- with(DataTrend,BaseOdds[Place] * exp(GrowthRate*(Year-min(Year))))
DataTrend$TruePrev <- with(DataTrend, TrueOdds/(1+TrueOdds))
DataTrend$Result <- with(DataTrend,as.numeric(runif(N) < 1-(1-TruePrev)^NumInPool))

PrevsYearPlaceTrend <- PoolPrev(DataTrend,Result,NumInPool,Place,Year,prior.absent = 0.05)
PrevsYearPlaceTrend

RegPlace <- PooledLogitRegression(DataTrend,"NumInPool",Result ~ Place,verbose = T)
RegPlace
exp(rstan::summary(RegPlace$fit)$summary)

#This has issues - the model matrix regularisation doesn't work that well here.
#This makes it slow to run, but should still have correct result.
#A way round this would be to do a change of varaibles to make 2000 year 0.
#RegFull <- PooledLogitRegression(DataTrend,"NumInPool",Result ~ Place*Year,verbose = T)
#RegFull
#exp(rstan::summary(RegFull$fit)$summary)


RegYearATrend <- PooledLogitRegression(subset(DataTrend,Place == "A"),"NumInPool",Result ~ Year,verbose = T)
RegYearATrend
exp(rstan::summary(RegYearATrend$fit)$summary)

RegYearBTrend <- PooledLogitRegression(subset(DataTrend,Place == "B"),"NumInPool",Result ~ Year,verbose = T)
RegYearBTrend
exp(rstan::summary(RegYearBTrend$fit)$summary)


RegYearPlaceTrend <- PooledLogitRegression(DataTrend,"NumInPool",Result ~ Year + Place,verbose = T)
RegYearPlaceTrend
exp(rstan::summary(RegYearPlaceTrend$fit)$summary)


test <- glm(Result ~ Year + Place,data = DataTrend,family = 'binomial')
test
rstanarmTest <- rstanarm::stan_glm(formula = Result ~ Year + Place,family = binomial(), data = DataTrend)
rstanarmTest
