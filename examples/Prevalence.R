#Build a synthetic dataset with 1000 pools taken from 4 different locations and 3 different times
Data <- data.frame(Place = sample(c("A","B","C","D"),1000, replace = TRUE),
                   Date = sample(as.Date(c("2000/1/1","2001/1/1","2002/1/1")),1000, replace = TRUE),
                   NumInPool = sample(1:10,1000, replace = TRUE),
                   Result = sample(0:1,1000,replace = TRUE)
)
#Prevalence across the whole (synthetic) dataset
PoolPrev(Data, Result, NumInPool)
#Prevalence at each location
PoolPrev(Data, Result, NumInPool, Place)
#Prevalence for each time period
PoolPrev(Data, Result, NumInPool, Date)
#Prevalence for each combination of location and time-period
PoolPrev(Data, Result, NumInPool, Place, Date)
