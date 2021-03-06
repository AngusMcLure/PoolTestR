#Try out on a synthetic dataset consisting of pools (sizes 1, 5, or 10) taken
#from 4 different regions and 3 different years. Within eaach region specimens
#are collected at 4 different villages, and within each village specimens are
#collected at 8 different sites.

\donttest{
  #Prevalence across the whole (synthetic) dataset
  PoolPrev(SimpleExampleData, Result, NumInPool)
  #Prevalence in each Region
  PoolPrev(SimpleExampleData, Result, NumInPool, Region)
  #Prevalence for each year
  PoolPrev(SimpleExampleData, Result, NumInPool, Year)
  #Prevalence for each combination of region and year
  PoolPrev(SimpleExampleData, Result, NumInPool, Region, Year)
}

