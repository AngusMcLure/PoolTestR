# Calculate prevalence for a synthetic dataset consisting of pools (sizes 1, 5,
# or 10) taken from 3 different years. Specimens are collected at 16 different
# villages, and within each village specimens are collected at 8 different
# sites.

\donttest{
  #Prevalence for each year:
  #ignoring hierarchical sampling frame be:
  PoolPrev(SimpleExampleData, Result, NumInPool, Year)
  #accounting hierarchical sampling frame within each region
  HierPoolPrev(SimpleExampleData, Result, NumInPool, c("Village","Site"), Year)
}


