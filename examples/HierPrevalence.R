# Calculate prevalence for a synthetic dataset consisting of pools (sizes 1, 5,
# or 10) taken from 4 different regions and 3 different years. Within each
# region specimens are collected at 4 different villages, and within each
# village specimens are collected at 8 different sites.

\dontrun{
  #Prevalence for each combination of region and year:
  #ignoring hierarchical sampling frame within each region
  PoolPrev(SimpleExampleData, Result, NumInPool, Region, Year)
  #accounting hierarchical sampling frame within each region
  HierPoolPrev(SimpleExampleData, Result, NumInPool, c("Village","Site"), Region, Year)
}


