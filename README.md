# PoolTestR
Tools for working with pooled samples. Currently in early stages. Inspired by PoolScreen.Currently only has basic functionally reproducing PoolScreen functionality for R, however more features are planned or in the works: e.g. adjustments for imperfect test specificity/sensitivity; functions for helping with optimal experimental design; functions for inferring whether a disease has been locally eliminated from a series of pooled tests over time

## Installation

PoolTestR is not currently available from CRAN, but you can install it from github with:

```R
install.packages("devtools") #you can skip this if you already have devtools installed
devtools::install_github("AngusMcLure/PoolTestR")
```

## Usage

Below is a example with a fake dataset

```R
library(PoolTestR)

# Set up fake data to use. Replace this with your own data or import data from a csv with read.csv()
# Our fake data is from 4 locations ("A","B","C","D") and 3 different dates ("2000/1/1","2001/1/1","2002/1/1")
# and each pooled sample has between 1 and 10 (there's no upper limit in practice). 

Data <- data.frame(Place = sample(c("A","B","C","D"),1000, replace = T),
                   Date = sample(as.Date(c("2000/1/1","2001/1/1","2002/1/1")),1000, replace = T),
                   NumInPool = sample(1:10,1000, replace = T),
                   Result = sample(0:1,1000,replace = T)
                   )

# Use our fake data to estimate the prevalence across the whole dataset i.e. a single prevalence for all Locations and Times
# First argument is the data, second argument is the name of the column in the data containing the result of the test. This column must be 0 for negative pool test and 1 for a positive pool test and does not accept missing values.

PrevWholeDataset <- PoolPrev(Data, Result,NumInPool)

PrevWholeDataset # This contains both a maximum likelihood and a Bayesian (uniform prior) estimate of prevalence for the whole dataset

# If we want to estimate prevalence seperately for each location we simply add the name of the column in the dataset holding the location (in our case Place) data to the end:

PrevByLocation <- PoolPrev(Data, Result,NumInPool,Place)
PrevByLocation # This contains estimates for every location in the dataset

# Similarly if we want to estimate prevalence seperately for each time we simply add the name of the column in the dataset holding the time (in our case Date) data to the end:

PrevByTime <- PoolPrev(Data, Result,NumInPool,Date)
PrevByTime # This contains estimates for every time in the dataset

# Finally if we want to estimate prevalence seperately for each time and place we add the name of both columns

PrevByLocationAndTime <- PoolPrev(Data, Result,NumInPool,Place,Date)
PrevByLocationAndTime # This contains estimates for every time and location in the dataset

#If we had more varaibles we wanted to group the data by (e.g. Gender or Species) we could keep on adding the appropriate column names. There is no limit to the number of columns we can group our data by. But remember that the more groups you split your data into, the smaller your groups will be and so confidence intervals and credible intervals will get wider. 

```
