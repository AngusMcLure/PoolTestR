# PoolTestR
Tools for working with pooled samples. Currently in early stages. Inspired by PoolScreen.Currently only has basic functionally reproducing PoolScreen functionality for R, however more features are planned or in the works: e.g. adjustments for imperfect test specificity/sensitivity; functions for helping with optimal experimental design; functions for inferring whether a disease has been locally eliminated from a series of pooled tests over time

## Installation

PoolTestR is not currently available from CRAN, but you can install it from github with:

```R
install.packages("devtools") #you can skip this if you already have devtools installed
devtools::install_github("AngusMcLure/PoolTestR")
```

## Usage

Start by loading the package:
```R
library(PoolTestR)
```

Now it's time to load up your data into R using read.csv() or a similar function. For this example we'll be making a fake dataset. Our fake dataset is from 4 locations (A, B, C, D) and 3 different dates (2000/1/1, 2001/1/1, 2002/1/1). We've taken a different number of pooled samples from each location and time period. The number of of specimiens/isolates/insects in each pool can be different. In our case there will be betwee 1 and 10 (there's no upper limit in practice). 

```R


Data <- data.frame(Place = sample(c("A","B","C","D"),1000, replace = T),
                   Date = sample(as.Date(c("2000/1/1","2001/1/1","2002/1/1")),1000, replace = T),
                   NumInPool = sample(1:10,1000, replace = T),
                   Result = sample(0:1,1000,replace = T)
                   )
head(Data) #Have a look at the form the data takes.                   
```
Now that we some data we begin by estimating the prevalence across the whole dataset i.e. a single prevalence for all locations and times

```R
# First argument is the data (in our case Data), the second argument is the name of the column in the data containing the result of the test (in our case Result). This column must be 0 for negative pool test and 1 for a positive pool test and does not accept missing values. The third argument is the name of the column with the number of specimiens/isolates/insect in each pool

PrevWholeDataset <- PoolPrev(Data, Result,NumInPool)

PrevWholeDataset # This contains both a maximum likelihood and a Bayesian (uniform prior) estimate of prevalence for the whole dataset
```
If we want to estimate prevalence seperately for each location we simply add the name of the column in the dataset holding the location the sample was taken from (in our case, Place) as an extra argument
```R
PrevByLocation <- PoolPrev(Data, Result,NumInPool,Place)
PrevByLocation # This contains estimates for every location in the dataset
```
Similarly if we want to estimate prevalence seperately for each time we simply add the name of the column in the dataset holding the time the sample taken (in our case, Date) as an extra argument
```R
PrevByTime <- PoolPrev(Data, Result,NumInPool,Date)
PrevByTime # This contains estimates for every time in the dataset
```
Finally if we want to estimate prevalence seperately for each time AND place we add the name of BOTH columns
```R
PrevByLocationAndTime <- PoolPrev(Data, Result,NumInPool,Place,Date)
PrevByLocationAndTime # This contains estimates for every time and location in the dataset
```
If we had more varaibles we wanted to group the data by (e.g. gender, species, age...) we could keep on adding the appropriate column names to the function call. There is no limit to the number of columns we can group our data by. But remember that the more groups you split your data into, the fewer samples in each group which can lead to wider confidence/credible intervals. 

```
