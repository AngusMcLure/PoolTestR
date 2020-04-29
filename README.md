# PoolTestR
Tools for working with pooled samples inspired by PoolScreen. The package is currently in early stages. Currently the pacakge only has core functionally, however more features are planned or in the works: e.g. adjustments for imperfect test specificity/sensitivity; functions for helping with optimal experimental design; functions for inferring whether a disease has been locally eliminated from a series of pooled tests over time. Suggestions are welcome.

## Installation

PoolTestR is not currently available from CRAN, but you can install it from github. Thankfully this can be done easily from within R.

### Windows only
If you are installing onto a windows machine you may need to adjust some settings in your Makevars file so that you can compile the package. If you regularly install R packages from source and are comfortable configuring this yourself, see [this link](https://github.com/stan-dev/rstan/wiki/Installing-RStan-from-source-on-Windows) for tips on how to do thus. Otherwise, just copy-paste and run the following code in R:

```R
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars.win")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=corei7 -mtune=corei7",
    "CXX14 = $(BINPREF)g++ -m$(WIN) -std=c++1y",
    "CXX11FLAGS=-O3 -march=corei7 -mtune=corei7",
    file = M, sep = "\n", append = TRUE)
```
You should now be able to proceed with the rest of the installation.

### All operating systems
The following commands in R should now install the package: (This will probably produce a lot of warning messages - these are related to the compilation of stan source code and I unfortunately cannot stop them for you)
```R
install.packages("devtools") #you can skip this if you already have devtools installed
devtools::install_github("AngusMcLure/PoolTestR")
```

## Usage

Start by loading the package: (If this doesn't work this means you were unable to install the package. Please let [me](mailto:angus.mclure@anu.edu.au) know if this happens for you)
```R
library(PoolTestR)
```

Now it's time to load up your data into R using read.csv() or a similar function. For this example we'll be making a fake dataset. Our fake dataset is from 4 locations (A, B, C, D) and 3 different dates (2000/1/1, 2001/1/1, 2002/1/1). We've taken a different number of pooled samples from each location and time period. The number of of specimiens/isolates/insects in each pool can be different. In our case there will be between 1 and 10 (there's no upper limit in practice). 

```R


Data <- data.frame(Place = sample(c("A","B","C","D"),1000, replace = T),
                   Date = sample(as.Date(c("2000/1/1","2001/1/1","2002/1/1")),1000, replace = T),
                   NumInPool = sample(1:10,1000, replace = T),
                   Result = sample(0:1,1000,replace = T)
                   )
head(Data) #Have a look at the form the data takes.                   
```
Now that we some data we begin by estimating the prevalence across the whole dataset i.e. a single prevalence across all locations and times

```R
# First argument is the data (in our case Data).
# The second argument is the name of the column in the data containing the result of the test (in our case Result):
# this column must be 0 for negative pool test and 1 for a positive pool test and does not accept missing values.
# The third argument is the name of the column with the number of specimiens/isolates/insect in each pool

PrevWholeDataset <- PoolPrev(Data, Result,NumInPool)

PrevWholeDataset 
```
The output contains a maximum likelihood estimate of the prevalece with 95% confidence intervals and a Bayesian (uniform prior) estimate of prevalence with 95% credible intervals. It also contains the total number of pools and the number of these that were positive.

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
