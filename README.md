# PoolTestR
This is an R package with tools for working with pooled samples, initially inspired by the stand-alone software, PoolScreen. The package is currently in early stages. The package can be used to perform the same analyses as PoolScreen i.e. estimate the prevalence of a marker in the population based on tests performed on pooled samples. In our pacakge, the estimates of prevalence can also be adjusted to account for the hierarchical sampling designs that are often used for xenomonitoring studies. Taking this a step further, our package enable users to perform to logistic-type regression (with fixed or mixed effects) to identify covariates associated with the outcome of interest, estimate odds ratios, and predict prevalence. All analyses are available in both frequentist and Bayesian frameworks. 

More features are planned or in the works: spatial mapping; models for combining and comparing results from human-based and vector-based surveillance, adjustments for imperfect test specificity/sensitivity; functions for helping with optimal experimental design; functions for inferring whether a disease has been locally eliminated from a series of pooled tests over time. Suggestions are welcome.

If you find this package helpful, please [let us know](mailto:angus.mclure@anu.edu.au) -- we'd love to hear how it's being used. The details of our package are soon to be published as an academic journal article. Once available, please cite this paper if you use this package in your work.

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

The following provides a working example how to use the core functionality of the package to estimate prevalence. You can copy and paste the code into your console to see it run. Examples of the additional functionality will be given in an upcoming academic publication.

Start by loading the package: (If this doesn't work this means installation was not successful. Please [let us know](mailto:angus.mclure@anu.edu.au) if this happens for you)
```R
library(PoolTestR)
```

Now it's time to load up data into R using `read.csv()` or a similar function. Each line of the dataset should contain the details of a single pooled test. At minimum this must include:
-   the the number of specimens in the pool
-   and the result of the test

Instead of working with real data, for this example we'll be using a simulated dataset.  For our simulated data with have variables for:
- the place the samples where collected (A, B, C, or D);
- the date it was collected (2000/1/1, 2001/1/1, or 2002/1/1);
- and the species of the specimens (X or Y).

In our simulated dataset there are about around 40 pools taken from each of the 24 combinations of species, place, and date. In our case there will be between 1 and 10 (there's no upper limit in general).

This code generates a synthetic dataset for our example:

```R
Data <- data.frame(Place = sample(c("A","B","C","D"),1000, replace = T),
                   Species = sample(c("X","Y"), 1000, replace = T),
                   Date = sample(as.Date(c("2000/1/1","2001/1/1","2002/1/1")),1000, replace = T),
                   NumInPool = sample(1:10,1000, replace = T),
                   Result = sample(0:1,1000,replace = T)
                   )
head(Data) #Have a look at the form the data takes.                   
```
Now that we some data, we begin by estimating the prevalence across the whole dataset i.e. a single prevalence for all locations, times and species

```R
# First argument is the data (in our case Data).
# The second argument is the name of the column in the data containing the result of the test (in our case Result):
# this column must be 0 for negative pool test and 1 for a positive pool test and does not accept missing values.
# The third argument is the name of the column with the number of specimiens/isolates/insects in each pool

PrevWholeDataset <- PoolPrev(Data, Result, NumInPool)

PrevWholeDataset 
```
The output contains a maximum likelihood estimate of the prevalece with 95% confidence intervals and a Bayesian estimate (Jeffrey's prior) with 95% credible intervals. It also contains the total number of pools and the number of these that were positive.

If we want to estimate prevalence seperately for each location we simply add the name of column in our dataset that holds the location data. In our case this the column called `Place`:
```R
PrevByLocation <- PoolPrev(Data, Result, NumInPool, Place)
PrevByLocation
```
Similarly if we want to estimate prevalence seperately for each sample time (but ignoring differences between places and species) we simply add `Date` as an extra argument:
```R
PrevByTime <- PoolPrev(Data, Result, NumInPool, Date)
PrevByTime
```
If we want to estimate prevalence seperately for each combination of time AND place (but ignoring differences between species):
```R
PrevByLocationAndTime <- PoolPrev(Data, Result, NumInPool, Place, Date)
PrevByLocationAndTime
```
If instead we want to estimate prevalence seperately for each combination of species AND place (but ignoring differences over time):
```R
PrevByLocationAndSpecies <- PoolPrev(Data, Result, NumInPool, Place, Species)
PrevByLocationAndSpecies 
```
Finally, if we want to estimate prevlance for every combination of species, place, and date:
```R
PrevByLocationDateSpecies <- PoolPrev(Data, Result, NumInPool, Place, Species, Date)
PrevByLocationDateSpecies
```

If we had more varaibles we wanted to group the data by (e.g. climate, season, sex, age...) we could keep on adding the appropriate column names to the function call. But remember that the more groups you split your data into, the fewer samples in each group which can lead to wider confidence/credible intervals.

Most xenomonitoring studies involve a hierarchical sampling design: vector traps are placed at randomly or systematically chosen sites around the area of interest. As there is likely to be some spatial variation in prevalence between sites it is best practice to use a hierarchical or spatially autocorrelated model to estimate prevalence in the area. Failing to do so may produce biased or overconfident estimates of the prevalence (i.e. confidence intervals will be unreasonably narrow). The details of how to use these more advanced methods with our package will be provided in upcoming publications.
