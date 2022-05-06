# PoolTestR
PoolTestR is an R package with tools for working with pooled or grouped samples, initially inspired by the stand-alone software, 'PoolScreen'. The package can be used to perform the same analyses as PoolScreen i.e. estimate the prevalence of a marker in the population based on tests performed on pooled samples. In our package, the estimates of prevalence can also be adjusted to account for the hierarchical sampling designs that are often used for molecular xenomonitoring studies. Taking this a step further, our package enable users to perform to mixed effect regression to identify covariates associated with the outcome of interest, estimate odds ratios, and predict prevalence. Analyses are available in both frequentist and Bayesian frameworks. 

More features are planned or in the works: spatial mapping; models for combining and comparing results from human-based and vector-based surveillance, adjustments for imperfect test specificity/sensitivity; functions for helping with optimal experimental design; functions for inferring whether a disease has been locally eliminated from a series of pooled tests over time. Suggestions and contributions are welcome.

If you find this package helpful, please [let us know](mailto:angus.mclure@anu.edu.au) -- we'd love to hear how it's being used. The details of our package are described in an [article published in Environmental Modelling and Software](https://doi.org/10.1016/j.envsoft.2021.105158). We are happy to share a pdf of this article with you [on request](mailto:angus.mclure@anu.edu.au).

## Installation

PoolTestR is now available on the official repository of R packages, CRAN, so can now be installed like any standard package; i.e. using the package manager in RStudio or the command:

```R
install.packages("PoolTestR")
```

You can also install the most up-to-date version from github:

```R
install.packages("devtools") #you can skip this if you already have devtools installed
devtools::install_github("AngusMcLure/PoolTestR")
```

If installing from source (the only option if installing from github) this will probably produce a lot of warning messages in your R console - these are related to the compilation of stan source code and can be safely ignored! If installing from CRAN this shoudn't be an issue.

Note: In the past, some windows users needed to adjust some settings (in the Makevars file) to install the package correctly, because of issues in other packages on which PoolTestR relies. These other packages have addressed these issues in most cases, so manual adjustments *shouldn't* be needed. However, future updates may break this. Please [let us know](mailto:angus.mclure@anu.edu.au) if you have issues with installation. 

## Usage

The following provides some examples of how to use the core functionality of the package to estimate prevalence. You can copy and paste the code into your R console to see it run.

Start by loading the package: (If this doesn't work this means installation was not successful. Please [let us know](mailto:angus.mclure@anu.edu.au) if this happens for you)
```R
library(PoolTestR)
```

Now it's time to load up data into R using `read.csv()` or a similar function. Each line of the dataset should contain the details of a single pooled test. At minimum this must include:
 * the the number of specimens in the pool
 * and the result of the test

Instead of working with real data, for this example we'll be using a simulated dataset called ```SimpleExampleData``` that's included with the package. The synthetic dataset consists of pools (sizes 1, 5, or 10) taken from 4 different regions and 3 different years. Within each region specimens are collected at 4 different villages, and within each village specimens are collected at 8 different sites, i.e. a hierarchical sampling frame. Take a look at the first few rows of the data:

```R
head(SimpleExampleData)
```

We begin by using the function ```PoolPrev``` to estimate the prevalence across the whole dataset i.e. a single prevalence for all locations and years. The first argument to ```PoolPrev``` is the data (in our case ```SimpleExampleData```). The second argument is the name of the column in the data containing the result of the test (in our case ```Result```): the entries in this column must be 0 for negative pool tests and 1 for a positive pool tests and cannot be missing. The third argument is the name of the column with the number of specimens/isolates/insects in each pool (in our case ```NumInPool```)

```R
PrevWholeDataset <- PoolPrev(SimpleExampleData,Result,NumInPool)

PrevWholeDataset 
```
The output contains a maximum likelihood estimate of the prevalence with 95% confidence intervals and a Bayesian estimate (Jeffrey's prior) with 95% credible intervals. It also contains the total number of pools and the number of these that were positive.

If we want to estimate prevalence separately for each region we simply include ```Region``` as an additional argument
```R
PrevByRegion <- PoolPrev(SimpleExampleData, Result, NumInPool, Region)
PrevByRegion
```
If we want to estimate prevalence at the level of villages:
```R
PrevByVillage <- PoolPrev(SimpleExampleData, Result, NumInPool, Village)
PrevByVillage
```
Similarly if we want to estimate prevalence separately for each year (but ignoring differences between places):
```R
PrevByYear <- PoolPrev(SimpleExampleData, Result, NumInPool, Year)
PrevByYear
```
If we want to estimate prevalence separately for each combination of region AND year:
```R
PrevByRegionYear <- PoolPrev(SimpleExampleData, Result, NumInPool, Region, Year)
PrevByRegionYear
```

If we had more variables we wanted to group the data by (e.g. climate, season, sex, age...) we could keep on adding the appropriate column names to the function call. But remember that the more groups you split your data into, the fewer samples in each group which will usually lead to wider confidence/credible intervals.

In some cases it may be appropriate to use a regression framework to estimate prevalence and adjust for variables (such as year) instead of stratifying. The following fits a logistic-type regression model treating region as a categorical variable and Year as an ordinal (with linear trend on the logit scale). The inputs follow the same pattern as the ```glm``` function, with an additional argument for the the pool sizes.

```R
Model <- PoolReg(Result ~ Year + Region, SimpleExampleData, NumInPool)
summary(Model)
```

We can use the ```getPrevalence``` function to extract or predict prevalence based off the regression model:
```
getPrevalence(Model)
```

So far we have ignored the hierarchical sampling structure implicit in our data (i.e. villages within regions and sites within villages). As there is likely to be some spatial variation in prevalence between sample sites it is best practice to account for this. One way to do this is to use a hierarchical model. Failing to do so may produce biased or overconfident estimates of the prevalence (i.e. confidence intervals will be unreasonably narrow). This is straightforward with ```PoolTestR```.

The hierarchical counterpart to ```PoolPrev``` is ```HierPoolPrev```, which takes a single additional argument with the names of the columns defining the hierarchy. Note: the function assumes that in the data every location is uniquely identified, e.g. village ID for village 1 in region A should be something like "A-1", not simply "1". To estimate the prevalence for the whole population adjusting for hierarchical sampling frame but ignoring Year:

```R
PrevByHier <- HierPoolPrev(SimpleExampleData, Result, NumInPool,
                           c("Region","Village","Site"))
PrevByHier
```

To also stratify by year

```R
PrevByYearHier <- HierPoolPrev(SimpleExampleData, Result, NumInPool, 
                               c("Region","Village","Site"), Year)
PrevByYearHier
```

The same kind of adjustments can be conducted in a regression framework, using mixed effect regression models. The following fits a mixed-effect regression model with Year as a fixed/population effect with a linear trend on the logit scale and Region, Village, and Site as nested random/group effects:

```R
HierModel <- PoolReg(Result ~ Year + (1|Region/Village/Site), SimpleExampleData, NumInPool)
summary(HierModel)
getPrevalence(HierModel)
```



