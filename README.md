# PoolTestR

<!-- badges: start -->
[![R-CMD-check](https://github.com/AngusMcLure/PoolTestR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AngusMcLure/PoolTestR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/AngusMcLure/PoolTestR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/AngusMcLure/PoolTestR?branch=master)
<!-- badges: end -->

PoolTestR is an R package with tools for working with pooled or grouped samples, 
initially inspired by the stand-alone software, 'PoolScreen'. The package can be
used to perform the same analyses as PoolScreen i.e. estimate the prevalence of 
a marker in the population based on tests performed on pooled samples. In our 
package, the estimates of prevalence can also be adjusted to account for the 
hierarchical sampling designs that are often used for molecular xenomonitoring 
studies. Taking this a step further, our package enable users to perform to 
mixed effect regression to identify covariates associated with the outcome of 
interest, estimate odds ratios, and predict prevalence. Analyses are available 
in both frequentist and Bayesian frameworks. 

More features are planned or in the works: spatial mapping; models for combining
and comparing results from human-based and vector-based surveillance, 
adjustments for imperfect test specificity/sensitivity; functions for helping 
with optimal experimental design; functions for inferring whether a disease has 
been locally eliminated from a series of pooled tests over time. Suggestions and
contributions are welcome.

The details of our package are described in an 
[article published in Environmental Modelling and Software](https://doi.org/10.1016/j.envsoft.2021.105158). 
We are happy to share a pdf of this article with you 
[on request](mailto:angus.mclure@anu.edu.au).

We also have a [graphical user interface](#graphical-user-interface) to 
PoolTestR available called [PoolTools](https://github.com/AngusMcLure/PoolTools).
PoolTools is a shiny web app that facilitates analyses of pool-tested data using 
functions from PoolTestR.

If you find this package helpful, please 
[let us know](mailto:angus.mclure@anu.edu.au) -- we'd love to hear how it's 
being used.

## Installation

There are three ways to download and install PoolTestR: 

- From [CRAN](https://CRAN.R-project.org/package=PoolTestR)
- From [R-universe](https://angusmclure.r-universe.dev/PoolTestR)
- From [GitHub](https://github.com/AngusMcLure/PoolTestR) (which requires installation from source)

To use the full functionality of the package, including running Bayesian 
regression models, you will need to install PoolTestR from source (see the
[Advanced Installation](#advanced-installation) section below).

We release official versions of PoolTestR on 
[CRAN](https://CRAN.R-project.org/package=PoolTestR) and on 
[GitHub](https://github.com/AngusMcLure/PoolTestR/releases). 

The development versions of PoolTestR can be used by installing PoolTestR from
[R-universe](https://angusmclure.r-universe.dev/PoolTestR) or 
[GitHub](https://github.com/AngusMcLure/PoolTestR/).

Instructions to install PoolTestR through each of these methods are listed 
below. These instructions have been tested on Windows, Linux, and Mac (both 
Intel and Apple Silicon chips). Please 
[get in touch](mailto:angus.mclure@anu.edu.au) if you have problems with 
installation.

### Basic Installation

#### CRAN

To install the latest official release of 
[PoolTestR from CRAN](https://CRAN.R-project.org/package=PoolTestR) (the 
official repository for R packages), use the following command in R:

```R
# Install PoolTestR from CRAN
install.packages("PoolTestR")
```

This will install an old version of PoolTestR that lacks the latest features. 
In addition, Bayesian regression modelling (via function ```PoolRegBayes```) 
may require you to install additional software.

#### R-universe

You can download development versions of PoolTestR from the 
[angusmclure](https://angusmclure.r-universe.dev) universe on R-universe. 

[Visit PoolTestR on R-universe here.](https://angusmclure.r-universe.dev/PoolTestR)

R-universe contains binaries for the current `master` branch of the PoolTestR 
GitHub repository. Usually, the binaries on R-universe are updated within a few 
hours of each new commit to the `master` branch of PoolTestR. 

```R
# Install PoolTestR from R-universe
install.packages("PoolTestR", repos = c("https://angusmclure.r-universe.dev"))
```

### Advanced Installation
To use the full functionality of the package, including running Bayesian 
regression models, you will need either `RTools` (for Windows) or `macrtools` 
(for Mac OS). You will also need the latest versions of two packages, 
`StanHeaders` and `rstan`.  

#### 1. Install necessary software
Download the necessary software for your operating system:

**Windows:** 

Install `RTools for Windows` following the instructions 
[here](https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html). 

**Mac OS:** 

Install the R package `macrtools` following the instructions 
[here](https://github.com/coatless-mac/macrtools?#quick-start)

```R
# From macrtools documentation, retrived 18/10/2024
# install.packages("remotes")
remotes::install_github("coatless-mac/macrtools")
```

Use `macrtools` to install the required components (XCode CLI, gfortran, and R 
Development binaries from the Recipes project) following the instructions 
[here](https://github.com/coatless-mac/macrtools?#quick-start)

```R
# From macrtools documentation, retrived 18/10/2024
macrtools::macos_rtools_install()
```

#### 2. Install `Stanheaders` and `rstan`

Install latest version of `Stanheaders` and `rstan` with:

```R
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

#### 3. Install the current version of `PoolTestR`.
To also install the most up-to-date version of `PoolTestR` directly from GitHub, 
in addition to the above steps also run:

```R
install.packages("devtools") # you can skip this if you already have devtools installed
options(timeout = 300) # increases time-limit for downloading package to 300 seconds (for slow connections)
devtools::install_github("AngusMcLure/PoolTestR")
```

If installing from source (the only option if installing from GitHub) this will 
take some time (really depends on your machine, perhaps up to an hour) and 
probably produce a lot of text in your R console. The text is related to the 
compilation of stan source code. 

If installing from CRAN (basic installation) installation should be very fast 
and not generate any warning messages. 

**Note:** In the past, some Windows users needed to adjust some settings (in the
`Makevars` file) to install the package correctly, because of issues in other 
packages on which PoolTestR relies. These other packages have addressed these 
issues in most cases, so manual adjustments *shouldn't* be needed. However, 
future updates may break this. 

**Please [let us know](mailto:angus.mclure@anu.edu.au) if you have issues with 
installation.**

## Usage

### Loading PoolTestR and preparing data
The following provides some examples of how to use the core functionality of the
package to estimate prevalence. You can copy and paste the code into your R 
console to see it run.

Start by loading the package: (If this doesn't work this means installation was 
not successful. Please [let us know](mailto:angus.mclure@anu.edu.au) if this 
happens for you)
```R
library(PoolTestR)
```

Now it's time to load up data into R using `read.csv()` or a similar function. 
Each line of the dataset should contain the details of a single pooled test. At 
minimum this must include:
 * the the number of specimens in the pool
 * and the result of the test

Instead of working with real data, for this example we'll be using a simulated 
dataset called ```SimpleExampleData``` that's included with the package. The 
synthetic dataset consists of pools (sizes 1, 5, or 10) taken from 4 different 
regions and 3 different years. Within each region 4 villages are selected, and 
within each selected village 8  sites are selected for the collection of 
specimens, i.e. a hierarchical sampling frame. Take a look at the first few rows
of the data:

```R
head(SimpleExampleData)
```

### Estimating prevalence
We begin by using the function ```PoolPrev``` to estimate the prevalence across 
the whole dataset i.e. a single prevalence for all locations and years. The 
first argument to ```PoolPrev``` is the data (in our case 
```SimpleExampleData```). The second argument is the name of the column in the 
data containing the result of the test (in our case ```Result```): the entries 
in this column must be 0 for negative pool tests and 1 for a positive pool tests
and cannot be missing. The third argument is the name of the column with the 
number of specimens/isolates/insects in each pool (in our case ```NumInPool```)

```R
PrevWholeDataset <- PoolPrev(SimpleExampleData,Result,NumInPool)

PrevWholeDataset 
```
The output contains a maximum likelihood estimate of the prevalence with 95% 
confidence intervals and a Bayesian estimate (Jeffrey's prior) with 95% credible
intervals. It also contains the total number of pools and the number of these 
that were positive.

### Stratifying prevalence estimates
PoolTestR allows us to use different variables within the dataset to stratify
(i.e., partition) data before estimating prevalence.

If we want to estimate prevalence separately for each region we simply include 
```Region``` as an additional argument

```R
PrevByRegion <- PoolPrev(SimpleExampleData, Result, NumInPool, Region)
PrevByRegion
```

If we want to estimate prevalence at the level of villages:

```R
PrevByVillage <- PoolPrev(SimpleExampleData, Result, NumInPool, Village)
PrevByVillage
```

Similarly if we want to estimate prevalence separately for each year (but 
ignoring differences between places):

```R
PrevByYear <- PoolPrev(SimpleExampleData, Result, NumInPool, Year)
PrevByYear
```

If we want to estimate prevalence separately for each combination of region AND 
year:

```R
PrevByRegionYear <- PoolPrev(SimpleExampleData, Result, NumInPool, Region, Year)
PrevByRegionYear
```
### Applying a regression framework
If we had more variables we wanted to group the data by (e.g. climate, season, 
sex, age...) we could keep on adding the appropriate column names to the 
function call. But remember that the more groups you split your data into, the 
fewer samples in each group which will usually lead to wider confidence/credible
intervals.


In some cases it may be appropriate to use a regression framework to estimate 
prevalence and adjust for variables (such as year) instead of stratifying. The 
following fits a logistic-type regression model treating region as a categorical
variable and Year as an ordinal (with linear trend on the logit scale). The 
inputs follow the same pattern as the ```glm``` function, with an additional 
argument for the the pool sizes.

```R
Model <- PoolReg(Result ~ Year + Region, SimpleExampleData, NumInPool)
summary(Model)
```

We can use the ```getPrevalence``` function to extract or predict prevalence 
based off the regression model:

```
getPrevalence(Model)
```

### Estimating prevalence from hierarchical/clustered data
So far we have ignored the hierarchical sampling structure implicit in our data 
(i.e. villages within regions and sites within villages). As there is likely to 
be some spatial variation in prevalence between sample sites it is best practice
to account for this. One way to do this is to use a hierarchical model. Failing 
to do so may produce biased or overconfident estimates of the prevalence (i.e. 
confidence intervals will be unreasonably narrow). This is straightforward with 
```PoolTestR```.

The hierarchical counterpart to ```PoolPrev``` is ```HierPoolPrev```, which 
takes a single additional argument with the names of the columns defining the 
hierarchy. Note: the function assumes that in the data every location is 
uniquely identified, e.g. village ID for village 1 in region A should be 
something like "A-1", not simply "1". To estimate the prevalence for the whole 
population adjusting for hierarchical sampling frame but ignoring Year:

```R
PrevByHier <- HierPoolPrev(SimpleExampleData, Result, NumInPool,
                           c("Village","Site"))
PrevByHier
```

To also stratify by Year

```R
PrevByYearHier <- HierPoolPrev(SimpleExampleData, Result, NumInPool, 
                               c("Village","Site"), Year)
PrevByYearHier
```

The same kind of adjustments can be conducted in a regression framework, using 
mixed effect regression models. The following fits a mixed-effect regression 
model with Year and Region as fixed/population effects with a linear trend on 
the logit scale and Village and Site random/group effects:

```R
HierModel <- PoolReg(Result ~ Year + Region + (1|Village) + (1|Site), SimpleExampleData, NumInPool)
summary(HierModel)
getPrevalence(HierModel)
```

## Graphical User Interface
[PoolTools](https://github.com/AngusMcLure/PoolTools) is a web app to analyse 
and design surveys with pool-tested data, using the R packages PoolTestR and 
[PoolPoweR](https://github.com/AngusMcLure/PoolPoweR). To analyse data in 
PoolTools, select the ```Analyse``` option. Documentation and tutorials are 
available at the [PoolTools wiki](https://github.com/AngusMcLure/PoolTools/wiki).

PoolTools makes certain 
[statistical assumptions](https://github.com/AngusMcLure/PoolTools/wiki/Statistical-assumptions-and-default-values) 
during data analysis. If the PoolTools default options are not appropriate for 
your analysis, we recommend using the PoolTestR R package instead.
