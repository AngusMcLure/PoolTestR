## Test environments
* local macOS install, R 4.1.2
* win-builder (development)
* R-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub: Fedora Linux, R-devel, clang, gfortran
* I have been having issues with other R-hub checks E.g. Windows Server builds fail to compile (but no problems on win-builder). Debian Linux, R-devel, GCC ASAN/UBSAN fails because it can't find the dependency packages, but the missing packages include really important/popular/well-maintained packages like ```Rcpp```. I'm guessing these are problems with R-hub rather than the PoolTestR, but I cannot confirm. Apologies if this causes problems.

## R CMD check results
There were no ERRORs or WARNINGs. 

There may be 4 NOTEs depending on platform (and psuedo-random number generation):
  
* ```checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.```
* ```Examples with CPU (user + system) or elapsed time > 10s```
  Occasionally, depending on starting conditions for MCMC sampling, some examples run a little over the 5 or 10 second limit depending on the platform (but always <10% over the limit in my experience)
* ```checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'rstantools'
  All declared Imports should be used.```
  PoolTestR *does* use ```rstantools```, so this NOTE is erroneous.
* ```installed package size ... NOTE```
  The large files appear to be in ```libs```, but the size appears to be highly platform dependant. On my local build this is only 3 MB but in others it is >40 MB. I'm not sure how to avoid this issue and I am guessing it is largely outside of the scope of my package.
  
  
## Downstream dependencies
PoolTestR has no downstream dependencies
