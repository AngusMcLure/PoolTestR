## Test environment
* local windows 10 64-bit install, R 4.3.1

## R CMD check results
There were no ERRORs or WARNINGs. 

There may be 4 NOTEs depending on platform (and psuedo-random number generation):
  
* ```checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.```
* ```Examples with CPU (user + system) or elapsed time > 10s```
  **Occasionally**, depending on starting conditions for MCMC sampling, some examples run a little over the 5 or 10 second limit depending on the platform (but always <10% over the limit in my experience)
* ```checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'RcppParallel' 'rstantools'
  All declared Imports should be used.```
  PoolTestR *does* use both of these packages, so this NOTE is erroneous.
* ```installed package size ... NOTE```
  The large files appear to be in ```libs```, but the size appears to be highly platform dependent. On my local build this is only ~3 MB but in others it is >40 MB. I'm not sure how to avoid this issue and I am guessing it is largely outside of the scope of my package.
  
  
## Downstream dependencies
PoolTestR has no downstream dependencies
