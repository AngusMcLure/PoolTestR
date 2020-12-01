## Test environments
* local OS X install, R 4.0.3
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
  
  My understanding is that requirement is an issue for Solaris installations. However I believe stan, which is a major dependancy of this PoolTestR, unfortunately does not support Solaris well, so I don't think there's a workaround that I can implement 
  
* New submission

## Downstream dependencies
As a new package, PoolTestR has no downstream dependancies
