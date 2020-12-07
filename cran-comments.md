## Test environments
* local OS X install, R 4.0.3
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There should be 2 NOTEs:
* checking CRAN incoming feasibility ... NOTE
  This is a new submission.
  Possibly mis-spelled words in DESCRIPTION:
  * PoolScreen -- this is the name of a software application
  * xenomonitoring -- this is techinical term for surveillance of animals for signs of diseases in human populations
  
* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
  
  My understanding is that requirement is an issue for Solaris installations. However I believe stan, which is a major dependancy of PoolTestR, unfortunately does not support Solaris well, so I don't think there's a workaround that I can implement 
  

## Downstream dependencies
As a new package, PoolTestR has no downstream dependancies
