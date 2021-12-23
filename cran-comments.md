## Test environments
* local macOS install, R 4.1.2
* win-builder (development, release)
* I have been unable to access R-hub due to outstanding SSL issues and do not have a linux build set up in my office. Apologies if this causes issues.

## R CMD check results
There were no ERRORs or WARNINGs. 

There may be 2 NOTEs depending on platform (and psuedo-random number generation):
  
* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
* Examples with CPU (user + system) or elapsed time > 10s
  Occasionally, depending on starting conditions for MCMC sampling, some examples run a little over 10 seconds (always <11s in my experience though)
  
  
## Downstream dependencies
PoolTestR has no downstream dependencies
