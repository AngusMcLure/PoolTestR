## Test environments
* local OS X install, R 4.0.3
* win-builder (old, release, development)
* solaris

#Resubmission
This is a minor patch in response from the CRAN team who noted that the package was not installing correctly on r-devel-windows-ix86+x86_64, r-patched-solaris-x86, and r-oldrel-windows-ix86+x86_64.

* The problem for solaris (related to compilation of C++ code) has been fixed (by avoiding pow() with int signatures)
* The CRAN installation error for r-oldrel-windows-ix86+x86_64 was: "there is no package called 'crayon'". Though my package never calls functions from crayon directly, one of the dependencies depends on crayon. However, I tried to reproduce this on win-builder but installation proceeded without error. I have checked the relevant fields in the DESCRIPTION and there appears to be no problems.
* The CRAN installation error for r-oldrel-windows-ix86+x86_64 was also: "there is no package called 'crayon'". When I tried to reproduce this on win-builder the installation came up with a similar error but for the package 'sp' rather than 'crayon' ('sp', like 'crayon', is not a direct dependency of this package). Again, I have checked the relevant fields in the DESCRIPTION and there appears to be no problems. When I tried again on win-builder there was no error.

## R CMD check results
There were no ERRORs or WARNINGs. 

There should be 2 NOTEs depending on platform (and psuedo-random number generation):
  
* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
* Examples with CPU (user + system) or elapsed time > 10s
  Occasionally, depending on starting conditions for MCMC sampling, some examples run a little over 10 seconds (always <11s in my experience though)
  
  
## Downstream dependencies
As a new package, PoolTestR has no downstream dependencies
