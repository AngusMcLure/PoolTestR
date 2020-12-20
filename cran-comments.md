## Test environments
* local OS X install, R 4.0.3
* win-builder (release, development)

## Resubmission
This is a resubmission. In this version I have:

* Addressed comments regarding DESCRIPTION file: Removed redundant words in title and description; quoted the name of the existing software 'PoolScreen' in single quotes
* Provided, as requested, a reference to a (pre-print) article describing underlying methodology in the package (reference will be updated to a doi once published)
* Provided, as requested, an (indirect) URL reference to 'PoolScreen'. 'PoolScreen' is the default software that researchers and field epidemiologists have been using for analysing molecular xenomonitoring data. However, 'PoolScreen' is only available by request from the  author (Charles Katholi, a retired academic). Moreover 'PoolScreen' is not open source, and is platform-specific (Windows only). These barriers to access, together with practical and statistical limitations of 'PoolScreen', are what prompted me to create this R package. As 'PoolScreen' is only available by request from the Prof Katholi, rather than distribute his contact details directly, I have provided a link to a website hosted by the university of Alabama (where the Prof Katholi is now Emeritus), which lists statistical software produced at the university, including a link labelled 'PoolScreen' which directs to Prof Katholi's staff page.
* Made minor improvements to one of the functions to allow more flexible priors.


## R CMD check results
There were no ERRORs or WARNINGs. 

There should be 2 NOTEs depending on platform:
* checking CRAN incoming feasibility ... NOTE
  This is a new submission.
  Possibly mis-spelled words in DESCRIPTION:
  * PoolScreen -- this is the name of a software application
  * xenomonitoring -- this is technical term for surveillance of animals for signs of diseases in human populations
  
* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
  
  My understanding is that requirement is an issue for Solaris installations. However I believe stan, which is a major dependancy of PoolTestR, unfortunately does not support Solaris well, so I don't think there's a workaround that I can implement 
  

## Downstream dependencies
As a new package, PoolTestR has no downstream dependancies
