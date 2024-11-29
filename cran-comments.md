## Test environment
* local Mac-OS Sonoma 14.6 arm64-bit install, R 4.4.1

## R CMD check results
There were no ERRORs or WARNINGs. 

There may be 5 NOTEs depending on platform (and pseudo-random number generation):

```
❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
```

- GNU make is necessary for the Stan models included in this package.

```
❯ checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 10s
```

- **Occasionally**, depending on starting conditions for MCMC sampling, some 
examples run a little over the 5 or 10 second limit depending on the platform 
(but always <10% over the limit in my experience)

```
❯ checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'RcppParallel' 'rstantools'
  All declared Imports should be used.
```

- PoolTestR *does* use both of these packages, so this NOTE is erroneous.

```
❯ checking for future file timestamps ... NOTE
  unable to verify current time
```

- The check command relies on an external resource to check the current time. 
This note occurs when the API call to check the current time fails (see 
[these](https://stat.ethz.ch/pipermail/r-package-devel/2020q3/005931.html) 
[posts](https://stat.ethz.ch/pipermail/r-package-devel/2020q3/005930.html) on 
the R-pkg-devel mailing list)
- This NOTE is not an issue with our package and should be resolved when the API
call succeeds

```
❯ installed package size ... NOTE
```

- This note was seen in previous versions of PoolTestR. 
- The large files appear to be in ```libs```, but the size appears to be highly 
platform dependent. 
- On my local build this is only ~3 MB but in others it is >40 MB. I'm not sure 
how to avoid this issue and I am guessing it is largely outside of the scope of 
my package.

## Notes in progress
```
❯ NOTE Found the following sources/headers with CR or CRLF line endings:
	src/stanExports_HierPoolPrevIndividualSD.cc
	src/stanExports_HierPoolPrevTotalSD.cc
	src/stanExports_PoolPrev.cc
  Some Unix compilers require LF line endings.
```
- Potentially an issue moving between Windows and Mac-OS (dev team uses 
different OSs). Updated git config to `core.autocrlf input` to convert CRLF to 
LF on commit

```
❯ checking compilation flags used ... 
  NOTE Compilation used the following non-portable flag(s):
  '-Wa,-mbig-obj' '-march=native'
```


## Downstream dependencies
PoolTestR has no downstream dependencies.
