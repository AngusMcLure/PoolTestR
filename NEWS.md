# PoolTestR v0.2.0 (Release date: 2024-11-XX)

Caitlin Cherryh has joined the development team and has been working on 
improving readability of outputs, documentation, and testing.

This update includes an option for `PoolPrev()` to skip the calculation of 
Bayesian estimates. When using `bayesian = FALSE`, only MLE and likelihood ratio 
confidence intervals will be calculated, substantially speeding up this function
(perhaps x100).

This updates also removes one source of bias from prevalence estimates returned 
for any hierarchical models. This effects the results of `HierPoolPrev()` and 
`getPrevalence()` applied to models with random effects. Under the update, 
prevalence estimates will typically slightly increase, though the difference
will not be notable if the sample size is large and there is little clustering.

Previous estimates of prevalence did not marginalise out the random effects when
calculating population-level prevalence, but as of this version, random effects 
are marginalised out. Due to the complexity introduced by this bias-correction 
we now longer-support specifying nested surveys using `~(1|Layer1/Layer2)` and 
recommend using the format `~(1|Layer1) + (1|Layer2)` which should be equivalent 
as long as each level in `Layer2` is unique --- i.e. the format already required 
for `HierPoolPrev()`.

Due to the complexity introduced by this bias-correction, the way of specifying 
priors for `HierPoolPrev()` has been updated. Priors for `HierPoolPrev()` are 
now directly on the real-scale (logit-transformed) parameters, rather than 
prevalence directly. We have also updated the default priors for `PoolRegBayes()` 
for regression parameters, as we believe the previous priors were too diffuse 
(`normal(0,100)`). The defaults for the centered predictors are now 
`student(6,0,1.5)`.

`HierPoolPrev()` now has functionality to return estimate of intracluster 
correlation coefficients (ICC) at one or more levels of clustering.

`HierPoolPrev()` and `PoolPrev()` now have custom output classes (inheriting 
from tibble, the previous class for these outputs). This has allowed us 
(Caitlin) to add pretty-print functions these outputs which are much more human 
readable. Saving the output with `write.csv()` or similar will still return a 
detailed, machine-readable output.

Both `PoolPrev()` and `HierPoolPrev()` have been updated to improve point 
estimates. New default function values have been added for both `PoolPrev()` and 
`HierPoolPrev()`. The default for the new `robust` parameter is `robust = TRUE`, 
which means the point estimate of prevalence is the posterior median. In both 
functions, the default value for `all.negative.pools = 'zero'`, meaning when all
pools are negative, the point estimate and the. lower bound for the interval 
will be 0.

# PoolTestR v0.1.3 (Release date: 2022-07-XX)
This is patch to fix a bug affecting `PoolPrev()`. The bug affected the maximum 
likelihood estimates (MLE) and likelihood ratio confidence intervals (LR-CIs) 
of prevalence when the default Jeffrey's prior was being used. The bug would 
usually make the MLE and LR-CIs much closer to the Bayesian estimates than they 
should have been. As both sets of estimates are valid, the results will still 
have been approximately correct.

This patch also includes an option, `replicate.poolscreen` (default to `FALSE`), 
for `PoolPrev()`. This options changes the way the likelihood ratio confidence 
intervals are calculated. With `replicate.poolscreen = TRUE`, PoolPrev will more 
closely reproduce the results produced by Poolscreen. We believe that our 
implementation of these intervals is more correct so would recommend that users 
continue to use the default (`replicate.poolscreen = FALSE`), but this option 
may be helpful for those who are trying to compare results across the two 
programs.

# PoolTestR v0.1.2 (Release date: 2021-07-XX)
We have published a paper about PoolTestR in *Environmental Modelling and Software* 
now available at https://doi.org/10.1016/j.envsoft.2021.105158. If you find this 
package useful, please let us know and/or cite our paper!

A couple bug fixes:

* Corrections to the Jeffrey's prior in PoolPrev
* Improved numerical stability of hierarchical models -- previous implementation was causing initialisation of MCMC to fail in some edge cases

A few improvements:

* Allow user to specify level of confidence intervals and credible intervals (default level is 0.95, for 95% intervals)
* Option for `getPrevalence()` to return the posterior median as the point estimate (instead of the posterior mean) for Bayesian models with with `PoolRegBayes()`
* Implement `PoolRegBayes()` with a logit link function as custom family in `brms`. This allows for better post-processing for the results of `PoolRegBayes()` -- e.g. simulating from the model, leave-one-out cross-validation, posterior predictive checks. See `brms` for details
* Allow users to pass more control variables to MCMC sampling routines across `PoolRegBayes()`, `HierPoolPrev()`, and `PoolPrev()`
* Allows users to specify the scale parameter for the half-Cauchy hyper-prior or the standard deviations of the random effect terms in `HierPoolPrev()`. Also reduced the default value of this hyperprior from 25 (very diffuse) to 2 (weakly informative). This is now very comparable to the equivalent default hyper-prior for `brms` models including those fit using `PoolRegBayes()` (i.e. a half t distribution three degrees of freedom )

# PoolTestR v0.1.1 (Release date: 2021-02-13)

Minor patch so that the package works across more platforms (namely solaris)


# PoolTestR v0.1.0 (Release date: 2021-02-08)

This is our first official release! Please see the github site (https://github.com/AngusMcLure/PoolTestR#pooltestr) for a basic crash course on using the package. An upcoming (open access) journal article will go into further detail. A preprint can be accessed at https://arxiv.org/abs/2012.05405. I'll post a link to the article when published.
