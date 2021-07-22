# PoolTestR v0.1.2 (Release date: 2021-07-XX)

A couple bug fixes:
* corrections to the Jeffrey's prior in PoolPrev
* improved numerical stability of hierarchical models -- previous implementation was causing initialisation of MCMC to fail in some edge cases

A few minor improvements:
* Allow user to specify level of confidence intervals and credible intervals
* option for getPrevalence to return the posterior median as the point estimate (instead of the posterior mean) for Bayesian models with with PoolRegBayes
* Implement PoolRegBayes with a logit link function as custom family in brms. This allows for better post-processing for the results of PoolRegBayes -- e.g. simulating from the model, leave-one-out cross-validation, posterior predictive checks. see brms for details
* Allow users to pass more control variables to MCMC sampling routines across PoolRegBayes, HierPoolPrev, and PoolPrev
* Allows users to specify the scale parameter for the half-Cauchy hyper-prior or the standard deviations of the random effect terms in HierPoolPrev. Also reduced the default value of this hyperprior from 25 (very diffuse) to 2 (weakly informative).
This is now very comprable to the equivalent default hyperprior

# PoolTestR v0.1.1 (Release date: 2021-02-13)

Minor patch so that the package works across more platforms (namely solaris)


# PoolTestR v0.1.0 (Release date: 2021-02-08)

This is our first official release! Please see the github site (https://github.com/AngusMcLure/PoolTestR#pooltestr) for a basic crash course on using the package. An upcoming (open access) journal article will go into further detail. A preprint can be accessed at https://arxiv.org/abs/2012.05405. I'll post a link to the article when published.
