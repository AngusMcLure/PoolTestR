#' A synthetic dataset for pooled testing
#'
#'
#' A synthetic dataset mimicking a realistic hierarchical sampling frame.
#' Simulated samples are taken from across three regions (A, B, and C) in which
#' the vectors have a low (0.5\%), medium (2\%), and high (4\%) prevalence of
#' the marker of interest. Ten villages are chosen within each region, and traps
#' are placed at ten sites within each village. Every site is sampled once a
#' year over three years (0, 1, and 2). Prevalence is not uniform within each
#' region or over time. At baseline (year 0), prevalence varies between villages
#' within each region around the mean for the region, and prevalence varies
#' between sites within each village around the mean for the village.
#' Consequently though the prevalence is different for each site, two sites
#' within the same village are likely to have a more similar prevalence than two
#' sites in different villages, or two sites in different regions. On average
#' the prevalence is declining over time (odds ratio of 0.8 per year), however,
#' the growth rate varies between villages. Consequently two sites in different
#' villages with similar prevalence at baseline may have different prevalence by
#' the third year, and prevalence may go up in some villages. Each year the
#' traps at each site catch a negative binomial number (mean 200, dispersion 5)
#' of vectors. The catch size at each site and year is independent. Each year,
#' the catches at each site are pooled into groups of 25 with an additional pool
#' for any remainder (e.g. a catch of 107 vectors will be pooled into 4 pools of
#' 25 and one pool of 7). Test results on each pool are simulated assuming the
#' test has perfect sensitivity and specificity.
#'
#' The 'true' model can be summarised in formula notation as:
#'
#' Result ~ Region + Year + (1+Year|Village) + (1|Site)
#'
#' where the coefficient for Year is log(0.8), the standard deviation for
#' intercept random effects for village and site are both 0.5, the standard
#' deviation for the year random effect for village is 0.2 and the random
#' effects are all uncorrelated/independent.
#'
#' @format A data frame with 6 variables: \describe{
#'   \item{NumInPool}{Number of specimens in pool. Range = 1:25}
#'   \item{Region}{ID of the region the pool was taken from. "A", "B", or "C"}
#'   \item{Village}{ID of village that pool was taken from. Includes name of region e.g. "B-3" is village 3 from region B}
#'   \item{Site}{ID of site that pool was taken from. Includes name of region and village e.g. "B-3-7" is site 7 from village 3 from region B}
#'   \item{Result}{Result of test on pool; 0 = negative, 1 = positive}
#'   \item{Year}{Year of sampling. Years are 0, 1, or 2} }
"ExampleData"
