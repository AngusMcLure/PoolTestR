#' A synthetic dataset for pooled testing
#'
#'
#' This dataframe contains the 'true' values of prevalence for each, site,
#' village, region and year used to generate the synthetic dataset
#' \code{ExampleData}
#'
#' The 'true' model can be summarised in formula notation as:
#'
#' Result ~ Region + Year + (1|Village) + (0 + Year|Village) + (1|Site)
#'
#' where the coefficient for Year is log(0.8), the standard deviation for
#' intercept random effects for village and site are both 0.5, the standard
#' deviation for the year random effect for village is 0.2 and the random
#' effects are all uncorrelated/independent.
#'
#' @format A data frame with 900 rows and 7 variables: \describe{
#'   \item{Region}{ID of the region the pool was taken from. "A", "B", or "C"}
#'   \item{Village}{ID of village pool was taken from. Includes name of region
#'   e.g. "B-3" is village 3 from region B} \item{Site}{ID of village pool was
#'   taken from. Includes name of region and village e.g. "B-3-7" is site 7 from
#'   village 3 from region B} \item{Year}{Year of sampling. Years are 0, 1, or
#'   2} \item{PrevalenceRegion}{'True' average prevalence in the region (in that
#'   year)} \item{PrevalenceVillage}{'True' average prevalence in the village
#'   (in that year)} \item{PrevalenceSite}{'True' prevalence at that site (in
#'   that year)} }
"TruePrev"
