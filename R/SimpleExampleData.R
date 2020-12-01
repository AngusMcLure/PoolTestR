#' A synthetic dataset for pooled testing
#'
#'
#' The simple synthetic dataset consisting of pools (sizes 1, 5, or 10) taken from 4 different
#' regions and 3 different years. Within each region specimens are collected
#' at 4 different villages, and within each village specimens are collected at 8
#' different sites.
#'
#' @format A data frame with 1152 rows and 6 variables:
#' \describe{
#'   \item{NumInPool}{Number of specimens in pool. Takes values 1, 5, or 10.}
#'   \item{Region}{ID of the region the pool was taken from. "A", "B", "C", or "D"}
#'   \item{Village}{ID of village pool was taken from. Includes name of region e.g. "B-3" is village 3 from region B}
#'   \item{Site}{ID of village pool was taken from. Includes name of region and village e.g. "B-3-7" is  site 7 from village 3 from region B}
#'   \item{Result}{Result of test on pool; 0 = negative, 1 = positive}
#'   \item{Year}{Year of sampling. Years are 0, 1, or 2}
#' }
"SimpleExampleData"
